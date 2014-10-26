{- File format: https://java.net/downloads/heap-snapshot/hprof-binary-format.html -}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative             ((*>), (<$>), (<*), (<*>))
import           Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString      as AP
import qualified Data.Attoparsec.ByteString.Lazy as APL
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.Map.Strict                 (Map, fromList)
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import           Data.Text.Encoding              (decodeUtf8With)
import qualified Data.Text.IO                    as TIO
import           Data.Word                       (Word32, Word64, Word8)
import           Debug.Trace                     (trace, traceShowId)

type HprofID = Word64

data HprofHeader = HprofHeader {
    headerFormatString   :: B.ByteString
  , headerIdentifierSize :: Int
  , headerTimestamp      :: Int
  } deriving(Show)

data HprofRecord = HprofRecord {
    recordTag     :: Word8
  , recordTime    :: Word32
--  , recordContent :: B.ByteString
  , recordContent :: HprofContent
  } deriving(Show)

data HprofContent =
    HprofString !HprofID !B.ByteString
  | HprofLoadClass !Int !HprofID !Int !HprofID
  | HprofUnloadClass !Int
  | HprofStackFrame !HprofID !HprofID !HprofID !HprofID !Int !Int
  | HprofStackTrace !Int !Int ![HprofID]
  | HprofAllocSites
  | HprofHeapSummary
  | HprofStartThread
  | HprofEndThread
  | HprofHeapDump [HprofSegmentContent]
  | HprofHeapDumpEnd
  | HprofCpuSamples
  | HprofControlSettings
  | HprofAny
  deriving Show

data HprofSegmentContent =
    HprofRootUnknown
  | HprofRootJniGlobal
  | HprofRootJniLocal
  | HprofRootJavaFrame
  | HprofRootNativeStack
  | HprofRootStickyClass
  | HprofRootThreadBlock
  | HprofRootMonitorUsed
  | HprofRootThreadObject
  | HprofClassDump
  | HprofInstanceDump HprofID HprofID Word32
  | HprofObjectArrayDump
  | HprofPrimitiveArrayDump
  deriving Show

parseHeader :: AP.Parser HprofHeader
parseHeader = do
  formatString <- AP.takeWhile (/= 0)
  _ <- AP.word8 0
  identifierSize <- anyWord32be
  timestamp <- anyWord64be
  return $ HprofHeader formatString (fromIntegral identifierSize) (fromIntegral timestamp)

parseRecord :: AP.Parser HprofRecord
parseRecord = do
  tag <- AP.anyWord8
  time <- anyWord32be
  len <- anyWord32be
  content <- parseRecordContent tag len
  return $ HprofRecord tag time content

idParser :: AP.Parser HprofID
idParser = anyWord64be

word32Parser :: AP.Parser Int
word32Parser = fromIntegral <$> anyWord32be

word64Parser :: AP.Parser Int
word64Parser = fromIntegral <$> anyWord64be

word16Parser :: AP.Parser Int
word16Parser = fromIntegral <$> anyWord16be

word8Parser :: AP.Parser Int
word8Parser = fromIntegral <$> AP.anyWord8

parseBasicType' :: Int -> AP.Parser Int
parseBasicType' t =
  case t of
   2 -> fromIntegral <$> idParser
   4 -> word8Parser
   5 -> word16Parser
   6 -> word32Parser
   7 -> word64Parser
   8 -> word8Parser
   9 -> word16Parser
   10 -> word32Parser
   11 -> word64Parser
   _ -> error $ "Failure in basic type: " <> show t

parseBasicType :: AP.Parser Int
parseBasicType = do
  t <- word8Parser
  parseBasicType' t

parseRecordSegmentContent :: Int -> AP.Parser HprofSegmentContent
parseRecordSegmentContent tag = case tag of
  0xff -> idParser *> return HprofRootUnknown
  0x01 -> idParser *> idParser *> return HprofRootJniGlobal
  0x02 -> idParser *> word32Parser *> word32Parser *> return HprofRootJniLocal
  0x03 -> idParser *> word32Parser *> word32Parser *> return HprofRootJavaFrame
  0x04 -> idParser *> word32Parser *> return HprofRootNativeStack
  0x05 -> idParser *> return HprofRootStickyClass
  0x06 -> idParser *> word32Parser *> return HprofRootThreadBlock
  0x07 -> idParser *> return HprofRootMonitorUsed
  0x08 -> idParser *> word32Parser *> word32Parser *> return HprofRootThreadObject
  0x20 -> do
    _ <- idParser
    _ <- word32Parser
    _ <- AP.count 6 idParser
    _ <- word32Parser
    constantPoolSize <- word16Parser
    _ <- AP.count constantPoolSize (word16Parser *> parseBasicType)
    staticFieldSize <- word16Parser
    _ <- AP.count staticFieldSize (idParser *> parseBasicType)
    instanceFieldSize <- word16Parser
    _ <- AP.count instanceFieldSize (idParser *> AP.anyWord8)
    return HprofClassDump
  0x21 -> do
    objectId <- idParser
    _ <- word32Parser
    classObjectId <- idParser
    fieldValues <- anyWord32be
    _ <- AP.count (fromIntegral fieldValues) word8Parser
    return $ HprofInstanceDump objectId classObjectId fieldValues
  0x22 -> do
    _ <- idParser
    _ <- word32Parser
    fieldValues <- word32Parser
    _ <- idParser
    AP.count fieldValues idParser
    return HprofObjectArrayDump
  0x23 -> do
    _ <- idParser
    _ <- word32Parser
    fieldValues <- word32Parser
    t <- word8Parser
    AP.count fieldValues (parseBasicType' t)
    return HprofObjectArrayDump
  _ -> error "Unknown type"

parseRecordSegment :: AP.Parser HprofSegmentContent
parseRecordSegment = do
  tag <- AP.anyWord8
  parseRecordSegmentContent (fromIntegral tag)

parseRecordContent :: Word8 -> Word32 -> AP.Parser HprofContent
parseRecordContent tag len = case tag of
  0x01 -> HprofString <$> idParser <*> (decodeUtf8With (\_ _ -> Just 'X') <$> AP.take (fromIntegral (len - 8)))
  0x02 -> HprofLoadClass <$> word32Parser <*> idParser <*> word32Parser <*> idParser
  0x03 -> HprofUnloadClass <$> word32Parser
  0x04 -> HprofStackFrame <$> idParser <*> idParser <*> idParser <*> idParser <*> word32Parser <*> word32Parser
  0x05 -> do
    stserialNumber <- word32Parser
    tserialNumber <- word32Parser
    nframes <- word32Parser
    frames <- AP.count nframes anyWord64be
    return $ HprofStackTrace stserialNumber tserialNumber frames
  0x06 -> AP.take (fromIntegral len) *> return HprofAllocSites
  0x07 -> AP.take (fromIntegral len) *> return HprofHeapSummary
  0x0a -> AP.take (fromIntegral len) *> return HprofStartThread
  0x0b -> AP.take (fromIntegral len) *> return HprofEndThread
  0x0c -> do
    segments <- AP.many' parseRecordSegment
    return $ HprofHeapDump segments
  0x1c -> do
    segments <- AP.many' parseRecordSegment
    return $ HprofHeapDump segments
  0x2c -> AP.take (fromIntegral len) *> return HprofHeapDumpEnd
  0x0d -> AP.take (fromIntegral len) *> do
    _ <- word32Parser
    traces <- word32Parser
    _ <- AP.count traces (word32Parser *> word32Parser)
    return HprofCpuSamples
  0x0e -> AP.take (fromIntegral len) *> return HprofControlSettings
  _ -> AP.take (fromIntegral len) *> return HprofAny

isHprofString :: HprofContent -> Bool
isHprofString (HprofLoadClass{}) = True
isHprofString _ = False

contentToText :: HprofContent -> T.Text
contentToText (HprofString _ s) = s
contentToText _ = ""

--processResult r = print (length (filter (isHprofString . recordContent) r))
processResult = mapM_ print'
  where print' (HprofRecord 0x0c _ (HprofHeapDump segments)) = mapM_ print segments
        print' r@HprofRecord{} = print r

stringMapFromResult :: [HprofRecord] -> Map HprofID B.ByteString
stringMapFromResult = fromList . catMaybes . map stringMapFromResult'
  where stringMapFromResult' (HprofRecord _ _ (HprofString id text)) = Just (id,text)
        stringMapFromResult' _ = Nothing

{-
  String-ID zu String

  Klassen-ID zu String-ID

  Wie kommt man von Instancedumps auf einen Abhängigkeitsbaum?
 -}
type StringMap = Map HprofID B.ByteString
type ClassNameMap = Map HprofID HprofID

main :: IO ()
main = do
  fileContents <- BL.readFile "test.hprof"
  case APL.parse (parseHeader *> AP.many' parseRecord) fileContents of
    APL.Fail _ _ e -> putStrLn $ "Error: " <> e
    APL.Done _ r -> processResult r
    --APL.Done _ r -> mapM_ (TIO.putStrLn . contentToText . recordContent) r
