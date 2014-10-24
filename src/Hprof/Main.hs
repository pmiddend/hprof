{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Attoparsec.ByteString.Lazy as APL
import Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Control.Applicative((<*),(<$>),(<*>),(*>))
import Data.Monoid((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding(decodeUtf8With)

data HprofHeader = HprofHeader {
    headerFormatString :: B.ByteString
  , headerIdentifierSize :: Int
  , headerTimestamp :: Int
  } deriving(Show)

data HprofRecord = HprofRecord {
    recordTag :: Int
  , recordTime :: Int
--  , recordContent :: B.ByteString
  , recordContent :: HprofContent
  } deriving(Show)

data HprofContent =
  HprofString Int T.Text |
  HprofLoadClass Int Int Int Int |
  HprofUnloadClass Int |
  HprofStackFrame Int Int Int Int Int Int |
  HprofStackTrace Int Int Int [Int] |
  HprofAny
  deriving Show

parseHeader :: AP.Parser HprofHeader
parseHeader = do
  formatString <- AP.takeWhile (/= 0)
  AP.word8 0
  identifierSize <- anyWord32be
  timestamp <- anyWord64be
  return $ HprofHeader formatString (fromIntegral identifierSize) (fromIntegral timestamp)

parseRecord :: AP.Parser HprofRecord
parseRecord = do
  tag <- AP.anyWord8
  time <- anyWord32be
  len <- anyWord32be
  content <- parseRecordContent (fromIntegral tag) (fromIntegral len)
  return $ HprofRecord (fromIntegral tag) (fromIntegral time) content

idParser :: AP.Parser Int
idParser = fromIntegral <$> anyWord64be

word32Parser :: AP.Parser Int
word32Parser = (fromIntegral <$> anyWord32be)

parseRecordContent :: Int -> Int -> AP.Parser HprofContent
parseRecordContent tag len = case tag of
  0x01 -> HprofString <$> idParser <*> (decodeUtf8With (\_ _ -> Just 'X') <$> AP.take (len - 8))
  0x02 -> HprofLoadClass <$> word32Parser <*> idParser <*> word32Parser <*> idParser
  0x03 -> HprofUnloadClass <$> word32Parser
  0x04 -> HprofStackFrame <$> idParser <*> idParser <*> idParser <*> idParser <*> word32Parser <*> word32Parser
  0x05 -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  0x06 -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  0x07 -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  0x0a -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  0x0b -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  0x0c -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  0x1c -> HprofStackTrace <$> word32Parser <*> word32Parser <*> word32Parser <*> AP.count ((len-12) `div` 8) idParser
  _ -> AP.take len *> return HprofAny

isHprofString :: HprofContent -> Bool
isHprofString (HprofLoadClass{}) = True
isHprofString _ = False

contentToText :: HprofContent -> T.Text
contentToText (HprofString _ s) = s
contentToText _ = ""

main :: IO ()
main = do
  fileContents <- BL.readFile "test.hprof"
  case APL.parse (parseHeader *> AP.many' parseRecord) fileContents of
    APL.Fail _ _ e -> putStrLn $ "Error: " <> e
    APL.Done _ r -> print (length (filter (isHprofString . recordContent) r))
    --APL.Done _ r -> mapM_ (TIO.putStrLn . contentToText . recordContent) r
