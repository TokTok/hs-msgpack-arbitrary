-- | A MessagePack parser.
--
-- Example usage:
--   $ echo -ne "\x94\x01\xa1\x32\xa1\x33\xa4\x50\x6f\x6f\x66" | ./msgpack-parser
-- or
--   $ echo 'ObjectArray [ObjectInt 97, ObjectStr "test",  ObjectBool True]' | ./msgpack-parser
--
-- This tool performs two symmetrical functions:
--   1. It can decode binary data representing a
--      Data.MessagePack.Object into a human-readable string.
--   2. It can do the reverse: encode a human-readable string into
--      a binary representation of Data.MessagePack.Object.
--
-- No flags are required as it automatically detects which of these
-- two functions it should perform.  This is done by first assuming
-- the input is human readable.  If it fails to parse it, it then
-- considers it as binary data.
--
-- Therefore, given a valid input, the tool has the following property
--   $ ./msgpack-parser < input.bin | ./msgpack-parser
-- will output back the contents of input.bin.
--
-- In case the input is impossible to parse, nothing is output.
--
-- Known bugs:
--   - If no input is given, the tool exits with
--     "Data.Binary.Get.runGet at position 0: not enough bytes"
--   - The tool does not check that all the input is parsed.
--     Therefore, "abc" is interpreted as just "ObjectInt 97".
--
module Test.MessagePack.Parser (parse) where

import           Control.Applicative        ((<|>))
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe)
import           Data.MessagePack.Types     (Object)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           System.IO                  (hPutStr, hPutStrLn, stderr)
import           Text.Read                  (readMaybe)


display :: Int64 -> Object -> String
display len | len > 10 * 1024 = const $ show len <> " bytes (too large to display)"
display _   = show


parseBidirectional
    :: (Object -> L.ByteString)
    -> (L.ByteString -> Maybe Object)
    -> L.ByteString
    -> L.ByteString
parseBidirectional pack unpack str = fromMaybe L.empty $
    pack <$> readMaybe (L8.unpack str)
    <|>
    L8.pack . flip (++) "\n" . display (L.length str) <$> unpack str


showSpeed :: Int64 -> Double -> String
showSpeed size time =
    show (fromIntegral size / (1024 * 1024) / time) <> " MiB/s"


parse :: (Object -> L.ByteString) -> (L.ByteString -> Maybe Object) -> IO ()
parse pack unpack = do
    start <- getCurrentTime
    packed <- L.getContents
    hPutStr stderr $ "Read " <> show (L.length packed) <> " bytes"
    readTime <- getCurrentTime
    hPutStrLn stderr $ " in " <> show (diffUTCTime readTime start)

    let parsed = parseBidirectional pack unpack packed
    hPutStr stderr $ "Parsed into " <> show (L.length parsed) <> " bytes"
    unpackTime <- getCurrentTime
    hPutStrLn stderr $ " in " <> show (diffUTCTime unpackTime readTime)

    hPutStrLn stderr $ "Unpacking speed: " <> showSpeed (L.length packed) (realToFrac (diffUTCTime unpackTime readTime))

    L.putStr parsed
