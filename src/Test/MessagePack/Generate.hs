module Test.MessagePack.Generate (generate) where

import           Control.Monad                    (when)
import qualified Data.ByteString.Lazy             as L
import           Data.Int                         (Int64)
import           Data.MessagePack.Arbitrary       ()
import           Data.MessagePack.Types           (Object (..))
import           Data.Time.Clock                  (diffUTCTime, getCurrentTime)
import           System.Environment               (getArgs)
import           System.IO                        (hPutStr, hPutStrLn, stderr)
import           Test.QuickCheck.Arbitrary        (arbitrary)
import qualified Test.QuickCheck.Gen              as Gen
import           Test.QuickCheck.Instances.Vector ()
import           Test.QuickCheck.Random           (mkQCGen)


seed :: Int
seed = 33


showBytes :: Int64 -> String
showBytes size
  | size > 10 * (1024 * 1024) = show (size `div` (1024 * 1024)) <> " MiB"
  | size > 10 * 1024 = show (size `div` 1024) <> " KiB"
  | otherwise = show size <> " B"


showSpeed :: Int64 -> Double -> String
showSpeed size time =
    show (fromIntegral size / (1024 * 1024) / time) <> " MiB/s"


generate :: (Object -> L.ByteString) -> IO ()
generate pack = do
    size:_ <- (++[30]) . map read <$> getArgs

    start <- getCurrentTime
    hPutStrLn stderr "Generating sample..."

    let sample@(ObjectArray array) = ObjectArray $ Gen.unGen (Gen.resize size arbitrary) (mkQCGen 0) seed
    when (sample == sample) $  -- force deep evaluation of the whole structure (kind of deepseq)
        hPutStr stderr $ "Generated msgpack array of length " <> show (length array)
    sampleTime <- getCurrentTime
    hPutStrLn stderr $ " in " <> show (diffUTCTime sampleTime start)

    let packed = pack sample
    hPutStr stderr $ "Message packed into " <> showBytes (L.length packed)
    packTime <- getCurrentTime
    hPutStrLn stderr $ " in " <> show (diffUTCTime packTime sampleTime)

    hPutStrLn stderr $ "Packing speed: " <> showSpeed (L.length packed) (realToFrac (diffUTCTime packTime sampleTime))

    L.putStr packed
