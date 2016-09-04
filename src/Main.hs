module Main (main) where

import Protolude

import System.Metrics.Prometheus.Concurrent.Http (serveHttpTextMetricsT)
import System.Metrics.Prometheus.Concurrent.RegistryT
import System.Metrics.Prometheus.Metric.Gauge (Gauge, set)

import System.Process (readProcess)
import Data.Attoparsec.Text (Parser, parseOnly, skip, isEndOfLine, endOfLine, skipSpace, decimal)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import GHC.Base (String)

-- output looks like:
-- nvidia-smi dmon -c 1
-- # gpu   pwr  temp    sm   mem   enc   dec  mclk  pclk
-- # Idx     W     C     %     %     %     %   MHz   MHz
--    0    32    39     0     0     0     0  3505  1075

data Stats a = Stats { gpu :: a, pwr :: a, temp :: a,  sm :: a, mem :: a, enc :: a, dec :: a, mclk :: a, pclk :: a }

parseStats :: Parser (Stats Int)
parseStats = do
  -- skip first two lines
  skip (not . isEndOfLine)
  endOfLine
  skip (not . isEndOfLine)
  endOfLine
  let sd = skipSpace *> decimal
  Stats <$> decimal <*> sd  <*> sd  <*> sd  <*> sd  <*> sd  <*> sd  <*> sd  <*> sd


readSMI :: IO String
readSMI = readProcess "nvidia-smi" ["dmon", "-c", "1"] ""

oneStats :: Stats Gauge -> IO ()
oneStats gauges = do
  out <- readSMI
  case parseOnly parseStats (toS out) of
    Left err -> panic (toS err)
    Right stats -> do
      set (fromIntegral (gpu stats)) (gpu gauges)
      set (fromIntegral (pwr stats)) (pwr gauges)
      set (fromIntegral (temp stats)) (temp gauges)
      set (fromIntegral (sm stats)) (sm gauges)
      set (fromIntegral (mem stats)) (mem gauges)
      set (fromIntegral (enc stats)) (enc gauges)
      set (fromIntegral (dec stats)) (dec gauges)
      set (fromIntegral (mclk stats)) (mclk gauges)
      set (fromIntegral (pclk stats)) (pclk gauges)

main :: IO ()
main = runRegistryT $ do
  -- dry-run so the program dies in thread 0 if nvidia-smi not available
  void (liftIO readSMI)
  -- Labels can be defined as lists or added to an empty label set
  stats <- Stats
           <$> registerGauge "gpu" mempty
           <*> registerGauge "pwr" mempty
           <*> registerGauge "temp" mempty
           <*> registerGauge "sm" mempty
           <*> registerGauge "mem" mempty
           <*> registerGauge "env" mempty
           <*> registerGauge "dec" mempty
           <*> registerGauge "mclk" mempty
           <*> registerGauge "pclk" mempty

  void $ liftIO $ forkIO $ forever $ do
    oneStats stats
    threadDelay (1000 * 1000)

  serveHttpTextMetricsT 9120 ["metrics"]
