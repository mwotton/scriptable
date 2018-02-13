{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import           ExtractTargets     (extractTargets, genLocal, parseHpack)
import           GHC.Base           (String)
import           Protolude
import           System.Directory   (canonicalizePath)
import           System.Environment (getArgs)
import           System.IO.Error    (userError)

abort :: String -> IO ()
abort = ioError . userError

main :: IO ()
main = getArgs >>= \case
  [directory] -> do
    c <- canonicalizePath directory
    parseHpack c >>= either abort (writeFiles c . fst)
  x -> abort $ "exactly one argument needed, got " <> show x

  where
    writeFiles f = mapM_ (uncurry writeFile . second genLocal) . extractTargets f
--    writeFiles f = print . extractTargets f
