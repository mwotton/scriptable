{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where
import           Data.List          (nub)
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import qualified Data.Text          as T
import           GHC.Base           (String)
import           Hpack.Config       (Executable, Library, Package (..),
                                     Section (..), readPackageConfig)
import           Protolude
import           System.Directory   (canonicalizePath, getAppUserDataDirectory)
import           System.Environment (getArgs)
import           System.IO.Error    (userError)

genLocals :: FilePath -> Package -> [(FilePath, Text)]
genLocals f = map (second
                   $ (prologue <>)
                   . (<> "\n")
                   . parens
                   . parens
                   . ("haskell-mode " <>)
                   . parens
                   . ("intero-targets " <>)
                   . T.unwords
                   . map show
                   . Set.toList
                  )
              . extractTargets f
  where
    parens :: Text -> Text
    parens = ("(" <>) . (<> ")")

    prologue :: Text
    prologue = ";;; Directory Local Variables\n;;; For more information see (info \"(emacs) Directory Variables\")\n\n"

-- FIX these are all terrible names
extractTargets :: FilePath -> Package -> [(FilePath, Set Text)]
extractTargets root Package{..}
  = collapse
    [ex "exe" packageExecutables
    ,ex "test" packageTests
    ,ex "bench" packageBenchmarks
    , lib]
  where
    lib = f ["lib"] <$> maybeToList packageLibrary

    ex :: String -> Map String (Section Executable) -> [[(FilePath, [Text])]]
    ex ty = map (\(target,section) -> f [ty,target] section) . Map.toList

    f :: [String] -> Section a -> [(FilePath, [Text])]
    f pieces Section{..} =
      map (\name -> (root <> "/" <> name <> "/.dir-locals.el"
                   ,lib <> [mkTarget pieces])) sectionSourceDirs
      where lib = const (mkTarget ["lib"]) <$> maybeToList packageLibrary
    collapse = Map.toList . Map.fromListWith Set.intersection
             . map (second Set.fromList) . concat .  concat

    mkTarget = T.intercalate ":" . map T.pack . (packageName:)

abort :: String -> IO ()
abort = ioError . userError

main :: IO ()
main = getArgs >>= \case
  [f1] -> do
    c <- canonicalizePath f1
    getAppUserDataDirectory "hpack"
      >>= (`readPackageConfig` (c <> "/package.yaml"))
      >>= either abort (writeFiles c . fst)
  x -> abort $ "exactly one argument needed, got " <> show x

  where
    writeFiles f = mapM_ (uncurry writeFile) . genLocals f
--    writeFiles f = print . extractTargets f
