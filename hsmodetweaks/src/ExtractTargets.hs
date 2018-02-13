{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ExtractTargets where
import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import qualified Data.Text        as T
import           GHC.Base         (String)
import           Hpack.Config     (Executable, Package (..), Section (..),
                                   readPackageConfig)

import           Protolude        hiding (packageName)
import           System.Directory (getAppUserDataDirectory)


parseHpack :: FilePath -> IO (Either String (Package, [String]))
parseHpack c = getAppUserDataDirectory "hpack"
  >>= (`readPackageConfig` (c <> "/package.yaml"))

-- FIX these are all terrible names
extractTargets :: FilePath -> Package -> [(FilePath, Set Text)]
extractTargets root Package{..}
  = collapse
    [ex "exe" packageExecutables
    ,ex "test" packageTests
    ,ex "bench" packageBenchmarks
    ,libStanza]
  where
    libStanza = f ["lib"] <$> maybeToList packageLibrary

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

-- this should probably really be done in elisp on the emacs side -
-- that way we can avoid overwriting .dir-locals.el
genLocal :: Set Text -> Text
genLocal
  = (prologue <>)
  . (<> "\n")
  . parens
  . parens
  . ("haskell-mode " <>)
  . parens
  . ("intero-targets " <>)
  . T.unwords
  . map show
  . Set.toList

  where
    parens :: Text -> Text
    parens = ("(" <>) . (<> ")")

    prologue :: Text
    prologue = ";;; Directory Local Variables\n;;; For more information see (info \"(emacs) Directory Variables\")\n\n"
