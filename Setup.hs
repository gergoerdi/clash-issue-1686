{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
import qualified Clash.Main as Clash
import Clash.Driver.Types (Manifest)

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Compiler
import Distribution.Simple.Utils (infoNoWrap)
import Distribution.Verbosity
import Distribution.ModuleName hiding (main)
import Distribution.Types.UnqualComponentName

import Distribution.Types.Lens
import Control.Lens hiding ((<.>))
import Control.Monad (forM, foldM)
import Data.List (intercalate, sort, nub)
import Data.Maybe (maybeToList, fromMaybe)
import System.FilePath

lookupX :: String -> BuildInfo -> Maybe String
lookupX key buildInfo = lookup ("x-clashilator-" <> key) (view customFieldsBI buildInfo)

clashToVerilog :: LocalBuildInfo -> BuildFlags -> [FilePath] -> BuildInfo -> ModuleName -> String -> FilePath -> IO ()
clashToVerilog localInfo buildFlags srcDirs buildInfo mod entity outDir = do
    pkgdbs <- absolutePackageDBPaths $ withPackageDB localInfo
    let dbpaths = nub . sort $ [ path | SpecificPackageDB path <- pkgdbs ]
        dbflags = concat [ ["-package-db", path] | path <- dbpaths ]
        iflags = [ "-i" <> dir | dir <- srcDirs ]
        clashflags = maybe [] words $ lookupX "clash-flags" buildInfo

    let args = concat
            [ [ "--verilog"
              , "-outputdir", outDir
              , "-main-is", entity
              , intercalate "." (components mod)
              ]
            , iflags
            , dbflags
            , clashflags
            ]
    infoNoWrap verbosity $ unwords $ "Clash.defaultMain" : args
    Clash.defaultMain args
  where
    verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)

buildVerilator :: LocalBuildInfo -> BuildFlags -> Maybe UnqualComponentName -> BuildInfo -> IO BuildInfo
buildVerilator localInfo buildFlags compName buildInfo = case top of
    Nothing -> return buildInfo
    Just mod -> buildVerilator' localInfo buildFlags compName buildInfo (fromString mod) entity
  where
    top = lookupX "top-is" buildInfo
    entity = fromMaybe "topEntity" $ lookupX "entity" buildInfo

buildVerilator' :: LocalBuildInfo -> BuildFlags -> Maybe UnqualComponentName -> BuildInfo -> ModuleName -> String -> IO BuildInfo
buildVerilator' localInfo buildFlags compName buildInfo mod entity = do
    clashToVerilog localInfo buildFlags srcDirs buildInfo mod entity synDir
    return buildInfo
  where
    verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)

    -- TODO: Maybe we could add extra source dirs from "x-clashilator-source-dirs"?
    srcDirs = view hsSourceDirs buildInfo
    outDir = case compName of
        Nothing -> buildDir localInfo
        Just name -> buildDir localInfo </> unUnqualComponentName name
    synDir = outDir </> "_clashilator" </> "clash-syn"

data Clashilatable where
    Clashilatable
        :: (HasBuildInfo a)
        => Traversal' PackageDescription a
        -> (a -> Maybe UnqualComponentName)
        -> Clashilatable

clashilatables :: [Clashilatable]
clashilatables =
    [ Clashilatable (executables . each) (Just . view exeName)
    , Clashilatable (library . each)     (const Nothing)
    , Clashilatable (testSuites . each)  (Just . view testName)
    , Clashilatable (benchmarks . each)  (Just . view benchmarkName)
    ]

itagged :: Traversal' s a -> (a -> b) -> IndexedTraversal' b s a
itagged l f = reindexed f (l . selfIndex)

clashilate :: PackageDescription -> LocalBuildInfo -> BuildFlags -> IO PackageDescription
clashilate pkg localInfo buildFlags =
    foldM (&) pkg $
      [ itraverseOf focus $ buildVerilator localInfo buildFlags
      | Clashilatable component getName <- clashilatables
      , let focus = itagged component getName <. buildInfo
      ]

clashilatorMain :: IO ()
clashilatorMain = defaultMainWithHooks simpleUserHooks
    { buildHook = clashilatorBuildHook
    }

clashilatorBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
clashilatorBuildHook pkg localInfo userHooks buildFlags = do
    pkg' <- clashilate pkg localInfo buildFlags
    buildHook simpleUserHooks pkg' localInfo userHooks buildFlags

main :: IO ()
main = clashilatorMain
