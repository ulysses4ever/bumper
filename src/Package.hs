-- * Contains helper functions to load and manipulate .cabal files
{-# LANGUAGE
    TemplateHaskell
  , TupleSections
  #-}
module Package where

import Control.Lens
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import Distribution.Types.Version (Version)
import Distribution.Package hiding (Package)
import Distribution.PackageDescription
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Text
import Distribution.Verbosity
import System.Process
import Text.Regex
import qualified Data.Map as M

import System.IO (hGetContents)

data Package = Package
    { _name         :: PackageName
    , _path         :: String
    , _version      :: Version
    , _dependencies :: [Dependency]
    } deriving (Show, Eq)

$(makeLenses ''Package)

type Packages = [Package]

-- | Helper functions

lookupPackage :: PackageName -> Packages -> Maybe Package
lookupPackage s = find ((== s) . view name)

lookupPackages ::  [PackageName] -> Packages -> Packages
lookupPackages ns ps = catMaybes . map (flip lookupPackage ps) $ ns

hasPackage :: PackageName -> Packages -> Bool
hasPackage n = isJust . lookupPackage n

removePackage :: PackageName -> Packages -> Packages
removePackage s = filter ((/= s). view name)

removeAll :: [PackageName] -> Packages -> Packages
removeAll = flip $ foldr removePackage

-- | Loading packages
packages :: IO Packages
packages =
  do (_, hOut, _, _) <- runInteractiveCommand "find . -name *.cabal -type f"
     paths <- lines <$> hGetContents hOut
     forM paths $ \p ->
        do gpd <- readGenericPackageDescription normal p
           let pkg = packageDescription gpd
           return $ Package
                      { _name = pkgName $ package pkg
                      , _path = p
                      , _version = pkgVersion $ package pkg
                      , _dependencies = targetBuildDepends $ (case allBuildInfo pkg of
                                                                (bi:_) -> bi
                                                                []     -> emptyBuildInfo)
                      }

getBaseVersions :: String -> Packages -> IO Packages
getBaseVersions ind ps =
 do (_, hOut, _, _) <- runInteractiveCommand $ "tar -tf " ++ ind
    gps <- lines <$> hGetContents hOut
    let vs = catMaybes $ map (parseVer . splitOn "/") gps
        parseVer (n:v:_) = fmap (mkPackageName n, ) $ simpleParse v
        parseVer _       = Nothing
        globver = M.fromListWith (\a b -> if a > b then a else b) vs
        updVer p = over version (maybe id (\v -> if view version p < v then const v else id) $ M.lookup (view name p) globver) p
    return $ map updVer ps

-- | Manipulating package contents
whiteReg :: String
whiteReg = "[ \n\t]*"

modifyVersion :: Version -> String -> String
modifyVersion v s = subRegex (mkRegexWithOpts regex False False) s result
  where regex = "(version" ++ whiteReg ++ ":" ++ whiteReg ++ ") ([0-9.a-zA-Z]+)"
        result = "\\1 " ++ display v

modifyDependency :: Dependency -> String -> String
modifyDependency (Dependency nm range _) s = subRegex (mkRegexWithOpts regex False False) s result
  where regex = "(build-depends" ++ whiteReg ++ ":" ++ "[^:]*"
              ++ "[ ,\n\t]" ++ display nm ++ whiteReg ++ ")([" ++ rangeChar ++ " \t\n]*[" ++ rangeChar ++ "])"
        rangeChar = "0-9.*&|()<>="
        result = "\\1" ++ display range

-- | Data structure containing package modifications
type PackageChanges = (Maybe Version, [Dependency])

-- | Writing to packages
modifyPackage :: PackageChanges -> String -> String
modifyPackage (mv, deps) = flip (foldr modifyDependency) deps
                         . maybe id modifyVersion mv

updatePackage :: Package -> PackageChanges -> IO ()
updatePackage p ch = readFile (view path p) >>= writeFile (view path p) . modifyPackage ch
