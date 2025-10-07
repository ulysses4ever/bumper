module Main where

import Config
import Control.Lens hiding ((<.>))
import Control.Monad
import Data.List
import Data.Maybe
import Distribution.Package hiding (Package)
import Distribution.Text
import Distribution.Version
import qualified Distribution.Compat.NonEmptySet as NESet
import Distribution.Types.LibraryName
import Package
import Version
import qualified Data.Map as M
import qualified Paths_bumper as Paths

main :: IO ()
main =
  do conf <- getConfig
     case view action conf of
       ShowHelp    -> printUsage options
       ShowVersion -> putStrLn $ "bumper, version " ++ show Paths.version
       ShowDeps    -> run conf showDeps
       Run         -> run conf updateDeps
  where
    showDeps   _    changed = putStr $ intercalate " " $ map display $ M.keys changed
    updateDeps base changed = mapM_ (\p -> updatePackage p (makeUpdates p)) base
      where
        makeUpdates p = (M.lookup (view name p) changed, dependencyUpdates changed p)

run :: Config -> (Packages -> Changes -> IO ()) -> IO ()
run conf act =
  do -- Load packages
     ps   <- packages

     --Check for non-existent packages
     let changePks = map fst (view setVersion conf) ++ concat (M.elems (view bump conf))
         notFound = filter (not . isJust . flip lookupPackage ps) changePks
     when (not $ null notFound) $ putStrLn $ "[Warning] packages not found: " ++ (intercalate "," $ map display notFound)

     -- Retrieve base versions
     base <- maybe (return ps) (flip getBaseVersions ps) $ view global conf
     let changed = (if view transitive conf then trans base else id)
                 $ concatChanges (map (\(p,pks) -> bumpVersions p pks base) (M.toAscList (view bump conf)))
               <.> userVersions (view setVersion conf) base
     act base changed

type Changes = M.Map PackageName Version

-- Combine changes, not updating already changed packages
infixr 5 <.>
(<.>) :: Changes -> Changes -> Changes
(<.>) = M.unionWith (flip const)

concatChanges :: [Changes] -> Changes
concatChanges = foldr (<.>) M.empty

-- | Update versions
userVersions :: [(PackageName, Version)] -> Packages -> Changes
userVersions vs ps = M.fromList $ filter (\nv -> hasPackage (fst nv) ps) vs

bumpVersions :: Int -> [PackageName] -> Packages -> Changes
bumpVersions pos ns ps = M.fromList $ map (\p -> (view name p, bumpPosition pos (view version p))) $ lookupPackages ns ps

-- | Make transitive changes
trans :: Packages -> Changes -> Changes
trans ps = fix (transStep ps)

transStep :: Packages -> Changes -> Changes
transStep ps old = new <.> old
  where deps = filter (not . null . dependencyUpdates old) ps
        new  = M.fromList . map (\p -> (view name p, bumpPosition 3 (view version p))) $ deps

fix :: (Eq a) => (a -> a) -> a -> a
fix f a | b == a    = a
        | otherwise = fix f b
  where b = f a

-- | Caclulate updated dependencies
dependencyUpdates :: Changes -> Package -> [Dependency]
dependencyUpdates ch = foldr addDep [] . view dependencies
  where addDep (Dependency n r _) dps =
          case M.lookup n ch of
            Just v  -> if withinRange v r
                        then dps
                        else Dependency n (addVersionToRange v r) (NESet.singleton LMainLibName) : dps
            Nothing -> dps
