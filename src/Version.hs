-- * Manipulation functions for cabal versions and version ranges
{-# LANGUAGE TemplateHaskell #-}
module Version where

import Distribution.Version

-- | Function to bump the nth position in a version to a higher number
-- trailing version number will be discarded
bumpPosition :: Int -> Version -> Version
bumpPosition p v = mkVersion $ addPos p (versionNumbers v)
  where
    addPos 0 []      = [1]
    addPos 0 (vn: _)  = [vn+1]
    addPos x []      = 0 : addPos (x - 1) []
    addPos x (vn: vns) = vn : addPos (x - 1) vns

previousVersion :: Version -> Version
previousVersion = mkVersion . mkPrevious . versionNumbers
  where mkPrevious = reverse . prevHelp . reverse
        prevHelp []     = []
        prevHelp (x:xs) = if x <= 1 then xs else (x - 1): xs -- No trailing zeros

nextVersion :: Version -> Version
nextVersion = mkVersion . mkNext . versionNumbers
  where mkNext = reverse . nextHelp . reverse
        nextHelp []     = []
        nextHelp (x:xs) = (x + 1) : xs

addVersionToRange :: Version -> VersionRange -> VersionRange
addVersionToRange new r =
  if withinRange new r
  then r
  else unionVersionRanges r (thisVersion new)


