
module SnapRotate where 

import Prelude hiding ( (<*>) )
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Data.List
import Data.Maybe

import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock

import Control.Applicative
import Control.Monad

copyright = "snaprotate, Copyright 2010-2021 Various authors"

data Snap = MkSnap { snapfn :: String, internalSnaptime :: SnapTimestamp }
  deriving (Show, Eq)

-- keep this as a barely-parsed string so that we can use parseTime to
-- get it into various different time formats that implement ParseTime
type SnapTimestamp = String

-- this should be polymorphic in its return type
snaptime :: (ParseTime t) => Snap -> t
snaptime (MkSnap _ t) = fromJust $ parseTime defaultTimeLocale ("%Y-%m-%d-%H%M%z") t

data Keep = MkKeep { keeper :: Snap, reason :: String } deriving Show
instance Eq Keep where 
  (MkKeep l _) == (MkKeep r _) = l == r
-- should remove this Eq and have explicit unwrapping of the keeper if thats what we want to compare, which also forces us to be more explicit about the reason string.

type LevelDef = [Snap] -> IO ([Keep],[Snap])
-- assert on behaviour of LevelDef functions:
--    (a,b) = f existing
--    a ++ b == existing (up to order)
--    any side-effects are OK as long as they're 'sane'

runLevels :: LevelDef -> IO ()
runLevels levels = do
  logProgress copyright
  filteredDirs <- readDirs
  (opts,_,errs) <- getOpt Permute commandLineOptions <$> getArgs

  -- these will short circuit execution and exit
  when (errs /= []) (logCLIError errs)
  when (OptHelp `elem` opts) cliHelp

  let base = extractBase opts
  logDebug $ show opts
  logDebug "snapdirectories to examine: "
  logDebug $ show filteredDirs
  let fltA = map (\fn -> (fn, fnToTime base fn)) filteredDirs
  logDebug "After checking snapdirectory names match base/time template (fltA)"
  logDebug $ show fltA
  let fltB = map (\(fn, t) -> maybe Nothing (\ut -> Just (fn,ut)) t) fltA
  let fltC = catMaybes fltB
  logDebug "After filtering non-matching snapdirectory names (fltC)"
  logDebug $ show fltC
  let sorted = sortBy (\(_,l) -> \(_,r) -> compare l r) fltC
  let original = map (\(fn,time) -> MkSnap fn time) sorted
  (keepF,evictF) <- levels original
  logDebug "keep: "
  logDebug $ show keepF
  logDebug "evict: "
  logDebug $ show evictF
  mapM_ (\(MkKeep (MkSnap fn _) reason) -> putStrLn $ "# keep: "++fn++": "++reason) keepF
  case (extractMode opts) of
    ModeRM -> mapM_ (\(MkSnap f _) -> putStrLn $ "rm -rf "++f) evictF
    ModeList -> mapM_ (\(MkSnap f _) -> putStrLn $ "# delete: "++f) evictF

-- mmm fake
readDirs = do
  dirs <- getDirectoryContents "."
  return $ filter (\s -> s /= "." && s /= "..") dirs

keepEverything :: LevelDef
keepEverything l = return (snapsToKeeps "keepEverything" l,[])


keepLast duration l = do 
    now <- getCurrentTime
    let keepable snap = (now `diffUTCTime` snaptime snap) > (getDuration duration)
    let rearrange (a,b) = (snapsToKeeps "keepLast" b,a)
    return $ rearrange $ partition keepable l


-- filters out non-matching times
-- this and snaptime are closely related - TODO share parse expression
-- eg:  home-2010-05-03-2309+0000
fnToTime :: String -> String -> Maybe SnapTimestamp
fnToTime base fn = let
    p = (parseTime defaultTimeLocale (base++"-%Y-%m-%d-%H%M%z") fn) :: Maybe UTCTime
  in maybe Nothing (\t -> stripPrefix (base++"-") fn) p


-- keep n snapshots. This assumes snaps are in date order so that taking
-- the last n snapshots in the list takes the most recent n snapshots.
keepN :: Int -> LevelDef
keepN n snaps = do
  let keepers = take n $ reverse snaps
  return (snapsToKeeps "keepN" keepers, snaps \\ keepers)

-- fmt defines an equivalence relation on dates, and we keep one in each
-- of those equivalence classes.
keepOneEvery :: (ParseTime t) => TimeSpec t -> [Snap] -> IO ([Keep],[Snap])
keepOneEvery timespec snaps = do
   let ymtime t = (getBucket timespec) t
   let timedSnaps = map (\s ->(s, snaptime s)) snaps -- annotate with time
   let ymSnaps = map (\(s,t) -> (s, ymtime t)) timedSnaps -- project year and month
   let groupedByYM = groupBy (\(_,l) -> \(_,r) -> l==r) ymSnaps
   logDebug $ "grouped: "++(show groupedByYM)
   -- partition by month -- map to time, then extract year and month component
   let firstOfEachYM = map fst (map head groupedByYM)
   -- sort each partition by date
   -- pick the first of each partition
   -- return all of those as keepers
   return (snapsToKeeps "keepOneEvery" firstOfEachYM, snaps \\ firstOfEachYM)

-- keeps if both of these levels signifies keep, otherwise evicts
(<&&>) :: LevelDef -> LevelDef -> LevelDef
(l <&&> r) snap = do
  (keepL, evictL) <- l snap
  (keepR, evictR) <- r snap
  let keepF = keepL `intersect` keepR -- TODO this intersect is no good because the reasons will differ...
  let evictF = snap \\ (keepsToSnaps keepF)
  return (keepF, evictF)

-- keeps if either of these levels signifies keep, otherwise evicts
(l <||> r) snap = do
  (keepL, evictL) <- l snap
  (keepR, evictR) <- r snap
  let keepF = keepL `union` keepR -- TODO this union is no good because the reasons will differ...
  let evictF = snap \\ (keepsToSnaps keepF)
  return (keepF, evictF)


-- old version of <||> that fed the evicts from LHS to RHS, which would lead
-- to different (always more?) being kept.
(<|-|>) :: LevelDef -> LevelDef -> LevelDef
(l <|-|> r) snap = do
  (keepL, evictL) <- l snap
  (keepR, evictR) <- r evictL
  return (keepL ++ keepR, evictR)

-- will be used for annotating keeps, but not impl yet
(<?>) :: LevelDef -> String -> LevelDef
(l <?> desc) x = do
  (keeps, evicts) <-  l x
  -- return (map (\(MkKeep s oldreason) -> MkKeep s (desc ++ ": " ++ oldreason)) keeps , evicts)
  return (snapsToKeeps desc (keepsToSnaps keeps), evicts)

keepsToSnaps :: [Keep] -> [Snap]
keepsToSnaps = map keepToSnap
keepToSnap (MkKeep s _) = s

snapsToKeeps :: String -> [Snap] -> [Keep]
snapsToKeeps reason = map (snapToKeep reason)

snapToKeep reason snap = MkKeep snap reason -- so almost the MkKeep constructor

-- logging
logProgress str = hPutStrLn stderr str
logDebug str = return () -- hPutStrLn stderr str

-- commandline

data RunMode = ModeRM | ModeList deriving (Show, Eq) -- and I want ModeDU
data CLIOpts = OptBase String | OptHelp | OptMode RunMode deriving (Show, Eq)

commandLineOptions = [
  Option "b" ["base"] (ReqArg OptBase "BASE") "base of snapshot directory names",
  Option "h" ["help"] (NoArg OptHelp) "display this help and exit",
  Option "m" ["mode"] (ReqArg parseRunMode "MODE") "mode (rm, list)"
 ]

parseRunMode "rm" = OptMode ModeRM
parseRunMode "list" = OptMode ModeList
parseRunMode m = error $ "Unknown --mode "++m

-- as base is a required opt, then this must always work
-- so this code doesn't handle the case of running out of opts...
-- but its not demonstrated statically that this will work.
-- also this def gives a pattern match overlap warning, but it works anyway.
extractBase :: [CLIOpts] -> String
extractBase ((OptBase s):rest) = s
extractBase (_:rest)= extractBase rest
extractBase [] = error "--base commandline option must be specified"

extractMode :: [CLIOpts] -> RunMode
extractMode ((OptMode m) : rest) = m
extractMode (_:rest) = extractMode rest
extractMode [] = error "--mode commandline option must be specified"

logCLIError errs = do
  putStrLn $ "Usage error:"
  mapM putStrLn errs
  putStrLn $ "Try `snaprotate --help' for more usage information"
  exitFailure

cliHelp = do
  putStrLn $ usageInfo "Usage: snaprotate [OPTION...]" commandLineOptions
  exitWith ExitSuccess



-- duration/time bucket definitions

data TimeSpec t = MkTimeSpec {
  getDuration :: NominalDiffTime,
  getBucket :: t -> Integer }

-- makes a new TimeSpec that is n times as long as the supplied TimeSpec
(<*>) :: Integer -> TimeSpec t -> TimeSpec t
n <*> spec = MkTimeSpec ( (fromInteger n) * getDuration spec) ( \ts -> (getBucket spec ts) `quot` n )

day = MkTimeSpec {
    getDuration = fromRational $ toRational $ secondsToDiffTime (24 * 60 * 60),
    getBucket = toModifiedJulianDay
  }

-- TODO month
-- for duration, 31 days *or* the time from now to the same day last month
-- both similar, 31 is strictly longer, I think
-- for buckets, year*12 + month, because year is an exact multiple months

month = MkTimeSpec {
    getDuration = fromRational $ toRational $ secondsToDiffTime (31 * 24 * 60 * 60),
    getBucket = \ts -> let
        (year, month, _) = toGregorian ts
      in 12 * year +  fromIntegral month - 1
  }

-- the bucket handling needs to: i) be able to be applied to a time to
-- distinguish buckets
-- ii) have <*> used to reduce the number of buckets
-- if we're using a bucket ID then that bucket ID must be something we can
-- divide, using <*> - so an integer, for example, but then it must have
-- the property that adjacent buckets are numbered sequentially, so eg
-- 200653 for the last week of 2006 followed by 200701 is no good.
-- plus using week numbers there must be careful because variable number of
-- ISO weeks in a year...

-- we can define weeks in terms of days, though
-- week = 7 <*> day

-- and we can define year in terms of months
-- year = 12 <*> month

-- so day and month appear to be two base units that are irreconcilable
-- and we can define the other stuff that we want in terms of those

-- so a timespec should give a bucketid, an integer, for each
-- timestamp, subject to the above constraints.

-- whats the best way to get this constant?

