
module SnapRotate where 

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.IO

import Data.List
import Data.Maybe

import Data.Time.Format
import System.Locale
import Data.Time.Clock

import Control.Applicative
import Control.Monad

data Snap = MkSnap { snapfn :: String, snaptime :: UTCTime }
  deriving (Show, Eq)

data Keep = MkKeep { keeper :: Snap, reason :: String } deriving Show
instance Eq Keep where 
  (MkKeep l _) == (MkKeep r _) = l == r
-- should remove this Eq and have explicit unwrapping of the keeper if thats what we want to compare, which also forces us to be more explicit about the reason string.

type LevelDef = [Snap] -> IO ([Keep],[Snap])
-- assert on behaviour of LevelDef functions:
--    (a,b) = f existing
--    a ++ b == existing (up to order)
--    any side-effects are OK as long as they're 'sane'

runLevels levels = do
  logProgress "snaprotate, Copyright 2010 Ben Clifford benc@hawaga.org.uk"
  filteredDirs <- readDirs
  (opts,_,errs) <- getOpt Permute commandLineOptions <$> getArgs

  -- these will short circuit execution and exit
  when (errs /= []) (logCLIError errs)
  when (OptHelp `elem` opts) cliHelp

  let base = extractBase opts
  logDebug $ show opts
  logDebug "after home prefix filter: "
  logDebug $ show filteredDirs
  let fltA = map (\fn -> (fn, fnToTime base fn)) filteredDirs
  logDebug "after fltA: "
  logDebug $ show fltA
  let fltB = map (\(fn, t) -> maybe Nothing (\ut -> Just (fn,ut)) t) fltA
  let fltC = catMaybes fltB
  logDebug "after ignoring: "
  logDebug $ show fltC
  let sorted = sortBy (\(_,l) -> \(_,r) -> compare l r) fltC
  let original = map (\(fn,time) -> MkSnap fn time) sorted
  (keepF,evictF) <- levels original
  logDebug "keep: "
  logDebug $ show keepF
  logDebug "evict: "
  logDebug $ show evictF
  mapM_ (\(MkKeep (MkSnap fn _) reason) -> putStrLn $ "# keep: "++fn++": "++reason) keepF
  mapM_ (\(MkSnap f _) -> putStrLn $ "rm -rf "++f) evictF

-- mmm fake
readDirs = do
  dirs <- getDirectoryContents "."
  return $ filter (\s -> s /= "." && s /= "..") dirs

keepEverything :: LevelDef
keepEverything l = return (snapsToKeeps "keepEverything" l,[])

-- whats the best way to get this constant?
oneday :: NominalDiffTime
oneday = fromRational $ toRational $ secondsToDiffTime (24 * 60 * 60)

keepLastWithinDuration duration l = do 
    now <- getCurrentTime
    let keepable snap = (now `diffUTCTime` snaptime snap) > duration
    let rearrange (a,b) = (snapsToKeeps "keepLastWithinDuration" b,a)
    return $ rearrange $ partition keepable l



fnToTime :: String -> String -> Maybe UTCTime
fnToTime base fn = parseTime defaultTimeLocale (base++"-%Y-%m-%d-%H%M%z") fn
-- eg:  home-2010-05-03-2309+0000


-- fmt defines an equivalence relation on dates, and we keep one in each
-- of those equivalence classes.
keepOnePerTimeFormat fmt snaps = do
   let ymtime t = formatTime defaultTimeLocale fmt t
   let timedSnaps = map (\s ->(s, snaptime s)) snaps -- annotate with time
   let ymSnaps = map (\(s,t) -> (s, ymtime t)) timedSnaps -- project year and month
   let groupedByYM = groupBy (\(_,l) -> \(_,r) -> l==r) ymSnaps
   logDebug $ "grouped: "++(show groupedByYM)
   -- partition by month -- map to time, then extract year and month component
   let firstOfEachYM = map fst (map head groupedByYM)
   -- sort each partition by date
   -- pick the first of each partition
   -- return all of those as keepers
   return (snapsToKeeps "keepOnePerTimeFormat" firstOfEachYM, snaps \\ firstOfEachYM)

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

data CLIOpts = OptBase String | OptHelp deriving (Show, Eq)

commandLineOptions = [
  Option "b" ["base"] (ReqArg OptBase "BASE") "base of snapshot directory names",
  Option "h" ["help"] (NoArg OptHelp) "display this help and exit"
 ]

-- as base is a required opt, then this must always work
-- so this code doesn't handle the case of running out of opts...
-- but its not demonstrated statically that this will work.
-- also this def gives a pattern match overlap warning, but it works anyway.
extractBase :: [CLIOpts] -> String
extractBase ((OptBase s):rest) = s
extractBase (_:rest)= extractBase rest

logCLIError errs = do
  putStrLn $ "Usage error:"
  mapM putStrLn errs
  putStrLn $ "Try `snaprotate --help' for more usage information"
  exitFailure

cliHelp = do
  putStrLn $ usageInfo "Usage: snaprotate [OPTION...]" commandLineOptions
  exitSuccess
