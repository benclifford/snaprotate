
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.IO

import Data.List
import Data.Maybe

import Data.Time.Format
import System.Locale
import Data.Time.Clock

import Control.Applicative

data Snap = MkSnap { snapfn :: String, snaptime :: UTCTime }
  deriving (Show, Eq)

type LevelDef = [Snap] -> IO ([Snap],[Snap])
-- assert on behaviour of LevelDef functions:
--    (a,b) = f existing
--    a ++ b == existing (up to order)
--    any side-effects are OK as long as they're 'sane'

main = do
  logProgress "snaprotate, Copyright 2010 Ben Clifford benc@hawaga.org.uk"
  filteredDirs <- readDirs
  (opts,_,_) <- getOpt Permute commandLineOptions <$> getArgs
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
  -- TODO -- multiple ignore stages (with recorded reasons) here
  -- reasons to ignore:
  -- i) directoryname prefix (hardcoded elsewhere at the moment)

  -- ii) unparseable date (at same time as converting to a Snap type)

  -- note the naming here -- in general, keeps should be appended, and
  -- evict should be serially threaded
  let level1 = keepLast24h
  -- assert keep ++ delete ==uptoorder== dirs
  let level2 = keepOnePerWeekLast4Weeks
  let level3 =  keepOnePerMonth
  (keepF,evictF) <- (level1 <||> level2 <||> level3) original
  logDebug "keep: "
  logDebug $ show keepF
  logDebug "evict: "
  logDebug $ show evictF
  mapM_ (\(MkSnap f _) -> putStrLn $ "rm -rfv "++f) evictF
  -- TODO some actual output

-- mmm fake
readDirs = do
  dirs <- getDirectoryContents "."
  return $ filter (\s -> s /= "." && s /= "..") dirs

keepEverything :: LevelDef
keepEverything l = return (l,[])

-- whats the best way to get this constant?
oneday :: NominalDiffTime
oneday = fromRational $ toRational $ secondsToDiffTime (24 * 60 * 60)

twodays = 2 * oneday
oneweek = 7 * oneday

-- keep everything from the last 24h, and everything that we can't
-- parse a time for
-- If we move to a tristate system later, then timeparsing should be
-- undecided, rather than keep or evict
-- The implementation of this considers each snap individually rather than
-- the group of available snaps as a whole.

keepLast24h = keepLastWithinDuration twodays -- this form makes it obvious that the function doesn't do what its name says

keepLastWithinDuration duration l = do 
    now <- getCurrentTime
    let keepable snap = (now `diffUTCTime` snaptime snap) > duration
    let swap (a,b) = (b,a)
    return $ swap $ partition keepable l



-- TODO this needs to work on something other than a "home" prefix. perhaps
-- the prefix could be specified on the command line, and we only pay
-- attention to files with that prefix.
fnToTime :: String -> String -> Maybe UTCTime
fnToTime base fn = parseTime defaultTimeLocale (base++"-%Y-%m-%d-%H%M%z") fn
-- eg:  home-2010-05-03-2309+0000


-- for each month, keeps the earliest in that month (that has not been
-- kept by previous levels)
-- again, we have to have some special behaviour for unparseable timestamps
-- perhaps I should filter things out right at the start that have no
-- valid timestamps? then each Snap will contain a definite UTCTime, rather
-- than a maybe.
-- output should then list 'ignored' directories as well as kept and
-- evicted.
-- or maybe 'ignored' then fits into a more abstract annotating framework too?
keepOnePerMonth :: LevelDef
keepOnePerMonth = keepOnePerTimeFormat "%Y-%m"

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
   return (firstOfEachYM, snaps \\ firstOfEachYM)

-- two filters here - one looks like keepOnePerMonth (but for weeks) and
-- another evicts everything more than 4 weeks old
-- we could define this as a level combinator for building a new leveldef
-- out of an existing pair of leveldefs, that looks different from the
-- existing idea I had of leveldef composition
-- the existing idea applies one level def that can unilaterally keep, and
-- then allows a second level def to also unilaterally keep. evictions from
-- this composite level are only if *both* level defs evict.
-- In this new combinator that I'm thinking of, either level def can
-- evict, and both levels need to keep in order for the combined level
-- to keep.
keepOnePerWeekLast4Weeks :: LevelDef
keepOnePerWeekLast4Weeks = keepOnePerWeek <&&> keepLast4Weeks

keepOnePerWeek = keepOnePerTimeFormat "%Y-%U"
keepLast4Weeks = keepLastWithinDuration (4 * oneweek)

-- keeps if both of these levels signifies keep, otherwise evicts
(<&&>) :: LevelDef -> LevelDef -> LevelDef
(l <&&> r) snap = do
  (keepL, evictL) <- l snap
  (keepR, evictR) <- r snap
  let keepF = keepL `intersect` keepR
  let evictF = snap \\ keepF
  return (keepF, evictF)

-- keeps if either of these levels signifies keep, otherwise evicts
(<||>) :: LevelDef -> LevelDef -> LevelDef
(l <||> r) snap = do
  (keepL, evictL) <- l snap
  (keepR, evictR) <- r evictL
  return (keepL ++ keepR, evictR)

-- will be used for annotating keeps, but not impl yet
(<?>) :: LevelDef -> String -> LevelDef
l <?> desc = l

-- logging
logProgress str = hPutStrLn stderr str
logDebug str = hPutStrLn stderr str

-- commandline

data CLIOpts = OptBase String deriving Show

commandLineOptions = [
  Option "b" ["base"] (ReqArg OptBase "BASE") "base of snapshot directory names"
 ]

-- as base is a required opt, then this must always work
-- so this code doesn't handle the case of running out of opts...
-- but its not demonstrated statically that this will work.
-- also this def gives a pattern match overlap warning, but it works anyway.
extractBase :: [CLIOpts] -> String
extractBase ((OptBase s):rest) = s
extractBase (_:rest)= extractBase rest

