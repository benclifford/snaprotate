import System.IO

import Data.List
import Data.Maybe

import Data.Time.Format
import System.Locale
import Data.Time.Clock

data Snap = MkSnap { snapfn :: String, snaptime :: UTCTime }
  deriving (Show, Eq)

type LevelDef = [Snap] -> IO ([Snap],[Snap])
-- assert on behaviour of LevelDef functions:
--    (a,b) = f existing
--    a ++ b == existing (up to order)
--    any side-effects are OK as long as they're 'sane'

main = do
  logProgress "snaprotate"
  filteredDirsA <- readDirs
  let filteredDirs = filter (\fn -> "home-" `isPrefixOf` fn) filteredDirsA
  logDebug "after home prefix filter: "
  logDebug $ show filteredDirs
  let fltA = map (\fn -> (fn, fnToTime fn)) filteredDirs
  logDebug "after fltA: "
  logDebug $ show fltA
  let fltB = map (\(fn, t) -> maybe Nothing (\ut -> Just (fn,ut)) t) fltA
  let fltC = catMaybes fltB
  logDebug "after ignoring: "
  logDebug $ show fltC
  let original = map (\(fn,time) -> MkSnap fn time) fltC
  -- TODO -- multiple ignore stages (with recorded reasons) here
  -- reasons to ignore:
  -- i) directoryname prefix (hardcoded elsewhere at the moment)

  -- ii) unparseable date (at same time as converting to a Snap type)

  -- note the naming here -- in general, keeps should be appended, and
  -- evict should be serially threaded
  (keep1,evict1) <- keepLast24h original
  -- assert keep ++ delete ==uptoorder== dirs
  (keep2,evict2) <- keepOnePerMonth evict1
  logDebug "keep1: "
  logDebug $ show keep1
  logDebug "keep2: "
  logDebug $ show keep2
  logDebug "evict: "
  logDebug $ show evict2
  -- TODO some actual output

-- mmm fake
readDirs = do
  h <- openFile "snapexample" ReadMode
  s <- hGetContents h
  -- hClose h -- can't do this here because hGetContents reads lazily
  -- but the assumption at the moment is that this program is short lived
  -- so it'll close 'soon' anyway when we exit
  return (lines s)

keepEverything :: LevelDef
keepEverything l = return (l,[])

-- whats the best way to get this constant?
oneday :: NominalDiffTime
oneday = fromRational $ toRational $ secondsToDiffTime (24 * 60 * 60)

twodays = 2 * oneday

-- keep everything from the last 24h, and everything that we can't
-- parse a time for
-- If we move to a tristate system later, then timeparsing should be
-- undecided, rather than keep or evict
-- The implementation of this considers each snap individually rather than
-- the group of available snaps as a whole.
keepLast24h l = do 
    now <- getCurrentTime
    let keepable snap = (now `diffUTCTime` snaptime snap) > twodays
    let swap (a,b) = (b,a)
    return $ swap $ partition keepable l

-- TODO this needs to work on something other than a "home" prefix. perhaps
-- the prefix could be specified on the command line, and we only pay
-- attention to files with that prefix.
fnToTime :: String -> Maybe UTCTime
fnToTime fn = parseTime defaultTimeLocale "home-%Y-%m-%d-%H%M%z" fn
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
keepOnePerMonth snaps = do
   let ymtime t = formatTime defaultTimeLocale "%Y-%m" t
   let timedSnaps = map (\s ->(s, snaptime s)) snaps -- annotate with time
   let ymSnaps = map (\(s,t) -> (s, ymtime t)) timedSnaps -- project year and month
   let groupedByYM = groupBy (\(_,l) -> \(_,r) -> l==r) ymSnaps
   print $ "grouped: "++(show groupedByYM)
   -- partition by month -- map to time, then extract year and month component
   let firstOfEachYM = map fst (map head groupedByYM)
   -- sort each partition by date
   -- pick the first of each partition
   -- return all of those as keepers
   return (firstOfEachYM, snaps \\ firstOfEachYM)


logProgress str = hPutStrLn stderr str
logDebug str = hPutStrLn stderr str

