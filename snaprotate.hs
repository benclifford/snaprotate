import System.IO

import Data.List

import Data.Time.Format
import System.Locale
import Data.Time.Clock

def :: [LevelDef]
def = [
      ]

data Snap = Snap String -- the directory name of the snapshot
  deriving Show

type LevelDef = [Snap] -> IO ([Snap],[Snap])
-- assert on behaviour of LevelDef functions:
--    (a,b) = f existing
--    a ++ b == existing (up to order)
--    any side-effects are OK as long as they're 'sane'

main = do
  putStrLn "snaprotate"
  dirs <- readDirs
  (keep,evict) <- keepLast24h dirs
  -- assert keep ++ delete ==uptoorder== dirs
  putStr "keep: "
  print keep
  putStr "evict: "
  print evict

-- mmm fake
readDirs = do
  h <- openFile "snapexample" ReadMode
  s <- hGetContents h
  -- hClose h -- can't do this here because hGetContents reads lazily
  -- but the assumption at the moment is that this program is short lived
  -- so it'll close 'soon' anyway when we exit
  return (map Snap (lines s))

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
    let keepable snap = let
           maybeSnaptime = snapToTime snap
         in maybe True (\snaptime -> (now `diffUTCTime` snaptime) > twodays ) maybeSnaptime
    let swap (a,b) = (b,a)
    return $ swap $ partition keepable l


snapToTime :: Snap -> Maybe UTCTime
snapToTime (Snap fn) = parseTime defaultTimeLocale "home-%Y-%m-%d-%H%M%z" fn
-- eg:  home-2010-05-03-2309+0000



