module Main ( main ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)

import Data.Char (isSpace)
import Data.List (findIndices, intercalate, isPrefixOf, sort)
import qualified Data.Map as M

import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.Posix.Files (fileExist, getFileStatus, isDirectory, isRegularFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

usage :: IO ()
usage = do
    putStrLn "usage: syn-kw [-p] <from-tree> <to-tree>"
    exitWith (ExitFailure 1)

debug :: String -> IO ()
debug str = putStrLn str

info :: String -> IO ()
info str = putStrLn str

-- We rely on sort order here
data Mark = Masked
          | Keyworded
          | Stable
     deriving (Eq, Ord)

instance Show Mark where
    show Masked    = "-"
    show Keyworded = "~"
    show Stable    = ""

-- We rely on sort order here
data HW = HW (String, String)
    deriving (Eq, Ord)

instance Show HW where
    show (HW (os, arch)) = arch ++ os

read_hw :: String -> HW
read_hw hw =
    let (arch, os) = break (== '-') hw
    in HW (os, arch)

data Keywords = Keywords (M.Map HW Mark)
    deriving Eq

instance Show Keywords where
    show (Keywords m) = intercalate " " $ map (\(hw, mark) -> show mark ++ show hw) $ M.toList m

read_kws :: String -> Keywords
read_kws = Keywords . M.fromList . map read_arch . words

read_arch :: String -> (HW, Mark)
read_arch ('-':kw) = (read_hw kw, Masked)
read_arch ('~':kw) = (read_hw kw, Keyworded)
read_arch      kw  = (read_hw kw, Stable)

data Ebuild = Ebuild { before_keywords :: String
                     , keywords        :: Keywords
                     , after_keywords  :: String
                     }

ltrim :: String -> String
ltrim = dropWhile isSpace

parse_ebuild :: String -> String -> Ebuild
parse_ebuild ebuild_path s_ebuild =
    let lns    = lines s_ebuild
        -- TODO: nicer pattern match and errno
        kw_lineno = case (findIndices (isPrefixOf "KEYWORDS" . ltrim) lns) of
                        [kw_ln] -> kw_ln
                        other   -> error $ ebuild_path ++ ": parse_ebuild: strange KEYWORDS lines: " ++ show other
        pre  = unlines $ take kw_lineno lns
        post = unlines $ drop (succ kw_lineno) lns
        kw_line = lns !! kw_lineno
        (pre_q1, q1)  = break (== '"') kw_line
        (kw, post_q1) = break (== '"') (tail q1)

    in  Ebuild { before_keywords = pre ++ pre_q1 ++ "\""
               , keywords        = read_kws kw
               , after_keywords  = post_q1 ++ "\n" ++ post
               }

show_ebuild :: Ebuild -> String
show_ebuild e = before_keywords e ++ show (keywords e) ++ after_keywords e

update_keywords :: Keywords -> Keywords -> Keywords
update_keywords (Keywords from) (Keywords to) =
    Keywords $ M.unionWith max from to

fetch_intersecting_ebuilds :: (FilePath, FilePath) -> FilePath -> IO [FilePath]
fetch_intersecting_ebuilds roots@(from_root_path,to_root_path) rel_path =
    do let from_full_path = from_root_path </> rel_path
           to_full_path   =   to_root_path </> rel_path

           is_ebuild = reverse ".ebuild" `isPrefixOf` reverse rel_path

       yet_dest <- fileExist to_full_path

       if yet_dest
          then do from_status <- getFileStatus from_full_path
                  to_status   <- getFileStatus   to_full_path
                  let are_files = isRegularFile from_status && isRegularFile to_status
                      are_dirs  = isDirectory   from_status && isDirectory   to_status

                  case () of
                      _ | are_files && is_ebuild -> return [rel_path]
                      _ | are_dirs -> do dir_entries <- sort . filter (`notElem` [".", "..", "_darcs", "CVS", "files", ".git"]) <$>
                                                            getDirectoryContents from_full_path
                                         sub_entries <- mapM (fetch_intersecting_ebuilds roots . (rel_path </>)) dir_entries
                                         return $ concat sub_entries
                      _ -> return []
          else return []

main :: IO ()
main = do
    args <- getArgs
    (from_tree, to_tree, pretend) <-
        case args of
            ["-p",from,to] -> return (from, to, True)
            [from,to]      -> return (from, to, False)
            _              -> usage >> undefined
    debug $ concat ["Syncing keywords from ", from_tree, " to ", to_tree, if pretend then " [dry run]" else "" ]

    intersecting_ebuilds <- fetch_intersecting_ebuilds (from_tree,to_tree) []

    debug $ concat ["Pulled ", show (length intersecting_ebuilds), " ebuild(s) for consideration"]

    forM_ intersecting_ebuilds $ \rel_path ->
        let from_ebuild = from_tree </> rel_path
            to_ebuild   = to_tree   </> rel_path
        in do from_e <- parse_ebuild from_ebuild <$> readFile from_ebuild
              to_e   <- parse_ebuild   to_ebuild <$> readFile   to_ebuild
              let new_keywords = update_keywords (keywords from_e) (keywords to_e)
                  res_e = to_e { keywords = new_keywords }
              when (keywords from_e /= keywords to_e) $
                  do info $ concat [to_ebuild, ":\n"
                                   , "    from: ", show (keywords from_e), "\n"
                                   , "      to: ", show (keywords to_e)]
              when (new_keywords /= keywords to_e) $
                  do info $ concat [ "     new: ", show (keywords  res_e)]
                     when (not pretend) $
                         writeFile to_ebuild (show_ebuild res_e)
