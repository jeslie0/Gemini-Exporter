module Gemini.Helper where

import Data.List
import Data.Char
import System.Environment
import System.IO
import System.Directory
import Control.Monad


astToHash :: String -> String
astToHash [] = []
astToHash (x:xs)
  | x == '*' = '#' : astToHash xs
  | otherwise = x : astToHash xs


addHeader :: String -> String -> String
addHeader head body = head ++ body

addFooter :: String -> String -> String
addFooter foot body = body ++ foot

-- Every org file starts with "#+title: " followed by the titlje
extractTitle :: String -> String
extractTitle text =
  if "#+title" `isSubsequenceOf` text || "#+TITLE" `isSubsequenceOf` text then
    let removedHead = drop (length "#+title: ") text
    in takeWhile (/= '\n') removedHead
  else
    ""

extractDate :: String -> Maybe String
extractDate text =
  let dateLine = filter (\x -> "#+date" `isSubsequenceOf` x || "#+DATE" `isSubsequenceOf` x) $ lines text
  in if null dateLine then Nothing
  else
    Just $ drop (length "#+DATE: ") $ head dateLine

-- We need to remove all of the org headers at the start of the file. These are all bunched together, so we use this to make removing them easier.

filterForOrg :: [FilePath] -> [FilePath]
filterForOrg [] = []
filterForOrg (x:xs)
  | ".org" `isSubsequenceOf` x = x : filterForOrg xs
  | otherwise = filterForOrg xs

filterForIndex :: [FilePath] -> [FilePath]
filterForIndex [] = []
filterForIndex (x:xs)
  | "index.org" `isSubsequenceOf` x = filterForOrg xs
  | otherwise = x:filterForOrg xs

removeOrgHeaders :: String -> String
removeOrgHeaders text
  | head text == '#' = []
  | otherwise = text

-- Links. Inline links should be removed. Links have the form "[[LINK]{DESCRIPTION]]" We want to replace inline links with descriptions

removeBetweenStrict :: (Eq a) => a -> a -> [a] -> [a]
removeBetweenStrict x y xs
  | x `notElem` xs || y `notElem` xs = xs
  | otherwise =
    let (pre, end) = break (==x) xs
        (word, post) = span (/=y) end
    in pre ++ tail post

destroyLinks :: String -> String
destroyLinks text
  | '[' `elem` text || ']' `elem` text =
    let firstLinkGone = removeBetweenStrict '[' ']' text
        niceDesc = delete '[' . delete ']' . delete ']' $ firstLinkGone
    in destroyLinks niceDesc
  | otherwise = text

between :: (Eq a) => a -> a -> [a] -> ([a], [a], [a])
between _ _ [] = ([], [], [])
between x y xs =
  let (fst, end) = break (==x) xs
      (snd, thd) = break (==y) end
  in (fst ++ [x], tail snd, thd)

lineLinks :: String -> String
lineLinks [] = []
lineLinks text =
  let (_, badLink, badDesc) = between '[' ']' text
      goodLink = purify badLink
      goodDesc = purify badDesc
  in "=> " ++ goodLink ++ "   " ++ goodDesc
  where
    purify = filter (/= '[') . filter (/= ']')

lists :: String -> String
lists [] = []
lists xs = "*" ++ tail xs


startOfTable :: String -> String
startOfTable text = "```\n" ++ text

endOfTable :: String -> String
endOfTable text = text ++ "\n```\n"

cleanLine :: String -> String
cleanLine "" = ""
cleanLine text
  | head text == '*' = "#" ++ astToHash text
  | take 2 text == "#+" = ""
  | head text == '-' = lists . destroyLinks $ text
  | head text == '[' = lineLinks text
  | otherwise = destroyLinks text

makeTables :: [String] -> [String]
-- makeTables = id
makeTables [] = []
makeTables [x]
  | null x = [x]
  | head x == '|' = (destroyLinks . endOfTable $ x):makeTables []
  | otherwise = [x]
makeTables (x:y:xs)
  | null x = x : makeTables (y:xs)
  -- | null y = (destroyLinks . endOfTable $ x):y:makeTables (xs)
  | null y && head x == '|' = (destroyLinks . endOfTable $ x):makeTables (y:xs)
  | null y && head x /= '|' = x:y:makeTables xs
  | head x == '|' && head y /= '|' = (destroyLinks . endOfTable $ x):makeTables (y:xs)
  | head x /= '|' && head y == '|' = x:(destroyLinks . startOfTable $ y):makeTables xs
  | otherwise = x:makeTables (y:xs)

orgToGem :: String -> String -> String -> String
orgToGem contents header footer =
  let title' = "# " ++ extractTitle contents
      title = extractTitle contents
      maybeDate = extractDate contents
      linesContent = lines contents
      cleanedContent = map cleanLine linesContent
      addinHeadFootTitle = [header] ++ [if title == "" then date maybeDate else title' ++ date maybeDate] ++ makeTables cleanedContent ++ [footer]
  in  orgLinkToGem . unlines $ addinHeadFootTitle
  where
    date :: Maybe String -> String
    date Nothing = ""
    date (Just str) = '\n':str

-- main :: IO String
-- main = do
--   (filepath:headerpath:footerpath:_) <- getArgs
--   contents <- readFile filepath
--   header <- readFile headerpath
--   footer <- readFile footerpath

--   let title = extractTitle contents
--       linesContent = lines contents
--       cleanedContent = map cleanLine linesContent
--       addinHeadFootTitle = [header] ++ ["# " ++ title] ++ cleanedContent ++ [footer]
--       unlined = unlines addinHeadFootTitle

--   return unlined

-- We need to extract the file tree of a directory, then map over it with the orgToGem function.

type Directory = FilePath
data FileTree = Node Directory [FilePath] [FileTree] | Empty FilePath deriving (Show, Eq)


-- We will use the following protocol for determining if a FilePath gives a directory or not: if it starts with a '.', it is ignored. If it has a '.' in it, it is a file, otherwise it is a folder

removeDotFiles :: [FilePath] -> [FilePath]
removeDotFiles = filter (\path -> head path /= '.')

extractDirs :: [FilePath] -> [FilePath]
extractDirs = filter (\path -> '.' `notElem` path)

extractFiles :: [FilePath] -> [FilePath]
extractFiles = filter (\path -> '.' `elem` path)


buildFileTree :: FilePath -> IO FileTree
buildFileTree filepath' = do
  let filepath = filepath' ++ "/"
  dirListingWithDots <- listDirectory filepath
  let dirListing = removeDotFiles dirListingWithDots
      dirs = extractDirs dirListing
      niceFileNames = map (filepath ++) (extractFiles dirListing)
      listDirs = map (filepath ++) dirs :: [FilePath]
      ioTree = mapM buildFileTree listDirs
  Node filepath niceFileNames <$> ioTree


-- changeFilePathinTree :: FilePath -> FilePath -> FileTree -> FileTree

-- changeFilePathinTree initPath outputBase Empty = Empty
-- changeFilePathinTree initPath outputBase (Node filepath files branches) =
--   let cleanedFiles = map (cleanFile initPath outputBase) files
--       cleanedTrees = map (changeFilePathinTree initPath outputBase) branches
--   in Node outputBase cleanedFiles cleanedTrees
--   where
--     cleanFile :: FilePath -> FilePath -> FilePath -> FilePath
--     cleanFile initpath newpath file = newpath ++ drop (length initpath) file


fileTreetoList :: FileTree -> [FilePath]
fileTreetoList (Empty path) = []
fileTreetoList (Node _ files branches) = files ++ concatMap fileTreetoList branches

fileTreeClone :: FilePath -> FilePath -> FileTree -> IO ()
fileTreeClone initPath outPath (Node dir files branches) = do
  let newDir = outPath ++ drop (length initPath) dir
      newFiles = map (\file -> outPath ++ drop (length initPath) file) files
  createDirectory newDir
  zipWithM_ copyFile files newFiles

  mapM_ (fileTreeClone initPath outPath) branches

-- Make new directory. copy all files to new location. map over the remaining branches
cloneDirectory :: FilePath -> FilePath -> IO ()
cloneDirectory inputDirectory outputLocation = do
  removePathForcibly outputLocation
  fileTree <- buildFileTree inputDirectory
  fileTreeClone inputDirectory outputLocation fileTree


editFilesInTree :: (FilePath -> IO ()) -> FileTree -> IO ()
editFilesInTree func (Node dir files branches) = do
  mapM_ func files
  mapM_ (editFilesInTree func) branches

copyAndEditFilesInFolder :: Directory -> Directory -> (FilePath -> IO ()) -> IO ()
copyAndEditFilesInFolder initDir outDir func = do
  cloneDirectory initDir outDir
  fileTree <- buildFileTree outDir
  editFilesInTree func fileTree



-- Blog post page maker
-- Extract title and date from org files. Make new org file called "index.org" whose contents are line links to the blog pages, in some fixed order


titleFromPath :: FilePath -> IO String
titleFromPath path = do
  contents <- readFile path
  return . extractTitle $ contents

gemListToLinks :: FilePath -> [FilePath] -> IO String
gemListToLinks path [] = return ""
gemListToLinks path (link:xs) = do
  title <- titleFromPath link
  endPart <- gemListToLinks path xs
  return $ "[[" ++ relitaviseLink path link ++ "][" ++ title ++ "]]\n" ++ endPart


buildInitGem :: FilePath -> IO String
buildInitGem path = do
  fileTree <- buildFileTree path
  let listOfFiles = fileTreetoList fileTree
      listOfOrgFiles = filterForOrg listOfFiles
      listOfOrgFiles' = filterForIndex listOfOrgFiles
  orgContents <- gemListToLinks path listOfOrgFiles'
  return $ "#+TITLE: Blog posts\n" ++ orgContents

relitaviseLink :: FilePath -> String -> String
relitaviseLink path = drop (length path + 1)


orgLinkToGem :: String -> String
orgLinkToGem "" = ""
orgLinkToGem [x] = [x]
orgLinkToGem [x,y] = [x,y]
orgLinkToGem [x,y,z] = [x,y,z]
orgLinkToGem (x:y:z:w:xs)
  | [x,y,z,w] == ".org" = ".gmi" ++ orgLinkToGem xs
  | otherwise = x:orgLinkToGem (y:z:w:xs)


blogPostMaker :: String -> IO ()
blogPostMaker dir = do
  initExists <- doesFileExist (dir ++ "/index.org")
  if initExists then do
    removeFile (dir ++ "/index.org")
    orgString <- buildInitGem dir
    writeFile (dir ++ "/index.og") orgString
    renameFile (dir ++ "/index.og") (dir ++ "/index.org")
    else do
    orgString <- buildInitGem dir
    writeFile (dir ++ "/index.og") orgString
    renameFile (dir ++ "/index.og") (dir ++ "/index.org")


-- Gem directory converter

orgGemFunc :: FilePath -> IO ()
orgGemFunc path =
  if drop (length path -4) path /= ".org" then
    return ()
    else do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let newContents = orgToGem contents header footer
    writeFile "temp" newContents
    hClose handle
    renameFile "temp" (take (length path -4) path ++ ".gmi")
    removeFile path

header :: String
header =
  "```\n  _  __           ______ _ _               _   _\n | |/ /          |  ____(_) |             | | (_)\n | ' / __ _ _ __ | |__   _| |__  _ __ __ _| |_ _  ___  _ __\n |  < / _` | '_ \\|  __| | | '_ \\| '__/ _` | __| |/ _ \\| '_ \\ \n | . \\ (_| | | | | |    | | |_) | | | (_| | |_| | (_) | | | |\n |_|\\_\\__,_|_| |_|_|    |_|_.__/|_|  \\__,_|\\__|_|\\___/|_| |_|\n\n++++++++++++++++*****************************++++++++++++++++\n                James Leslie's Gemini Capsule\n++++++++++++++++*****************************++++++++++++++++\n```"


footer :: String
footer = "\n\n# Capsule Navigation\n=> /index.gmi   Home\n=> /gemlog/index.gmi   Gemlog\n=> /contact/index.gmi   Contact"



gemDirMk :: String -> String -> IO ()
gemDirMk initDir outDir =
  copyAndEditFilesInFolder initDir outDir orgGemFunc
