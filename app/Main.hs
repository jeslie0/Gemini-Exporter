module Main where
import Gemini.Helper
import System.Environment

main :: IO ()
main = do
  (initDir:outDir:orgDir:_) <- getArgs
  blogPostMaker orgDir
  gemDirMk initDir outDir
