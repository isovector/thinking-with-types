module Main where

import Common (escapeLatexChars)
import Control.Lens ((^.))
import Control.Monad (guard)
import Data.List
import Data.List.Utils (replace)
import Data.Maybe (listToMaybe, isNothing, fromJust, fromMaybe)
import System.Directory
import System.Environment
import System.FilePath.Lens (basename)
import System.FilePath.Posix


main :: IO ()
main = do
  let dir = ".latex-live-snippets"

  filename <- head <$> getArgs
  decl <- listToMaybe . tail <$> getArgs
  file <- readFile filename
  createDirectoryIfMissing True dir
  let filename' = dir </> filename ^. basename ++ "." ++ fromMaybe "FILE" decl ++ ".tex"
  writeFile filename' $ getDefinition file decl


matchDefinition :: String -> String -> Maybe ([String] -> [String])
matchDefinition decl line =
  listToMaybe $ do
    (form, f) <- [ ("", id)
                 , ("-- # ", tail)
                 , ("type family ", id)
                 , ("data family ", id)
                 , ("data ", id)
                 , ("type ", id)
                 , ("newtype ", id)
                 , ("class ", id)
                 ]
    guard $ isPrefixOf (form ++ decl) line
    pure f


getDefinition :: String -> Maybe String -> String
getDefinition file (Just decl)
    = unlines
    . ("\\begin{code}" :)
    . (++ ["\\end{code}"])
    . fmap annotate
    . fmap escapeLatexChars
    . func
    $ ls
  where
    ls = takeWhile (not . null)
       . dropWhile (isNothing . matchDefinition decl)
       $ lines file
    func = fromJust . matchDefinition decl $ head ls
getDefinition file Nothing
    = unlines
    . ("\\begin{code}" :)
    . (++ ["\\end{code}"])
    . fmap annotate
    . fmap escapeLatexChars
    $ lines file



annotate :: String -> String
annotate [] = ""
annotate ('-':'-':' ':'!':' ':zs) = "!\\annotate{" ++ zs ++ "}!"
annotate (a:as) = a : annotate as

