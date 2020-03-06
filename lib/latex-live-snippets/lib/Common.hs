{-# LANGUAGE ViewPatterns #-}

module Common where

import Data.List.Utils (replace)

escapeLatexChars :: String -> String
escapeLatexChars
  = replace "~" "!\\tyeq!"


escapeGHCILatexChars :: String -> String
escapeGHCILatexChars
  = id

    -- escapeLatexChars
  -- . replace "!!!!!!!!!!" "\\"
  -- . replace "\\" "!!!!!!!!!!textbackslash{}"
  -- . replace "{" "!!!!!!!!!!{"
  -- . replace "}" "!!!!!!!!!!}"


runSub :: Maybe (String -> String) -> String -> String
runSub Nothing = id
runSub (Just f) = f

getSub :: String -> (String, Maybe (String -> String))
getSub ('/' : line) =
  let (rep,  tail -> line') = span (/= '/') line
      (with, tail -> str')  = span (/= '/') line'
   in (str', Just $ replace rep with)
getSub str = (str, Nothing)

