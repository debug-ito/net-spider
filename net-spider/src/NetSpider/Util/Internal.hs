-- |
-- Module: NetSpider.Util.Internal
-- Description: 
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- __internal module__
module NetSpider.Util.Internal
  ( replaceAll,
    replacePrefix
  ) where

-- | Match and replace a prefix of the input string. Only non-empty
-- prefixes are input to the matcher.
replacePrefixNonEmpty :: ([a] -> Maybe [a]) -> [a] -> Maybe ([a], [a])
replacePrefixNonEmpty _ [] = Nothing
replacePrefixNonEmpty f (input_head : input_rest) = go [input_head] input_rest
  where
    go start end =
      case f start of
        Just rep -> Just (rep, end)
        Nothing ->
          case end of
            [] -> Nothing
            (x : rest) -> go (start ++ [x]) rest

-- | Like 'replacePrefixNonEmpty', but this one inputs the empty
-- prefix to the matcher function at first.
replacePrefix :: ([a] -> Maybe [a]) -> [a] -> Maybe ([a], [a])
replacePrefix f input =
  case f [] of
    Just rep -> Just (rep, input)
    Nothing -> replacePrefixNonEmpty f input

-- | Match and replace substrings in the input list. Only non-empty
-- lists are input to the matcher function.
--
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) ""
-- ""
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) "uuu"
-- "uuu"
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) "uffu"
-- "uffffffu"
-- >>> replaceAll (\x -> if x == "f" then Just "fff" else Nothing) "fuuf"
-- "fffuufff"
-- >>> replaceAll (\x -> case x of "f" -> Just "fff"; "go" -> Just "gg"; "hii" -> Just "H"; _ -> Nothing) "gfgoihgfhiig"
-- "gfffggihgfffHg"
replaceAll :: ([a] -> Maybe [a]) -- ^ matcher/replacer
           -> [a] -- ^ input string
           -> [a]
replaceAll f input = go [] input
  where
    go top [] = top
    go top body@(bhead : brest) =
      case replacePrefixNonEmpty f body of
        Nothing -> go (top ++ [bhead]) brest
        Just (rep, rest) -> go (top ++ rep) rest
