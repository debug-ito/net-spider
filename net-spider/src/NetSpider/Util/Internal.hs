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

replacePrefix :: ([a] -> Maybe [a]) -> [a] -> Maybe ([a], [a])
replacePrefix f input = go [] input
  where
    go start end =
      case f start of
        Just rep -> Just (rep, end)
        Nothing ->
          case end of
            [] -> Nothing
            (x : rest) -> go (start ++ [x]) rest

-- |
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
replaceAll :: ([a] -> Maybe [a]) -> [a] -> [a]
replaceAll f input = go [] input
  where
    go top [] = top
    go top body@(bhead : brest) =
      case replacePrefix f body of
        Nothing -> go (top ++ [bhead]) brest
        Just (rep, rest) -> go (top ++ rep) rest
