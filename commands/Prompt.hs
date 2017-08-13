{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
--import Common
import Control.Monad (join)
import Data.Monoid ((<>))
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Maybe

type Path = String

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "list directories"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> argument str
            (  metavar "STRING"
            <> help "string parameter"
            )
        <*> option auto
            (  long "limit"
            <> short 'l'
            <> metavar "INT"
            <> help "number parameter"
            <> value 0
            <> showDefault
            )

getShort :: String -> String
getShort "" = ""
getShort a = concatMap
  (\x ->
     if ((length x) > 3) && (not $ elem '»' x)
     then (take 3 x ++ "…/")
     else (x ++ "/")
  )
  $ splitOn "/" a

iteration :: String -> String -> String
iteration x y = "/" ++ getShort x ++ y

joinBy :: [a] -> [[a]] -> [a]
joinBy sep cont = drop (length sep) $ concat $ map (\w -> sep ++ w) cont


zipLeftover :: [a] -> [a] -> [a]
zipLeftover []     []     = []
zipLeftover xs     []     = xs
zipLeftover []     ys     = ys
zipLeftover (_:xs) (_:ys) = zipLeftover xs ys

lastN :: Int -> [a] -> [a]
lastN n xs = zipLeftover (drop n xs) xs

splitAtN :: Int -> [a] -> ([a], [a])
splitAtN n x = (take n x, lastN (length x - n) x)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

applyAll :: [(a -> a)] -> a -> a
applyAll [] = id
applyAll (f:fs) = applyAll fs . f

work :: Path -> Int -> IO ()
work a b = do
  let a' = applyAll
        [ replace "/home/chris/Projects/Clients" "»Clients"
        , replace "/home/chris/Projects/Haskell" "»Haskell"
        , replace "/home/chris/Projects" "»Projects"
        , replace "/home/chris/" "~"
        ] a
  let dirsList = filter ((/=) "") $ splitOn "/" a'
  let range = [0 .. (max ((length dirsList) - 1) (0))]
  let shortened i = iteration (joinBy "/" $ p1) (intercalate "/" p2) where
               (p1,p2) = splitAtN i dirsList
  let mapped = map (\i -> do
           case (length (shortened i) < b) of
             True -> Just $ shortened i ++ "/"
             False -> Nothing
           ) range
  case (catMaybes $ (mapped ++ [Just $ shortened $ last range])) of
    (x:_) -> putStrLn $ applyAll
                        [ replace "/»" "»"
                        , replace "/~" "~"
                        ] x
    _ -> error "Something went wrong"
