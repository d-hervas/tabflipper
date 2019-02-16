import Text.Regex.Posix
import System.IO

isTab :: String -> Bool
isTab line = line =~ "^[ABCDEFG]#?\\|" :: Bool

parse :: [String] -> Int -> [String]
parse [] _ = []
parse (first:list) multiplier 
  | isTab first = reverse (take (6*multiplier) (first:list)) ++ parse (drop (6*multiplier) (first:list)) multiplier
  | otherwise = first : (parse list multiplier) 

main :: IO()
main = do
  putStrLn "Input file name"
  hFlush stdout
  file_name <- getLine
  tab <- readFile file_name
  putStrLn "Input number of line breaks between strings"
  hFlush stdout
  line_breaks <- getLine
  writeFile "output.txt" (unlines (parse (lines tab) (read line_breaks :: Int)))
  putStr "Done"
