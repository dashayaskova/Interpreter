module Main where
import Lib

main :: IO ()
main = do
       name <- getName
       x <- parseFile name
       putStrLn $ show $ x