module Main where

import Control.Applicative
import Data.List
import Reshume
import System.Directory
import System.Environment
import System.IO
import System.Process

testPhrase = "hejsa du er mega #!First1 og jeg synes du er #!Hvad"

main :: IO ()
main = do
    args <- getArgs
    (a, b) <- parseArgs args
    let conf = parseJson b
    print conf
    Just (w, f) <- return $ findPhrase testPhrase "#!"
    putStrLn w
    putStrLn $ f "erstat"
    Nothing <- return $ findPhrase "hejsa du er mega #! cringe" "##!"
    print $ replaceKeys (lines testPhrase) "#!" conf
    let pruned = unlines $ replaceKeys (lines a) "@!" conf
    putStrLn "replaced file"
    putStrLn pruned
    withCreateProcess (proc "pdflatex" ["-interaction=nonstopmode"]) {std_in = CreatePipe} $ \(Just stdin) _ _ ph -- TODO: ARGS (output and others)
     -> do
        hPutStr stdin pruned
        hFlush stdin
        waitForProcess ph
    putStrLn "done"

-- TODO: Prettify with CLI tool (args and such)
parseArgs :: [String] -> IO (String, String)
parseArgs [template, content] = do
                --t <- getTemplates template
    t <- getConfiguration template
    e <- getConfiguration content
    return (t, e)
parseArgs _ = error "Wrong amount of arguments, try again (stupid bitch)"

getConfiguration :: String -> IO String
getConfiguration path = readFile path <|> error ("Invalid file path: " ++ path)

getTemplates :: String -> IO [String]
getTemplates path =
    do
        files <- getDirectoryContents path
        contents $ filter (not . isPrefixOf ".") files -- TODO: Directory not quite working
        <|> pure <$> readFile path
        <|> error ("Invalid file path: " ++ path)
  where
    contents (x:xs) = do
        xs' <- contents xs
        putStrLn x
        x' <- readFile x
        return (x' : xs')
    contents [] = return []
