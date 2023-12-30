module Reshume (
    parseJson, replaceKeys,
    findPhrase, -- Test-funcs
    test, test2 -- Types (expose to main)

) where

import Types
import Text.JSON (Result (Ok, Error), decode)
import Control.Applicative
import Data.List
import Data.Char

parseJson :: String -> [ConfEntry]
parseJson s = takeOk (decode s :: Result [ConfEntry])
              where takeOk (Ok v) = v
                    takeOk (Error s) = error s


replaceKeys :: [String] -> String -> [ConfEntry] -> [String]
replaceKeys [] _ _ = []
--replaceKeys (content:rest) prefix vals = content:rest
replaceKeys (content:rest) prefix vals = case left of
                                            v@(Just a) -> a
                                            Nothing -> content:(replaceKeys rest prefix vals)
                                            --Nothing -> replaceKeys rest prefix vals >>= return . (content:)
                                         where left = do
                                                         (phrase, func) <- findPhrase content prefix
                                                         E _ val <- find (\(E a _) -> a == phrase) vals
                                                         let replaced = handleConf val func
                                                         --return replaced
                                                         return $ replaceKeys (replaced ++ rest) prefix vals
                                         

handleConf :: ConfVal            -- ^ The value to handle
           -> (String -> String) -- ^ The callback function that mods the string the string
           -> [String]           -- ^ List of modified strings
handleConf (Single s) f = [f s]
handleConf (List xs) f = map f xs
           
findAndReplacePhrase :: String      -- ^ Phrase
                     -> String      -- ^ Prefix
                     -> [ConfEntry] -- ^ Replacers
                     -> Maybe [String]    -- ^ Modified string (list because duplications may be needed)
findAndReplacePhrase _ _ [] = return []
findAndReplacePhrase str prefix vals = do (phrase, func) <- findPhrase str prefix
                                          E _ val <- find (\(E a _) -> a == phrase) vals
                                          return []  
                                          -- TODO: HANDLE LIST
                                       -- <|> findPhrase  

findPhrase :: String -- ^ Phrase to search
           -> String -- ^ Prefix
           -> Maybe (String, String -> String) -- ^ The phrase and a callback function to replace the phrase
findPhrase a b = findPhraseCont a b id
--findPhrase [] _ = Nothing
--findPhrase w@(x:xs) prefix = if isPrefixOf prefix w then Just ("Bitch", id) else findPhrase xs prefix

findPhraseCont :: String                           -- ^ Phrase to search
               -> String                           -- ^ Prefix
               -> (String -> String)               -- ^ Cont func
               -> Maybe (String, String -> String) -- ^ The phrase and a callback function to replace the phrase
findPhraseCont [] _ _  = Nothing
findPhraseCont w@(x:xs) prefix cont = if isPrefixOf prefix w
                                        then Just (takeWhile pred w', \s -> cont s ++ (dropWhile pred w') )
                                        else findPhraseCont xs prefix (\s -> cont (x:s))
                                            where pred c = isLetter c || isNumber c
                                                  w' = w \\ prefix
