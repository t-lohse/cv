{-# LANGUAGE FlexibleInstances #-}
module Types (
    test, test2, ConfEntry (E), ConfVal (Single, List)
) where

import Data.List (intercalate)
import Text.JSON

data ConfVal = Single String | List [String]

instance Show (ConfVal) where
    show (Single v) = v
    show (List xs) = intercalate ", " $ xs

instance JSON ConfVal where
    showJSON (Single a) = JSString $ toJSString a
    showJSON (List a) = showJSON $ map (toJSString) a

    readJSON (JSArray xs) = do res <- parseArr xs
                               Ok $ List res
        where parseArr ((JSString v):xs) = do
                                              other <- parseArr xs
                                              Ok $ (fromJSString v):other
              parseArr [] = Ok []
              parseArr _ = Error "Invalid format"
    readJSON (JSString s) = Ok $ Single $ fromJSString s
    readJSON _ = Error "Incorrect format"


test = decode "[\"What\", \"The\", [\"actual\", \"fuck\"]]" :: Result [ConfVal]


data ConfEntry = E String ConfVal

instance Show ConfEntry where
    show (E s c) = s ++ ": " ++ (show c)

instance JSON ConfEntry where
    --showJSON a = toJSObject a
    showJSON _ = undefined

    readJSON (JSObject a) | length arr == 1 = do
                                                    v' <- readJSON v :: Result ConfVal
                                                    Ok $ E k v'
                          | otherwise = Error "Object should only have one entry"
                            where arr = fromJSObject a
                                  (k, v) = head arr

    --readJSON (JSString s) = Ok $ Single $ fromJSString s
    readJSON _ = Error "Incorrect format"


instance {-# OVERLAPPING #-} JSON [ConfEntry] where
    showJSON _ = undefined

    readJSON (JSObject a) = parseObj arr
                            where arr = fromJSObject a
                                  parseObj (x:xs) = do
                                                        x' <- readJSON (JSObject $ toJSObject [x]) :: Result ConfEntry
                                                        xs' <- parseObj xs
                                                        Ok $ x':xs'
                                  parseObj [] = Ok []
    readJSON _ = Error "Incorrect format"

test2 = decode "{\"First\": \"What\", \"Sec\": \"The\", \"third\": [\"actual\", \"fuck\"]}" :: Result [ConfEntry]
    

