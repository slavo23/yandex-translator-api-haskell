{-# LANGUAGE OverloadedStrings #-}

module Main  where

import           Control.Lens    hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Char
import qualified Data.List       as L
import qualified Data.Text as T(pack, unpack)
import           Network.Wreq
import           System.IO


apiUrl :: API
apiUrl = "https://translate.yandex.net/api/v1.5/tr.json/translate"

langParam :: API
langParam = "en-ru"


type API = String


formatWords :: [String] -> String
formatWords list = readyString
            where  cleanWords  =   L.map  (L.filter isLetter) list
                   readyString =   concat
                                 . L.intersperse "\n" 
                                 $ L.map (++";") cleanWords


makeRequest :: String -> IO String
makeRequest wList = do 
                       putStrLn "Paste your Yandex API key: "
                       k   <- getLine
                       res <- post (apiUrl) [
                                            "lang":=langParam,
                                            "key":=k,
                                            "text":=wList
                                            ]
                       {--Format text contents of an array to regular string--}               
                       pure $ T.unpack      (res ^.
                                            responseBody
                                            . key "text"
                                            . _Array 
                                            . each
                                            . _String)



concatTriple :: (Int, String, String) -> String
concatTriple (a, b, c) = (show a ++ ". ") ++ b ++ " " ++ c


formatOutput :: [String] -> [String] -> String
formatOutput eng rus =  concat . L.intersperse "\n" $ zipped
                        where zipped      =   map concatTriple $ zip3 enumeration original translation
                              enumeration =   [1..(length rus)]
                              original    =   (L.map (filter isLetter) rus)
                              translation =   eng

main :: IO ()
main = do
        hSetEncoding stdin utf8 {--To make cyrillic chars visible, encoding is set this way--}
        wordsFile <- openFile "words.txt" ReadMode
        cts       <- hGetContents wordsFile
        let wrds =   formatWords (lines cts)
        tr        <- makeRequest wrds
        writeFile "output.txt" $ formatOutput (lines tr) (words wrds)
        hClose wordsFile

