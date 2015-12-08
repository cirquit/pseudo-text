{-# LANGUAGE OverloadedStrings, BangPatterns, ScopedTypeVariables #-}

module Main where

import System.Directory (doesFileExist)
import Control.Monad    (forever)
import System.Exit      (exitSuccess)
import System.Random    (randomRIO)
import System.IO        (hSetBuffering, stdout, BufferMode(..))
import Debug.Trace

import qualified Data.HashMap as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO (readFile, putStrLn)
import qualified Data.Vector  as V

type Storage = M.HashMap T.Text (V.Vector (T.Text, Int))

printSentence :: Int -> Storage -> IO ()
printSentence n store = printS n store "non" ["non"]
  where printS :: Int -> Storage -> T.Text -> [T.Text] -> IO ()
        printS 0 store word acc = TIO.putStrLn (T.unwords acc)
        printS n store word acc = do
          case M.lookup word store of
              Just vec -> do
                 let vlen = length vec
                 index <- randomRIO (0, vlen-1)
                 let (word',_) = vec V.! index
                 printS (n-1) store word' (acc ++ [word'])
              Nothing  -> undefined -- TIO.putStrLn $ (T.pack "Could not find a map under key: ") ++ word

normalize :: Storage -> Storage
normalize = M.map (V.foldl' norm V.empty)
  where norm acc (w,i)
          | i < 2     = acc
          | otherwise = V.cons (w,i) acc

updateMap :: T.Text -> Storage -> Storage
updateMap text store = update words store
  where words = T.words text
        update :: [T.Text] -> Storage -> Storage
        update []       store = store -- trace (show store) store
        update [y]      store = store -- trace (show store) store
        update (w:y:ws) store =
            case M.lookup w store of
                Just vec ->
                    let !vec'   = updateV y vec
                        !store' = M.insert w vec' store
                    in update (y:ws) store'
                Nothing          ->
                    let !vec    = V.singleton (y, 1)
                        !store' = M.insert w vec store
                    in update (y:ws) store'

updateV :: T.Text -> V.Vector (T.Text, Int) -> V.Vector (T.Text, Int)
updateV e v
    | (Just _) <- lookupV e v = V.map (isEqInc e) v
    | otherwise    = V.cons (e,1) v
  where isEqInc a (b,i) | a == b    = (b, i + 1)
                        | otherwise = (b, i)

lookupV :: T.Text -> Vector (T.Text, Int) -> Maybe T.Text
lookupV e = findElem (\(a,_) -> a == e)

parseInputFiles :: [FilePath] -> Storage -> IO Storage
parseInputFiles []       store = return store
parseInputFiles (fp:fps) store = do
    exists <- doesFileExist fp
    case exists of
        True  -> do
            putStrLn $ fp ++ " exists!"
            input <- TIO.readFile fp
            let !nstore = updateMap input store
            parseInputFiles fps nstore
        False -> do
            putStrLn $ "Could not find file: " ++ fp
            parseInputFiles fps store

loop :: Storage -> IO ()
loop store = do
    hSetBuffering stdout NoBuffering
    forever $ do
        putStr "How many thunks: "
        input <- getLine
        case input of
            _ | [(n :: Int, [])] <- reads input -> printSentence n store
            _ | True             <- quit input  -> exitSuccess
            _                                   -> putStrLn "This is not a valid input."

  where quit "q"    = True
        quit "Q"    = True
        quit ":Q"   = True
        quit ":q"   = True
        quit "exit" = True
        quit "quit" = True
        quit _      = False

createRandomMap :: [FilePath] -> IO Map Int T.Text
createRandomMap []       txt = foldl' (\acc (i,x) -> M.insert i x acc) M.empty (T.words txt)
createRandomMap (fp:fps) txt = do
    exists <- doesFileExist fp
    case exists of
        True  -> do
            input <- TIO.readFile fp
            parseInputFiles fps (txt ++ input)
        False -> do
            putStrLn $ "Could not find file: " ++ fp
            parseInputFiles fps store


main :: IO ()
main = do
    let fps = ["b_04_06_2015.txt", "b_05_06_2015.txt", "b_08_06_2015.txt", "b_09_06_2015.txt"]
    !store <- parseInputFiles fps M.empty
    -- let !nstore = normalize store
    loop nstore