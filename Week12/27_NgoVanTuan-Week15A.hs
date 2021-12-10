module Main where

import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)
import Data.Maybe (fromMaybe)
import Data.Either (either)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md file before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- This is a CLI application that allows the user to manage a TODO list.
-- The user can add and remove items from the list and save and load the list
-- from a file.
-- It's a working prototype, but it has some bugs. Specifically, it doesn't
-- handle errors very well. Your mission, should you choose to accept it, is to
-- fix those bugs and make the application more robust. Hint: Try to interact
-- with the application in unexpected ways and see what happens! You should be
-- able to find and fix 3 bugs.


printTodoItem :: (Int, String) -> IO ()
printTodoItem (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
  putStrLn ""
  putStrLn "Current TODO list:"
  foldr (\x k -> printTodoItem x >> k) (return ()) (zip [0 ..] todos)
  command <- getLine
  interpretCommand command todos

-- Sử dụng Maybe để xử lý trường hợp không hợp lệ khi xóa mục
delete :: Int -> [a] -> Maybe [a]
delete _ [] = Nothing
delete 0 (_ : as) = Just as
delete n (a : as) = (a :) <$> delete (n - 1) as

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos = case cmd of
  "q" -> return ()
  ('+' : ' ' : todo) -> prompt (todo : todos)
  ('-' : ' ' : num) -> case reads num of
    [(n, "")] -> case delete n todos of
      Nothing -> do
        putStrLn "Invalid number: No such item to delete."
        prompt todos
      Just newTodos -> prompt newTodos
    _ -> do
      putStrLn "Invalid number format."
      prompt todos
  ('s' : ' ' : fn) -> do
    result <- tryWriteFile fn (show todos)
    either putStrLn (const $ return ()) result
    prompt todos
  ('l' : ' ' : fn) -> do
    result <- tryReadFile fn
    either putStrLn prompt result
  -- Xử lý lệnh không hợp lệ
  _ -> do
    putStrLn ("Invalid command: `" ++ cmd ++ "`")
    prompt todos

-- Sử dụng Either và Exception để xử lý lỗi khi ghi tệp
tryWriteFile :: FilePath -> String -> IO (Either String ())
tryWriteFile fn content = catch (Right <$> writeFile fn content) handler
  where
    handler :: IOException -> IO (Either String ())
    handler e = return . Left $ "Error writing to file: " ++ show e

-- Sử dụng Either để xử lý lỗi khi đọc tệp
tryReadFile :: FilePath -> IO (Either String [String])
tryReadFile fn = catch (Right . read <$> readFile fn) handler
  where
    handler :: IOException -> IO (Either String [String])
    handler e
      | isDoesNotExistError e = return . Left $ "File does not exist: " ++ fn
      | otherwise = return . Left $ "Error reading file: " ++ show e

printCommands :: IO ()
printCommands = do
  putStrLn "Commands:"
  putStrLn "+ <Item Name>   - Add a TODO entry"
  putStrLn "- <Item Number> - Delete the numbered entry"
  putStrLn "s <File Name>   - Save the current list of TODOs"
  putStrLn "l <File Name>   - Load the saved list of TODOs"
  putStrLn "q               - Quit without saving"

main :: IO ()
main = do
  printCommands
  prompt []

  ok ne
  
