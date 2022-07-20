{-# LANGUAGE ScopedTypeVariables #-}

module Homework15B where

import Data.Char (isDigit, isLower, isUpper)
import Data.Maybe (catMaybes)
import Control.Monad (guard)
import Control.Exception (try, IOException)

--------------------------------------------------------------------------------
-- 1. Write a function that takes a list and returns the head if the list is not empty.
-- If the list is empty, return Nothing.
headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

--------------------------------------------------------------------------------
-- 2. Write a function that takes a list of Maybe values and returns a list of all the Just values.
-- If there are no Just values, return an empty list.
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of
                            Just val -> val : acc
                            Nothing  -> acc) []

--------------------------------------------------------------------------------
-- 3. Write a function that tries to read from a file and returns the contents of the file.
-- If the file does not exist, return Nothing.
readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe fn = do
  result <- tryReadFile fn
  return $ case result of
    Left _  -> Nothing
    Right content -> Just content

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile fn = try (readFile fn)

--------------------------------------------------------------------------------
-- 4. Write a function that checks all the requirements for a password using the
-- Either type with a custom data type for errors.
-- The requirements are:
-- - The password must be at least 10 characters long.
-- - The password must contain at least one digit.
-- - The password must contain at least one uppercase letter.
-- - The password must contain at least one lowercase letter.

data PasswordError = TooShort
                   | NoDigit
                   | NoUppercase
                   | NoLowercase
                   deriving Show

-- Check if the password is long enough
passwordLongEnough :: String -> Either PasswordError String
passwordLongEnough pwd
  | length pwd >= 10 = Right pwd
  | otherwise         = Left TooShort

-- Check if the password has at least one digit
passwordHasDigit :: String -> Either PasswordError String
passwordHasDigit pwd
  | any isDigit pwd = Right pwd
  | otherwise       = Left NoDigit

-- Check if the password has at least one uppercase letter
passwordHasUppercase :: String -> Either PasswordError String
passwordHasUppercase pwd
  | any isUpper pwd = Right pwd
  | otherwise       = Left NoUppercase

-- Check if the password has at least one lowercase letter
passwordHasLowercase :: String -> Either PasswordError String
passwordHasLowercase pwd
  | any isLower pwd = Right pwd
  | otherwise       = Left NoLowercase

-- Check all the password requirements
passwordRequirements :: String -> Either PasswordError String
passwordRequirements pwd = do
  pwd' <- passwordLongEnough pwd
  pwd'' <- passwordHasDigit pwd'
  pwd''' <- passwordHasUppercase pwd''
  passwordHasLowercase pwd'''

