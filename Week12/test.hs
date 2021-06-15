import qualified Data.Map as Map
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.IORef

type Name = String
type PhoneNumber = String
type PhoneBook = Map.Map Name PhoneNumber

-- Them mot lien he moi vao danh ba
addContact :: Name -> PhoneNumber -> PhoneBook -> Either String PhoneBook
addContact name number phoneBook =
  if Map.member name phoneBook then
    Left "Lien he da ton tai !"
  else
    Right (Map.insert name number phoneBook)

-- Xoa mot lien he khoi danh ba
removeContact :: Name -> PhoneBook -> Either String PhoneBook
removeContact name phoneBook =
  if Map.member name phoneBook then
    Right (Map.delete name phoneBook)
  else
    Left "Lien he khong ton tai !"

-- Tim kiem lien he theo ten
findContact :: Name -> PhoneBook -> Maybe PhoneNumber
findContact = Map.lookup

-- Hien thi tat ca cac lien he
listContacts :: PhoneBook -> [(Name, PhoneNumber)]
listContacts = Map.toList

-- Sap xep danh ba theo ten
sortContacts :: PhoneBook -> [(Name, PhoneNumber)]
sortContacts = sortOn fst . listContacts

-- Ham de xu ly ket qua tu Either
handleEitherRight :: b -> Either a b -> b
handleEitherRight _ (Right b) = b
handleEitherRight defaultValue _ = defaultValue

-- Ham hien thi menu va xu ly lua chon cua nguoi dung
menu :: IORef PhoneBook -> IO ()
menu phoneBookRef = do
  putStrLn "----- Danh Ba -----"
  putStrLn "1. Them lien he"
  putStrLn "2. Xoa lien he"
  putStrLn "3. Tim kiem lien he"
  putStrLn "4. Sap xep danh ba theo ten"
  putStrLn "0. Thoat"
  putStr "Chon tuy chon (0-4): "
  option <- getLine
  handleOption option phoneBookRef

-- Xu ly cac tuy chon cua nguoi dung
handleOption :: String -> IORef PhoneBook -> IO ()
handleOption "1" phoneBookRef = addContactMenu phoneBookRef
handleOption "2" phoneBookRef = removeContactMenu phoneBookRef
handleOption "3" phoneBookRef = findContactMenu phoneBookRef
handleOption "4" phoneBookRef = sortContactsMenu phoneBookRef
handleOption "0" _ = putStrLn "Thoat chuong trinh."
handleOption _ phoneBookRef = putStrLn "Lua chon khong hop le." >> menu phoneBookRef

-- Them lien he tu menu
addContactMenu :: IORef PhoneBook -> IO ()
addContactMenu phoneBookRef = do
  putStr "Nhap ten lien he: "
  name <- getLine
  putStr "Nhap so dien thoai: "
  number <- getLine
  phoneBook <- readIORef phoneBookRef
  let newPhoneBook = handleEitherRight phoneBook $ addContact name number phoneBook
  writeIORef phoneBookRef newPhoneBook
  putStrLn "Lien he da duoc them thanh cong."
  menu phoneBookRef

-- Xoa lien he tu menu
removeContactMenu :: IORef PhoneBook -> IO ()
removeContactMenu phoneBookRef = do
  putStr "Nhap ten lien he de xoa: "
  name <- getLine
  phoneBook <- readIORef phoneBookRef
  let newPhoneBook = handleEitherRight phoneBook $ removeContact name phoneBook
  writeIORef phoneBookRef newPhoneBook
  putStrLn "Lien he da duoc xoa thanh cong."
  menu phoneBookRef

-- Tim kiem lien he tu menu
findContactMenu :: IORef PhoneBook -> IO ()
findContactMenu phoneBookRef = do
  putStr "Nhap ten lien he de tim kiem: "
  name <- getLine
  phoneBook <- readIORef phoneBookRef
  let contact = findContact name phoneBook
  putStrLn $ "Khong tin thay nguoi ten " ++ name
  menu phoneBookRef

-- Sap xep danh ba tu menu
sortContactsMenu :: IORef PhoneBook -> IO ()
sortContactsMenu phoneBookRef = do
  phoneBook <- readIORef phoneBookRef
  putStrLn "Danh sach lien he sau khi sap xep: "
  print $ sortContacts phoneBook
  menu phoneBookRef

-- Ham chinh de chay chuong trinh
main :: IO ()
main = do
  let initialPhoneBook = Map.fromList [("Alice", "123-4567"), ("Yob", "987-6543"), ("Dharlie", "555-5555")]
  phoneBookRef <- newIORef initialPhoneBook
  putStrLn "Danh sach lien he ban dau: "
  print $ listContacts initialPhoneBook
  menu phoneBookRef
