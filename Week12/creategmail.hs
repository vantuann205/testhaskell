{--
Đăng kí tài khoản và mật khẩu gmail sử dụng hàm bậc cao hàm  lambda đệ quy hàm foldr if else pattern matching và guard ạ 
Nếu chúng ta nhập định dạng gmail mà sai thì nó sẽ in thông báo ra 
Vd: example@gmailcom thì sẽ in ra thiếu đuôi .com hay nếu k có @ sẽ in ra thiếu @ ... về mật khẩu sẽ yêu cầu có đủ 
chữ thường chữ viết hoa kí tự đặc biệt và số.
--}

-- Ham phu tro

isLowerchar :: Char -> Bool
isLowerchar c = c >='a' && c <= 'z'

isUpperchar :: Char -> Bool
isUpperchar c = c>='A' && c<='Z'

isdigitnumber :: Char -> Bool
isdigitnumber c = c>='0' && c<='9'

kitudacbiet :: Char -> Bool
kitudacbiet c = c `elem` "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

coduoi :: String -> String -> Bool
coduoi duoi str = drop(length str -length duoi) str==duoi

miengmail :: String -> Bool
miengmail str = coduoi "gmail.com" str

cokitu :: Char -> String -> Bool
cokitu _ [] = False
cokitu c (x:str) = c == x || cokitu c str

--Ham chinh
kiemtrapassword :: String -> String 
kiemtrapassword matkhau =
    let 
        ktrachuhoa = if any isUpperchar matkhau then "" else "Mat khau thieu ki tu in hoa. "
        ktraso = if any isdigitnumber matkhau then "" else "Mat khau chua co ki tu so. "
        ktraktudacbiet = if any kitudacbiet matkhau then "" else "Mat khau chua co ki tu dac biet. "
    in foldr (\x acc -> x++acc) "" [ktrachuhoa,ktraktudacbiet,ktraso]

kiemtradinhdangemail :: String -> String 
kiemtradinhdangemail email =
    let
        ktradiachi = if miengmail email then "" else "Dia chi email phai la gamil.com. "
        ktrakitu = if cokitu '@' email then "" else "Thieu @ trong dia chi email. "
        ktraduoi = if coduoi ".com" email then "" else "Thieu duoi .com trong dia chi email. "
    in foldr (\x acc -> x++acc) "" [ktradiachi,ktrakitu,ktraduoi]

ktraemailvamatkhau :: String -> String -> String
ktraemailvamatkhau email matkhau =
    case kiemtradinhdangemail email of
        "" -> case kiemtrapassword matkhau of 
            "" -> "Da tao thanh cong"
            loi -> loi
        loi -> loi

