-- Tìm hiểu 5 hàm, kiểu dữ liệu mới mà bạn thấy hữu ích, mô tả cách dùng và ví dụ

-- 1. Hàm pipe:

-- Cách dùng: Hàm pipe cho phép kết hợp các hàm một cách đơn giản. Nó hoạt động bằng cách truyền đầu ra của hàm trước vào hàm tiếp theo như tham số.
-- Ví dụ:
-- ghci> pipe (head . filter even) [1, 2, 3, 4, 5, 6]

-- 2. Hàm traverse:

-- Cách dùng: Hàm traverse áp dụng một hàm cho mỗi phần tử trong một cấu trúc dữ liệu và trả về cấu trúc dữ liệu mới với kết quả được áp dụng. Nó hoạt động tốt với các danh sách, tập hợp và các kiểu dữ liệu lồng nhau khác.
-- Ví dụ:
-- ghci> traverse (\x -> if odd x then Just x else Nothing)  [1,2,3,4]
-- Nothing
-- ghci> traverse id [Right 1, Right 2, Right 3, Right 4]
-- Right [1,2,3,4]
-- ghci> traverse id [Right 1, Right 2, Right 3, Right 4, Left 0]
-- Left 0

-- 3. Kiểu dữ liệu Either:

-- Cách dùng: Kiểu dữ liệu Either được sử dụng để biểu diễn hai trường hợp có thể xảy ra. Nó có hai constructor: Left và Right. Left được sử dụng để biểu thị lỗi hoặc trường hợp thất bại, trong khi Right được sử dụng để biểu thị giá trị thành công.
-- Ví dụ:
divide :: Int -> Int -> Either String Int
divide x 0 = Left "Cannot divide by zero"
divide x y = Right (x `div` y)

-- 4. Hàm try:

-- Cách dùng: Hàm try cho phép bọc một biểu thức có thể gây lỗi trong một monadic Either. Nó trả về Ok nếu biểu thức thành công và Error nếu xảy ra lỗi.

-- 5. Hàm bracket:

-- Cách dùng: Hàm bracket được sử dụng để đảm bảo rằng các tài nguyên được giải phóng đúng cách sau khi sử dụng. Nó hoạt động bằng cách thực hiện một hành động, sau đó thực hiện một hành động dọn dẹp nếu hành động đầu tiên thất bại.
-- Signature:
-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c