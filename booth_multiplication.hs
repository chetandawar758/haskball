-- For multiplication of two positive binary number
import System.IO  
import System.Directory   
import Data.List  
import Data.Char
main = do
       putStrLn "Enter the number of bits:"
       nbit <- getLine
       putStrLn "Enter the two numbers:"
       num1 <- getLine
       num2 <- getLine
       let intnum1 = (0:tolist num1)
           intnum2 = (0:tolist num2) 
           bit     = read nbit
       main2 (listmake (bit+1)) intnum2 intnum1 0 (bit+1)

main2 ::[Int]->[Int]->[Int]->Int->Int->IO()
main2 a q m q0 0 = do
                   let mult = a ++ q
                   print (fromDigits mult) 
                   putStrLn "end program"
main2 a q m q0 n = do
                   if((last q) == q0)
                   then
                       do
                       let qlast = last q
                           alast = last a
                           newq  = shift q alast
                           newa  = shift a qlast
                       main2 newa newq m qlast (n-1)
                   else if((last q) < q0)
                   then
                       do
                       let sumv = binaryadd a m
                           qlast = last q
                           alast = last sumv
                           newq  = shift q alast
                           newa  = shift sumv qlast
                       main2 newa newq m qlast (n-1)
                   else
                       do
                       let sumv = binaryadd a (comp2 m)
                           qlast = last q
                           alast = sumv
                           newq  = shift q qlast
                           newa  = shift sumv qlast
                       main2 newa newq m qlast (n-1) 

--------------alist------------------------
listmake :: Int -> [Int]
listmake 0 = []
listmake n = 0:listmake (n-1)                       
                          
--------------to list-----------------------------------
tolist :: String -> [Int]
tolist [] = []
tolist (x:xs) = (toint (x)):tolist (xs)

toint :: Char -> Int
toint n = (ord n)-48

-----------------------add-----------------------------
add :: [Int]-> [Int]->Int ->[Int]
add [] [] _ = []
add xs ys r 
      | sum<2 = sum:( add (init xs) (init ys) 0)
      | sum ==3 = 1:(add (init xs)  (init ys) 1)
      | otherwise = 0: (add (init xs) (init ys) 1)
      where 
         sum = (r+(last xs)+(last ys))
binaryadd :: [Int] ->[Int]->[Int]
binaryadd xs ys = reverse(add xs ys 0)

--------------------fromDigits-------------------------

fromDigits :: [Int]->Int
fromDigits xs = foldl add 0 xs
       where add a b = 10*a+b 
-----------------------shift--------------------
shift :: [Int]->Int->[Int]
shift xs x =  (x:(init xs))

---------------------------------------------
comp1 :: [Int] ->[Int]
comp1 [] = []
comp1 xs 
       | (head xs)==0 = 1:comp1 (tail xs)
       | otherwise    = 0:comp1 (tail xs) 
---------------------------------------------
comp2 :: [Int] ->[Int]
comp2 xs = binaryadd (comp1 xs) (init(listmake (length xs)) ++ [1])
