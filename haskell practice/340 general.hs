plusOne :: (Num a)=> a -> a
plusOne x = x + 1
-- square a Num
square :: (Num a)=> a -> a
square x = x * x
-- use of if else then
smaller :: (Ord a)=> a -> a -> a
smaller a b = if (a<b)
              then a 
			  else b
-- use of Guarded equations 			  
smaller2 :: (Ord a)=> a -> a -> a			  
smaller2 a b | (a<b)     = a
             | otherwise = b	
-- use of pattern matching			 
plusTo :: (Num a, Eq a)=> a -> a
plusTo 0 = 0
plusTo a = a + (plusTo (a-1))
-- use of where
plusTo2 :: (Num a, Eq a)=> a -> a
plusTo2 0 = 0
plusTo2 a = a + adding
  where adding = plusTo2 (a-1)
-- use of until
doubleTo100 :: (Num a,Ord a)=> a -> a
doubleTo100 a = until (>100) (*2) a
-- basic haskell type : Bool, Float, Double, Integer, Int, Char, String
aString :: String
aString = "I hate haskell"
bChar :: Char
bChar = '"' 
-- (Int -> Int -> Int) = (Int -> (Int -> Int)) /= ((Int -> Int) -> Int)
-- aeb = a * 10^b
cBool :: Bool
cBool = (12e-2 == 0.12)
-- give (n-t) argument to a function require n argument create a new function require t argument
sumOfThree a b c = a+b+c
sumOf1And2AndOther = sumOfThree 1 2
-- Haskell is lazy: the value for x is not needed, so it is not evaluated
dump x = 7
resultOne = (7 == (dump (iterate (*2) 2)))
-- define a function lamaba way
sumTwo a b = a+b
sumTwo2 = \x -> (\y -> x+y)









--There are several type classes pre-de ned in Haskell
--class Show : values that can be displayed
--class Eq : values that can be tested for equality ==
--class Enum : values that can be counted
--class Num : values that can be used in numeric expressions
--class Fractional : subclass of Num for operations that need
--oating point ( Float or Double )
--class Ord : values that can be ordered using <
--and lots of others
--Know these basic ones; be aware that others exist (especially
--when interpreting error messages)


type Mantissa = Integer
type Exponent = Integer

data MyFloat = MyFloat (Mantissa,Exponent)

-- Custom show instance (not required)
instance Show MyFloat where
    show = myfloatShow

myfloatShow :: MyFloat -> String
myfloatShow (MyFloat (m,e)) | m < 0   = "-0." ++ (show (negate m)) ++ "*10^" ++ (show e)
                          | otherwise = "0." ++ (show m) ++ "*10^" ++ (show e)
						  
instance Eq MyFloat where
    (==) = myfloatEq

myfloatEq :: MyFloat -> MyFloat -> Bool
myfloatEq (MyFloat (0,_)) (MyFloat (0,_)) = True
myfloatEq (MyFloat (m1,e1)) (MyFloat (m2,e2)) 
    | otherwise = ((trimZeroes m1) == (trimZeroes m2)) && (e1 == e2)
    where trimZeroes 0 = 0
          trimZeroes n = if n `mod` 10 == 0 then trimZeroes (n `div` 10) else n

whole :: MyFloat -> Integer
whole (MyFloat (m,e)) | e <= 0 || m == 0 = 0
                      | m < 0 = negate (whole (MyFloat (-m, e)))
                      | decPlaces(m) > e = whole (MyFloat (div m 10, e))
                      | decPlaces(m) < e = whole (MyFloat (m*10, e))
                      | otherwise = m

fraction :: MyFloat -> Double
fraction (MyFloat (m,e)) = normalize(fromIntegral m) * (fromIntegral 10 ^^ e) - (fromIntegral (whole (MyFloat (m,e))))
  where normalize x | abs(x) < 1.0 = x
                    | otherwise = normalize (x / 10.0)

-- (abs, signum, fromInteger not required)
instance Num MyFloat where
    (+) = adjustMantissa
    (-) = myFloatSub
    (*) = myFloatMul 
    negate = myFloatNegate  
    abs = myFloatAbs
--    signum = myFloatSignum
--    fromInteger = myFloatFromInteger

-- Equalizes the number of digits in the mantissas before passing on to shiftMantissa
adjustMantissa :: MyFloat -> MyFloat -> MyFloat
adjustMantissa (MyFloat (m1,e1)) (MyFloat (m2,e2)) 
    | ((decPlaces m1) /= (decPlaces m2)) = if (decPlaces m1) > (decPlaces m2) 
                                               then shiftMantissa (MyFloat (m1,e1)) (MyFloat (  (addPlaces m2 ((decPlaces m1)-(decPlaces m2)) ) ,e2))
                                           else shiftMantissa (MyFloat (  (addPlaces m1 ((decPlaces m2)-(decPlaces m1)) ) ,e1)) (MyFloat (m2,e2))
    | otherwise = shiftMantissa (MyFloat (m1,e1)) (MyFloat (m2,e2))

-- Based on value of exponents, shifts the mantissa of the larger number appropriately
shiftMantissa :: MyFloat -> MyFloat -> MyFloat
shiftMantissa (MyFloat (m1,e1)) (MyFloat (m2,e2)) 
    | (e1 == e2) = myFloatAdd (MyFloat (m1,e1)) (MyFloat (m2,e2)) 
    | (e1 > e2)  = myFloatAdd (MyFloat (m2,e1)) (MyFloat ( (addPlaces m1 (e1-e2)), e1))
    | (e2 > e1)  = myFloatAdd (MyFloat (m1,e2)) (MyFloat ( (addPlaces m2 (e2-e1)), e2))

-- Performs addition of the mantissas, only appropriate if the inputs have been pre-processed
myFloatAdd :: MyFloat -> MyFloat -> MyFloat
myFloatAdd (MyFloat (m1,e1)) (MyFloat (m2,_)) = MyFloat (m1+m2,e1-calcExp)
    where calcExp = (max (decPlaces m1) (decPlaces m2)) - (decPlaces (m1+m2))

-- Helper functions for + -- 

-- Calculates the number of decimal places of the integer "m"
decPlaces :: Integer -> Integer
decPlaces m | m == 0     = 0
            | otherwise  = 1 + decPlaces ((abs m) `div` 10)

-- Adds n trailing zeroes to m
addPlaces :: Integer -> Integer -> Integer
addPlaces m n = m * (10^n)
                                                         
-- Negates the second operand then sends it to +                                      
myFloatSub :: MyFloat -> MyFloat -> MyFloat
myFloatSub f1 f2 = f1 + (negate f2)

-- * definition
myFloatMul :: MyFloat -> MyFloat -> MyFloat
myFloatMul (MyFloat (m1,e1)) (MyFloat (m2,e2)) = (MyFloat ((m1 * m2),(e1 + e2 - calcExp)) )
    where calcExp = (decPlaces m1) + (decPlaces m2) - decPlaces (m1 * m2)

-- Negation
myFloatNegate :: MyFloat -> MyFloat
myFloatNegate (MyFloat (m,e)) = (MyFloat ((m*(-1)),e) )

-- Not required
myFloatAbs :: MyFloat -> MyFloat
myFloatAbs (MyFloat (m,e)) | m > 0     = (MyFloat (m,e))
                           | otherwise = negate (MyFloat (m,e))

-- Note this section was provided by Allan Kerr
instance Fractional MyFloat where
    (/) = myFloatDiv   

myFloatDiv :: MyFloat -> MyFloat -> MyFloat
myFloatDiv _ (MyFloat (0,_)) = error "You cannot divide by 0"
myFloatDiv (MyFloat (m1,e1)) (MyFloat (m2,e2)) = MyFloat (toMantissa (fromIntegral m1 / fromIntegral m2), (e1 - e2 - calcExp))
    where toMantissa n | n == fromIntegral (round n) = round n
                       | otherwise                   = toMantissa (10*n)
          calcExp = (decPlaces m1) - (decPlaces m2) - decPlaces (round (fromIntegral m1 / fromIntegral m2))						   
			 
-- Note this section was provided by Allan Kerr along with his helper functions.
-- Much of the work done in the helper functions below is previously defined above in
-- a slightly different way. This is done to show two different ways of doing the same
-- task.

-- Note that > >= <= can be derived from < and ==
instance Ord MyFloat where
    x > y  = not (x <= y)
    x >= y = not (x < y)
    x <= y = (x < y) || (x == y)
    (<) = myFloatLT
    
myFloatLT :: MyFloat -> MyFloat -> Bool
myFloatLT (MyFloat (m1,e1)) (MyFloat (m2,e2)) = lessThanEqualExponent (normalize (MyFloat (m1,e1), MyFloat (m2,e2)))
    where lessThanEqualExponent (MyFloat (m1,_), MyFloat (m2,_)) = m1 < m2
    
-- Helper functions for Ord functions
-- normalizeExponent :: (MyFloat, MyFloat) -> (MyFloat, MyFloat)
-- normalizeExponent takes a MyFloat pair with normalized mantissa.
-- These are then normalized so that they both have the same exponent.
normalize :: (MyFloat, MyFloat) -> (MyFloat, MyFloat)
normalize = normalizeExponent . normalizeMantissa
  where normalizeExponent (MyFloat (m1,e1), MyFloat (m2,e2))
          | e1 > e2 = normalizeExponent (MyFloat (10*m1,e1), MyFloat (m2,e2+1))
          | e1 < e2 = normalizeExponent (MyFloat (m1,e1+1), MyFloat (10*m2,e2))
          | otherwise = (MyFloat (m1,e1), MyFloat (m2,e2))

-- normalizeMantissa takes a MyFloat pair and pads the one with fewer digits with trailing zeros.
-- This padding is done until they have the same number of digits
normalizeMantissa :: (MyFloat, MyFloat) -> (MyFloat, MyFloat)
normalizeMantissa (MyFloat (0,e1), MyFloat (m2,e2)) = (MyFloat (0,e2), MyFloat (m2,e2))
normalizeMantissa (MyFloat (m1,e1), MyFloat (0,e2)) = (MyFloat (m1,e1), MyFloat (0,e1))
normalizeMantissa (MyFloat (m1,e1), MyFloat (m2,e2))
  | countDigits m1 < countDigits m2  = normalizeMantissa (MyFloat (10*m1,e1), MyFloat (m2,e2))
  | countDigits m1 > countDigits m2  = normalizeMantissa (MyFloat (m1,e1), MyFloat (10*m2,e2))
  | otherwise                        = (MyFloat (m1,e1), MyFloat (m2,e2))

-- Helper function for computing the number of digits in an integer
countDigits :: Integer -> Integer
countDigits num | num < 0 = countDigits (-num)
                | num == 0 = 0
                | otherwise = 1 + countDigits (div num 10)
				
--list buildin operations
--take n -> take first n element of list
--drop n -> take from element n+1 to end of list
--head   -> first element of list
--tail   -> all element except first of list
--init   -> all element except last of list
--last   -> last element of list
--reverse-> reverse list
--length -> number of element in list
length2 [] = 0
length2 (x:xs) = 1 + length2 xs

reverse2 [] = []
reverse2 (x:xs) = reverse2 xs ++ [x]

take2 0 _  = []
take2 _ [] = []
take2 n (x:xs) = x : (take2 (n-1) xs)

drop2 0 _  = []
drop2 _ [] = []
drop2 n (x:xs) = legal 0 (x:xs)
  where legal b (x:xs) | (b == (n-1))  = xs
                       | otherwise = legal (b+1) xs
					   
init2 (x:[]) = []
init2 (x:xs) = x : init2 xs

last2 (x:[]) = x
last2 (x:xs) = last2 xs

head2 (x:xs) = x

tail2 (x:xs) = xs

map2 f [] = []
map2 f (x:xs) = (f x) : map2 f xs


filter2 f [] = []
filter2 f (x:xs) | (f x)     = x : filter2 f xs
                 | otherwise = filter2 f xs

folder2 f (x:[]) = f x
folder2 f (x:xs) = (f x) + folder2 f xs				 
				 
addTo f g x = (f.g) x
addTo2 f g x = f (g x)

result3 = (addTo (+1) (*5) 2) == (addTo2 (+1) (*5) 2)

dup f x = f x x

