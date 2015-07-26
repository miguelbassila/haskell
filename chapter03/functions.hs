-- Pattern Matching
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Pattern Matching with Tuples
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- Pattern Matching with Lists and List Comprehensions
head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

-- As-patterns
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards
bmiTell :: Double -> Double -> String
bmiTell weight height
	| bmi <= 18.5 = "You're underweight, eat more!"
	| bmi <= 25.0 = "Looking good!"
	| bmi <= 30.0 = "You're overweight. Let's work out together!"
	| otherwise = "You're obese. Go see a doctor."
	where bmi = weight / height ^ 2

-- Where's
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where
		(f:_) = firstname
		(l:_) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
	where bmi weight height = weight / height ^ 2


-- Let
cylinder :: Double -> Double -> Double
cylinder r h = 
	let 
		sideArea = 2 * pi * r * h
		topArea = pi * r * 2
	in sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w,h) <- xs, let bmi = w/h^2, bmi > 25.0]

-- Case
describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of 
		[] -> "empty."
		[x] -> "a singleton list."
		xs -> "a longer list."