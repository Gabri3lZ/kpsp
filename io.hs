module InOut02 where

import Data.Char

t01 :: IO String
t01 = getLine

t02 :: IO ()
t02 = putStrLn "dasdf"

t03 = putStrLn (show [1,2,3])

t04 = print [i^2 | i <- [1 .. 10]]

test01 :: IO()
test01 = 
	do 
		putStrLn "Your first name: "
		-- firstName = getLine -- geht nicht
		firstName <- getLine
		putStrLn "Your first name: "		
		familyName <- getLine
		-- let upperFirstName <- map toUpper firstName  -- geht nicht
		let upperFirstName  = map toUpper firstName
		    upperFamilyName = map toUpper familyName
		putStrLn ("Hallo " ++ upperFirstName ++ " " ++ upperFamilyName)
		
test02 :: IO (String, String)
test02 = 
	do 
		putStrLn "Your first name: "
		-- firstName = getLine -- geht nicht
		firstName <- getLine
		putStrLn "Your first name: "		
		familyName <- getLine
		return (firstName, familyName) -- monadische funktion
		
test03 :: IO()		
test03 = 
	do
		(firstName, familyName) <- test02
		let upperFirstName  = map toUpper firstName
		    upperFamilyName = map toUpper familyName
		putStrLn ("Hallo " ++ upperFirstName ++ " " ++ upperFamilyName)
		
test04 :: IO()
test04 = 
	do
		fileName <- getLine
		stringFileContents <- readFile (fileName ++ ".inp")
		let xys = read stringFileContents :: [(Int,Int)]
		let result = [x + y | (x,y) <- xys]
		writeFile (fileName ++ ".outp") (show result)

test05 :: IO [String]		
test05 = 
	do
		s <- getLine
		if s == "." then
			return []
		else
			do 
				ss <- test05
				return (s:ss)
				
test06 =
	do 
		ss <- test05
		print (concat ss)