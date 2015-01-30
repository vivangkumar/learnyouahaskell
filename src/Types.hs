removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree a b c = a + b + c

main = do
	let upCaseResult = removeNonUpperCase "aBXzS"
	print upCaseResult -- prints "BXS"

	let addThreeResult = addThree 1 3 4 
	print addThreeResult -- prints 8