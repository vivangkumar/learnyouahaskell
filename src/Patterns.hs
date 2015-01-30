lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

main = do
	let resultOne = lucky 7
	print resultOne -- prints "LUCKY NUMBER SEVEN!"

	let resultTwo = lucky 9
	print resultTwo -- prints "Sorry, you're out of luck, pal!"

	let factorialOne = factorial 0
	print factorialOne -- prints 1

	let factorialTwo = factorial 9
	print factorialTwo -- prints 362880

	let vectorOne = addVectors (4, 5) (5, 6)
	print vectorOne -- prints (9, 11)

	let firstResult = first (1, 5, 7)
	print firstResult -- prints 1