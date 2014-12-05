{-
Name: Brent McIvor
-}

module HW01 where


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = 
	let
		step1Moves = hanoi (n-1) a c b
		step2Move = (a, b)
		step3Moves = hanoi (n-1) c b a
	in
	    step1Moves ++ [step2Move] ++ step3Moves