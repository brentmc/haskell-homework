{-
Name: Brent McIvor
-}

module HW01 where


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi numDiscs pegA pegB pegC = 
	let
		step1Moves = hanoi (numDiscs-1) pegA pegC pegB
		step2Move = (pegA, pegB)
		step3Moves = hanoi (numDiscs-1) pegC pegB pegA
	in
	    step1Moves ++ [step2Move] ++ step3Moves    