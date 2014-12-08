{-
Name: Brent McIvor
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

{-
	Loops through and determines if the first letter in the word is also in the hand.
	If so, it strips the first letter off the word and deletes it from the hand
	before recursively calling the function with the remaining letters in the word and hand
-}
formableBy :: String -> Hand -> Bool
formableBy [] _ = True							--if there are no more letters left in the substring
formableBy _ [] = False            				--if there are still letters in the word but none left in the hand return false
formableBy (x:xs) hand
   | x `notElem` hand = False					--if the next letter in the requested word is not in the hand, return false
   | otherwise = formableBy xs (delete x hand)  --otherwise splice the first letter from the start of the substring and remove the first instance of the letter from the hand

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords