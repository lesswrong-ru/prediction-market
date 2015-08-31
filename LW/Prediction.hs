module LW.Prediction
    (
        Person,
        Confidence,
        Bet(..),
        Topic(..),
        normalize,
        Scoreboard,
        showScore,
        getListScore
    ) where

import qualified Data.Map as Map

-- common aliases
type Person = String
type Confidence = Double

-- bets
data Bet = Bet Person Confidence
instance Show Bet where
    show (Bet p c) = "Bet " ++ p ++ " " ++ show c
inverse (Bet p c) = Bet p (100 - c)

-- topics - sets of bets
data Topic = Topic {
    title :: String,
    outcome :: Bool,
    bets :: [Bet]
}
instance Show Topic where
    show topic = showOutcome (outcome topic) ++ " " ++ title topic ++ " | " ++ (show $ bets topic)
        where showOutcome o = if o then "[y]" else "[n]"

normalize topic =
    if outcome topic
    then topic
    else topic {
        title = "NOT " ++ title topic,
        outcome = not $ outcome topic,
        bets = map inverse $ bets topic
    }

-- scoreboard
type Scoreboard = Map.Map Person Double

emptyScore :: Scoreboard
emptyScore = Map.fromList []

addScore :: Scoreboard -> Scoreboard -> Scoreboard
addScore = Map.unionWith (+)

sumScore :: [Scoreboard] -> Scoreboard
sumScore = foldr addScore emptyScore

showScore :: Scoreboard -> String
showScore = concat . map showPair . Map.toList
    where showPair (person, score) = person ++ ": " ++ show score ++ "\n"

-- scoring for topics and topic lists
pairScore :: (Bet, Bet) -> Scoreboard
pairScore (Bet p1 c1, Bet p2 c2) = Map.fromList [(p2, value)]
    where value = 100 * logBase 2 (c2/c1)

getScore :: Topic -> Scoreboard
getScore topic = sumScore $ map pairScore betPairs
    where bs = bets $ normalize topic
          betPairs = zip bs $ tail bs

getListScore :: [Topic] -> Scoreboard
getListScore = sumScore . map getScore
