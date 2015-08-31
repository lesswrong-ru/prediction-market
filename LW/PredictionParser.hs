module LW.PredictionParser
    (parsePredictions
    ) where

import qualified LW.Prediction as P

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Monad (void)

document :: Parser [P.Topic]
document = topics <* eof

topics :: Parser [P.Topic]
topics = topic `sepBy` (string "\n")

outcome :: Parser Bool
outcome = do
    char '['
    answer <- char 'y' <|> char 'n'
    char ']'
    if answer == 'y'
    then return True
    else return False

whitespace = void $ many $ oneOf " \n\t"

confidence :: Parser Double
confidence = do
    c <- many1 $ digit <|> char '.'
    return $ read c

bet = do
    n0 <- many1 $ noneOf "\n1234567890"
    let n = unwords (words n0)
    whitespace
    c <- confidence
    char '\n'
    return $ P.Bet n c

topic :: Parser P.Topic
topic = do
    o <- outcome
    whitespace
    title <- manyTill anyChar $ char '\n'
    bets <- many bet
    return $ P.Topic {
        P.outcome = o,
        P.title = title,
        P.bets = bets
    }

parsePredictions :: String -> Either ParseError [P.Topic]
parsePredictions = parse document "(unknown)"
