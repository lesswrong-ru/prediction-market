import LW.Prediction (Topic(..), showScore, getListScore)
import LW.PredictionParser (parsePredictions)


processTopics :: [Topic] -> IO ()
processTopics topics = do
    mapM_ print $ topics
    putStr $ showScore $ getListScore topics

main = do
    content <- getContents
    let parsed = parsePredictions content
    case parsed of
        Left parseError -> print parseError
        Right topics -> processTopics topics
