import Data.Time.Clock
import Data.Time.Format
import System.Locale -- На новых версиях такого модуля нет

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString entry = 
    let 
        msg      = message entry
        timeMsg  = timeToString (timestamp entry)
        levelMsg = logLevelToString (logLevel entry)
    in
        timeMsg ++ ": " ++ levelMsg ++ ": " ++ msg 


data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 { lastName = lastName p1 }

abbrFirstName :: Person -> Person
abbrFirstName p@(Person name _ _ ) | length name >= 2 = p { firstName = [head name, '.']  } 
                                   | otherwise        = p