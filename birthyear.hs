import Data.Time.Clock -- visit https://hackage.haskell.org/ to install the libraries
import Data.Time.Calendar
import Data.Time.LocalTime

ageconversion :: Integer -> Integer -> Integer -> Float -- this is an example function
ageconversion 1 year birthyear = fromInteger(year-birthyear) -- this calculates the age
ageconversion 2 year birthyear = 365.25*(fromInteger (year-birthyear))
ageconversion 3 year birthyear = 24*365.25*(fromInteger (year-birthyear))
ageconversion 4 year birthyear = 60*24*365.25*(fromInteger (year-birthyear))
ageconversion 5 year birthyear = 60*60*24*365.25*(fromInteger (year-birthyear)) -- this calculates the age in seconds
ageconversion _ _    _         = fromInteger(1337) -- errorcode

timeunitfunction :: Integer -> String
timeunitfunction 1 = "year"
timeunitfunction 2 = "day"
timeunitfunction 3 = "hour"
timeunitfunction 4 = "minute"
timeunitfunction 5 = "second"
timeunitfunction _ = "error 88"

main = do
    
    putStrLn "Please enter your birthyear:"
    birthyearstring <- getLine
    let birthyear = read birthyearstring :: Integer
    putStrLn "For the right timeunit enter the respective digit:"
    putStrLn "1: Years, 2: Days 3: Hours, 4: Minutes, 5: Seconds"
    timeunitchoice <- getLine
    let timeunitchoiceint = read timeunitchoice :: Integer

    let timeunit = timeunitfunction timeunitchoiceint
    
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (year, month, day) = toGregorian $ localDay zoneNow
    let agefloat = ageconversion timeunitchoiceint year birthyear
    let age = show agefloat
    putStrLn ("Yikes, your survived " ++ age ++ " " ++ timeunit ++ "s!")
    putStrLn ("Program is terminating, have a nice day!")
-- todo: nice formating of huge numbers
