
{-

this Microproject was produced by Peter Feldgrill and Thomas Kristan
Additional Libraries that need to be installed: math-functions-0.3.1.0, aeson, http-conduit

-}

--------------------------------------------------------------------------------------------------------IMPORTS--------------------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-} -- OverloadedStrings has to imported in combination with aeson.

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Aeson.Types (Parser, Value, parseEither)
import qualified Control.Monad as M
import System.Exit (exitSuccess) -- exitSuccess is used to nicely terminate the application.
import System.Environment (getArgs) -- getArgs is used to be able to start the application with additional quarters.
import qualified Data.Set as Set -- Set is used to get rid of duplicates on multiple occasions.
import Data.List (sortOn, isInfixOf)
import Data.Char (toLower, isDigit)
import Text.Printf
import System.IO -- System.IO is used for our removeSpecialChars function
import qualified Numeric.Sum as N -- we use Numeric.Sum.sum in order to avoid calculation errors while summing up floating point numbers. (There seems to be a small problem with sum rounding up/down.)


------------------------------------------------------------------------------------FEATURE SET 0 & 4: MAIN, MAINMENU, HELP, INPUT AND LOADING/RELOADING DATA----------------------------------------------------------------------------------------


main :: IO()
main = do
  removeSpecialChars stdout
  dataForQuarters <- getArgs
  loadQuarters dataForQuarters


-- The removeSpecialChars function prevents the program from crashing when displaying a croatian character. (There is a recipient that has one in 20172 and 20173).
-- If confronted with an unsupported character, the program now simply prints a '?' instead of crashing.
-- When googling the error message "<stdout>: commitBuffer: invalid argument (invalid character)" we found out that this is a known bug in the GHC.
-- This function was implemented as a workaround for this bug. Source: https://stackoverflow.com/questions/27616611/run-time-exception-when-attempting-to-print-a-unicode-character

removeSpecialChars :: Handle -> IO ()
removeSpecialChars h = do
    enc <- hGetEncoding h
    case enc of
      Just name -> mkTextEncoding (takeWhile (/= '/') (show name) ++ "//TRANSLIT") >>= hSetEncoding h
      _ -> return ()


loadQuarters :: [String] -> IO a
loadQuarters dataForQuarters = do
  let quarters = if null dataForQuarters then ["20184"] else dataForQuarters
  putStr "loading data for "
  print quarters
  parsedData <- mapM buildQuarters quarters
  inputFunc (return (concat parsedData)) quarters -- here we combine the parsedData into one dataSet so that we dont have to call multiple dataSets. Since they each have their data entry "quartal" they are still identifiable
  -- we also pass on the loaded quarters here since we need it later for the quarters function (so that we can dynamically create only the entries for our loaded quarters)


inputFunc :: IO [Entry] -> [String] -> IO b
inputFunc parsedData quarters = do
  putStrLn "Enter your command or type 'help' for assistance"
  input <- getLine
  let command n = head $ drop (n-1) $ words input
  let commandDrop n = unwords $ drop (n-1) $ words input
  let unnullable n = not (null (commandDrop n)) -- unnullable is used to check for errors in the user input.
  mainMenu parsedData command commandDrop input unnullable quarters


mainMenu :: (Num t2, Num t1, Num t) => IO [Entry] -> (t -> String) -> (t2 -> String) -> String -> (t1 -> Bool) -> [String] -> IO b
mainMenu parsedData command commandDrop input unnullable quarters
  | null input = inputFunc parsedData quarters
  | input == "help"  = help >> inputFunc parsedData quarters
  | input == "exit"  = putStrLn "Bye!" >> exitSuccess
  | input == "payers" = listParticipants parsedData "payers" >> inputFunc parsedData quarters
  | input == "recipients" = listParticipants parsedData "recipients" >> inputFunc parsedData quarters
  | input == "quarters" = paymentsPerQuarter parsedData quarters >> inputFunc parsedData quarters
  | command 1 == "top" && unnullable 2 && unnullable 3 && unnullable 4 && not (null (tail (command 4)))
    && isDigit (head (commandDrop 2)) && (command 3 == "payers" || command 3 == "recipients") && isDigit (head(tail (command 4))) && head (command 4) == '§'
    = topFunc (read (command 2)::Int) (command 3) (read (tail (command 4))::Int) parsedData >> inputFunc parsedData quarters
  | command 1 == "search" && unnullable 2 && unnullable 3 && (command 2 == "payers" || command 2 == "recipients") = searchFunc (command 2) parsedData (tail (words (commandDrop 2))) >> inputFunc parsedData quarters
  | command 1 == "load" && unnullable 2 = loadQuarters (words (commandDrop 2)) >> inputFunc parsedData quarters
  | command 1 == "details" && unnullable 2 && unnullable 3 && (command 2 == "payers" || command 2 == "recipients") = detailsFunc (command 2) parsedData (commandDrop 3) >> inputFunc parsedData quarters
  | otherwise = putStrLn ("Sorry, the command '" ++ input ++"' is unknown!") >> inputFunc parsedData quarters


help :: IO ()
help = putStrLn "\t 'help' ... print this message" >>
  putStrLn "\t 'exit' ... terminate this application" >>
  putStrLn "\t 'payers' ... print a list of all payers" >>
  putStrLn "\t 'recipients' ... print a list of all recipients" >>
  putStrLn "\t 'quarters' ... print an overview of the currently loaded quarters for every payment category" >> -- quarters was added here even though it was not in the instructions since it is still a command that is available.
  putStrLn "\t 'top' n 'payers'|'recipients' '§2'|'§4'|'§31' ... print the n biggest payers|recipients for the given payment type" >>
  putStrLn "\t 'search' 'payers'|'recipients' searchTerm ... print a list of all payers|recipients containing the given searchTerm" >>
  putStrLn "\t 'load' quarter1 quarter2 .. quartern ... load date for the given list of quarters" >>
  putStrLn "\t 'details' 'payers'|'recipients' organization ... print a list of all payments payed or received by the given payers/recipients"


------------------------------------------------------------------------------------CUSTOM DATA TYPE AND PARSING WITH AESON--------------------------------------------------------------------------------------------------------------------

-- The approach to writing functions to pass JSON format to our custom data type was discussed by Lorenz Possnig in his tutorial.
-- Furthermore he showed us some relevant entries on https://artyom.me/aeson and https://stackoverflow.com/questions/42031937/parse-json-with-aeson and how to handle with aeson.


data Entry = Entry {rechtstraeger :: String,
                    quartal :: String,
                    bekanntgabe :: Int,
                    medieninhaber :: String,
                    euro :: Double} deriving (Show, Read, Eq, Ord)


parseEntry :: Value -> Parser Entry
parseEntry = withObject "entry" $ \e -> do
  re <- e .: "rechtstraeger"
  qu <- e .: "quartal"
  be <- e .: "bekanntgabe"
  me <- e .: "mediumMedieninhaber"
  eu <- e .: "euro"
  return $ Entry re qu be me eu


dataParse :: Value -> Parser [Entry]
dataParse = withObject "entry" $ \e -> do
  dataSet <- e .: "data" -- we tell the parser that our data starts after the keywoard "data" on the website.
  M.mapM parseEntry dataSet


createURL :: String -> String
createURL quarter = firstPart ++ quarter ++ lastPart where
    firstPart = "https://data.rtr.at/api/v1/tables/MedKFTGBekanntgabe.json?quartal="
    lastPart = "&leermeldung=0&size=0"


buildQuarters :: String -> IO [Entry]
buildQuarters quarter = do
  src <- simpleHttp $ createURL quarter
  case eitherDecode src of
    Left _ ->  putStrLn ("there was an error parsing data for " ++ quarter) >> return []
    Right result -> case parseEither dataParse result of
      Left err -> putStrLn ("there was an error parsing data for " ++ quarter) >> return []
      Right r -> putStrLn (if length quarter == 5 && head quarter == '2' then "loaded data for " ++ quarter
        else "the quarter '" ++ quarter ++ "' does not exist. Please enter a correct quarter!") >> return r


------------------------------------------------------------------------------------CREATING FUNCTIONS IN ORDER TO GENERATE OUTPUT FROM OUR DATATYPE----------------------------------------------------------------------------------------------


printPayer :: Entry -> String
printPayer (Entry rechtstraeger _ _ _ _) = rechtstraeger

printRecipient :: Entry -> String
printRecipient (Entry _ _ _ recipients _) = recipients

printMoney :: Entry -> Double
printMoney (Entry _ _ _ _ euro) = euro


------------------------------------------------------------------------------------FEATURE SET 2: PAYERS/RECIPIENTS-----------------------------------------------------------------------------------------------------------------------------


--listParticipants :: IO [Entry] -> t -> IO ()
listParticipants parsedData participant = do
  dataSet <- parsedData
  let output = unlines $ sortOn (map toLower) $ Set.elems $ Set.fromList $ map (if participant == "payers" then printPayer else printRecipient) dataSet

  putStrLn $ if null output then "Seems like you don't currently have any valid quarters loaded. Try loading some! \n" else output


------------------------------------------------------------------------------------FEATURE SET 3: QUARTERS OVERVIEW------------------------------------------------------------------------------------------------------------------------------


paymentsPerQuarter :: IO [Entry] -> [String] -> IO ()
paymentsPerQuarter parsedData quarters = do
  dataSet <- parsedData
  let applyCalculation = sortOn (\[(x,_,_,_)]->read x::Int) $ filter (\[(x,_,_,_)]-> length x ==5 && head x == '2') $ map (bekanntgabeSums [dataSet]) quarters
  let format [(quarter,zwei, vier, einunddreissig)] = printf "%s, %20.2f (§2), %20.2f (§4), %20.2f (§31)" quarter zwei vier einunddreissig
  let output = unlines (map format applyCalculation)

  putStrLn $ if null output then "Seems like you don't currently have any valid quarters loaded. Try loading some! \n" else output

bekanntgabeSums :: Monad m => m [Entry] -> String -> m (String, Double, Double, Double)
bekanntgabeSums parsedData quarter = do
  dataSet <- parsedData
  let geldProParagraph paragraph = map printMoney (filter (\(Entry _ quartal bekanntgabe _ _) -> bekanntgabe == paragraph && quartal == quarter) dataSet)
  return (quarter,N.sum N.kbn (geldProParagraph 2), N.sum N.kbn (geldProParagraph 4), N.sum N.kbn (geldProParagraph 31))


------------------------------------------------------------------------------------FEATURE SET 5: TOP N PAYERS/RECIPIENTS §2/4/31--------------------------------------------------------------------------------------------------------------


--topFunc :: (Eq t, Data.String.IsString t) => Int -> t -> Int -> IO [Entry] -> IO ()
topFunc n participant paragraph parsedData = do
  dataSet <- parsedData
  let formatter (name,amount) = printf "%-92s: %17.2f" name amount
  let sumName name = (name,N.sum N.kbn $ map printMoney $ filter (\(Entry rechtstraeger _ bekanntgabe medieninhaber _) -> (if participant == "payers" then rechtstraeger else medieninhaber) == name && bekanntgabe == paragraph) dataSet)
  let filterAndFormat list = map formatter $ reverse $ sortOn snd $ Set.elems $ Set.fromList $ filter (\(_,x) -> x /= 0) list
  let output = unlines $ take n $ filterAndFormat $ map (sumName . (if participant == "payers" then printPayer else printRecipient)) dataSet

  putStrLn $ if null output then "Seems like there are currently no top "++ participant ++" for §"++ show paragraph ++"! \n" else output


------------------------------------------------------------------------------------FEATURE SET 6: SEARCH-----------------------------------------------------------------------------------------------------------------------------------------


--searchFunc :: (Data.String.IsString t, Eq t) => t -> IO [Entry] -> [String] -> IO ()
searchFunc participant parsedData name = do
  dataSet <- parsedData
  let partSelection = map (if participant == "payers" then printPayer else printRecipient) dataSet
  let filtered = filter (isInfixOf (map toLower (unwords name)) . map toLower) partSelection
  let output = unlines $ sortOn (map toLower) $ Set.elems $ Set.fromList filtered

  putStrLn $ if null output then "The searchTerm '" ++ unwords name ++ "' does not yield any results in the currently loaded quarters! \n" else output


------------------------------------------------------------------------------------FEATURE SET 7: DETAILS-----------------------------------------------------------------------------------------------------------------------------------------


--detailsFunc :: (Eq t, Data.String.IsString t) => t -> IO [Entry] -> String -> IO ()
detailsFunc participant parsedData name = do
  dataSet <- parsedData
  let formatter (name,amount) = printf "%-92s %17.2f" name amount
  let filteredParagraph paragraph = filter (\(Entry rechtstraeger _ bekanntgabe medieninhaber _) -> (if participant == "payers" then rechtstraeger else medieninhaber) == name && bekanntgabe == paragraph) dataSet
  let sumName source name = (name,N.sum N.kbn $ map printMoney $ filter (\(Entry rechtstraeger _ _ medieninhaber _) -> (if participant == "payers" then medieninhaber else rechtstraeger) == name) source)
  let filterAndFormat list = unlines $ map formatter $ reverse $ sortOn snd $ Set.elems $ Set.fromList $ filter (\(_,x) -> x /= 0) list
  let details paragraph = filterAndFormat $ map (sumName (filteredParagraph paragraph) . (if participant == "payers" then printRecipient else printPayer)) dataSet
  let output = "\nPayments according to §2:\n" ++ details 2 ++ "\nPayments according to §4:\n" ++ details 4 ++ "\nPayments according to §31:\n" ++ details 31

  putStrLn $ if null (details 2) && null (details 4) && null (details 31) then "The organization '" ++ name ++ "' does not yield any results in the currently loaded quarters! \n" else output
