import Text.Parsec
import System.Environment

-- File objects:
-- records, an array of records to parse
data File = File [Record]

-- Record objects:
-- leader, which **MUST** be a string, exactly 24 characters in length.
-- fields, an array which **MUST** only contain control field and variable field objects.
data Record = Record Leader [Field] 

--LEADER 01471cjm a2200349 a 4500
data Leader = Leader Header NumTabs
type Header = String
type NumTabs = Int

-- MARC control fields MUST be represented as a JSON object with a single key/value pair. 
-- Variable fields MUST be represented as JSON objects with a single key/value pair. 
-- ind1: a one (1) character string representing the 1st MARC field indicator
-- ind2: a one (1) character string representing the 2nd MARC field indicator
-- subfields: an array containing at least one subfield object
data Field = ControlField Code Value NumTabs | VariableField Code Ind1 Ind2 [SubField] NumTabs
type Code = String
type Value = String
type Ind1 = Char
type Ind2 = Char

-- MARC subfields MUST be represented as JSON objects with a single key/value pair. 
data SubField = SubField CodeSF Value NumTabs
type CodeSF = Char

instance Show File where
  show (File records) = "[\n" ++ show records ++ "\n]"

instance Show Record where
    show (Record leader fields) = "{\n" ++ show leader ++ show fields ++ "\n}"

instance Show Leader where
    show (Leader header tabs) = "\"leader\":" ++ header ++ "\",\n\"fields\":\n"

instance Show Field where
    show (ControlField code value tabs) = "\n" ++ insertTabs tabs ++ "{\n" ++ insertTabs (tabs+1) ++ "\"" ++ code ++ "\":\"" ++ value ++ "\"\n" ++ insertTabs tabs ++ "}"
    show (VariableField code ind1 ind2 (x:xs) tabs) = "\n" ++ insertTabs tabs ++ "{\n" ++ insertTabs (tabs+1) ++ "\"" ++ code ++ "\":\n" ++ insertTabs (tabs+1) ++ "{\n" ++ insertTabs (tabs+2) ++ "\"subfields\":\n" ++ insertTabs (tabs+2) ++ show (x:xs) ++ ",\n" ++ insertTabs (tabs+2) ++ "\"ind1\":\"" ++ [ind1] ++"\",\n" ++ insertTabs (tabs+2) ++ "\"ind2\":\"" ++ [ind2] ++ "\"\n" ++ insertTabs (tabs+1) ++ "}\n" ++ insertTabs tabs ++ "}"  

instance Show SubField where
    show (SubField code value tabs) = "\n" ++ insertTabs tabs ++ "{\n" ++ insertTabs (tabs+1) ++ "\"" ++ [code] ++ "\":\"" ++ value ++ "\"\n" ++ insertTabs tabs ++ "}"

-- heleper function for writing tabs to JSON file
insertTabs :: Int -> String
insertTabs 0 = ""
insertTabs c = "\t" ++ insertTabs (c - 1)

parseFile :: Parsec String Int File
parseFile = do
              records <- many1 parseRec
              eof
              return (File records)

parseRec :: Parsec String Int Record
parseRec = do 
            record <- try format21 <|> try format02 <|> try format03 <|> try format04 <|> try format05
            spaces
            return record
  
-- 1. FORMAT -> in01.txt --      

-- LDR 01142cam**2200301*a*4500      
format21 :: Parsec String Int Record
format21 = do 
            try (string "LDR")
            space
            header <- count 24 anyChar <?> "format21"
            c <- getState
            let leader = Leader header c
            modifyState (+1)
            fields <- many1 (try createFields01)
            modifyState (subtract 1)
            return (Record leader fields)

createFields01 :: Parsec String Int Field
createFields01 = do 
            spaces
            code <- count 3 digit
            space 
            indicator <- many (noneOf "\n ")
            if length indicator < 3 
            then 
                -- variable field with 2 indicators: 020 ## $a0152038655 :$c$15.95
                do 
                    space 
                    modifyState (+3)
                    subfields <- many1 (try createSubfield01)
                    modifyState (subtract 3)
                    spaces
                    c <- getState
                    return (VariableField code (indicator !! 0) (indicator !! 1) subfields c)
            else 
                -- control field: 001 92005291
                do 
                    value <- many (noneOf "\n")
                    spaces
                    c <- getState
                    return (ControlField code (indicator ++ value) c)

createSubfield01 :: Parsec String Int SubField
createSubfield01 = do 
                   char '$' <?> "subfield for 1. or 3. format"
                   modifyState (+1)
                   code <- anyChar
                   value <- many (noneOf "$\n")
                   modifyState (subtract 1)
                   c <- getState
                   return (SubField code value c)

-- 2. FORMAT -> in02.txt --

-- 00000888cam a2200253u 4500
format02 :: Parsec String Int Record
format02 = do 
            try (string "000")
            header <- count 24 anyChar <?> "format02"
            c <- getState
            let leader = Leader header c
            modifyState (+1)
            fields <- many1 (try createFields02)
            modifyState (subtract 1)
            return (Record leader fields)

createFields02 :: Parsec String Int Field
createFields02 = do 
            spaces
            code <- count 3 digit

            maybeInd1 <- anyChar
            maybeInd2 <- anyChar

            maybeSpace <- lookAhead (anyChar)
            if maybeSpace /= ' '
              then
                -- control field: 0016840872
                do 
                  value <- many (noneOf "\n")
                  c <- getState
                  return (ControlField code ([maybeInd1] ++ [maybeInd2] ++ value) c)
              else 
                -- variable field: 05000 |a PR2810.A2 |b H4
                do
                  space 
                  modifyState (+3)
                  subfields <- many1 (try createSubfield02)
                  modifyState (subtract 3)
                  c <- getState
                  return (VariableField code maybeInd1 maybeInd2 subfields c)

createSubfield02 :: Parsec String Int SubField
createSubfield02 = do 
                   char '|' <?> "subfield for 2. format"
                   modifyState (+1)
                   code <- anyChar
                   space
                   value <- many (noneOf "|\n")
                   modifyState (subtract 1)
                   c <- getState
                   return (SubField code value c)

-- 3. FORMAT -> in03.txt -- 

-- LEADER 01471cjm a2200349 a 4500
format03 :: Parsec String Int Record
format03 = do 
            try (string "LEADER")
            spaces
            header <- count 24 anyChar <?> "format03"
            c <- getState
            let leader = Leader header c
            modifyState (+1)
            fields <- many1 (try createFields03)
            modifyState (subtract 1)
            return (Record leader fields)

createFields03 :: Parsec String Int Field
createFields03 = do 
            spaces
            code <- count 3 digit
            space 

            maybeInd1 <- lookAhead (anyChar)
            if maybeInd1 == '$'
              then 
                -- variable field without identifiers: 035 $9 (DLC) 93707283
                do
                  modifyState (+3)
                  subfields <- many1 (try createSubfield01)
                  modifyState (subtract 3)
                  c <- getState
                  return (VariableField code '#' '#' subfields c)
              else
                do
                  maybeInd1 <- anyChar
                  maybeInd2 <- anyChar

                  if maybeInd2 == ' '
                    then
                      -- variable field with 1 identifier: 041 0 $d eng $g eng
                      do 
                        modifyState (+3)
                        subfields <- many1 (try createSubfield01)
                        modifyState (subtract 3)
                        c <- getState
                        return (VariableField code maybeInd1 '#' subfields c)
                    else
                      do
                        maybeSpace <- anyChar
                        if maybeSpace /= ' '
                          then 
                            -- control field: 005 20030305110405.0
                            do 
                              value <- many (noneOf "\n")
                              c <- getState
                              return (ControlField code ([maybeInd1] ++ ([maybeInd2] ++ ([maybeSpace] ++ value))) c)
                          else 
                            -- variable field with 2 identifiers: 050 00 $a Columbia CS 8786
                            do
                              modifyState (+3)
                              subfields <- many1 (try createSubfield01)
                              modifyState (subtract 3)
                              c <- getState
                              return (VariableField code maybeInd1 maybeInd2 subfields c)

-- 4. FORMAT -> in04.txt --

-- no leader, no control fields
format04 :: Parsec String Int Record
format04 = do 
            c <- getState
            let leader = Leader "" c
            modifyState (+1)
            fields <- many1 (try createFields04)
            modifyState (subtract 1)
            return (Record leader fields)

-- variable field: 215 ## [a]2+82 str.[d]25 cm
createFields04 :: Parsec String Int Field
createFields04 = do 
            code <- count 3 digit
            space 

            ind1 <- anyChar
            ind2 <- anyChar
            space

            modifyState (+3)
            subfields <- many1 (try createSubfield04)
            modifyState (subtract 3)
            spaces <|> eof
            c <- getState
            return (VariableField code ind1 ind2 subfields c)

createSubfield04 :: Parsec String Int SubField
createSubfield04 = do 
                   char '[' <?> "subfield for 4. format"
                   modifyState (+1)
                   code <- anyChar
                   char ']'
                   value <- many (noneOf "[\n")
                   modifyState (subtract 1)
                   c <- getState
                   return (SubField code value c)

-- 5. FORMAT -> in05.txt -- 

-- no leader, no control fields
format05 :: Parsec String Int Record
format05 = do 
            c <- getState
            let leader = Leader "" c
            modifyState (+1)
            fields <- many1 (try createFields05)
            modifyState (subtract 1)
            return (Record leader fields)

-- variable fileds: seperated by invisible char with ordinal number 30 in ASCII table 
createFields05 :: Parsec String Int Field
createFields05 = do 
            code <- count 3 digit

            ind1 <- anyChar
            ind2 <- anyChar

            modifyState (+3)
            subfields <- many1 (try createSubfield05)
            many (char (toEnum 30))
            modifyState (subtract 3)
            c <- getState
            return (VariableField code ind1 ind2 subfields c)

-- subfields: seperated by invisible char with ordinal number 31 in ASCII table 
createSubfield05 :: Parsec String Int SubField
createSubfield05 = do 
                   char (toEnum 31) <?> "subfield for 5. format"
                   modifyState (+1)
                   code <- anyChar
                   value <- many (noneOf ([toEnum 31]++[toEnum 30]++"\n"))
                   modifyState (subtract 1)
                   c <- getState
                   return (SubField code value c)
               
main :: IO ()
main = do (input:output:[]) <- getArgs
          cont <- readFile input
          case (runParser parseFile 0 input cont) of
            Left err -> putStrLn . show $ err
            Right rss -> writeFile output . show $ rss
