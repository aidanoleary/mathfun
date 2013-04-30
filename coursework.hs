-- 
-- 
-- MATHFUN - DISCRETE MATHEMATICS AND FUNCTIONAL PROGRAMMING
-- Functional Programming Assignment, 2012/13
--
-- Student number - UP636800
--


import Data.Char
import Data.List

-- Types
type Title = String
type Cast = [String]
type Year = Int
type Fans = [String]

-- Define Film type here
type Film = (Title, Cast, Year, Fans)

-- Define database type here
type Database = [Film]

testDatabase :: [Film]
testDatabase = [("Casino Royale", ["Daniel Craig", "Eva Green", "Judi Dench"], 2006, ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
                ("Cowboys & Aliens", ["Harrison Ford", "Daniel Craig", "Olivia Wilde"], 2011, ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"]),
                ("Catch Me If You Can", ["Leonardo DiCaprio", "Tom Hanks"], 2002, ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"]),
                ("Mamma Mia!", ["Meryl Streep", "Pierce Brosnan"], 2008, ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"]),
                ("Saving Private Ryan", ["Tom Hanks", "Matt Damon"], 1998, ["Heidi", "Jo", "Megan", "Olga", "Zoe", "Wally"]),
                ("Life of Pi", ["Suraj Sharma"], 2012, ["Kevin", "Olga", "Liz", "Tim", "Zoe", "Paula", "Jo", "Emma"]),
                ("Titanic", ["Leonardo DiCaprio", "Kate Winslet"], 1997, ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"]),
                ("Quantum of Solace", ["Daniel Craig", "Judi Dench"], 2008, ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
                ("You've Got Mail", ["Meg Ryan", "Tom Hanks"], 1998, ["Dave", "Amy"]),
                ("Collateral", ["Tom Cruise", "Jamie Foxx"], 2004, ["Dave", "Garry", "Megan", "Sam", "Wally"]),
                ("The Departed", ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"], 2006, ["Zoe", "Emma", "Paula", "Olga", "Dave"]),
                ("Inception", ["Leonardo DiCaprio"], 2010, ["Chris", "Emma", "Jo", "Bill", "Dave", "Liz", "Wally", "Zoe", "Amy", "Sam", "Paula", "Kevin", "Olga"]),
                ("Up in the Air", ["George Clooney", "Vera Farmiga"], 2009, ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
                ("The Shawshank Redemption", ["Tim Robbins", "Morgan Freeman"], 1994, ["Jo", "Wally", "Liz", "Tim", "Sam", "Zoe", "Emma", "Garry", "Olga", "Kevin"]),
                ("Gladiator", ["Russell Crowe", "Joaquin Phoenix"], 2000, ["Garry", "Ian", "Neal"]),
                ("The King's Speech", ["Colin Firth", "Geoffrey Rush"], 2010, ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"]),
                ("The Descendants", ["George Clooney"], 2011, ["Wally", "Liz", "Kevin", "Tim", "Emma", "Chris", "Megan"]),
                ("Cloud Atlas", ["Tom Hanks", "Halle Berry"], 2012, ["Dave", "Amy", "Garry", "Ian", "Neal"]),
                ("The Reader", ["Kate Winslet", "Ralph Fiennes"], 2008, ["Emma", "Bill", "Dave", "Liz"]),
                ("Minority Report", ["Tom Cruise"], 2002, ["Dave", "Garry", "Megan", "Sam", "Wally"]),
                ("Revolutionary Road", ["Leonardo DiCaprio", "Kate Winslet"], 2008, ["Wally", "Sam", "Dave", "Jo"]),
                ("Forrest Gump", ["Tom Hanks"], 1994, ["Ian", "Garry", "Bill", "Olga", "Liz", "Sam", "Dave", "Jo", "Chris", "Wally", "Emma"]),
                ("Larry Crowne", ["Tom Hanks", "Julia Roberts"], 2011, ["Liz", "Wally"]),
                ("The Terminal", ["Tom Hanks", "Catherine Zeta Jones"], 2004, ["Olga", "Heidi", "Bill", "Sam", "Zoe"]),
                ("Django Unchained", ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"], 2012, ["Kevin", "Tim", "Emma", "Olga"])]


-- ****************
-- Functional Code
-- ****************

-- helper functions 
-- ******************

--A function that checks if two films are the same
sameFilm :: String -> Film -> Bool
sameFilm title (t, c, y, f)
        | title == t = True
        | otherwise = False

findFilm :: String -> Database -> Database
findFilm title database = filter (sameFilm title) database


-- A function that converts a list of strings to a string where each list item is divided by a comma.
displayList :: [String] -> String
displayList [] = ""
displayList [x] = x ++ "." ++  displayList []
displayList (x:xs) = x ++ ", " ++ displayList xs

-- A function that displays a single film as a well formatted string.
displayFilm :: Film -> String
displayFilm (title, cast, year, fans) = "\nTitle: " ++ title ++ "\nCast: " ++ (displayList cast) ++ "\nYear: " ++ (show year) ++ "\nFans: " ++ show (length fans) ++ "\n"


-- A function that checks if a String is in a list.
hasString :: String -> [String] -> Bool
hasString string [] = False
hasString string (x:xs)
         | string == x = True
         | otherwise = hasString string xs


-- A function that converts all characters of a string to lower case.
stringToLower :: String -> String
stringToLower "" = ""
stringToLower (x:xs) = (toLower x) : stringToLower xs 

-- A function that checks if a film was released within a given year
filmYear :: Year -> Film -> Bool
filmYear year (t, c, y, f)
         | year == y = True
		 | otherwise = False
		 
-- A function that checks if someone is a fan of a film
isFan :: String -> Film -> Bool
isFan fan (t, c, y, f)
         | elem fan f = True
		 | otherwise = False
		 
-- A function that checks if a film has a specific actor.
hasActor :: String -> Film -> Bool
hasActor actor (t, c, y, f)
         | elem actor c = True
		 | otherwise = False

-- A function that checks if a film was released between 2 given dates.
filmPeriod :: Year -> Year -> Film -> Bool
filmPeriod startYear endYear (t, c, y, f)
         | y >= startYear && y <= endYear = True
		 | otherwise = False

-- A function that checks two films and outputs greater than or less then depending on the number of fans.
maxFans (t1, c1, y1, f1) (t2, c2, y2, f2)
         | (length f1) <= (length f2) = GT
         | (length f1) > (length f2) = LT



-- Main Functions
-- ****************** 

-- i. add a new film
-- *******************
addFilm :: Film -> Database -> Database
addFilm film database = film : database

-- ii. display all films
-- **********************
displayFilms :: Database -> String
displayFilms database = concat(map displayFilm database)

-- iii. display all films that were released in a given year.
-- ***********************************************************
displayFilmsYear :: Year -> Database -> String
displayFilmsYear year database = displayFilms (filter(filmYear year) database)

-- iv. display all the films that a given user is a fan of.
-- *********************************************************
displayFilmsFan :: String -> Database -> String
displayFilmsFan fan database = displayFilms (filter (isFan fan) database)

-- v. display all the films of a given actor that were released during a particular period.
-- *************************************************************************************
displayFilmsActorDates :: String -> Year -> Year -> Database -> String
displayFilmsActorDates actor startYear endYear database = displayFilms (filter (filmPeriod startYear endYear) ((filter (hasActor actor) database)))

-- vi. allow a user to say they are a fan of a particular film.
-- *************************************************************
becomeFan :: String -> String -> Database -> Database
becomeFan fan filmTitle [] = []
becomeFan fan filmTitle ((t, c, y, f) : xs)
         | (filmTitle == t) &&  not(isFan fan (t, c, y, f)) = (t, c, y, fan : f) : becomeFan fan filmTitle xs
		 | otherwise = (t, c, y, f) : becomeFan fan filmTitle xs

-- Vii. display the best film (i.e one with the greatest number of fans) for a given actor.
-- *****************************************************************************************
displayFilmActorGreatest :: String -> Database -> String
displayFilmActorGreatest actor database 
          | (sortBy maxFans (filter (hasActor actor) database)) == [] = ""
          | otherwise = displayFilm(head (sortBy maxFans (filter (hasActor actor) database))) 

-- viii. display the overall top five films (in terms of fan numbers), sorted in descending order of number of fans.
-- ***************************************************************************************************
displayTopFive :: Database -> String
displayTopFive database = displayFilms(take 5 (sortBy maxFans database))


-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2013 film "The Great Gatsby"
--starring "Leonardo DiCaprio" and "Tobey Maguire" to testDatabase
demo 1 = putStrLn (displayFilms(addFilm("The Great Gastby", ["Leonardo DiCaprio", "Tobey Maguire"], 2013, []) testDatabase))
--demo 2  = putStrLn (fnToTurnAListOfFilmsIntoAMultiLineString testDatabase)
demo 2 = putStrLn (displayFilms testDatabase)
--demo 3  = putStrLn all films from 2012
demo 3 = putStrLn (displayFilmsYear 2012 testDatabase)
--demo 4  = putStrLn all films that "Zoe" is a fan of
demo 4 = putStrLn (displayFilmsFan "Zoe" testDatabase)
--demo 5  = putStrLn all "Tom Hanks" films from 2000 until 2011
demo 5 = putStrLn (displayFilmsActorDates "Tom Hanks" 2000 2011 testDatabase)
--demo 6  = putStrLn all films after "Zoe" becomes fan of "Forrest Gump"
demo 6 = putStrLn (displayFilms(becomeFan "Zoe" "Forrest Gump" testDatabase))
--demo 61 = putStrLn all films after "Zoe" becomes fan of "Inception"
demo 61 = putStrLn (displayFilms(becomeFan "Zoe" "Inception" testDatabase))
--demo 7  = putStrLn best "Tom Hanks" film
demo 7 = putStrLn (displayFilmActorGreatest "Tom Hanks" testDatabase)
--demo 8  = putStrLn top 5 films
demo 8 = putStrLn (displayTopFive testDatabase)


-- ******************
-- User interface code
-- ******************

main :: IO ()
main = do tempDatabase <- readFile "films.txt"
          let database = read tempDatabase 
          putStrLn "Enter your name: "
          username <- getLine
          database <- userInterface (username, database)
          writeFile "films.txt" (show database)
          putStrLn "Your changes to the database have been successfully saved. :)"

userInterface :: (String, Database) -> IO Database 
userInterface (username, database) = do let info = (username, database)
                                        let message1 = "Enter anything to go back to the main menu: "
                                        putStrLn "*****************"
                                        putStrLn "  Film Database  "
                                        putStrLn "*****************"
                                        putStrLn "1. - add a new film\n"
                                        putStrLn "2. - display all films\n"
                                        putStrLn "3. - display all films that were released in a given year\n"
                                        putStrLn "4. - display all films that a given user is a fan of\n"
                                        putStrLn "5. - display all the films of a given actor that were released"
                                        putStrLn "     during a particular period\n"
                                        putStrLn "6. - allow a user to say they are a fan of a particular film\n"
                                        putStrLn "7. - display the best film for a given actor (has the most fans)\n"
                                        putStrLn "8. - display the overall top five films, sorted in descending order of fans\n"
                                        putStrLn "0. - Exit and update database\n"
                                        putStrLn "****************************************************************************\n"
                                        putStr "Enter a number to perform an action or 0 to exit and save the database: "
                                        input <- getLine
                                        if input /= "0"
                                           then case input of
                                                     "1" -> do info <- selection 1 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "2" -> do info <- selection 2 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "3" -> do info <- selection 3 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "4" -> do info <- selection 4 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "5" -> do info <- selection 5 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "6" -> do info <- selection 6 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     "7" -> do info <- selection 7 info
                                                               putStr message1 
                                                               entry <- getLine
                                                               userInterface info
                                                     "8" -> do info <- selection 8 info
                                                               putStr message1
                                                               entry <- getLine
                                                               userInterface info
                                                     _ -> do putStrLn "You entered an invalid number."
                                                             userInterface info                                            
                                        else return (snd info)


selection :: Int -> (String, Database) -> IO (String, Database)
selection 1 (username, database) = do putStrLn "******************"
                                      putStrLn "  Add a new Film  "
                                      putStrLn "******************"
                                      putStrLn ""
                                      putStr "Enter the title of the film or nothing to return to the main menu: "
                                      title <- getLine
                                      if title == ""
                                        then return(username, database)
                                      else do let filmCheck = findFilm title database
                                              if filmCheck /= []
                                                then do putStrLn "That film already exists."
                                                        selection 1 (username, database)
                                                else do putStrLn "Enter the names of the actors that are in the film."
                                                        cast <- addCast []
                                                        putStr "Enter the year the film was made: "
                                                        tempYear <- getLine
                                                        case reads tempYear :: [(Integer, String)] of
                                                             [(n, "")] -> do let year = read tempYear :: Int
                                                                             let newDatabase = addFilm (title, cast, year, []) database
                                                                             return (username, newDatabase) 
                                                             _ -> do putStrLn "The year you entered is invalid. "
                                                                     selection 1 (username, database)
                                                    

selection 2 (username, database) = do putStrLn "*********************"
                                      putStrLn "  Display all films  "
                                      putStrLn "*********************" 
                                      putStrLn (displayFilms database)
                                      return (username, database)

selection 3 (username, database) = do putStrLn "************************************************************"
                                      putStrLn "  Display all films that were released within a given year  "
                                      putStrLn "************************************************************"
                                      putStrLn ""
                                      putStr "Enter a year: "
                                      input <- getLine
                                      case reads input :: [(Integer, String)] of
                                           [(n, "")] -> do let year = read input :: Int
                                                           let films = (displayFilmsYear year database)
                                                           if films == ""
                                                              then do putStrLn "Your search didn't return any films."
                                                                      return (username, database)
                                                           else do putStrLn "Your search returned the following films."
                                                                   putStrLn films
                                                                   return (username, database)
                                           _ -> do putStrLn "The year you entered is invalid."
                                                   return (username, database)

selection 4 (username, database) = do putStrLn "***************************************************"
                                      putStrLn "  Display all films that a given user is a fan of  "
                                      putStrLn "***************************************************"
                                      putStrLn ""
                                      let films = (displayFilmsFan username database)
                                      if films == ""
                                         then do putStrLn "You are not a fan of any films."
                                                 return (username, database)
                                         else do putStrLn "You are a fan of the following films."
                                                 putStrLn films
                                                 return (username, database)

selection 5 (username, database) = do putStrLn "****************************************************"
                                      putStrLn "  Display all the films of a given actor that were  "
                                      putStrLn "  released during a particular period  "
                                      putStrLn "****************************************************"
                                      putStrLn ""
                                      putStr "Enter the actors name: "
                                      actor <- getLine
                                      putStr "Enter the start year: "
                                      input1 <- getLine
                                      case reads input1 :: [(Integer, String)] of
                                           [(n, "")] -> do let startYear = read input1 :: Int
                                                           putStr "Enter the end year: "
                                                           input2 <- getLine
                                                           case reads input2 :: [(Integer, String)] of
                                                                [(n, "")] -> do let endYear = read input2 :: Int
                                                                                let films = (displayFilmsActorDates actor startYear endYear database)
                                                                                if films == ""
                                                                                   then do putStrLn "The search returned no films."
                                                                                           return (username, database)
                                                                                   else do putStrLn "The search returned the following films."
                                                                                           putStrLn films
                                                                                           return (username, database)
                                                                _ -> do putStrLn "The year you entered is invalid."
                                                                        return (username, database)
                                           _ -> do putStrLn "The year you entered is invalid."
                                                   return (username, database)
                                      
selection 6 (username, database) = do putStrLn "************************************************************"
                                      putStrLn "  Allow a user to say they are a fan of a particular film.  "
                                      putStrLn "************************************************************"
                                      putStrLn ""
                                      putStr "Enter the name of the film you wish to become a fan of: "
                                      title <- getLine
                                      let newDatabase = (becomeFan username title database)
                                      let database1 = displayFilms database
                                      let database2 = displayFilms newDatabase
                                      if database1 == database2
                                         then do putStrLn "You are either already a fan or the film you entered doesnt exist."
                                                 return (username, database)
                                         else do putStrLn "You successfully became a fan."
                                                 return (username, newDatabase)

selection 7 (username, database) = do putStrLn "********************************************"
                                      putStrLn "  Display the best film for a given actor.  "
                                      putStrLn "********************************************"
                                      putStrLn ""
                                      putStr "Enter the name of the actor: "
                                      actor <- getLine
                                      let film = displayFilmActorGreatest actor database
                                      if film == ""
                                         then do putStrLn "The actor you entered produced no results."
                                                 return (username, database)
                                         else do putStrLn "The best film for the actor you entered is."
                                                 putStrLn film
                                                 return (username, database)
                                      
selection 8 (username, database) = do putStrLn "**********************************************************"
                                      putStrLn "  Display the overall top five films in desending order.  "
                                      putStrLn "**********************************************************"
                                      putStrLn ""
                                      putStrLn "The top 5 films are."
                                      putStrLn (displayTopFive database)
                                      return (username, database)

-- A IO functions that is used to build a list of strings to add actors to a film.
addCast :: [String] -> IO [String]
addCast list = do putStr "Enter the actors name or nothing to finish: "
                  actor <- getLine
                  if actor == ""
                     then return list
                     else do if (elem actor list)
                                then do putStrLn "The actor you entered has already been added, try again."
                                        addCast list
                                else do let newList = (actor:list)
                                        putStrLn (show newList)
                                        addCast newList 


