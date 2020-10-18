import Data.Char
-- IB015 2019 - Kostra řešení dvanácté domácí úlohy
--   * V kostře nahraďte ‚undefined‘ vlastní implementací.
--   * Definicím funkcí můžete přidávat formální parametry.
--   * DULEŽITÉ: Zadané datové typy nemodifikujte.
--   * DŮLEŽITÉ: Zadaným funkcím neměňte typové signatury.
--   * DŮLEŽITÉ: Ke všem globálně definovaným funkcím uvádějte typovou signaturu.
--   * Řešení si zkuste spustit na Aise s GHC 8.6.
--   * Vyřešenou úlohu nahrajte do odevzdávárny své seminární skupiny.
-- Před tento řádek nic nepřidávejte
import Text.Read
import Data.List (words)


---------------------------------------------------
--      Z A D A N É   D A T O V É   T Y P Y      --
---------------------------------------------------

-- loď se seznamem příslušných koordinátů s pravdivostní hodnotou
-- podle toho, zda jsou zasaženy (True = zasažen, False = nezasažen)

data Ship = Ship [(Coord, Status)]
          deriving (Eq, Show)

-- moře jsou čtvercová

type PlanSize = Int

data ShipsPlan = ShipPlan PlanSize [Ship]
               deriving (Eq, Show)

type Coord = (Int, Int)

data ShipOrientation = Horizontal
                     | Vertical
                     deriving Show

type ShipSize = Int

data Status = AsNew
            | Damaged
            deriving (Eq, Show)

data ShotResult = Ocean
                | Hit
                | Sunk
                deriving (Show, Eq)


---------------------------------------------------
--   F U N K C E   K   I M P L E M E N T A C I   --
---------------------------------------------------

isEmpty :: ShipsPlan -> Bool
isEmpty (ShipPlan _ array) = null array

toShipRec :: Coord -> ShipOrientation -> ShipSize -> Int -> [(Coord, Status)]
toShipRec (x, y) Vertical size counter = if counter == size then []
                                        else ((x, y + counter), AsNew) : toShipRec (x, y) Vertical size (counter + 1)
toShipRec (x, y) Horizontal size counter = if counter == size then []
                                        else ((x + counter, y), AsNew) : toShipRec (x, y) Horizontal size (counter + 1)

toShip :: Coord -> ShipOrientation -> ShipSize -> Ship
toShip coords direction size = Ship (toShipRec coords direction size 0)

checkShipSize :: ShipSize -> Bool
checkShipSize = (<) 0

checkOutOfPlan :: Int -> PlanSize -> Bool
checkOutOfPlan = (<=)

-- True, if no colision
checkColision1 :: Coord -> Ship -> Bool
checkColision1 _ (Ship []) = True
checkColision1 (x1, y1) (Ship (((x2, y2), _):xs)) = if x1 == x2 && y1 == y2 then False
                                                    else checkColision1 (x1, y1) (Ship xs)

checkColision2 :: Ship -> Ship -> Bool
checkColision2 (Ship []) _ = True
checkColision2 (Ship (((x1, y1), _):xs)) sh = if checkColision1 (x1, y1) sh
                                              then checkColision2 (Ship xs) sh
                                              else False

checkColision3 :: [Ship] -> Ship -> Bool
checkColision3 [] _ = True
checkColision3 (sh1:xs) sh2 = if checkColision2 sh1 sh2
                              then checkColision3 xs sh2
                              else False


placeShip :: Coord -> ShipOrientation -> ShipSize -> ShipsPlan -> Maybe ShipsPlan
placeShip (x, y) direction shSize (ShipPlan planSize ships) = 
                    if checkShipSize shSize 
                       && checkOutOfPlan (getMaxCoord direction newShip) planSize
                       && checkColision3 ships newShip
                       && x > 0 && x <= planSize
                       && y > 0 && y <= planSize
                    then Just (ShipPlan planSize (newShip : ships))
                    else Nothing

                    where newShip = toShip (x, y) direction shSize
                          getMaxCoord Horizontal (Ship array) = (fst . fst . last) array 
                          getMaxCoord Vertical (Ship array) = (snd . fst . last) array

-- 

getCoords :: Ship -> [(Coord, Status)]
getCoords (Ship array) = array

-- True if Ocean
checkMissedShot :: Coord -> [Ship] -> Bool
checkMissedShot _ [] = True
checkMissedShot (x, y) (sh:xs) = if checkColision1 (x, y) sh 
                                 then checkMissedShot (x, y) xs
                                 else False
checkSunk :: Ship -> Bool
checkSunk (Ship array) = foldr (&&) True (map checkDamaged array)
                            where checkDamaged (_, Damaged) = True
                                  checkDamaged (_, _) = False

removeSunk :: [Ship] -> [Ship]
removeSunk [] = []
removeSunk (sh:xs) = if checkSunk sh then removeSunk xs
                            else sh : removeSunk xs

updateShip :: Coord -> Ship -> [(Coord, Status)]
updateShip _ (Ship []) = []
updateShip (x1, y1) (Ship (((x2, y2), status):xs)) = if x1 == x2 && y1 == y2
                                                then ((x2, y2), Damaged) : updateShip (x1, y1) (Ship xs)
                                                else ((x2, y2), status) : updateShip (x1, y1) (Ship xs)

checkSunkAllShips :: [Ship] -> Bool
checkSunkAllShips [] = False
checkSunkAllShips (sh:xs) = if checkSunk sh then True
                            else checkSunkAllShips xs

updateAllShips :: Coord -> [Ship] -> [Ship]
updateAllShips _ [] = []
updateAllShips (x, y) (sh:xs) = (Ship (updateShip (x, y) sh)) : updateAllShips (x, y) xs

shoot :: Coord -> ShipsPlan -> (ShipsPlan, ShotResult)
shoot (x1, y1) (ShipPlan planSize ships) = if checkMissedShot (x1, y1) ships
                                            then ((ShipPlan planSize ships), Ocean)
                                           else if checkSunkAllShips updatedShips
                                            then ((ShipPlan planSize (removeSunk updatedShips), Sunk))
                                           else ((ShipPlan planSize updatedShips), Hit)
                                           
                  where updatedShips = updateAllShips (x1, y1) ships

equalsCoord :: Coord -> Ship -> Bool
equalsCoord _ (Ship []) = True
equalsCoord (x1, y1) (Ship (((x2, y2), _):xs)) = if x1 == x2 && y1 == y2 then False
                                                    else equalsCoord (x1, y1) (Ship xs)

getStatus :: Coord -> [Ship] -> Status
getStatus coords ((Ship array):xs) = 
                            if null checkShip
                             then getStatus coords xs
                           else 
                             (snd . head) checkShip

                            where checkShip = filter ((equals coords) . fst) array
                                  equals (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

getSymbol :: Status -> Char
getSymbol Damaged = 'X'
getSymbol AsNew = '#'

getLineField :: ShipsPlan -> Int -> Int -> [Char]
getLineField (ShipPlan planSize ships) x y = if x > planSize
                                                 then []
                                               else if foldr (&&) True (map (equalsCoord (x, y)) ships)
                                                 then '~' : getLineField (ShipPlan planSize ships) (x+1) y
                                               else 
                                                 (getSymbol (getStatus (x, y) ships)) : getLineField (ShipPlan planSize ships) (x+1) y

printPlanRec :: ShipsPlan -> Int -> IO ()
printPlanRec shPlan y = do
                          putStrLn (getLineField shPlan 1 y)
                          if y < getPlanSize shPlan
                            then printPlanRec shPlan (y+1)
                          else pure ()

                          where getPlanSize (ShipPlan size _) = size 

printPlan :: ShipsPlan -> IO ()
printPlan shPlan = printPlanRec shPlan 1

isN :: Maybe a -> Bool
isN Nothing = True
isN _ = False

fromJ :: Maybe a -> a
fromJ (Just a) = a

checkEmptyAndShoot :: ShipsPlan -> IO ()
checkEmptyAndShoot (ShipPlan planSize array) = do
                    if isEmpty (ShipPlan planSize array)
                        then putStrLn "You won!"
                    else do
                            putStrLn "Shoot to X Y         (or enter end)"
                            input <- getLine
                            checkedInput <- pure (parseShootInput input)
                            if input == "end"
                                then putStrLn "Ending..."
                            else if isN checkedInput
                                     || fst (fromJ checkedInput) < 1 
                                     || snd (fromJ checkedInput) < 1
                                     || fst (fromJ checkedInput) > planSize
                                     || snd (fromJ checkedInput) > planSize
                                then do
                                        putStrLn "Invalid Coordinates!"
                                        checkEmptyAndShoot (ShipPlan planSize array)
                            else
                                do
                                    tmp <- pure (shoot (fromJ checkedInput) (ShipPlan planSize array))
                                    putStrLn (show (snd tmp))
                                    checkEmptyAndShoot (fst tmp)

shooting :: ShipsPlan -> IO ()
shooting (ShipPlan planSize array) = do
                                        putStrLn "Shoot to X Y         (or enter end)"
                                        input <- getLine
                                        checkedInput <- pure (parseShootInput input)
                                        if input == "end"
                                            then putStrLn "Ending..."
                                        else if isN checkedInput
                                                || fst (fromJ checkedInput) < 1 
                                                || snd (fromJ checkedInput) < 1
                                                || fst (fromJ checkedInput) > planSize
                                                || snd (fromJ checkedInput) > planSize
                                            then do
                                                    putStrLn "Invalid Coordinates!"
                                                    shooting (ShipPlan planSize array)
                                        else
                                            do
                                                tmp <- pure (shoot (fromJ checkedInput) (ShipPlan planSize array))
                                                putStrLn (show (snd tmp))
                                                putStrLn "You won!"


buildShips :: ShipsPlan -> IO ()
buildShips shp = do
                    putStrLn "Enter ship: X Y [V | H] SIZE      (or end)"
                    input <- getLine
                    checkedInput <- pure (parseShipInput input)
                    if input == "end" 
                       then do
                                printPlan shp
                                if isEmpty shp then
                                    shooting shp
                                else
                                    checkEmptyAndShoot shp
                    else
                        if isN checkedInput
                            then do
                                    putStrLn "Invalid ship!"
                                    buildShips shp
                        else do
                                tmp <- pure (placeShip (getF (fromJ checkedInput)) (getS (fromJ checkedInput)) (getT (fromJ checkedInput)) shp)
                                if isN tmp
                                    then do
                                            putStrLn "Invalid ship"
                                            buildShips shp
                                else do
                                        printPlan (fromJ tmp)
                                        buildShips (fromJ tmp)
                    
                    where getF (a, _, _) = a
                          getS (_, a, _) = a
                          getT (_, _, a) = a




game :: IO ()
game = do   
            putStrLn "Enter plan size"
            str <- getLine
            planSize <- pure (readMaybe str :: Maybe Int)
            if isN planSize || (fromJ planSize) < 1
                then do
                        putStrLn "Invalid size"
                        game
            else
                do
                    gamePlan <- pure (ShipPlan (fromJ planSize) [])
                    buildShips gamePlan

            



---------------------------------------------------
--         P O M O C N É   F U N K C E           --
---------------------------------------------------

-- Pomocná funkce pro zpracování řádku načteného pro zadání lodě.
-- Funkce řeší pouze zpracování řádku zadávající loď a v případě,
-- že je tento vstup validní, vrací zpracované parametry zabalené
-- v Maybe. V opačném případě vrací Nothing.
-- Možná vás překvapí do-notace bez IO. Ve skutečnosti tu využíváme
-- toho, že Maybe je stejně jako IO tzv. monádou - podrobnosti pře-
-- sahují rámec tohoto kurzu. Nám stačí vědět, že (stejně jako u IO)
-- pokud nějaký z výpočtů selže (takže funkce z níž si vytahujeme
-- hodnotu pomocí "<-" vrátí Nothing), tak selže celá funkce jako
-- celek -> návratová hodnota bude Nothing. Můžete si zkusit volání
-- vyhodnotit:   parseShipInput "3 4 A 10"
-- (Výsledkem bude Nothing - selže parseOrientation. Všimněte si, že
-- není potřeba po každém volání kontrolovat, zdali volání funkce
-- uspělo - o to se nám postará do-notace, resp. funkce (>>) a (>>=)).

parseShipInput :: String -> Maybe (Coord, ShipOrientation, ShipSize)
parseShipInput input = if length inputs /= 4 then Nothing
                       else do
                            x <- readMaybe str_x
                            y <- readMaybe str_y
                            orientation <- parseOrientation str_or
                            size <- readMaybe str_size
                            return ((x, y), orientation, size)
    where
          inputs = words input
          [str_x, str_y, str_or, str_size] = inputs
          parseOrientation "V" = Just Vertical
          parseOrientation "H" = Just Horizontal
          parseOrientation  _  = Nothing


-- Analogicky pomocná funkce pro zpracování řádku načteného pro zadání souřadnic
-- pro střelbu.
parseShootInput :: String -> Maybe Coord
parseShootInput input = if length inputs /= 2 then Nothing
                        else do
                             x <- readMaybe str_x
                             y <- readMaybe str_y
                             return (x, y)
    where inputs = words input
          [str_x, str_y] = inputs


-- při kompilaci pomocí `ghc zadani12.hs -o <jméno_výstupního_souboru>` a následném
-- spuštění výsledné binárky se nám automaticky zavolá funkce game
main :: IO ()
main = game
