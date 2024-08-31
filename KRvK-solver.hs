import GHC.Read (Read(readPrec))
import Text.Read (Read(..), readMaybe)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
-- udělám klasický postup, který používají lidé při matování věží, jelikož nemůžu použít minmax.. 
-- tento postup má docela jasné algoritmické kroky, takže by to nemuselo být těžké
-- na začátku dám uživateli na výběr pozice všech tří figurek (hráč a počítač můžou mít vždy stejné barvy) 
-- vždycky na začátku bude na tahu počítač (? ve skutečnosti to je asi jedno)
-- hráč ovládá napsáním souřadnic políčka

-- alg:
-- 1. věž se pokusí postavit mezi krále, pokud to nejde, pokusí se dát šach, což mu vždycky umožní v dalším tahu to udělat (potřeba dokázat)
-- 2. další postup je, že se snaží oponentova krále dostat ke kraji, takže nejprve dát věž na stranu blíže k vlastnímu králi, 
-- a poté nahánět oponenta dokud nebudou králové v opozici -> šach
-- pokud se oponent posune dozadu, posuneme věž blíže k němu a budeme opakovat 2

data Piece = BlackKing | WhiteKing | WhiteRook

type Square = ((Int,Int),Piece)
type Board = [Square]

getPiece :: Board -> (Int, Int) -> String
getPiece board (x, y) = 
    case lookup (x, y) board of
        Just BlackKing -> "B "
        Just WhiteKing -> "W "
        Just WhiteRook -> "R "
        _ -> ". "

-- Function to create a line for a given row
line :: Board -> Int -> String
line board y = concat [getPiece board (x, y) | x <- [1..8]]

-- Function to print the entire board
printBoard :: Board -> String
printBoard board = unlines [line board y | y <- [1..8]]

move :: Board -> Board
move = undefined

playerMove :: (Int, Int) -> Board -> Board
playerMove square board = undefined

board :: Board
board = []


gameLoop :: Board -> IO ()
gameLoop board = do
    let newBoardComputer = move board
    putStr $ printBoard newBoardComputer

    putStrLn "Enter your move:"
    playerMoveInput <- readLn

    let newBoardPlayer = playerMove playerMoveInput board
    putStr $ printBoard newBoardPlayer

    gameLoop newBoardPlayer


main :: IO ()
main = do
    
    blackKingPos <- readLn
    whiteKingPos <- readLn
    whiteRookPos <- readLn

    let board = [(whiteRookPos,WhiteRook), (whiteKingPos,WhiteKing), (blackKingPos,BlackKing)]

    putStr $ printBoard board

    gameLoop board
    

testBoard :: Board
testBoard = [((1,3),WhiteRook), ((3,3),WhiteKing), ((7,5),BlackKing)]