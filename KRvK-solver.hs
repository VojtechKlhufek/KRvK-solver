{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
import GHC.Read (Read(readPrec))
import Text.Read (Read(..), readMaybe)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Data.Maybe
import Data.List
import Data.Ord
-- udělám klasický postup, který používají lidé při matování věží, jelikož nemůžu použít minmax.. 
-- tento postup má docela jasné algoritmické kroky, takže by to nemuselo být těžké
-- na začátku dám uživateli na výběr pozice všech tří figurek (hráč a počítač můžou mít vždy stejné barvy) 
-- vždycky na začátku bude na tahu počítač (? ve skutečnosti to je asi jedno)
-- hráč ovládá napsáním směru, kterým se chce pohnout

-- alg:
-- 1. věž se pokusí postavit mezi krále, pokud to nejde, pokusí se dát šach, což mu vždycky umožní v dalším tahu to udělat (potřeba dokázat)
-- 2. další postup je, že se snaží oponentova krále dostat ke kraji, takže nejprve dát věž na stranu blíže k vlastnímu králi, 
-- a poté nahánět oponenta dokud nebudou králové v opozici -> šach
-- pokud se oponent posune dozadu, posuneme věž blíže k němu a budeme opakovat 2

data Piece = BlackKing | WhiteKing | WhiteRook deriving (Show)
instance Eq Piece where
  (==) BlackKing BlackKing = True
  (==) WhiteKing WhiteKing = True
  (==) WhiteRook WhiteRook = True
  (==) _ _ = False

data Orientation = Horizontal | Vertical
instance Show Orientation where
    show Vertical   = "Vertical"
    show Horizontal = "Horizontal"


data PlayerMoveInputChars = U | UR | R | DR | D | DL | L | UL
instance Read PlayerMoveInputChars where
  readPrec = do
    lift $ choice [ string "u" >> return U,
                    string "ur" >> return UR,
                    string "r" >> return R,
                    string "dr" >> return DR,
                    string "d" >> return D,
                    string "dl" >> return DL,
                    string "l" >> return L,
                    string "ul" >> return UL
                  ]

type Position = (Int,Int) 
type Square = (Position,Piece)
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
printBoard board = unlines [line board y | y <- [1..8]] ++ "\n"











playerMove :: PlayerMoveInputChars -> Board -> Maybe Board
playerMove move board =
    let bb = filter checkIfLegal [map (\x -> if snd x == BlackKing then (newSquare move (fst x), snd x) else x) board]
    in case bb of
        [] -> Nothing
        _ -> Just $ head bb
    where
        newSquare :: PlayerMoveInputChars -> Position -> Position
        newSquare move curSquare =
            let x = fst curSquare
                y = snd curSquare
            in
            case move of
                U -> (x, y - 1)
                UR -> (x + 1, y - 1)
                R -> (x + 1, y)
                DR -> (x + 1, y + 1)
                D -> (x, y + 1)
                DL -> (x - 1, y + 1)
                L -> (x - 1, y)
                UL -> (x - 1, y - 1)

checkIfLegal :: Board -> Bool
checkIfLegal board =
    let blackKingPos = fst $ head $ filter (\(_, piece) -> piece == BlackKing) board
        whiteKingPos = fst $ head $ filter (\(_, piece) -> piece == WhiteKing) board
        rookPos = fst $ head $ filter (\(_, piece) -> piece == WhiteRook) board
        whiteAttacks = squaresAttackedByWhite whiteKingPos rookPos board
    in not (blackKingPos `elem` whiteAttacks || blackKingPos == whiteKingPos || blackKingPos == rookPos)
       && isValidSquare blackKingPos

-- Function to check squares attacked by the white king and white rook
squaresAttackedByWhite :: Position -> Position -> Board -> [Position]
squaresAttackedByWhite whiteKingPos rookPos board = validKingMoves whiteKingPos ++ validRookMoves board rookPos

-- Determine valid moves for the rook, ignoring the black king
validRookMoves :: Board -> Position -> [Position]
validRookMoves board (x, y) =
    -- Horizontal moves (left and right)
    validHorizontalMoves ++
    -- Vertical moves (up and down)
    validVerticalMoves
  where
    -- Extract the positions of all pieces on the board
    occupiedSquares = map fst board

    -- Filter out the black king from the occupied squares
    whitePieces = filter (\(_, piece) -> piece /= BlackKing) board
    whiteOccupiedSquares = map fst whitePieces

    -- Moves to the left of the rook along the rank
    validHorizontalMoves = takeWhile (\(r, _) -> (r, y) `notElem` whiteOccupiedSquares) [(r, y) | r <- [x-1, x-2..1]] ++
                           takeWhile (\(r, _) -> (r, y) `notElem` whiteOccupiedSquares) [(r, y) | r <- [x+1..8]]

    -- Moves to the top and bottom of the rook along the file
    validVerticalMoves = takeWhile (\(_, c) -> (x, c) `notElem` whiteOccupiedSquares) [(x, c) | c <- [y-1, y-2..1]] ++
                         takeWhile (\(_, c) -> (x, c) `notElem` whiteOccupiedSquares) [(x, c) | c <- [y+1..8]]




-- King attacks adjacent squares
validKingMoves :: Position -> [Position]
validKingMoves (x, y) = filter isValidSquare [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

-- Ensure position is within the 8x8 board
isValidSquare :: Position -> Bool
isValidSquare (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

















playerPrompt :: Board -> IO PlayerMoveInputChars
playerPrompt board = do
    putStrLn "Enter your move:"
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Invalid input! Please try again."
            playerPrompt board -- Reprompt on invalid input
        Just playerMoveInput ->
            let newBoardPlayer = playerMove playerMoveInput board
            in case newBoardPlayer of
                Nothing -> do
                    putStrLn "Illegal move! Please try again."
                    playerPrompt board -- Reprompt on illegal move
                Just _ -> return playerMoveInput -- Return valid input if move is legal

gameLoop :: Board -> IO ()
gameLoop board = do
    let newBoardComputer = computerMove board
    putStr $ printBoard newBoardComputer

    let mated = isCheckmated newBoardComputer

    if mated then
        putStr "Checkmate!"
    else do

        playerMoveInput <- playerPrompt newBoardComputer

        let newBoardPlayer = fromMaybe newBoardComputer $ playerMove playerMoveInput newBoardComputer

        putStr $ printBoard newBoardPlayer

        gameLoop newBoardPlayer


main :: IO ()
main = do
    putStrLn "Enter black kings position:"
    blackKingPos <- readLn
    putStrLn "Enter white kings position:"
    whiteKingPos <- readLn
    putStrLn "Enter white rooks position:"
    whiteRookPos <- readLn

    let board = [(whiteRookPos,WhiteRook), (whiteKingPos,WhiteKing), (blackKingPos,BlackKing)]

    putStr $ printBoard board

    gameLoop board









{--
computerMove :: Board -> Board
computerMove board =
    let blackKingPos = fst $ head $ filter (\(_, piece) -> piece == BlackKing) board
        whiteKingPos = fst $ head $ filter (\(_, piece) -> piece == WhiteKing) board
        rookPos = fst $ head $ filter (\(_, piece) -> piece == WhiteRook) board
        
        -- Move the rook and white king
        newRookPos = moveRook rookPos blackKingPos
        newWhiteKingPos = moveWhiteKing whiteKingPos blackKingPos

        -- Update the board with the new positions
        newBoard = map (\(pos, piece) ->
                        if piece == WhiteRook then (newRookPos, WhiteRook)
                        else if piece == WhiteKing then (newWhiteKingPos, WhiteKing)
                        else (pos, piece)) board
    in newBoard

-- Move rook to control the file or rank
moveRook :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveRook (rx, ry) (bx, by)
    -- If rook isn't aligned, move closer to the black king
    | rx /= bx = (bx, ry)  -- Align rook horizontally
    | otherwise = (rx, by) -- Align rook vertically

-- Move the white king closer to the black king
moveWhiteKing :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveWhiteKing (wx, wy) (bx, by)
    | wx < bx = (wx + 1, wy)  -- Move white king down
    | wx > bx = (wx - 1, wy)  -- Move white king up
    | wy < by = (wx, wy + 1)  -- Move white king right
    | wy > by = (wx, wy - 1)  -- Move white king left
    | otherwise = (wx, wy)    -- Already close, stay put--}

squaresAttackedByBlack :: Position -> Board -> [Position]
squaresAttackedByBlack blackKingPos board = validKingMoves blackKingPos


isRookBetweenKings :: Position -> Position -> Position -> Maybe Orientation
isRookBetweenKings (x1, y1) (x2, y2) (xr, yr)
  | between y1 y2 yr = Just Vertical
  | between x1 x2 xr = Just Horizontal
  | otherwise = Nothing
  where
    between a b c = (a < c && c < b) || (b < c && c < a)



-- Main function to determine the computer's move

computerMove :: Board -> Board
computerMove board =
    let
        whiteRookPos = fst $ head $ filter (\(_, piece) -> piece == WhiteRook) board
        blackKingPos = fst $ head $ filter (\(_, piece) -> piece == BlackKing) board
        whiteKingPos = fst $ head $ filter (\(_, piece) -> piece == WhiteKing) board

        rookBetweenKings = isRookBetweenKings blackKingPos whiteKingPos whiteRookPos

        tryBetween = tryToMoveRookBetweenKings board blackKingPos whiteKingPos whiteRookPos



    in 
        case rookBetweenKings of
            Just dir -> undefined
                
            Nothing -> 
                case tryBetween of
                    Just newBoard -> newBoard
                    Nothing -> undefined
        

tryToMoveRookBetweenKings :: Board -> Position -> Position -> Position -> Maybe Board
tryToMoveRookBetweenKings board blackKingPos whiteKingPos rookPos =
    let
        attackedSquares = squaresAttackedByBlack blackKingPos board
        possibleMoves = validRookMoves board rookPos
        movesBetween = filter (\move ->
            case isRookBetweenKings blackKingPos whiteKingPos move of
                Nothing -> False
                Just _ -> True) possibleMoves
        safeMoves = filter (`notElem` attackedSquares) movesBetween
        closestMove = if not (null safeMoves)
                        then Just $ minimumBy (comparing $ distance blackKingPos) safeMoves
                        else Nothing
    in 
        case closestMove of
            Just pos -> Just $ map (\x -> if snd x == WhiteRook then (pos, snd x) else x) board
            Nothing -> Nothing


distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)


testBoard :: Board
testBoard = [((3,3), BlackKing),((4,3), WhiteKing),((1,1), WhiteRook)]



































-- Check if the black king is in checkmate
isCheckmated :: Board -> Bool
isCheckmated board =
    let whiteRookPos = fst $ head $ filter (\(_, piece) -> piece == WhiteRook) board
        blackKingPos = fst $ head $ filter (\(_, piece) -> piece == BlackKing) board
        bx = fst blackKingPos
        by = snd blackKingPos
        -- Check if the black king is in check
        blackKingInCheck = blackKingPos `elem` validRookMoves board whiteRookPos
        -- Check if the black king has any legal moves
        blackKingLegalMoves = filter (isLegalMove board) [(bx + x, by + y) | x <- [-1..1], y <- [-1..1]]
    in blackKingInCheck && null blackKingLegalMoves

-- Check if a move for the black king is legal
isLegalMove :: Board -> (Int, Int) -> Bool
isLegalMove board movePos =
    let isInsideBoard (x, y) = x >= 1 && x <= 8 && y >= 1 && y <= 8
        validMove = isInsideBoard movePos
        -- Check if the move would put the black king in check
        newBoard = map (\x -> if snd x == BlackKing then (movePos, snd x) else x) board
    in validMove && not (blackKingInCheck newBoard)

-- Helper function to check if the black king is in check
blackKingInCheck :: Board -> Bool
blackKingInCheck board  =
    let whiteRookPos = fst $ head $ filter (\(_, piece) -> piece == WhiteRook) board
        blackKingPos = fst $ head $ filter (\(_, piece) -> piece == BlackKing) board
        whiteKingPos = fst $ head $ filter (\(_, piece) -> piece == WhiteKing) board

    in blackKingPos `elem` squaresAttackedByWhite whiteKingPos whiteRookPos board

