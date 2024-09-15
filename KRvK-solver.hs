{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
import GHC.Read (Read(readPrec))
import Text.Read (Read(..), readMaybe)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Data.Maybe
import Data.List
import Data.Ord

-- **Data definitions**

data Piece = BlackKing | WhiteKing | WhiteRook deriving (Show)
instance Eq Piece where
  (==) BlackKing BlackKing = True
  (==) WhiteKing WhiteKing = True
  (==) WhiteRook WhiteRook = True
  (==) _ _ = False

data Orientation = Horizontal | Vertical deriving (Eq)
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

-- **Printing**

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

-- **Player movement**

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

-- **Attack calculation**

-- Function to check squares attacked by the white king and white rook
squaresAttackedByWhite :: Position -> Position -> Board -> [Position]
squaresAttackedByWhite whiteKingPos rookPos board = validKingMoves whiteKingPos ++ validRookMoves board rookPos

-- Determine valid moves for the rook, ignoring the black king
validRookMoves :: Board -> Position -> [Position]
validRookMoves board (x, y) =
    validHorizontalMoves ++ validVerticalMoves
  where
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



squaresAttackedByBlack :: Position -> Board -> [Position]
squaresAttackedByBlack blackKingPos board =
    let
        whiteKingPos = fst $ head $ filter (\(_, piece) -> piece == WhiteKing) board
    in
    filter (`notElem` validKingMoves whiteKingPos) $ validKingMoves blackKingPos


-- **Computer movement**

isRookBetweenKings :: Position -> Position -> Position -> Maybe Orientation
isRookBetweenKings (x1, y1) (x2, y2) (xr, yr)
  | between y1 y2 yr = Just Horizontal
  | between x1 x2 xr = Just Vertical
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
            Just dir -> progress board dir blackKingPos whiteKingPos whiteRookPos

            Nothing ->
                case tryBetween of
                    Just newBoard -> newBoard
                    Nothing -> moveRookAwayFromKing board whiteKingPos whiteRookPos


-- Used to make it possible to put the rook between the kings
moveRookAwayFromKing :: Board -> Position -> Position -> Board
moveRookAwayFromKing board whiteKingPos whiteRookPos =
    if fst whiteKingPos == fst whiteRookPos then
        if fst whiteKingPos > 4 then
            updateBoardWithMove board (1, snd whiteRookPos)WhiteRook
        else updateBoardWithMove board (8, snd whiteRookPos) WhiteRook
    else
        if snd whiteKingPos > 4 then
            updateBoardWithMove board (fst whiteRookPos, 1)  WhiteRook
        else updateBoardWithMove board (fst whiteRookPos, 8) WhiteRook

updateBoardWithMove board move pieceToMove = map (\(pos, piece) -> if piece == pieceToMove then (move, pieceToMove) else (pos, piece)) board

progress :: Board -> Orientation -> Position -> Position -> Position -> Board
progress board orientation blackKingPos whiteKingPos rookPos =
    let
        attackedSquares = squaresAttackedByBlack blackKingPos board

        -- Distance between the rook and the black king on x and y
        xDistanceRBK = fst blackKingPos - fst rookPos
        yDistanceRBK = snd blackKingPos - snd rookPos

        possibleRookMoves = validRookMoves board rookPos

        safeMoves = filter (\move -> not $ move `elem` attackedSquares) possibleRookMoves

        targetY = if snd whiteKingPos < snd blackKingPos then 1 else 8
        targetX = if fst whiteKingPos < fst blackKingPos then 1 else 8

        -- Safe move means a move, that avoids getting the rook captured
        bestSafeVerticalMove =
            minimumBy (comparing (\(_, yr) -> abs (yr - targetY)))
                     $ filter (\(_, yr) -> yr == targetY) safeMoves

        bestSafeHorizontalMove =
            minimumBy (comparing (\(xr, _) -> abs (xr - targetX)))
                     $ filter (\(xr, _) -> xr == targetX) safeMoves

        -- Aggressive move means a move, moves closer to the black king
        bestVerticalAggressiveMove =
            if xDistanceRBK > 1 then
                tryMoveRook (fst rookPos + xDistanceRBK - 1, snd rookPos)
            else if xDistanceRBK < -1 then
                tryMoveRook (fst rookPos + xDistanceRBK + 1, snd rookPos)
            else if snd blackKingPos == snd whiteKingPos && abs (fst whiteKingPos - fst blackKingPos) == 2 then
                if xDistanceRBK > 0 then
                    tryMoveRook (fst rookPos + 1, snd rookPos)
                else
                    tryMoveRook (fst rookPos - 1, snd rookPos)
            else
                Nothing

        bestHorizontalAggressiveMove =
            if yDistanceRBK > 1 then
                tryMoveRook (fst rookPos, snd rookPos + yDistanceRBK - 1)
            else if yDistanceRBK < -1 then
                tryMoveRook (fst rookPos, snd rookPos + yDistanceRBK + 1)
            else if fst blackKingPos == fst whiteKingPos && abs (snd whiteKingPos - snd blackKingPos) == 2 then
                if yDistanceRBK > 0 then
                    tryMoveRook (fst rookPos, snd rookPos + 1)
                else
                    tryMoveRook (fst rookPos, snd rookPos - 1)
            else
                Nothing

        -- Helper function to attempt the move
        tryMoveRook :: (Int, Int) -> Maybe (Int, Int)
        tryMoveRook move =
            if move `elem` safeMoves
                then Just move
                else Nothing

        -- Moves king closer to the desired position
        kingLMove =
            case orientation of
                Vertical -> 
                    let xDiff = fst whiteKingPos - fst blackKingPos
                        movesToL = filter (\x -> x `elem` lShape && abs (fst x - fst blackKingPos) > 1) $ validKingMoves whiteKingPos
                    in
                    if not $ null movesToL then
                        head movesToL
                    else
                        if abs xDiff == 2 then
                            if snd blackKingPos > snd whiteKingPos then
                                (fst whiteKingPos, snd whiteKingPos + 1)
                            else (fst whiteKingPos, snd whiteKingPos - 1)
                        else if xDiff > 2 then
                            (fst whiteKingPos - 1, snd whiteKingPos)
                        else (fst whiteKingPos + 1, snd whiteKingPos)


                Horizontal -> 
                    let yDiff = snd whiteKingPos - snd blackKingPos
                        movesToL = filter (\x -> x `elem` lShape && abs (snd x - snd blackKingPos) > 1) $ validKingMoves whiteKingPos
                    in
                    if not $ null movesToL then
                        head movesToL
                    else
                        if abs yDiff == 2 then
                            if fst blackKingPos > fst whiteKingPos then
                                (fst whiteKingPos + 1, snd whiteKingPos)
                            else (fst whiteKingPos - 1, snd whiteKingPos)
                        else if yDiff > 2 then
                            (fst whiteKingPos, snd whiteKingPos - 1)
                        else (fst whiteKingPos, snd whiteKingPos + 1)

        -- Makes a waiting rook move that does not affect the position
        uselessRookMove =
            case orientation of
                Vertical ->
                    case tryMoveRook (fst rookPos, snd rookPos + 1) of
                        Just move -> Just move
                        Nothing -> tryMoveRook (fst rookPos, snd rookPos - 1)
                Horizontal ->
                    case tryMoveRook (fst rookPos + 1, snd rookPos) of
                        Just move -> Just move
                        Nothing -> tryMoveRook (fst rookPos - 1, snd rookPos)
        areKingsL = whiteKingPos `elem` lShape

        -- List of spaces for the white king to make an L shape with the black king
        lShape = 
            [ (fst blackKingPos + dx, snd blackKingPos + dy) | dx <- [-2, -1, 1, 2], dy <- [-2, -1, 1, 2],
          abs dx /= abs dy, isValidSquare (fst blackKingPos + dx, snd blackKingPos + dy) ]

    in
    case orientation of
        Vertical ->
            if rookPos `elem` attackedSquares then
                -- Rook is attacked, move toward rank 1 or 8
                updateBoardWithMove board bestSafeVerticalMove WhiteRook
            else
                if (abs xDistanceRBK > 1) || (snd blackKingPos == snd whiteKingPos && (abs (fst whiteKingPos - fst blackKingPos) == 2)) then
                    case bestVerticalAggressiveMove of
                        Just move -> updateBoardWithMove board move WhiteRook
                        Nothing ->
                            updateBoardWithMove board bestSafeVerticalMove WhiteRook
                else if areKingsL then
                                -- Make a useless rook move
                                case uselessRookMove of
                                    Just move -> updateBoardWithMove board move WhiteRook
                                    Nothing -> board
                            else
                                -- Move the king toward black king in an L-shape
                                updateBoardWithMove board kingLMove WhiteKing
        Horizontal ->
            if rookPos `elem` attackedSquares then
                -- Rook is attacked, move toward file 1 or 8
                updateBoardWithMove board bestSafeHorizontalMove WhiteRook
            else
                if (abs yDistanceRBK > 1) || (fst blackKingPos == fst whiteKingPos && (abs (snd whiteKingPos - snd blackKingPos) == 2)) then
                    case bestHorizontalAggressiveMove of
                        Just move -> updateBoardWithMove board move WhiteRook
                        Nothing -> updateBoardWithMove board bestSafeHorizontalMove WhiteRook
                else 
                    if areKingsL then
                        case uselessRookMove of
                            Just move -> updateBoardWithMove board move WhiteRook
                            Nothing -> board
                    else
                        updateBoardWithMove board kingLMove WhiteKing


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

-- **Checkmate check**

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

-- **Game loop handling**

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
    if isValidBoard board then do
        putStr $ printBoard board
        if isCheckmated board then
            putStrLn "Checkmate!"
        else
            gameLoop board
    else
        putStrLn "Invalid board!"

-- **Initial board validation**

isValidBoard :: Board -> Bool
isValidBoard board = 
    all (\(pos, piece) -> isValidSquare pos) board &&
    arePositionsUnique (map fst board) && not (areKingsAdjacent whiteKingPos blackKingPos)
  where
    blackKingPos = fst $ head $ filter (\(_, piece) -> piece == BlackKing) board
    whiteKingPos = fst $ head $ filter (\(_, piece) -> piece == WhiteKing) board

arePositionsUnique :: [Position] -> Bool
arePositionsUnique positions = length positions == length (removeDuplicates positions)
  where
    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

areKingsAdjacent :: Position -> Position -> Bool
areKingsAdjacent (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1


-- **Test data**

test1 = gameLoop [((1,1),BlackKing), ((3,3),WhiteKing), ((5,5),WhiteRook)]
test2 = gameLoop [((3,3),BlackKing), ((5,3),WhiteKing), ((7,3),WhiteRook)]
test3 = gameLoop [((2,3),BlackKing), ((3,5),WhiteKing), ((1,4),WhiteRook)]
test4 = gameLoop [((1,3),BlackKing), ((2,5),WhiteKing), ((2,7),WhiteRook)]
test5 = gameLoop [((1,3),BlackKing), ((1,5),WhiteKing), ((2,1),WhiteRook)]

