module Board
  ( Player(..)
  , Rank(..)
  , Piece(..)
  , Square(..)
  , Coord
  , Board
  , boardSize
  , opponentOf
  , inBounds
  , isDarkSquare
  , initialBoard
  , getSquare
  , setSquare
  , movePiece
  , piecesOf
  ) where

data Player = White | Black
  deriving (Eq, Show)

data Rank = Normal | King
  deriving (Eq, Show)

data Piece = Piece
  { owner :: Player
  , rank  :: Rank
  } deriving (Eq, Show)

data Square = Empty | Occupied Piece
  deriving (Eq, Show)

type Coord = (Int, Int) -- (row, col) en 0..7
type Board = [[Square]]

boardSize :: Int
boardSize = 8

opponentOf :: Player -> Player
opponentOf White = Black
opponentOf Black = White

inBounds :: Coord -> Bool
inBounds (r, c) =
  r >= 0 && r < boardSize &&
  c >= 0 && c < boardSize

isDarkSquare :: Coord -> Bool
isDarkSquare (r, c) = (r + c) `mod` 2 == 1

-- | Tablero inicial 8x8:
-- Filas 0..2: Black en casillas oscuras
-- Filas 3..4: Empty
-- Filas 5..7: White en casillas oscuras
initialBoard :: Board
initialBoard =
  [ [ initialSquare (r, c) | c <- [0 .. boardSize - 1] ]
  | r <- [0 .. boardSize - 1]
  ]

initialSquare :: Coord -> Square
initialSquare coord@(r, _)
  | not (isDarkSquare coord) = Empty
  | r <= 2                   = Occupied (Piece Black Normal)
  | r >= 5                   = Occupied (Piece White Normal)
  | otherwise                = Empty

getSquare :: Board -> Coord -> Maybe Square
getSquare b coord@(r, c)
  | not (inBounds coord) = Nothing
  | otherwise            = Just ((b !! r) !! c)

-- | Actualización pura por índice usando recursión.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 f (x:xs) = f x : xs
updateAt n f (x:xs)
  | n < 0     = x : xs
  | otherwise = x : updateAt (n - 1) f xs

setSquare :: Board -> Coord -> Square -> Board
setSquare b coord@(r, c) newSq
  | not (inBounds coord) = b
  | otherwise            = updateAt r (updateAt c (const newSq)) b

-- | Mueve una ficha de origen a destino dejando el origen vacío.
movePiece :: Board -> Coord -> Coord -> Board
movePiece b fromCoord toCoord =
  case getSquare b fromCoord of
    Just sq ->
      let bCleared = setSquare b fromCoord Empty
      in setSquare bCleared toCoord sq
    Nothing -> b

-- | Lista de coordenadas y piezas de un jugador.
piecesOf :: Board -> Player -> [(Coord, Piece)]
piecesOf b p =
  [ ((r, c), piece)
  | (r, row) <- zip [0..] b
  , (c, Occupied piece) <- zip [0..] row
  , owner piece == p
  ]