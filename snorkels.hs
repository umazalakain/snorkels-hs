import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


type Position = (Int, Int)

data Snorkel = Green | Purple
    deriving (Show, Eq)

type Player = Snorkel

data Group = Group { positions :: Set.Set Position
                   , player :: Player
                   } deriving (Show, Eq)

data Piece = Snorkel Snorkel | Stone
    deriving (Show, Eq)

data Board = Board { pieces :: Map.Map Position Piece
                   , size :: (Int, Int)
                   } deriving (Eq)

toChar :: Maybe Piece -> Char
toChar (Just (Snorkel Green)) = 'G'
toChar (Just (Snorkel Purple)) = 'P'
toChar (Just Stone) = 'O'
toChar Nothing = ' '

toString :: Maybe Piece -> String
toString p = ['[', toChar p, ']']

instance Show Board where
    show b = intercalate "\n"
             [concat [toString (Map.lookup (x, y) (pieces b)) | x <- [0..(fst (size b))]] | y <- [0..(snd (size b))]]

inRange :: (Int, Int) -> Int -> Bool
inRange (min, max) check = min <= check && check < max

inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) = inRange (0, maxX) x && inRange (0, maxY) y

offset :: Position -> (Int, Int) -> Position
offset (x, y) (x2, y2) = (x+x2, y+y2)

neighbours :: (Int, Int) -> Position -> Set.Set Position
neighbours bounds pos = Set.fromList (filter (inBounds bounds) (map (offset pos) [(-1, 0), (1, 0), (0, -1), (0, 1)]))

getGroups :: Board -> Set.Set Group
getGroups _ = Set.empty


isTrapped :: Board -> Group -> Bool
isTrapped _ _ = True

playersGroups :: Board -> Player -> Set.Set Group
playersGroups b p = Set.filter ((== p) . player) (getGroups b)

hasLost :: Board -> Player -> Bool
hasLost b p = any (isTrapped b) (Set.toList (playersGroups b p))


sampleBoard :: Board
sampleBoard = Board { pieces = (Map.insert (0, 0) (Snorkel Green) .
                                Map.insert (1, 1) (Snorkel Purple) .
                                Map.insert (5, 3) Stone $
                                Map.empty)
                    , size = (10, 10)}
