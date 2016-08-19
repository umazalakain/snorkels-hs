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
             [concat [toString (Map.lookup (x, y) (pieces b)) | x <- [0..width]] | y <- [0..height]]
             where (width, height) = (size b)

inRange :: (Int, Int) -> Int -> Bool
inRange (min, max) check = min <= check && check < max

inBounds :: (Int, Int) -> Position -> Bool
inBounds (maxX, maxY) (x, y) = inRange (0, maxX) x && inRange (0, maxY) y

offset :: (Int, Int) -> Position -> Position
offset (x, y) (x2, y2) = (x+x2, y+y2)

neighbours :: (Int, Int) -> Position -> Set.Set Position
neighbours bounds pos = Set.fromList (filter (inBounds bounds) (map ((flip offset) pos) neighbourOffsets))
                        where neighbourOffsets = [(-1, 0), (1, 0), (0, -1), (0, 1)]

matchingNeighbours :: Board -> Position -> Set.Set Position
matchingNeighbours board pos = Set.filter ((== match) . ((flip Map.lookup) pcs)) (neighbours (size board) pos)
                               where pcs = (pieces board)
                                     match = Map.lookup pos pcs

-- TODO: Should this return a Maybe Group (to account for the possibility of the
-- given position on the board being empty) or allow groups of empty positions
-- too? Such groups might be useful for AI if we ever dare go there.
groupFrom :: Board -> Position -> Maybe Group
groupFrom board pos
    | Map.lookup pos (pieces board) == Nothing    = Nothing
    | Map.lookup pos (pieces board) == Just Stone = Nothing
    | otherwise = Just Group { positions = let finishGroup :: Set.Set Position -> Position -> Set.Set Position
                                               finishGroup soFar pos
                                                   = Set.foldl (finishGroup board)
                                                               (evaluatedSet)
                                                               (neighbours Set.\\ evaluatedSet)
                                                     where evaluatedSet = Set.insert pos soFar
                                                           neighbours = matchingNeighbours board pos
                                           in finishGroup (Set.singleton pos) pos
                             , player = let (Snorkel p) = ((pieces board) Map.! pos) in p}


getGroups :: Board -> Set.Set Group
getGroups _ = Set.empty


isTrapped :: Board -> Group -> Bool
isTrapped _ _ = True

playersGroups :: Board -> Player -> Set.Set Group
playersGroups b p = Set.filter ((== p) . player) (getGroups b)

hasLost :: Board -> Player -> Bool
hasLost b p = any (isTrapped b) (Set.toList (playersGroups b p))


sampleBoard :: Board
sampleBoard = Board { pieces = (Map.fromList [((0, 0), Snorkel Green),
                                              ((0, 1), Snorkel Green),
                                              ((0, 3), Snorkel Green),
                                              ((0, 2), Snorkel Purple),
                                              ((1, 2), Snorkel Purple),
                                              ((2, 4), Stone)])
                    , size = (10, 10)}
