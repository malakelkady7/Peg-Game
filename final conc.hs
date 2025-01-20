type Position = (Int,Int)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
data Move = M Position deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)
 
docb _ [] flag | flag == False = error "The position is not valid."
                   | otherwise = []
docb (a,b) (Peg (x,y) c:r) flag = if not(x==a) || not(y==b) then Peg (x,y) c :(docb (a,b) r flag) 
                                  else Peg (x,y) W :(docb (a,b) r True)
createBoard:: Position -> Board
createBoard (a,b) =  docb (a,b) r False 
                     where r = [Peg (-3,-1) B,Peg (-3,0) B,Peg (-3,1) B,Peg (-2,-1) B,Peg (-2,0) B,Peg (-2,1) B,Peg (-1,-3) B,Peg (-1,-2) B,Peg (-1,-1) B,Peg (-1,0) B,Peg (-1,1) B,Peg (-1,2) B,Peg (-1,3) B,Peg (0,-3) B,Peg (0,-2) B,Peg (0,-1) B,Peg (0,0) B,Peg (0,1) B,Peg (0,2) B,Peg (0,3) B,Peg (1,-3) B,Peg (1,-2) B,Peg (1,-1) B,Peg (1,0) B,Peg (1,1) B,Peg (1,2) B,Peg (1,3) B,Peg (2,-1) B,Peg (2,0) B,Peg (2,1) B,Peg (3,-1) B,Peg (3,0) B,Peg (3,1) B]

--- part d)					 
winningState :: [Peg]
winningState = [Peg (-3,-1) W,Peg (-3,0) W,Peg (-3,1) W,Peg (-2,-1) W, Peg (-2,0) W,Peg (-2,1) W,Peg (-1,-3) W,Peg (-1,-2) W, Peg (-1,-1) W,Peg (-1,0) W,Peg (-1,1) W,Peg (-1,2) W, Peg (-1,3) W,Peg (0,-3) W,Peg (0,-2) W,Peg (0,-1) W, Peg (0,0) W,Peg (0,1) W,Peg (0,2) W,Peg (0,3) W, Peg (1,-3) W,Peg (1,-2) W,Peg (1,-1) W,Peg (1,0) W, Peg (1,1) W,Peg (1,2) W,Peg (1,3) W,Peg (2,-1) W, Peg (2,0) W,Peg (2,1) W,Peg (3,-1) W,Peg (3,0) W,Peg (3,1) W]

isWinningState :: [Peg] -> Bool
isWinningState board = board == winningState

showPossibleNextStates :: Board -> [State]
showPossibleNextStates board
  | isWinningState board = error "No Possible States Exist."
  | otherwise = [ S (M pos) (flipPeg pos board) | pos <- blackPegs board, hasWhiteNeighbor pos board ]
  where
    flipPeg :: Position -> Board -> Board
    flipPeg pos = map (\(Peg p color) -> if p == pos then Peg p W else Peg p color)

    blackPegs :: Board -> [Position]
    blackPegs = map (\(Peg pos color) -> pos) . filter (\(Peg _ color) -> color == B)

    hasWhiteNeighbor :: Position -> Board -> Bool
    hasWhiteNeighbor (x, y) board =
      any (\(Peg (nx, ny) color) -> color == W && abs (x - nx) <= 1 && abs (y - ny) <= 1 && x /= nx && y /= ny) board




----- part c)

isGoal:: Board -> Bool

isGoal[]= True
isGoal((Peg (_,_) B):t) = False
isGoal((Peg (_,_) W):t) = isGoal t

------------part b
 
isValidMove:: Move -> Board -> Bool
isValidMove (M (a,b)) i = if not(check (M (a,b)) i) && (check (M (a+1,b)) i || check (M (a-1,b)) i || check (M (a,b+1)) i ||check (M (a,b-1)) i) then True else False

check (M (a,b)) (Peg (x,y) c:t) = if (not(x==a)||not(y==b)) then check(M (a,b)) t else if (x==a && y==b && c == B) then False else True
 