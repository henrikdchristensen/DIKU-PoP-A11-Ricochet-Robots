open Robots

let r = 4 
let c =7
let board = Board(r, c)        
// first add elements
board.AddElement( BoardFrame(r,c) )
board.AddElement( HorizontalWall(1, 4, 2) )
board.AddElement( VerticalWall(2, 3, 1) )
board.AddElement( HorizontalWall(2, 3, 1) )
board.AddElement( Goal(3,6) )
board.AddElement( Teleport(4,2, board) )

// add robots after for calling interact before other elements
board.AddRobot(Robot(1,1,"BB") )
board.AddRobot(Robot(4,7,"CC") )
board.AddRobot(Robot(2,3,"AA") )

let g = Game( board )
printfn "Game over. You finished in %i moves. Best score is set by %s moves" (g.Play()) (g.ReadHighScore().Value)