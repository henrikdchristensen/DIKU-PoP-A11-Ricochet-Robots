// module Robots

type Direction = North | South | East | West
type Position = int * int

type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore

/// <summary>The class 'BoardDisplay' contains the methods to display- and set inner walls on the board.</summary>
/// <param name="rows (argument)">The number of rows that the board must contain.</param>
/// <param name="cols (argument)">The number of columns that the board must contain.</param>
/// <param name="Set (method)">Set the provided characters on the specified coordinate.</param>
/// <param name="SetRightWall (method)">Sets a inner wall ("|") to the right of the specified coordinate.</param>
/// <param name="SetBottomWall (method)">Sets a inner wall ("--") under the specified coordinate.</param>
/// <param name="Show (method)">Prints out the board to the console.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type BoardDisplay(rows:int, cols:int) =
    let board = Array2D.create (rows*2+1) (cols*2+1) ("  ")
    member this.Set((row:int),(col:int),(cont:string)): unit =
        let r = if row < 1 then 0 elif row > rows then (rows*2) else row*2-1
        let c = if col < 1 then 0 elif col > cols then (cols*2) else col*2-1
        board.[r,c] <- (if cont.Length = 1 && not (cont = "|") then cont+ " " else cont)
    
    member this.SetRightWall (row:int) (col:int) = board.[row*2-1,col*2] <- "|"
    member this.SetBottomWall (row:int) (col:int) = board.[row*2,col*2-1] <- "--"

    member this.Show() = 
        let mutable str = ""
        for row=0 to rows*2 do
            for col=0 to cols*2 do
                if row % 2 = 0 && col % 2 = 0 then str <- str + board.[row,col].Replace("  ", "+") // equal rows and columns
                elif row % 2 = 1 && col % 2 = 0  then str <- str + board.[row,col].Replace("  ", " ")  // unequal rows equal columns
                else str <- str + board.[row,col]
            str <- str + "\n"
        printfn "%s" str

/// <summary>The abstract class 'BoardElement' contains the abstract methods to represent the game elements.</summary>
/// <param name="RenderOn (abstract method)">Render an element on a BoardDisplay.</param>
/// <param name="Interact (abstract method)">Determine the action to send to a robot before it is moved.</param>
/// <param name="GameOver (abstract method)">Determine if a game has been completed, based on the robots' positions.</param>
/// <returns>Returns an object of the class when instantiated</returns>
[< AbstractClass >]
type BoardElement () =
    abstract member RenderOn : BoardDisplay -> unit
    abstract member Interact : Robot -> Direction -> Action
    default __ . Interact _ _ = Ignore
    abstract member GameOver : Robot list -> bool
    default __ . GameOver _ = false

/// <summary>The class 'Robot' contains the methods to represent the robots. Robots are also game elements, as they can affect other robots.
///          Therefore it must implement the methods from BoardElement.
/// </summary>
/// <param name="row (argument)">Which row the robot should be placed at initialization.</param>
/// <param name="col (argument)">Which column the robot should be placed at initialization.</param>
/// <param name="name (argument)">Which name (id) the robot should have.</param>
/// <param name="Step (method)">Move the robot a field in the provided direction. It is just an auxiliary method to change Position, as it doesn't check for other game elements.</param>
/// <param name="Interact (abstract method)">Determine the action to send to a moving robot.</param>
/// <param name="RenderOn (overrided method)">Place the robot (id) at the provided coordinate.</param>
/// <returns>Returns an object of the class when instantiated</returns>
and Robot(row:int, col:int, name:string) =
    inherit BoardElement()
    let mutable position : Position = (row, col)
    member this.Position 
        with get () = position
        and set pos = position <- pos
    member this.Name = name

    member this.Step(dir:Direction) =
        match dir with
            | North -> position <- (fst position-1, snd position)
            | South -> position <- (fst position+1, snd position)
            | East -> position <- (fst position, snd position+1)
            | West -> position <- (fst position, snd position-1)
        this.Position
    
    override this.Interact(other:Robot) (dir: Direction) = 
        let (r, c) = this.Position
        let (otherRow, otherCol) = other.Position
        if other.Name = this.Name then Ignore 
        else
        match dir with
            | North -> if otherRow = r+1 && otherCol = c then Stop((r+1), c) else Ignore
            | South -> if otherRow = r-1 && otherCol = c then Stop((r-1), c) else Ignore 
            | East -> if  otherRow = r && otherCol = c-1 then Stop(r, (c-1)) else Ignore
            | West -> if otherRow = r && otherCol = c+1 then Stop(r, c+1) else Ignore
        override this.RenderOn (display: BoardDisplay) = display.Set(fst this.Position, snd this.Position, this.Name)

/// <summary>The class 'Goal' contains the methods to represent the target field.</summary>
/// <param name="r (argument)">Which row the goal should be placed.</param>
/// <param name="c (argument)">Which column the goal should be placed.</param>
/// <param name="GameOver (overrided method)">Returns true if a robot is stopped on the target field.</param>
/// <param name="RenderOn (overrided method)">Place the target field ("gg") at the provided coordinate.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type Goal(r:int, c:int) = 
    inherit BoardElement()
    override this.GameOver (robotList: Robot list) = 
        let rec checkGameOver (list: Robot list) =
            match list with
                [] -> false
                | currentRobot::rest -> if (r,c) = currentRobot.Position then true else checkGameOver rest
        checkGameOver robotList
    override this.RenderOn (display: BoardDisplay) = display.Set(r, c, "gg")


/// <summary>The class 'VerticalWall' contains the methods to represent an inner vertical wall.</summary>
/// <param name="r (argument)">Which row the vertical wall should be placed.</param>
/// <param name="c (argument)">Which column the vertical wall should be placed.</param>
/// <param name="n (argument)">The lenght of the wall (number of rows).</param>
/// <param name="RenderOn (overrided method)">Place the wall on the board.</param>
/// <param name="Interact (overrrided method)">Determine the action to send to a moving robot.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type VerticalWall (r:int, c: int, n: int) =
    inherit BoardElement()
    override this.RenderOn (display: BoardDisplay) = 
        for i = 0 to abs(n) - 1 do display.SetRightWall (r + i * (sign n)) c
            
    override this.Interact(other: Robot) (dir: Direction) = 
        let (oRow, oCol) = other.Position 
        let verticalAligned row = (n > 0 && row >= r && row < r + n) || (n < 0 && row <= r && row > r + n)
        match dir with
            | East -> if c = oCol && verticalAligned oRow then Stop(oRow, oCol) else Ignore
            | West -> if c = oCol - 1 && verticalAligned oRow then Stop(oRow, oCol) else Ignore
            | _ -> Ignore

/// <summary>The class 'HorizontalWall' contains the methods to represent an inner horizontal wall.</summary>
/// <param name="r (argument)">Which row the horizontal wall should be placed.</param>
/// <param name="c (argument)">Which column the horizontal wall should be placed.</param>
/// <param name="n (argument)">The lenght of the wall (number of columns).</param>
/// <param name="RenderOn (overrided method)">Place the wall on the board.</param>
/// <param name="Interact (overrrided method)">Determine the action to send to a moving robot.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type HorizontalWall (r:int, c: int, n: int) =
    inherit BoardElement()
    override this.RenderOn (display: BoardDisplay) = 
       for i = 0 to abs(n) - 1 do display.SetBottomWall r (c + i * (sign n))

    override this.Interact(other: Robot) (dir: Direction) = 
        let (oRow, oCol) = other.Position 
        let horizontalAligned col = (n > 0 && col >= c && col < c + n) || (n < 0 && col <= c && col > c + n)
        match dir with
            | North -> if r = oRow - 1 && horizontalAligned oCol then Stop(oRow, oCol) else Ignore
            | South -> if r = oRow && horizontalAligned oCol then Stop(oRow, oCol) else Ignore
            | _ -> Ignore

/// <summary>The class 'BoardFrame' contains the methods to represent the frame of a game board (the outer walls).</summary>
/// <param name="r (argument)">The number of rows the board contains.</param>
/// <param name="c (argument)">The number of columns the board contains.</param>
/// <param name="walls">Walls surrounding the board.</param>
/// <param name="RenderOn (overrided method)">Sets the outer walls on the board.</param>
/// <param name="Interact (overrrided method)">Determine the action to send to a moving robot.</param>
/// <returns>Returns an object of the class when instantiated</returns>

type BoardFrame(r:int, c:int) =
    inherit BoardElement()
    let walls : BoardElement list =
        [HorizontalWall(0, 1, c);
         HorizontalWall(r, 1, c);
         VerticalWall(1, 0, r);
         VerticalWall(1, c, r)]

    override this.RenderOn display =
        for wall in walls do wall.RenderOn display

    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction (wallList: BoardElement list) robo direction = 
            match wallList with
                [] -> Ignore
                | currentWall::rest -> 
                    match currentWall.Interact robo direction with
                        Ignore -> checkForAction rest other dir                         
                        | Stop p -> Stop p
                        | Continue (a, b) -> Continue (a, b)
        checkForAction walls other dir

/// <summary>The class 'Board' contains game elements and the robots including a method to move the robot.</summary>
/// <param name="AddRobot (method)">Adds a robot to the list of robots and to Board Element List.</param>
/// <param name="AddElement (method)">Adds a game element to the list of elements.</param>
/// <param name="Move (method)">Moves a robot in the provided direction until possible.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type Board(r: int, c: int) =
    let boardDisplay = BoardDisplay(r, c)
    let mutable robots: Robot list = []
    let mutable elements: BoardElement list = []

    member this.AddRobot(robot:Robot) = 
        elements <- (robot :> BoardElement) :: elements // upcasting type Robot to type BoardElement
        robots <- robot :: robots
    member this.AddElement(element:BoardElement) = elements <- element :: elements
    member this.Elements: BoardElement list = elements
    member this.Robots: Robot list = robots

    member this.IsGameOver() = 
            let rec gameOver (list: BoardElement list) = 
                    match list with 
                        []->  false
                        | currentEl::rest -> if currentEl.GameOver this.Robots = true then true else gameOver rest
            gameOver this.Elements

    member this.GetRobot() =
            printfn "Choose robot:" 
            let rec getRobot(input: string) =
                    try Some(List.find( fun (rob:Robot) -> rob.Name = input ) this.Robots) 
                    with _ -> 
                        printfn "Robot does not exist. Try again:" 
                        getRobot(System.Console.ReadLine())
            getRobot (System.Console.ReadLine())

    member this.RenderView() =
            for element in this.Elements do element.RenderOn(boardDisplay)
            boardDisplay.Show()
    
    member this.Move(robot:Robot, dir:Direction) = 
        let rec moveRobot (thisRobot: Robot) (elList: BoardElement list ) direction =
            boardDisplay.Set(fst robot.Position, snd robot.Position, "  ") // removing robot from display
            match elList with 
                [] ->
                    thisRobot.Step dir |> ignore
                    moveRobot thisRobot this.Elements direction
                | currentBoardEl::rest -> 
                        match (currentBoardEl.Interact thisRobot direction) with
                            Ignore -> moveRobot thisRobot rest dir
                            | Stop(r,c) -> thisRobot.Position <- (r,c)
                            | Continue(conDir, pos) -> 
                                thisRobot.Position <- pos
                                moveRobot thisRobot this.Elements conDir // continuing at new pos = go though all elements agains
        moveRobot robot this.Elements dir

/// <summary>The class 'Teleport' contains the methods to teleport a robot to a random field on the board which is free.</summary>
/// <param name="r (argument)">Which row the teleport should be placed.</param>
/// <param name="c (argument)">Which column the teleport should be placed.</param>
/// <param name="board (argument)">The playing board. Used to move the robot to a new position.</param>
/// <param name="RenderOn (overrided method)">Place the teleport ("tp") at the provided coordinate.</param>
/// <param name="Interact (overrrided method)">Determine the action to send to a moving robot.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type Teleport(r:int, c:int, board: Board) =
    inherit BoardElement()
    override this.RenderOn (display: BoardDisplay) = display.Set(r, c, "tp")

    override this.Interact (robot: Robot) dir =
        let generateRandomPos() = (System.Random().Next(1, r), System.Random().Next(1, c))
        let rec getRandomFreePos (rp: Position) (list:Robot list) =
            match list with
                []-> rp
                | currentBot::rest ->
                    if currentBot.Position = rp then getRandomFreePos (generateRandomPos()) list 
                    else getRandomFreePos rp rest 
        
        let randomPos = getRandomFreePos (generateRandomPos()) board.Robots
        let (otherRow, otherCol) = robot.Position
        let samePosition = otherRow = r && otherCol = c
        match dir with
            | North -> if samePosition then Continue(North, randomPos ) else Ignore
            | South -> if samePosition then Continue(South, randomPos) else Ignore
            | East -> if  samePosition then Continue(East, randomPos) else Ignore
            | West -> if  samePosition then Continue(West, randomPos) else Ignore

/// <summary>The class 'Game' contains the methods to play the game and to have fun!</summary>
/// <param name="WriteHighScore (method)">Writes the player's highscore in a file.</param>
/// <param name="ReadHighScore (method)">Reads the highscores and returns it/them.</param>
/// <param name="Play (method)">Starts the game and handles the interaction with the user.</param>
/// <returns>Returns an object of the class when instantiated</returns>
type Game(board : Board) = 
    let bestScoreFilename = "highscore.txt"
    let mutable movesMade = 0
    let mutable currentRobot : Robot option = None

    member this.GetPlayerName player = 
        if player = "" then 
            printfn "Your name:" 
            (System.Console.ReadLine() |> string) else player

    member this.ReadHighScore() = 
            try Some(System.IO.File.ReadAllText bestScoreFilename)
            with _ -> None

    member this.WriteHighScore (moves: int) (player: string) =
        let writeToFile() = 
            System.IO.File.WriteAllText(bestScoreFilename, player + ": " + string moves )
            printfn "Congratulations %s! You made a new best score: %i moves." player moves
        match this.ReadHighScore() with 
            None -> writeToFile()
            | currentBestString -> 
                let score = int <| currentBestString.Value.Split(' ').[1]
                if moves < score then writeToFile()

    member this.Play() =
        let rec gameLoop(player: string) =
            System.Console.Clear() 
            printfn "Moves: %A" movesMade
            board.RenderView()
            let playerName = this.GetPlayerName player

            if board.IsGameOver() then 
                System.Console.Clear()
                this.WriteHighScore movesMade playerName
                movesMade 
            else
            match currentRobot with
                | None ->currentRobot <- board.GetRobot()
                | Some robot -> 
                    let moveBot (dir: Direction) = 
                        let oldPos = robot.Position
                        board.Move(robot, dir)
                        if  oldPos <> robot.Position then movesMade <- movesMade+1
                    let pressedKey = System.Console.ReadKey true
                    match pressedKey.Key with
                        System.ConsoleKey.UpArrow -> moveBot North
                        | System.ConsoleKey.DownArrow -> moveBot South 
                        | System.ConsoleKey.RightArrow -> moveBot East
                        | System.ConsoleKey.LeftArrow -> moveBot West
                        | System.ConsoleKey.Enter -> currentRobot <- None
                        | _ -> printfn "Press enter to choose another robot"
            gameLoop(playerName)
        gameLoop("")

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