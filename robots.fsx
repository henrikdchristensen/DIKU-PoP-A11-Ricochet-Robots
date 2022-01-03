type Direction = North | South | East | West
type Position = int * int

type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore

type BoardDisplay(rows:int, cols:int) =
    let board = Array2D.create (rows*2+1) (cols*2+1) ("  ")
    member this.Set((row:int),(col:int),(cont:string)): unit =
        let r = if row < 1 then 0 elif row > rows then (rows*2) else row*2-1
        let c = if col < 1 then 0 elif col > cols then (cols*2) else col*2-1
        board.[r,c] <- (if cont.Length = 1 && not (cont = "|") then cont+ " " else cont)
    
    member this.SetRightWall((row:int),(col:int)) = board.[row*2-1,col*2] <- "|"
    member this.SetBottomWall((row:int),(col:int)) = board.[row*2,col*2-1] <- "--"

    member this.Show() = 
        let mutable str = ""
        for row=0 to rows*2 do
            for col=0 to cols*2 do
                if row % 2 = 0 && col % 2 = 0 then str <- str + board.[row,col].Replace("  ", "+") // equal rows and columns
                elif row % 2 = 1 && col % 2 = 0  then str <- str + board.[row,col].Replace("  ", " ")  // unequal rows equal columns
                else str <- str + board.[row,col]
            str <- str + "\n"
        printfn "%s" str
   
[< AbstractClass >]
type BoardElement () =
    abstract member RenderOn : BoardDisplay -> unit
    abstract member Interact : Robot -> Direction -> Action
    default __ . Interact _ _ = Ignore
    abstract member GameOver : Robot list -> bool
    default __ . GameOver _ = false
    
    member this.interActHelper r c  (robot: Robot) dir =
        let (otherRow, otherCol) = robot.Position
        let samePosition = otherRow = r && otherCol = c
        match dir with
            | North -> if otherRow = r+1 && otherCol = c then Stop((r+1), c) else Ignore
            | South -> if otherRow = r-1 && otherCol = c || samePosition then Stop((r-1), c) else Ignore 
            | East -> if  otherRow = r && otherCol = c-1 || samePosition then Stop(r, (c-1)) else Ignore
            | West -> if otherRow = r && otherCol = c+1 then Stop(r, c+1) else Ignore

and Robot(row:int, col:int, name:string) =
    inherit BoardElement()
    let mutable position : Position = (row, col)
    member this.Position 
        with get () = position
        and set ( pos: Position ) = position <- pos
    member this.Name = name

    member this.Step(dir:Direction) =
        match dir with
            | North -> position <- (fst position-1, snd position)
            | South -> position <- (fst position+1, snd position)
            | East -> position <- (fst position, snd position+1)
            | West -> position <- (fst position, snd position-1)
        this.Position
    
    override this.Interact(other:Robot) (dir: Direction) = 
        let (thisRow, thisCol) = this.Position
        if other.Name = this.Name then Ignore 
        else
            // printfn "Result from %A interact : %A" this.Name (this.interActHelper thisRow thisCol other dir)
            this.interActHelper thisRow thisCol other dir
    override this.RenderOn (display: BoardDisplay) = display.Set(row, col, this.Name)

type Goal(r:int, c:int) = 
    inherit BoardElement()
    override this.GameOver (robotList: Robot list) = 
        let rec checkGameOver (list: Robot list) =
            match list with
                [] -> false
                | currentRobot::rest -> 
                    if (r,c) = currentRobot.Position then true else checkGameOver rest
        checkGameOver robotList
    override this.RenderOn (display: BoardDisplay) = display.Set(r, c, "gg")

type BoardFrame(r:int, c:int) =
    inherit BoardElement()
    let mutable coordinateList = []

    override this.RenderOn (display: BoardDisplay) = 
        for i = 1 to c do
            display.Set(0, i, "--") // upper
            coordinateList <- (0,i) :: coordinateList
            display.Set(r+1, i, "--") // lower
            coordinateList <- (r+1,i) :: coordinateList
        for i = 1 to r do
            display.Set(i, (c+1), "|") // right
            coordinateList <- (i, c+1) :: coordinateList
            display.Set(i, 0, "|") // left
            coordinateList <- (i,0) :: coordinateList

    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction (list: Position list) other dir =
            match list with
                [] -> Ignore
                | head::rest ->
                    match (this.interActHelper (fst head) (snd head) other dir) with
                        | Stop(r,c) -> 
                            // printfn "Boardframe interact: sending stop to %A, at position %A" other (r,c)
                            Stop(r, c)
                        | Ignore -> checkForAction rest other dir 
                        | Continue(_,_) -> Ignore
        checkForAction coordinateList other dir
        
        
type VerticalWall (r:int, c: int, n: int) =
    inherit BoardElement()
    let wallEndRow = r+n
    let maxRow = System.Math.Max(r, wallEndRow)
    let minRow = System.Math.Min(r, wallEndRow)

    override this.RenderOn (display: BoardDisplay) = 
        for i = minRow to maxRow do display.SetRightWall(i,c)

    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction n c other dir =
            if n < minRow then Ignore else
            match (this.interActHelper n c other dir) with
                | Stop(r,c) ->
                    match dir with
                        East -> Stop(r, c+1)
                        | West -> Stop(r, c)
                        | _ -> Ignore
                | Ignore -> checkForAction (n-1) c other dir 
                | Continue(_,_) -> Ignore
        checkForAction maxRow c other dir 

type HorizontalWall (r:int, c: int, n: int) =
    inherit BoardElement()
    let wallEndCol = c+n
    let maxCol = System.Math.Max(c, wallEndCol)
    let minCol = System.Math.Min(c, wallEndCol)

    override this.RenderOn (display: BoardDisplay) = 
        for i = minCol to maxCol do display.SetBottomWall(r,i)

    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction r n other dir =
            if n < minCol then Ignore else
            match (this.interActHelper r n other dir) with
                | Stop(r,c) -> // Stop(r,c)
                    match dir with
                        North -> Stop(r, c)
                        | South -> Stop(r+1, c)
                        | _ -> Ignore
                | Ignore -> checkForAction r (n-1) other dir 
                | Continue(_,_) -> Ignore
        checkForAction r maxCol other dir

type Board() =
    let mutable robots: Robot list = []
    let mutable elements: BoardElement list = []

    member this.AddRobot(robot:Robot) = robots <- robot :: robots
    member this.AddElement(element:BoardElement) = elements <- element :: elements
    member this.Elements: BoardElement list = elements
    member this.Robots: Robot list = robots
    
    member this.Move(robot:Robot, dir:Direction) = 
        let rec moveRobot (thisRobot: Robot) (elList: BoardElement list ) direction =   
            match elList with 
                [] ->
                    thisRobot.Step dir |> ignore
                    moveRobot thisRobot this.Elements direction
                | currentBoardEl::rest -> 
                        match (currentBoardEl.Interact thisRobot direction) with
                            Ignore -> moveRobot thisRobot rest dir
                            | Stop(r,c) ->  
                                // printfn "Move: received stop from %A at position %A" currentBoardEl (r,c)
                                thisRobot.Position <- (r,c)
                            | Continue(conDir, pos) -> 
                                thisRobot.Position <- pos
                                moveRobot thisRobot this.Elements conDir // continuing at new pos = go though all elements agains
        moveRobot robot this.Elements dir


type Teleport(r:int, c:int, board: Board) =
    inherit BoardElement()
    override this.RenderOn (display: BoardDisplay) = display.Set(r, c, "tp")

    override this.Interact (robot: Robot) dir =
        let generateRandomPos() = (System.Random().Next(1, r), System.Random().Next(1, c))
        let rec getRandomFreePos (rp: Position) (list:Robot list) =
            match list with
                []-> rp
                | currentBot::rest ->
                    if currentBot.Position = rp then 
                        getRandomFreePos (generateRandomPos()) list 
                    else getRandomFreePos rp rest 
        let randomPos = getRandomFreePos (generateRandomPos()) board.Robots

        let (otherRow, otherCol) = robot.Position
        let samePosition = otherRow = r && otherCol = c
        match dir with
            | North -> if samePosition then Continue(North, randomPos ) else Ignore
            | South -> if samePosition then Continue(South, randomPos) else Ignore
            | East -> if  samePosition then Continue(East, randomPos) else Ignore
            | West -> if  samePosition then Continue(West, randomPos) else Ignore

type Game() = 
    let bestScoreFilename = "highscore.txt"

    member this.WriteHighScore (highscore: int) (player: string) =
        printfn "Congratulations %s! You made a new best score: %i moves." player highscore
        System.IO.File.WriteAllText(bestScoreFilename, player + ": " + string highscore )
    
    member this.ReadHighScore() = 
        let reader =
            try
                Some ( System.IO.File.ReadAllText bestScoreFilename)
            with
            | _ -> None
        reader

    member this.Play() =
        let r = 4 
        let c =7
        let board = Board()        
        let boardDisplay = BoardDisplay(r,c) 
        // first add elements
        board.AddElement( BoardFrame(r,c) )
        board.AddElement( HorizontalWall(1, 4, 1) )
        board.AddElement( VerticalWall(2, 3, 0) )
        board.AddElement( HorizontalWall(2, 3, 0) )
        board.AddElement( Goal(3,6) )
        // add robots after for calling interact before other elements
        board.AddRobot(Robot(1,1,"BB") )
        board.AddRobot(Robot(4,7,"CC") )
        board.AddRobot(Robot(2,3,"AA") )
        for robot in board.Robots do board.AddElement robot
        // board.AddElement( Teleport(4,4, board) )
        for element in board.Elements do element.RenderOn(boardDisplay)

        let rec gameLoop(moves: int, player: string) =
            let mutable movesMade = moves 
            System.Console.Clear() 
            boardDisplay.Show()
        
            let playerName = 
                if player = "" then 
                    printfn "Your name:" 
                    (System.Console.ReadLine() |> string) else player

            let rec getRobot() =
                printfn "Choose robot:" 
                let chosenRobotName = (System.Console.ReadLine() |> string)
                try List.find( fun (rob:Robot) -> rob.Name = chosenRobotName ) board.Robots
                with
                | _ -> 
                    printfn "This robot does not exist. Try typing the name again." 
                    getRobot()
            
            let chosenRobot = getRobot()
            printfn "You can now move: %A" chosenRobot.Name

            let rec gameOver (list: BoardElement list) = 
                    match list with 
                        []->  false
                        | currentEl::rest -> if currentEl.GameOver board.Robots = true then true else gameOver rest                

            let rec moveLoop() =
                if gameOver board.Elements = true then 
                    System.Console.Clear()
                    let bestscore = this.ReadHighScore()
                    match bestscore with 
                        None -> this.WriteHighScore movesMade playerName
                        | currentBestString -> 
                            let currentBest = currentBestString.Value.Split(' ')
                            //let name = currentBest.[0]
                            let score = currentBest.[1]
                            if movesMade < int (score) then this.WriteHighScore movesMade playerName
                    movesMade
                else 

                let moveAndDraw (dir: Direction) = 
                    System.Console.Clear()
                    let oldPos = chosenRobot.Position
                    boardDisplay.Set(fst chosenRobot.Position, snd chosenRobot.Position, "  ") // removing robot
                    board.Move(chosenRobot, dir)
                    boardDisplay.Set(fst chosenRobot.Position, snd chosenRobot.Position, chosenRobot.Name) 
                    boardDisplay.Show()
                    let newPos = chosenRobot.Position
                    if not(oldPos = newPos) then movesMade <- movesMade+1
                    printfn "Moves: %A" movesMade
                    moveLoop()  
                
                let pressedKey = System.Console.ReadKey true
                match pressedKey.Key with
                    System.ConsoleKey.UpArrow -> moveAndDraw North
                    | System.ConsoleKey.DownArrow -> moveAndDraw South 
                    | System.ConsoleKey.RightArrow -> moveAndDraw East
                    | System.ConsoleKey.LeftArrow -> moveAndDraw West
                    | System.ConsoleKey.Escape -> gameLoop(movesMade, playerName)
                    | System.ConsoleKey.R -> 
                        printfn "Are you sure you want to restart? (Y/N)"
                        let key = System.Console.ReadKey true
                        match key.Key with
                            System.ConsoleKey.N -> 
                                printfn "Restart prevented."
                                moveLoop()
                            | System.ConsoleKey.Y -> this.Play() 
                            | _ -> gameLoop(movesMade, playerName)
                    | _ -> gameLoop(movesMade, playerName)

            moveLoop()
        gameLoop(0, "")

let g = Game()
printfn "Game over. You finished in %i moves. Best score is set by %s moves" (g.Play()) (g.ReadHighScore().Value)