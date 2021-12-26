type BoardDisplay(rows:int, cols:int) =
    let board = Array2D.create (rows*2+1) (cols*2+1) ("  ")
    
    member this.Set((row:int),(col:int),(cont:string)): unit =
        let r = if row < 1 then 0 elif row > rows then (rows*2) else row*2-1
        let c = if col < 1 then 0 elif col > cols then (cols*2) else col*2-1
        //printfn "fand sidste box %A" (col*2)
        board.[r,c] <- (if cont.Length = 1 && not (cont = "|") then cont+ " " else cont)
    
    member this.SetRightWall((row:int),(col:int)) = board.[row*2-1,col*2] <- "|"
    member this.SetBottomWall((row:int),(col:int)) = board.[row*2,col*2-1] <- "--"

    member this.Show() = 
        printfn "%A" board
        printfn "\n"
        let mutable str = ""
        for row=0 to rows*2 do
            for col=0 to cols*2 do
                if row % 2 = 0 && col % 2 = 0 then str <- str + board.[row,col].Replace("  ", "+") // equal rows and columns
                elif row % 2 = 1 && col % 2 = 0  then str <- str + board.[row,col].Replace("  ", " ")  // unequal rows equal columns
                else str <- str + board.[row,col]
            str <- str + "\n"
        printfn "%s" str

    // member this.Show() = 
    //         let mutable str = ""
    //         for row=0 to rows*2 do
    //             for col=0 to cols*2 do
    //                 str <- str + board.[row,col]
    //             str <- str + "\n"
    //         printfn "%s" str

   
let display1 = new BoardDisplay(3,3)
display1.Set(1,1,"bB")
display1.Set(1,2,"tt")

display1.Set(2,1,"GG")



display1.Set(1,1,"UU")

//display1.Set(1,3,"HH")
// display1.SetRightWall(1,1)
// display1.SetRightWall(2,1)
// display1.SetRightWall(2,3)

// display1.SetRightWall(1,3)

// display1.SetBottomWall(1,3)

// display1.Set(0,1,"--")
// display1.Set(3,1,"--")
// display1.Set(1, 0, "|")


// display1.Show()
// let r = 3
// for i = 1 to r do
// //     //if i % 2 = 1 then 
//         // printfn " hit unequal: %A" i
//         // display1.Set(r, i, "--") // lower
//         // display1.Set(i, 0, "|") // left
//         // display1.Set(0, i, "|") // right
//     display1.Set(0, i, "--") // upper
//     display1.SetBottomWall(r,i) // lower 
//     display1.SetRightWall(i, r) // right
//     display1.Set(i, 0, "|")    

    //display1.Set(r+1, i, "--") // lower



type Direction = North | South | East | West
type Position = int * int

type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore



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
            | North -> if samePosition then Stop((r+1), c) else Ignore
            | South -> if samePosition then Stop((r-1), c) else Ignore 
            | East -> if  samePosition then Stop(r, (c-1)) else Ignore
            | West -> if samePosition then Stop(r, (c+1)) else Ignore


    
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
        let (otherRow, otherCol) = other.Position
        let (thisRow, thisCol) = this.Position

        if other.Name = this.Name then Ignore 
        else
            this.interActHelper thisRow thisCol other dir
    override this.RenderOn (display: BoardDisplay) = display.Set(row, col, this.Name)

// let robot1 = new Robot(3,3,"robot1")
// printfn "%A" robot1.Position
// robot1.Step(North)
// printfn "%A" robot1.Position
// robot1.Step(South)
// printfn "%A" robot1.Position
// robot1.Step(East)
// printfn "%A" robot1.Position
// robot1.Step(West)
// printfn "%A" robot1.Position

// let interActHelper r c  (robot: Robot) dir =
//         let (otherRow, otherCol) = robot.Position
//         // let (thisRow, thisCol) = this.Position
//         let samePosition = otherRow = r && otherCol = c
//         match dir with
//             | North -> if samePosition then Stop((r+1), c) else Ignore
//             | South -> if samePosition then Stop((r-1), c) else Ignore // bottom wall method
//             | East -> if  samePosition then Stop(r, (c-1)) else Ignore // right wall method
//             | West -> if samePosition then Stop(r, (c+1)) else Ignore

type Goal(r:int, c:int) = 
    inherit BoardElement()
    member this.GameOver r c (robot: Robot) = if (r,c) = robot.Position then true else false
    override this.RenderOn (display: BoardDisplay) = display.Set(r, c, "GF")

type BoardFrame(r:int, c:int) =
    inherit BoardElement()
    let mutable coordinateList = [];
    member this.wallCoordinateList = coordinateList

    override this.RenderOn (display: BoardDisplay) = 
        for i = 1 to r do
            display.Set(0, i, "--") // upper
            coordinateList <- (0,i) :: coordinateList

            display.Set(r+1, i, "--") // lower
            coordinateList <- (r+1,i) :: coordinateList

            //display.SetRightWall(i, r) // right
            display.Set(i, (r+1), "|") // left
            coordinateList <- (i, r+1) :: coordinateList

            display.Set(i, 0, "|") // left
            coordinateList <- (i,0) :: coordinateList

    
    //override this.Interact(other: Robot) (dir: Direction) = 
    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction (list: Position list) other dir =
            match list with
                [] -> Ignore
                | head::rest ->
                    match (this.interActHelper (fst head) (snd head) other dir) with
                        | Stop(r,c) -> 
                            printfn "Boardframe interact: sending stop to %A, at position %A" other (r,c)
                            Stop(r, c)
                        | Ignore -> checkForAction rest other dir 
                        | Continue(_,_) -> Ignore
        checkForAction this.wallCoordinateList other dir
        
        
type VerticalWall (r:int, c: int, n: int) =
    inherit BoardElement()
    let wallEndRow = r+n
    let maxRow = System.Math.Max(r, wallEndRow)
    let minRow = System.Math.Min(r, wallEndRow)
    
    override this.RenderOn (display: BoardDisplay) = 
        for i = minRow to maxRow do display.SetBottomWall(r,i)

    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction r c other dir =
            if r = minRow then Ignore else
            match (this.interActHelper r c other dir) with
                | Stop(r,c) -> Stop(r, c)
                | Ignore -> checkForAction (r-1) c other dir 
                | Continue(_,_) -> Ignore
        checkForAction r maxRow other dir 


type HorizontalWall (r:int, c: int, n: int) =
    inherit BoardElement()
    let wallEndCol = c+n
    let maxCol = System.Math.Max(c, wallEndCol)
    let minCol = System.Math.Min(c, wallEndCol)

    override this.RenderOn (display: BoardDisplay) = 
        for i = minCol to maxCol do display.SetBottomWall(r,i)

    override this.Interact(other: Robot) (dir: Direction) = 
        let rec checkForAction r n other dir =
            if n = minCol then Ignore else
            match (this.interActHelper r n other dir) with
                | Stop(r,c) -> Stop(r, c)
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
        let rec moveRobot (thisRobot: Robot) (elList: BoardElement list ) =             
            match elList with 
                [] ->
                    thisRobot.Step dir |> ignore
                    moveRobot thisRobot this.Elements
                | currentBoardEl::rest -> 
                        match (currentBoardEl.Interact thisRobot dir) with
                            Ignore -> moveRobot thisRobot rest
                            | Stop(r,c) ->  
                                printfn "Move: received stop from %A at position %A" currentBoardEl (r,c)
                                thisRobot.Position <- (r,c)
                            | Continue(_,_) -> thisRobot.Position <-(1,1)
        moveRobot robot this.Elements

type Game() = 
    member this.Play() =
        let r = 4 
        let c =4

        let board = Board()
        //board.AddRobot(Robot(3,3,"AA") )
        board.AddRobot(Robot(1,2,"BB") )
        for robot in board.Robots do board.AddElement robot

        board.AddElement( BoardFrame(r,c) )
        
        let boardDisplay = BoardDisplay(r,c) 
        for element in board.Elements do
            element.RenderOn(boardDisplay)

        let rec gameLoop() =
            boardDisplay.Show()
            printfn "Choose robot:" 
            let chosenRobotName = (System.Console.ReadLine() |> string)
            printfn "You chose: %A" chosenRobotName
            
            let chosenRobot = List.find( fun (rob:Robot) -> rob.Name = chosenRobotName ) board.Robots
            printfn "Old robot pos: %A" chosenRobot.Position
            boardDisplay.Set(fst chosenRobot.Position, snd chosenRobot.Position, "  ")

            let pressedKey = System.Console.ReadKey true
            // let pressedKeyResult = pressedKey.Key
            //if pressedKeyResult = System.ConsoleKey.UpArrow then printf "ypu pressed up" else printfn "tou d mj"
            match pressedKey.Key with
                System.ConsoleKey.UpArrow -> board.Move(chosenRobot, North)
                | System.ConsoleKey.DownArrow -> board.Move(chosenRobot, South)
                | System.ConsoleKey.RightArrow -> board.Move(chosenRobot, East)
                | System.ConsoleKey.LeftArrow -> board.Move(chosenRobot, West)
                | _ -> gameLoop()
            printfn "New robot pos: %A" chosenRobot.Position
            boardDisplay.Set(fst chosenRobot.Position, snd chosenRobot.Position, chosenRobotName)
            boardDisplay.Show()


                        
        gameLoop()


        

let g = Game()
g.Play()


// let keyTestFunc() = 
//         let key = System.Console.ReadKey true
//         printfn "You pressed: %A" key.Key

// keyTestFunc()