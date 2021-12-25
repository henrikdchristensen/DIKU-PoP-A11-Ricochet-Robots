type BoardDisplay(rows:int, cols:int) =
    let mutable board = Array2D.create (rows*2) (cols*2) "*"
    
    member this.Set((row:int),(col:int),(cont:string)): unit = board.[(row-1)*2,(col-1)*2] <- cont
    
    member this.SetRightWall((row:int),(col:int)) = board.[(row-1)*2,col*2-1] <- "|"
    member this.SetBottomWall((row:int),(col:int)) = board.[row*2-1,(col-1)*2] <- "-"

    member this.Show() = 
        let mutable str = ""
        for row=0 to rows*2-1 do
            for col=0 to cols*2-1 do
                str <- str + board.[row,col]
            str <- str + "\n"
        printfn "%s" str

   
let display1 = new BoardDisplay(3,3)
//display1.Set(1,1,"1")
display1.SetRightWall(1,1)
// display1.SetBottomWall(1,1)
display1.Show()
// // display1.Set(1,2,"1")
// // display1.Show()
// display1.Set(2,1,"1")
// display1.SetRightWall(2,2)
// display1.Show()

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
        match dir with
            | North -> if otherRow < thisRow then Stop(thisRow+1, thisCol) else Ignore
            | South -> if otherRow > thisRow then Stop(thisRow-1, thisCol) else Ignore
            | East -> if otherCol < thisCol then Stop(thisRow, thisCol-1) else Ignore
            | West -> if otherCol > thisCol then Stop(thisRow, thisCol+1) else Ignore

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

let interActHelper r c  (robot: Robot) dir =
        let (otherRow, otherCol) = robot.Position
        // let (thisRow, thisCol) = this.Position
        match dir with
            | North -> if otherRow = r then Stop((r+1), c) else Ignore
            | South -> if otherRow = r then Stop((r-1), c) else Ignore
            | East -> if otherCol = c then Stop(r, (c-1)) else Ignore
            | West -> if otherCol = c then Stop(r, (c+1)) else Ignore

type Goal(r:int, c:int) = 
    inherit BoardElement()
    member this.GameOver r c (robot: Robot) = if (r,c) = robot.Position then true else false
    override this.RenderOn (display: BoardDisplay) = display.Set(r, c, "GF")

type BoardFrame(r:int, c:int) =
    inherit BoardElement()
    override this.RenderOn (display: BoardDisplay) = 
        for i = 0 to r do
            // upper walls
            display.Set(1, i, "-")
            // lower walls
            display.Set(r, i, "-")
            // left walls
            display.Set(i, 1, "|")
            // right walls
            display.Set(i, c, "|")
    
    //override this.Interact(other: Robot) (dir: Direction) = 
    override this.Interact(other: Robot) (dir: Direction) = 
        let mutable pairList = []
        for i = 1 to c do 
            pairList <- (1,i) :: pairList // 1,1 1,2 1,3
            pairList <- (r, i) :: pairList // 4,1 4,2 
            pairList <- (i,1) :: pairList // 1,1 2,1 3,1
            pairList <- (i, r) :: pairList // 1,4 2,4 3,4

        let rec checkForAction (list: (int*int) list) other dir =
            match list with
                [] -> Ignore
                | head::rest ->
                    match (interActHelper (fst head) (snd head) other dir) with
                        | Stop(r,c) -> Stop(r, c)
                        | Ignore -> checkForAction rest other dir 
                        | Continue(_,_) -> Ignore
        checkForAction pairList other dir
        
        
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
            match (interActHelper r c other dir) with
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
            match (interActHelper r n other dir) with
                | Stop(r,c) -> Stop(r, c)
                | Ignore -> checkForAction r (n-1) other dir 
                | Continue(_,_) -> Ignore
        checkForAction r maxCol other dir 

        // if n > 0 then
        //     for i=0 to n do interActHelper( r,(c+i), dir other)
        // else
        //     for i=(n) to 0 do interActHelper( r,(c+i), dir other)
    


type Board() =
    let mutable robots: Robot list = []
    let mutable elements: BoardElement list = []

    member this.AddRobot(robot:Robot) = robots <- robot :: robots
    member this.AddElement(element:BoardElement) = elements <- element :: elements
    member this.Elements: BoardElement list = elements
    member this.Robots: Robot list = robots
    
    member this.Move(robot:Robot, dir:Direction) = 
        let rec moveRobot (thisRobot: Robot) (elList: BoardElement list ) = 
            if elList.Length < this.Elements.Length then thisRobot.Step dir |> ignore
            match elList with 
                [] -> thisRobot.Position |> ignore
                | currentBoardEl::rest -> 
                    match (currentBoardEl.Interact thisRobot dir) with
                        Ignore -> moveRobot thisRobot rest
                        | Stop(r,c) ->  thisRobot.Position <- (r,c)
                        | Continue(_,_) -> thisRobot.Position <-(1,1)
        moveRobot robot this.Elements

