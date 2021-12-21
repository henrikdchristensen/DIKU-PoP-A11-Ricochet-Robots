type BoardDisplay(rows:int, cols:int) =
    let mutable board = Array2D.create (rows*2) (cols*2) "*"
    member this.Set((row:int),(col:int),(cont:string)) = board.[(row-1)*2,(col-1)*2] <- cont
    
    member this.SetRightWall((row:int),(col:int)) = board.[(row-1)*2,col*2-1] <- "|"
    member this.SetBottomWall((row:int),(col:int)) = board.[row*2-1,(col-1)*2] <- "-"

    member this.Show() = 
        let mutable str = ""
        for row=0 to rows*2-1 do
            for col=0 to cols*2-1 do
                str <- str + board.[row,col]
            str <- str + "\n"
        printfn "%s" str

   
let test1 = new BoardDisplay(3,3)
test1.Set(1,1,"1")
test1.SetRightWall(1,1)
test1.SetBottomWall(1,1)
test1.Show()
// test1.Set(1,2,"1")
// test1.Show()
test1.Set(2,1,"1")
test1.SetRightWall(2,2)
test1.Show()

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
type Robot(row:int, col:int, name:string) =
    let mutable position:Position = (row, col)
    member this.Position:Position = position
    member this.Name = name
    member this.Step(dir:Direction) =
        match dir with
            | North -> position <- (fst position-1, snd position)
            | South -> position <- (fst position+1, snd position)
            | East -> position <- (fst position, snd position+1)
            | West -> position <- (fst position, snd position-1)
    //override this.Interact(other:Robot) = 
    //override this.RenderOn display = .

let robot1 = new Robot(3,3,"robot1")
printfn "%A" robot1.Position
robot1.Step(North)
printfn "%A" robot1.Position
robot1.Step(South)
printfn "%A" robot1.Position
robot1.Step(East)
printfn "%A" robot1.Position
robot1.Step(West)
printfn "%A" robot1.Position


type Board() =
    let mutable robots:(Robot list) = []
    //let mutable elements:(BoardElement list) = []
    member this.AddRobot(robot:Robot) = robot :: robots
    //member this.AddElement(element:BoardElement) = element :: elements
    //member this.Elements:(BoardElement list) = elements
    member this.Robots:(Robot list) = robots
    //member this.Move(robot:Robot, dir:Direction) =
