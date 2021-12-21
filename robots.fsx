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

// ny ændring til main fra aske 
// ændring

type Direction = North | South | East | West
type Position = int * int

type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore


