type BoardDisplay(rows:int, cols:int) =
    let mutable board = Array2D.create (rows*2+1) (cols*2+1) "-"
    member this.Set((row:int),(col:int),(cont:string)) = board.[row*2-1,col*2-1] <- cont
    //member this.SetBottomWall((row:int),(col:int)) =
    //member this.SetRightWall((row:int,(col:int)) =
    member this.Show() = 
        let mutable str = ""
        for row=0 to rows*2 do
            for col=0 to cols*2 do
                str <- str + board.[row,col]
            str <- str + "\n"
        printfn "%s" str

let test1 = new BoardDisplay(3,3)
test1.Set(1,1,"1")
test1.Show()
test1.Set(2,2,"1")
test1.Show()
test1.Set(1,2,"1")
test1.Show()