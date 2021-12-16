type BoardDisplay(rows:int, cols:int) =
    let mutable board = Array2D.create (rows*2) (cols*2) "-"
    member this.Set((row:int),(col:int)) = board.[row*2,col*2] <- "1"
    member this.Show() = 
        let mutable str = ""
        for row=0 to (rows*2)-1 do
            for col=0 to (cols*2)-1 do
                str <- str + board.[row, col]
            str <- str + "\n"
        printfn "%s" str

let test1 = new BoardDisplay(2,2)
test1.Set(1,1)
test1.Show()