let filename = "test.txt"

let reader =
    try
        //Some ( System.IO.File.Open (filename, System.IO.FileMode.Open ) )
        // System.IO.File.ReadAllText filename
        Some (System.IO.File.ReadAllText filename)

    with
    | _ -> None


let reader2 = 
    try 
        System.IO.File.ReadAllText filename
    with
        | :? System.IO.FileNotFoundException as ex -> 
            printfn "No file found : %A" (ex.Message)
            "0"

printfn "The file %A was successfully opened. With val %A" filename ( reader.Value )