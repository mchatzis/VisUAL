namespace VisualTest

module Main = 
    open Expecto
    open CommonTop
    /// configuration for this testing framework      
    /// configuration for expecto. Note that by default tests will be run in parallel
    /// this is set by the fields oif testParas above
    let readLines filePath = System.IO.File.ReadLines(filePath)|>Seq.toList;

    //Entry point here
    [<EntryPoint>]
    let main args =
        printfn "%A" (run (readLines "foo.txt"))
        0