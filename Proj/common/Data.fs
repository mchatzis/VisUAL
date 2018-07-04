//Anything to do with reading or converting data
///Get uint32 value of ca reg of cpuData
module Data
    open CommonData
    let getCpuRegValue reg cpuData =
        cpuData.Regs.[reg]
            
    let checkFor0 zero= if zero = 0u then true else false
    let checkLess0 (result:uint32) = if (result|>int <0) then true else false    
    let setFlags n z c v = {N = n; Z= z; C=c;V=v}

        
    let getOperands (operands: string) =
        operands.Split ','

    //Get the correct type from a string input by searching a map
    let listLookup mapName root=
        let op= mapName|>Map.tryFind root 
        match op with
        |Some x -> Ok x
        |None -> Error (sprintf "'%A' is not vaild" root) 
        
    let (|Prefix|_|) (start:string) (input:string) =
        if input.StartsWith(start) then
            Some(input.Substring(start.Length))
        else
            None

    //***************************************RYAN'S HELPER FUNCTIONS************************************************************

    /// Searches tables with result type output
    let mapSearchRes table item  = 
        match Map.tryFind item table with 
        | Some res -> Ok res
        | None -> Error (sprintf "Item %A not found in map %A" item table)

    ///Handles errors in a Result<'a,'b> list outputting a Result<'a list, 'b>
    let resListHandle lst = 
        let rec err list =
            match list with
            | [] -> Ok lst
            | x :: xs -> match x with
                            | Ok _ -> err xs
                            | Error i -> Error i
        let arr = List.map (fun (Ok x) -> x) //De-resulting list items. Already checked for errors, so incomp. matching fine
                           
        lst |> err |> Result.map arr 

    /// Replaces all instance of a character with a string in a string           
    let strReplAll str cond out = 
        let repl x = if (x = cond) then out else string x
        String.collect repl str

                    
    let opConv typ = fun x-> x |> Result.map typ |> Result.map Some  

    let validList func = fun lst -> lst |> List.map func |> resListHandle 

    let divBy x =
        fun n -> if (n % x = 0u) then Ok n else Error (sprintf "ERROR: Invalid address %A not divisible by 4" x)
    let posValue =
        fun n -> if (n> 0u) then Ok n else Error (sprintf "ERROR: Invalid address %A is not positive" n)
    let validAddr x = x |> divBy 4u |> Result.bind posValue

    let checkRange low hi = 
        fun inp -> if (inp >= low && inp <= hi) then Ok inp else Error (sprintf "ERROR: Value %A outside range" inp)
    let getCpuMem (cpuData:CPUState)= 
        match cpuData with
        |{DPath = _ ; MEMState = x} -> x
    let getCpuData (cpuData:CPUState)= 
        match cpuData with
        |{DPath = x ; MEMState = _} -> x
    //**************************************************************************************************************************
    