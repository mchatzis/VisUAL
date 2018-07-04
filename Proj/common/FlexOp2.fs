//All Flex opcode in here 
module FlexOp2
    //Jack Pearsons FlexOp2 type definition
    open CommonData
    open CommonLex
    open Data

    type Invert = |Invert | NoInvert
    // literal value = (K % 256) rotated right by (R % 16)*2. 
    type LiteralValue = {K: uint32; R: int}
    type Literal = LiteralValue*Invert
    //possible shifts
    type ShiftOp = LSL | LSR | ASR | ROR 

    //type thing = Number | regval
    type Dat = Num of Literal | Reg of RName

    //Shifted registers are either shifted by 
    type Op2Option = NotRRX of ShiftOp*Dat | RRX  
    
    
    type Direction = |Left |Right

    //Op2 is either an immediatoe or two types of flexible op2, shifted register or an unshifted register
    type OpTwo = Immediate of Literal | Rs of RName*Op2Option | Rm of RName
    let validShiftOps = ["LSL", LSL; "LSR", LSR; "ASR", ASR; "ROR", ROR] |> Map.ofList 
    let directionList = [LSL, Left; LSR, Right; ASR, Right; ROR, Right]|>Map.ofList
    let weirdRegNamesMap = ["PC", "R15"; "LR", "R14"; "SP", "R13"]|> Map.ofList
    ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///////////////////////////////////////////////////////////////////////////////////////////////////////////
    //My own computation for flexOp2

    

    //Allows for use of listLookup function in DP module
    let  shifts = ["LSL","LSL"; "LSR","LSR"; "ASR","ASR"; "ROR","ROR"; "RRX", "RRX"]|> Map.ofList
    
    //Convert a literal to a uint32 by rotating
    let toUint32 (lit:Literal):uint32 = 
        match lit with
        | x, NoInvert -> x.K >>> (x.R*2) ||| x.K <<< (32 - (x.R*2))
        | x, Invert -> -(x.K >>> (x.R*2) ||| x.K <<< (32 - (x.R*2))|>int) |>uint32
    
    //Create literal by checking if it can be made by rotating an 8 bit instruction to the right by an even number of bits
    let makeLitCheck (lit:uint32) = 
        let rotate (k:uint32) n = (k >>> n) ||| (k <<< 32 - n),n
        [0..2..30] 
        |> List.map (rotate 0xFFu)
        |> List.tryFind (fun (mask,_) -> (mask &&& lit) = lit)
        |> Option.map (fun (_,n) -> { K=(rotate lit (32 - n)) |> fst; R=n/2})
    let makeLiteral (lit: uint32) = 
        match makeLitCheck lit, makeLitCheck (-(lit|>int)|>uint32) with
        | Some x, _ -> Ok (x, NoInvert)
        | None, Some x -> Ok (x, Invert)
        | _, _ -> Error (sprintf "%A is not a valid literal" lit)

        //Get the value of a register after it has been shifted as required
    let performShift regOut shift cpuData (doShift:uint32->int->uint32) ror= 
        let sValue=
            match shift with //Shift by reg or literal and get the uint32 value of that shift
            |Num literal -> ((toUint32 literal)|>int)
            |Reg register -> (cpuData.Regs.[register]|>int)%32
        let x = cpuData.Regs.[regOut] //Get uint32 value of data to be shifted
        match ror with
        |true-> doShift x sValue
        |false->if sValue > 31 then 0u else doShift x sValue //Perform shift on the uint32 value and return new value

    let doROR a n =
        (a >>> n) ||| (a <<< (32-n))

    //Return the uint32 value of a flexOp2 instruction
    let flexOp2 op2 cpuData =
        match op2 with
        |Immediate literal -> toUint32 literal
        |Rm register -> cpuData.Regs.[register]
        |Rs (regOut, option)->
            match option with
            |NotRRX (shift, value)->
                match shift with
                |LSL -> performShift regOut value cpuData (<<<) false
                |LSR -> performShift regOut value cpuData (>>>) false
                |ASR -> performShift regOut value cpuData (fun a b -> (int a) >>> b |> uint32) false
                |ROR -> performShift regOut value cpuData doROR true
            |RRX->
                match cpuData.Fl.C with
                |true -> (cpuData.Regs.[regOut]>>>1) ||| (1u<<<31)
                |false -> cpuData.Regs.[regOut]>>>1

    //split a string on a given character into a list
    let (|ExpSplit|_|) (splitChar:char)(input:string) =
        if (input.Split(splitChar)).Length>=2 then
            Some(input.Split(splitChar))
        else
            None

    let isInt x = box x :? int
    //Evaluate expressions
    let rec expressions (input:string) =
        let x = if (input.[0] = '-') then "0" else ""    
        match x+input with
        |  ExpSplit '+' rest ->
            Seq.fold(fun acc (x) -> acc+ (expressions x)) 0 rest
        | ExpSplit '-' rest ->
            Seq.fold(fun acc (x) -> acc- (expressions x)) ((expressions (rest.[0]))*2) rest
        | ExpSplit '*' rest ->
            Seq.fold(fun acc (x) -> acc* (expressions x)) 1 rest
        | ExpSplit '/' rest ->(rest.[0]|>int)/(rest.[1]|>int)
        | x when (input|>int<0 ||input|>int>=0) -> input|> int
        |_ -> 0

    ///Determine if a flexOp2 instruction is using a number or a register
    ///An expression is a number
    let numOrReg data (symboltable:SymbolTable option)= 
        printfn "%A" symboltable
        match data with
        |Prefix "R" rest -> //If data starts with R then register
            match listLookup regNames data with
            |Ok x -> if x = R15 then Error "R15 is not a valid register for flexible Operand 2 " else Ok (x|>Reg) //R15 is not valid for flexOp 2
            |Error _-> Error (sprintf "%A is not a valid register" data)
        |Prefix "#" rest ->  //If data starts with # then literal
            match symboltable with
            |Some x ->
                printf "%A" symboltable
                match x|>Map.tryFind(rest) with
                |Some x-> 
                //match listLookup (Some symboltable) rest with
                //|Ok y ->
                    match makeLiteral x with
                    |Ok j  -> Ok (j|>Num)
                    |Error e-> Error e
                //|Error _ ->
                |None ->
                    match makeLiteral ((expressions rest)|> uint32) with
                    |Ok y  -> Ok (y|>Num)
                    |Error e-> Error e
            |None -> 
                match makeLiteral ((expressions rest)|>uint32) with
                |Ok y  -> Ok (y|>Num)
                |Error e-> Error e
        | _ -> //Check if wierd map name eg PC
            match listLookup weirdRegNamesMap data with
            |Ok x -> 
                match x with 
                |Prefix "R" rest -> //If data starts with R then register
                    match listLookup regNames data with
                    |Ok x -> if x = R15 then Error "R15 is not a valid register for flexible Operand 2 " else Ok (x|>Reg) //R15 is not valid for flexOp 2
                    |Error (_)-> Error (sprintf "%A is not a valid register" data)
                |_ -> Error "Not a valid register or literal"
            |_ -> Error "Not a valid register or literal"


    //Make the flexOp2 with shift
    let makeShift reg shift value symboltable=
        match listLookup shifts shift with //See if shift is valid
        | Ok validShift -> 
            match validShift with 
            |"RRX"->Ok ((reg,RRX)|>Rs) //Special case of shift
            | arithShift -> 
                match listLookup validShiftOps arithShift with //Get shift type
                | Ok x-> 
                    match numOrReg value symboltable with //Shifting by what
                    |Ok y ->  
                        let shiftOp2Option = (x,y)|>NotRRX
                        (reg,shiftOp2Option)|>Rs|>Ok
                    |Error e -> Error e
                | Error e -> Error e
        |_ -> Error (sprintf "%A is not a valid shift code" shift)
    
    let workOutCarry op2 cpuData=
            match op2 with
            |Rs (reg, option)->
                match option with
                |RRX -> (getCpuRegValue reg cpuData) &&& 1u
                |NotRRX (shift, dat) ->
                    let value = 
                        match dat with
                        |Num literal-> toUint32 literal
                        |Reg reg -> (getCpuRegValue reg cpuData)%32u
                    let getLocation (location:uint32) direction= (direction) 1 (location|>int)

                    match listLookup directionList shift with
                    |Ok Left -> (getCpuRegValue reg cpuData) &&& ((getLocation (32u-value) (<<<))|>uint32)
                    |Ok Right -> (getCpuRegValue reg cpuData) &&& ((getLocation value (>>>))|>uint32)
                    |Error _ -> failwithf "This should not be possible, error should be picked up earlier"
            |_-> 
                match cpuData.Fl.C with
                | true -> 1u
                | false -> 0u




    let checkAndMakeLiteral (lit:uint32) = //As per tick 3 feedback
        let rotate (k:uint32) n = (k >>> n) ||| (k <<< 32 - n),n
        [0..2..30] 
        |> List.map (rotate 0xFFu)
        |> List.tryFind (fun (mask,_) -> (mask &&& lit) = lit)
        |> Option.map (fun (_,n) -> { K=(rotate lit (32 - n)) |> fst; R=n/2})

    ///Helper function returning result from checking literal and checking if negative of literal is valid
    let checkLiteral lit = 
        match checkAndMakeLiteral lit with
        | Some _ -> Ok lit
        | None -> match checkAndMakeLiteral (-1 |> uint32 |> fun x -> lit * x) with
                    | Some _ -> Ok lit
                    | None -> Error (sprintf "ERROR: Invalid literal %A" lit)  



//////////////////////////NOT MY FLEX OP


    // let compRRX (reg:RName)  (cpuData : DataPath) =
    //     let C = if (Map.find reg cpuData.Regs &&& 1u) <> 0u then true else false
                   
    //     //get the 32 bit number from the register shifted 
    //     let num = match cpuData.Fl.C with
    //                 |true -> (Map.find reg cpuData.Regs >>> 1) + 0x80000000ul
    //                 |false -> Map.find reg cpuData.Regs >>> 1
    //     (num, C)

    // //register values for shifting all bitwise anded with 31 to limit to 5 bits
    // let compLSL (reg:RName) (cpuData : DataPath) (shiftNum:Dat) =
    //     let data = Map.find reg cpuData.Regs

    //     match shiftNum with
    //         | Num x -> (data <<< (x |> toUint32 |> int),
    //                     if (data &&& (0x2u <<< (x |> toUint32 |> int) )) <> 0u then true else false)

    //         | Reg regNum -> (data <<< int (Map.find regNum cpuData.Regs &&& 31u), 
    //                             if (data &&& (0x2u <<< ((Map.find regNum cpuData.Regs &&& 31u) |> int) )) <> 0u then true else false) 

     
    // let compLSR (reg:RName) (cpuData : DataPath) (shiftNum:Dat) =
    //     let data = Map.find reg cpuData.Regs
    //     match shiftNum with
    //         | Num x -> (data >>> (x |> toUint32 |> int), 
    //                       if ((data <<< (31 - (x |> toUint32 |> int)))) <> 0u  then true else false)

    //         | Reg regNum -> (data >>> int (Map.find regNum cpuData.Regs &&& 31u), 
    //                             if (data  <<< ((Map.find regNum cpuData.Regs &&& 31u) |> int)) <> 0u  then true else false)
          

    // let compASR (reg:RName) (cpuData : DataPath) (shiftNum:Dat) = 
    //     let MSB = Map.find reg cpuData.Regs &&& 0x80000000ul //preserve sign
    //     let data = Map.find reg cpuData.Regs
    //     match shiftNum with
    //         | Num x ->( ((data-MSB) >>> (x |> toUint32 |> int) ) + (MSB <<< (32-(x |> toUint32 |> int))),
    //                     if (data &&& (1u <<< (x |> toUint32 |> int))) <> 0u  then true else false)
    //         | Reg regNum -> ( ((data-MSB) >>> int (Map.find regNum cpuData.Regs &&& 31u )) + (MSB <<< (32-(int (Map.find regNum cpuData.Regs &&& 31u)))),
    //                             if (data &&& (1u <<< ((Map.find regNum cpuData.Regs &&& 31u) |> int))) <> 0u  then true else false)


    // let compROR (reg:RName) (cpuData : DataPath) (shiftNum:Dat) = 
    //     let num = match shiftNum with
    //                 | Num x -> ((Map.find reg cpuData.Regs) >>> (x |> toUint32 |> int) ) + ((Map.find reg cpuData.Regs) <<< 32 - (x |> toUint32 |> int) )
    //                 | Reg regNum -> ((Map.find reg cpuData.Regs) >>> int (Map.find regNum cpuData.Regs &&& 31u)) + ((Map.find reg cpuData.Regs) <<< (32 - int (Map.find regNum cpuData.Regs &&& 31u))) 
    //     let C = if (num &&& 0x80000000u) <> 0u then true else false
    //     (num, C)

    // let flexOp2Log (op2: OpTwo) (cpuData : DataPath)  : uint32*bool = 
    //     match op2 with
    //         | Immediate x ->  (toUint32 x, false)
    //         | Rm regName -> (Map.find regName cpuData.Regs, false)
    //         | Rs (regName, option) -> match option with
    //                                       | RRX -> (compRRX regName cpuData )               //RRX func
    //                                       | NotRRX (LSL, x) -> (compLSL regName cpuData x ) //LSL func
    //                                       | NotRRX (LSR, x) -> (compLSR regName cpuData x ) //LSR func
    //                                       | NotRRX (ASR, x) -> (compASR regName cpuData x ) //ASR func
    //                                       | NotRRX (ROR, x) -> (compROR regName cpuData x ) //ROR func
