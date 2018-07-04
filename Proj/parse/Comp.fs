module Comp
    open CommonData
    open CommonLex
    open FlexOp2
    open Data
    open Log

    type Operation = |Add | Sub
    type Carry = |Carry |NoCarry
    type Reverse = bool
    type Set= bool


    /// Contains the instruction
    type Instr =  {
        Instruction: Operation //Math instruction split into constituent parts
        Suffix: Set //Suffix
        Input1: RName // Input location
        Input2: OpTwo //FlexOp2 input
        }


    /// parse error 
    type ErrInstr = string

    let arithSpec = {
        InstrC = Compare
        Roots = ["CMP" ; "CPN"]
        Suffixes = [""; "S"]
    }

    //Maps for all the differenct parts and instuction relates to
    let opMap = ["CMP", Add; "CMN",Sub]|> Map.ofList
    let suffixMap = ["S", true; "", false]|> Map.ofList
    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand arithSpec


    
    //Convert a value to a FlexOp2 literal in Result monad form
    let checkLiteral literal = 
        match makeLiteral literal with
        | Ok x -> Ok x
        | Error e -> Error e 
    
    //Check if a data is a number or a register
 
    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr // address this instruction is loaded into memory
            // this does the real work of parsing
            let opList = getOperands ls.Operands
            
            //Get the flexOp2 instruction for input 2        
            let in2= 
                match opList.Length with
                | 4-> //With shift
                    //Get register
                    let reg = 
                        match listLookup regNames opList.[2] with //Get register being shifted
                        |Ok x ->if x = R15 then None else Some x
                        |Error (_)-> None
                    let oplistPlace = opList.[3]
                    match reg with //All Shift instructions 3 long so isolate first 3 characters of instruction
                    |Some x -> makeShift x oplistPlace.[0..2] oplistPlace.[3..(oplistPlace.Length - 1)] ls.SymTab
                    |None -> Error (sprintf "%A is not a valid register" reg)
                | 3-> //WithOut Shift
                    match numOrReg opList.[2] ls.SymTab with
                    |Ok (Num(nms)) -> Ok (nms|>Immediate)
                    |Ok (Reg(rg))-> Ok(rg|>Rm)
                    |Error e -> Error e
                | _ -> Error "Not a valid Flexible Operand 2 instruction"
            
            let opR = listLookup opMap root
            let suffixR = listLookup suffixMap suffix
            let outputR = listLookup regNames opList.[0]
            let input1R = listLookup regNames opList.[1]
            let input2R = in2
            match opR, suffixR, outputR, input1R, input2R with
            | Ok op, Ok suf, Ok output, Ok input1, Ok input2->
                Ok { 
                    // Normal (non-error) return from result monad
                    // This is the instruction determined from opcode, suffix and parsing
                    // the operands. Not done in the sample.
                    // Note the record type returned must be written by the module author.
                    PInstr=
                        {
                        Instruction= op
                        Suffix = suf
                        Input1 = input1
                        Input2 = input2
                    }; 


                    // This is normally the line label as contained in
                    // ls together with the label's value which is normally
                    // ls.LoadAddr. Some type conversion is needed since the
                    // label value is a number and not necessarily a word address
                    // it does not have to be div by 4, though it usually is
                    PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 


                    // this is the number of bytes taken by the instruction
                    // word loaded into memory. For arm instructions it is always 4 bytes. 
                    // For data definition DCD etc it is variable.
                    //  For EQU (which does not affect memory) it is 0
                    PSize = 4u; 

                    // the instruction condition is detected in the opcode and opCodeExpand                 
                    // has already calculated condition already in the opcode map.
                    // this part never changes
                    PCond = pCond 
                }
            | Error e, _, _, _, _  -> Error e
            | _, Error e, _, _, _ -> Error e
            | _, _, Error e, _, _ -> Error e
            | _, _, _, Error e, _ -> Error e
            | _, _, _, _, Error e -> Error e
        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse

    ///////////EXECUTE
    let errorResultRName result cpuData= 
        match result with
        |R15 -> ((getCpuRegValue R15 cpuData)+8u)
        |x-> (getCpuRegValue x cpuData)
    //Get value of FlexOp2 from a resul tMonad
    let errorResultFlexOp2 (result) cpuData= (flexOp2 result cpuData)|>uint32

    let checkPositiveIn x = if (x<0x80000000u) then true else false 

    //Work out flags based on instruction and inputs
    let checkFlags (result:uint32) (x:uint32) (y:uint32) instr=
        match instr, checkFor0 result, checkLess0 result, checkPositiveIn x, checkPositiveIn y with
        | (Sub, zero, negative, false, true) -> setFlags negative zero true (not negative) //Sub negative-positive
        | (Sub, zero, negative, true, false) -> ( setFlags negative zero false negative) //Sub positive-negative
        | (Sub, zero, negative, true, true) -> ( setFlags negative zero (not negative) false) // Sub positive - positive 
        | (Sub, zero, negative, false, false) -> (setFlags negative zero (not negative) true) // Sub negative - negative
        | (Add, zero, negative, true, true) -> ( setFlags negative zero false negative) //Adding positive + positive numbers
        | (Add, zero, negative, false, false) -> ( setFlags negative zero true (not negative)) //Adding negative+ negative numbers
        | (Add, zero, negative, _, _) -> ( setFlags negative zero (not negative) false) //Add negative + positve
        
    //Perform Addition
    let comp in1 in2 (math:uint32->uint32->uint32) instr= checkFlags (math in1 in2) in1 in2 instr



    //Determine which function to call
    let math (instruction:Instr) (cpuData:DataPath)=
        match instruction.Instruction with
        |Add -> comp (errorResultRName instruction.Input1 cpuData) (errorResultFlexOp2 instruction.Input2 cpuData) (+)  Add 
        |Sub -> comp (errorResultRName instruction.Input1 cpuData) (errorResultFlexOp2 instruction.Input2 cpuData) (-)  Sub
    let createCpuData data cpuData=
        let reg = Map.add R15 ((getCpuRegValue R15 cpuData)+4u) cpuData.Regs;
        {cpuData with Fl = data; Regs = reg}
    
    //Simulate and instruction
    let execute (instruction:Instr) (cpuData:CPUState) =
        //createCpuData (math instruction cpuData) cpuData
        Ok {DPath = createCpuData (math instruction (getCpuData cpuData)) (getCpuData cpuData);
         MEMState = getCpuMem cpuData}

    // Given a parsed instruction extract instruction and simulate used for testing only
    let parseError (parse: Result<Parse<Instr>,string> option) (cpuData:CPUState)=
        match parse with
        |Some instr->
            match instr with
            |Ok x->
                match x.PInstr with
                |y -> Ok (execute y cpuData)
            |Error e-> Error e 
        |None -> Error "Instruction failed to parse"