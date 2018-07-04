module Arith
    open CommonData
    open CommonLex
    open FlexOp2
    open Data
    open EEExtensions
    open System

    type Operation = |Add | Sub
    type Carry = |Carry |NoCarry
    type Reverse = bool
    type Set= bool


    //All instructions are a combination of these three parts
    type Arith= {
        Op: Operation
        Rev: Reverse
        Car: Carry
    }

    /// Contains the instruction
    type Instr =  {
        Instruction: Arith //Math instruction split into constituent parts
        Suffix: Set //Suffix
        OutputReg: RName //Output location
        Input1: RName // Input location
        Input2: OpTwo //FlexOp2 input
        }


    /// parse error 
    type ErrInstr = string

    let arithSpec = {
        InstrC = Arithmetic
        Roots = ["ADD" ; "SUB" ; "RSB" ; "ADC"; "SBC"; "RSC"]
        Suffixes = [""; "S"]
    }

    //Maps for all the differenct parts and instuction relates to
    let opMap = ["ADD", Add; "ADC",Add; "SUB", Sub; "RSB", Sub; "SBC", Sub; "RSC",Sub]|> Map.ofList
    let revMap = ["ADD", false; "ADC",false; "SUB",false; "RSB", true; "SBC", false; "RSC",true]|> Map.ofList
    let carMap = ["ADD", NoCarry; "ADC",Carry; "SUB",NoCarry; "RSB", NoCarry; "SBC", Carry; "RSC",Carry]|> Map.ofList
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
            let carR = listLookup carMap root
            let revR = listLookup revMap root
            let suffixR = listLookup suffixMap suffix
            let outputR = listLookup regNames opList.[0]
            let input1R = listLookup regNames opList.[1]
            let input2R = in2
            match opR, carR, revR, suffixR, outputR, input1R, input2R with
            | Ok op, Ok car, Ok rev, Ok suf, Ok output, Ok input1, Ok input2->
                Ok { 
                    // Normal (non-error) return from result monad
                    // This is the instruction determined from opcode, suffix and parsing
                    // the operands. Not done in the sample.
                    // Note the record type returned must be written by the module author.
                    PInstr=
                        {
                        Instruction= 
                            {
                            Op = op
                            Car = car
                            Rev = rev
                            }
                        Suffix = suf
                        OutputReg = output
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
            | Error e, _, _, _, _, _, _ -> Error e
            | _, Error e, _, _, _, _, _ -> Error e
            | _, _, Error e, _, _, _, _ -> Error e
            | _, _, _, Error e, _, _, _ -> Error e
            | _, _, _, _, Error e, _, _ -> Error e
            | _, _, _, _, _, Error e, _ -> Error e
            | _, _, _, _, _, _, Error e -> Error e
        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse


/////EXECUTE
    let getCpuRegValue reg cpuData =
        cpuData.Regs.[reg]
    //Get Rname with value from a Result Monad
    let errorResultRName result cpuData= 
        match result with
        |R15 -> ((getCpuRegValue R15 cpuData)+8u)
        |x-> (getCpuRegValue x cpuData)
    //Get value of FlexOp2 from a resul tMonad
    let errorResultFlexOp2 result cpuData= (flexOp2 result cpuData)|>uint32
    let setFlags n z c v = {N = n; Z= z; C=c;V=v}
    let checkSame x y = if x=y then true else false
    let checkPositiveIn x = if (x<0x80000000u) then true else false 

    //Work out flags based on instruction and inputs
    let performAndCheckFlags (result:uint32) (x:uint32) (y:uint32) instr=
        match instr, checkFor0 result, checkLess0 result, checkPositiveIn x, checkPositiveIn y with
        | (Sub, zero, negative, false, true) -> (result, setFlags negative zero true (not negative)) //Sub negative-positive
        | (Sub, zero, negative, true, false) -> (result, setFlags negative zero false negative) //Sub positive-negative
        | (Sub, zero, negative, true, true) -> (result, setFlags negative zero (not negative) false) // Sub positive - positive 
        | (Sub, zero, negative, false, false) -> (result, setFlags negative zero (not negative) true) // Sub negative - negative
        | (Add, zero, negative, true, true) -> (result, setFlags negative zero false negative) //Adding positive + positive numbers
        | (Add, zero, negative, false, false) -> (result, setFlags negative zero true (not negative)) //Adding negative+ negative numbers
        | (Add, zero, negative, _, _) -> (result, setFlags negative zero (not negative) false) //Add negative + positve
        
    //Perform Addition
    let add in1 in2 (math:uint32->uint32->uint32) carry cpuData set instr= 
        match in1,in2, carry, cpuData.Fl.C, set with
        |(x, y, Carry, true, false)->((math x y+1u), cpuData.Fl)
        |(x, y, Carry, false, false)->((math x y), cpuData.Fl)
        |(x, y, Carry, true, true)->(performAndCheckFlags (math x y+1u) x y instr)
        |(x, y, Carry, false, true)->(performAndCheckFlags (math x y) x y instr)
        |(x, y, _, _,false)->((math x y), cpuData.Fl)
        |(x, y, _, _, true)->(performAndCheckFlags (math x y) x y instr)

    //Perform subtraction
    let sub in1 in2 (math:uint32->uint32->uint32) carry cpuData set instr=
        match in1,in2, carry, cpuData.Fl.C, set with
        |(x, y, Carry, false, false)->((math x (y+1u)), cpuData.Fl)
        |(x, y, Carry, true, false)->((math x y), cpuData.Fl)
        |(x, y, Carry, false, true)->(performAndCheckFlags (math x (y+1u)) x y instr)
        |(x, y, Carry, true, true)->(performAndCheckFlags (math x y) x y instr)
        |(x, y, _, _, false)->((math x y), cpuData.Fl)
        |(x, y, _, _, true)->(performAndCheckFlags (math x y) x y instr)

    //Determine which function to call
    let math (instruction:Instr) (cpuData:DataPath)= 
        match instruction.Instruction with
        |{Op = Add; Rev =_; Car =carry } -> add (errorResultRName instruction.Input1 cpuData) (errorResultFlexOp2 instruction.Input2 cpuData) (+) carry cpuData instruction.Suffix Add //Add 
        |{Op = Sub; Rev =false; Car =carry } -> sub (errorResultRName instruction.Input1 cpuData) (errorResultFlexOp2 instruction.Input2 cpuData) (-) carry cpuData instruction.Suffix Sub //Normal sub
        |{Op = Sub; Rev =true; Car =carry } -> sub (errorResultFlexOp2 instruction.Input2 cpuData) (errorResultRName instruction.Input1 cpuData) (-) carry cpuData instruction.Suffix Sub //Sub inputs reversed

    let createCpuData data regOut cpuData=
        let regValue reg = 
            if regOut = reg then fst data else cpuData.Regs.[reg]
        let reg = Map.ofList [ R0, regValue R0 ; R1, regValue R1 ; R2, regValue R2 ; R3, regValue R3 ; R4, regValue R4 ; R5, regValue R5;
            R6, regValue R6 ; R7, regValue R7 ; R8, regValue R8 ; R9, regValue R9 ; R10, regValue R10 ; R11, regValue R11 ; 
            R12, regValue R12 ; R13, regValue R13 ;R14, regValue R14 ; R15, (regValue R15) + 4u ; 
            ]
        {cpuData with Fl = snd data; Regs = reg}
    
    //Simulate and instruction
    let execute (instruction:Instr) (cpuData:CPUState) =
        Ok {DPath = createCpuData (math instruction (getCpuData cpuData)) instruction.OutputReg (getCpuData cpuData);
         MEMState = getCpuMem cpuData} //If Suffix set call instructions that set flags

    // Given a parsed instruction extract instruction and simulate used for testing only
    let parseError (parse: Result<Parse<Instr>,string> option) (cpuData:CPUState)=
        match parse with
        |Some instr->
            match instr with
            |Ok x->
                match x.PInstr with
                |y -> (execute y cpuData) 
        |None -> Error "Instruction failed to parse"
    
