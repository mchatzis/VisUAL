module Misc
//////////////////////////////////////////////////////////////////////////////////////////
//                          MISC Instruction Module 
//////////////////////////////////////////////////////////////////////////////////////////

    open CommonLex
    open Data
    open FlexOp2
    open Expressions
    open Data
    open CommonData
    open Arith


    /// Valid Operand Types for Misc instructions
    type Value = LAB of string | ADDR of uint32 | WVAL of uint32 list 


    /// instruction 
    type Instr =  { 
        OpRoot: string //Didn't seem worth converting 
        Symbol: string option
        Operand: Value option //
        PrevAddr: CommonData.WAddr
    }

    /// parse error (dummy, but will do)
    type ErrInstr = string 

    /// sample specification for set of instructions
    let miscSpec = {
        InstrC = MISC 
        Roots = ["BL"; "B"; "END"; "DCD"; "DCB"; "FILL"; "EQU"]
        Suffixes = [""] 
    }
 
    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand miscSpec

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) = 
            let computeExpression symTab s=
                match numOrReg s symTab with
                    |Ok (Num(Nms)) -> Ok (Nms|>toUint32)
                    |_ -> Error (sprintf "Error, %A is not a valid EQU argument" ls.Operands) 
            let getOperand = 
                match root with
                | "END" -> if ls.Operands = "" then Ok None else Error (sprintf "ERROR: Instruction END contains unexpected operand %A" ls.Operands)
                | "B" | "BL" -> expParse ls.SymTab ls.Operands |> Result.bind validAddr |> opConv ADDR // Check if Operand is a valid symbol in table and eval                                                                         
                | "FILL" -> expParse ls.SymTab ls.Operands |> Result.bind validAddr |> opConv ADDR //*****CHECK RIGHT TYPE AS INT *****************
                | "DCD" -> ls.Operands.Split(',')
                            |> List.ofArray 
                            |> validList (computeExpression ls.SymTab)  
                            |> opConv WVAL //Check Valid List of literals                                                                  
                | "DCB" -> ls.Operands.Split(',')
                            |> List.ofArray 
                            |> validList (expParse ls.SymTab) //Check Valid List of literals 
                            |> Result.bind (validList (checkRange (-128|>uint32) 255u)) //Check limited to 8 bits 
                            |> opConv WVAL                                          
                | "EQU" -> 
                    match numOrReg ls.Operands ls.SymTab with
                    |Ok (Num(Nms)) -> Ok (Some ((Nms|>toUint32)|>ADDR))
                    |_ -> Error (sprintf "Error, %A is not a valid EQU argument" ls.Operands)                                         
                |  _    -> Error (sprintf "ERROR: Uncaught pattern %A in Operand parse handling" ls.Operands) //Should never be run
            let checkLabels =
                match ls.Label with 
                | Some (_) -> Ok ls.Label  
                | None -> 
                    if List.contains root ["DCD" ; "DCB" ; "EQU"] 
                    then Error (sprintf "ERROR: Instruction %A does not contain a label" root)
                    else Ok None 
            let symbolR = checkLabels                           
            let operandR = getOperand
            match symbolR, operandR with
            |Ok symbol, Ok operand ->      
                Ok { 
                    PInstr={
                        // set records
                            OpRoot = root //Already checked
                            Symbol = symbol                           
                            Operand = operand
                            PrevAddr = ls.LoadAddr 
                               
                        };
                PLabel = None ; PSize = 4u; PCond = pCond //These are as given in v1.1 but appear different in v1.0
                }
            | Error e, _ -> Error e
            | _, Error e -> Error e 
            
            
            //Error message here?
        Map.tryFind ls.OpCode opCodes 
        |> Option.map parse' 


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse
    
    let createCpuData data regOut cpuData=
        let regValue reg = 
            if regOut = reg then data else cpuData.Regs.[reg]
        let reg = Map.ofList [ R0, regValue R0 ; R1, regValue R1 ; R2, regValue R2 ; R3, regValue R3 ; R4, regValue R4 ; R5, regValue R5;
            R6, regValue R6 ; R7, regValue R7 ; R8, regValue R8 ; R9, regValue R9 ; R10, regValue R10 ; R11, regValue R11 ; 
            R12, regValue R12 ; R13, regValue R13 ;R14, regValue R14 ; R15, regValue R15; 
            ]
        {cpuData with Regs = reg}

    let execute (instruction:Instr) (cpuData:CPUState) = 
        let getValue ins=
            match ins with
            |Some x -> 
                match x with
                |ADDR uint -> uint
                |_ -> failwith "not implemented yet urgh"
            |None -> failwith "not implemented yet urgh"
        let valu (ins:WAddr) = 
            match ins with 
            |WA x -> x
        match instruction.OpRoot with
        |"B" -> Ok {DPath = createCpuData (getValue instruction.Operand) R15 (getCpuData cpuData); MEMState = getCpuMem cpuData}
        | "BL" -> Ok {DPath = createCpuData (valu instruction.PrevAddr) R14 (createCpuData (getValue instruction.Operand) R15 (getCpuData cpuData)); MEMState = getCpuMem cpuData}
        | "END" -> Ok {DPath = createCpuData (1u) R15 (getCpuData cpuData); MEMState = getCpuMem cpuData}
        | "EQU" -> Error ((getValue instruction.Operand)|>string)
    /// Function to Execute instruction for Branch
    // let execute inst (prevAddr: WAddr option) newAddr dPath = 
    //     let addr = newAddr |> fun (Ok i) -> i |> uint32
                    
                     
    //     // let updatePCReg lst = lst |> Map.map (fun key value -> if key = R15 then newAddr else value)
    //     // let updateLRReg lst = lst |>  Map.map (fun key value -> if key = R14 then prevAddr else value)
    //     //Type issue could not resolve in time
    //     // let newReg = 
    //     //     match inst with
    //     //     | "B" -> dPath.Regs |> updatePCReg
    //     //     | "BL" -> dPath.Regs |> updateLRReg |> updatePCReg
          
    //     let newReg = dPath.Regs |> Map.map (fun key value -> if key = R15 then addr else value)
     
    //     let (CpuData: DataPath) = {
    //         Fl = dPath.Fl
    //         Regs = newReg
    //     } 
    //     Ok CpuData 