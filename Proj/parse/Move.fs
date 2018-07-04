module Move

    //------------------MODULE REFERENCES--------------//
    open System
    open CommonLex
    open CommonData
    open Data
    open FlexOp2


    //-----------------------TYPES--------------------//
    //Defined for readability relevent to class and Instr types
    type doNOT = |Not |NoNot
    type SuffixT = WriteFlags | NoWriteFlags
    type Operation = | MOV | TST | TEQ

    //operation type
    type CLASS = {
            Op : Operation 
            Not: doNOT
        }
    ///instruction 
    type Instr =  {
        Class: CLASS
        Suffix: SuffixT
        //condition?
        Dest: RName
        In2: OpTwo
    }


    /// parse error (dummy, but will do)
    type ErrInstr = string

    /// Property on RName to return register number, for convenience
    /// Aliasses not included, since they are not RNames
    type RName with
        /// Return the number of a register as an integer
        member r.RegNum = regNums.[r]
        
    /// Return a register name from an integer
    let register n = if 0 <= n && n < 16 
                        then inverseRegNums.[n] 
                        else (failwithf "Register %d does not exist!" n)

    //-----------------instruction specifications--------------------//

    ///logical bitwise operations
    ///move and test operations similar formats for parsing 
    let mooAndTestSpec = {
        InstrC = MOO
        Roots = ["MOV"; "MVN"; "TST"; "TEQ"]
        Suffixes = ["" ;"S" ] 
    }
    ///mapping for move instructions so far, easier to add more items to later
    let opLookup = ["MOV", MOV; "MVN", MOV; "TST", TST; "TEQ", TEQ;] |> Map.ofList

    ///perform not operation before move?
    let notLookup = ["MOV", NoNot; "MVN", Not; "TST", NoNot; "TEQ", NoNot;] |> Map.ofList

    ///suffix lookup
    let suffixLookup = ["S", WriteFlags; "", NoWriteFlags] |> Map.ofList 

    ///Op's with an op1
    let hasOp1 = ["MOV", false; "MVN", false; "TST", false; "TEQ", false; "EOR", true; "AND", true; "ORR", true; "BIC", true ] |> Map.ofList


    /// map of possible opcodes recognised
    let opCodes = opCodeExpand mooAndTestSpec
    ///lookup function for PIinstr

    ///split the list up into their seperate components
    let splitOps (operands:string) (character:char) = operands.Split [|character|] 

    ///length of opList for parsing

    ///remove characters in a string and put it back together
    let strip (stripChars:string) (text:string) =
        text.Split(stripChars.ToCharArray(), StringSplitOptions.RemoveEmptyEntries) |> String.Concat

    /// Map converts RName into register number (no aliasses)
    let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings
        
    /// Map converts register number into RName (no aliasses)
    let inverseRegNums = 
        regNums 
        |> Map.toList 
        |> List.map (fun (rn,n)->(n,rn)) 
        |> Map.ofList


        






    ///
    // ---------------------------PARSE FUNCTION--------------------//

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    let parse (ls: LineData) : Result<Parse<Instr>,string> option  =
        let parse' (instrC, (root,suffix,pCond)) =
            //get split up Operands into a string list
            let opList = (splitOps ls.Operands ',') 
            // address this instruction is loaded into memory
            let (WA la) = ls.LoadAddr 
            //Get op1 if it is there
            let in2R= 
                match opList.Length with
                | 3-> //With shift
                    //Get register
                    let reg = 
                        match listLookup regNames opList.[1] with //Get register being shifted
                        |Ok x ->if x = R15 then None else Some x
                        |Error (_)-> None
                    let oplistPlace = opList.[2]
                    match reg with //All Shift instructions 3 long so isolate first 3 characters of instruction
                    |Some x -> makeShift x oplistPlace.[0..2] oplistPlace.[3..(oplistPlace.Length - 1)] ls.SymTab
                    |None -> Error (sprintf "%A is not a valid register" reg)
                | 2-> //WithOut Shift
                    match numOrReg opList.[1] ls.SymTab with
                    |Ok (Num(nms)) -> Ok (nms|>Immediate)
                    |Ok (Reg(rg))-> Ok(rg|>Rm)
                    |Error e -> Error e
                | _ -> Error "Not a valid Flexible Operand 2 instruction"


            let opR = listLookup opLookup root
            let notR = listLookup notLookup root
            let suffixR = listLookup suffixLookup suffix //check set func
            let destR = listLookup regNames (opList.[0] |> strip " " )  //output reg func
            match opR, notR, suffixR, destR, in2R with
            | Ok op, Ok not, Ok suf, Ok dest, Ok in2 ->
                Ok { 
                    // Normal (non-error) return from result monad
                    // This is the instruction determined from opcode, suffix and parsing
                    // the operands. Not done in the sample.
                    // Note the record type returned must be written by the module author.
                    PInstr = {
                               
                                Class = { //implement find class func later
                                            Op = op
                                            Not = not
                                }
                                Suffix = suf //check set func
                                Dest = dest  //output reg func
                                In2 = in2
                    }; 


                    // This is normally the line label as contained in
                    // ls together with the label's value which is normally
                    // ls.LoadAddr. Some type conversion is needed since the
                    // label value is a number and not necessarily a word address
                    // it does not have to be div by 4, though it usually is
                    PLabel = ls.Label |> Option.map (fun lab -> lab, la); 


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
                | Error e, _, _, _, _ -> Error e
                | _, Error e, _, _, _ -> Error e
                | _, _, Error e, _, _ -> Error e
                | _, _, _, Error e, _ -> Error e
                | _, _, _, _, Error e -> Error e
     
        Map.tryFind ls.OpCode opCodes  // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse

    ///EXECUTE

    let createCpuData data regOut cpuData=
        let regValue reg = 
            if regOut = reg then fst data else cpuData.Regs.[reg]
        let reg = Map.ofList [ R0, regValue R0 ; R1, regValue R1 ; R2, regValue R2 ; R3, regValue R3 ; R4, regValue R4 ; R5, regValue R5;
            R6, regValue R6 ; R7, regValue R7 ; R8, regValue R8 ; R9, regValue R9 ; R10, regValue R10 ; R11, regValue R11 ; 
            R12, regValue R12 ; R13, regValue R13 ;R14, regValue R14 ; R15, (regValue R15) + 4u ; 
            ]
        {cpuData with Fl = snd data; Regs = reg}
    let createCpuData2 data cpuData=
        let regValue reg = cpuData.Regs.[reg]
        let reg = Map.ofList [ R0, regValue R0 ; R1, regValue R1 ; R2, regValue R2 ; R3, regValue R3 ; R4, regValue R4 ; R5, regValue R5;
            R6, regValue R6 ; R7, regValue R7 ; R8, regValue R8 ; R9, regValue R9 ; R10, regValue R10 ; R11, regValue R11 ; 
            R12, regValue R12 ; R13, regValue R13 ;R14, regValue R14 ; R15, regValue R15 + 4u ; 
            ]
        {cpuData with Fl = snd data; Regs = reg}
    let execute (instruction:Instr) (cpuData) = 
        //get unint32's from registers 
        let data = (getCpuData cpuData)
        let op2' = flexOp2 instruction.In2 data 
        let out = match instruction.Class with 
                    | {Op = MOV; Not = NoNot} -> op2'
                    | {Op = MOV; Not = Not} -> ~~~ op2'
                    | {Op = TST; Not = _} -> (getCpuRegValue instruction.Dest data) &&& op2'
                    | {Op = TEQ; Not = _} -> (getCpuRegValue instruction.Dest data) ^^^ op2'

        let outN = checkLess0 out
        let outZ = checkFor0 out
        let outC = 
            match workOutCarry instruction.In2 data with
            |1u-> true
            |0u-> false
            |_ -> failwithf "Should never occur as workoutCarry only outputs 1u and 0u"


        let newFlags = match instruction.Suffix with 
                        | WriteFlags ->  {data.Fl with N = outN; Z = outZ; C = outC}
                        | NoWriteFlags -> data.Fl
        let dataPath = 
            match instruction.Class with
            |{Op = MOV; Not = _} -> createCpuData (out,newFlags) instruction.Dest data;
            |_ -> createCpuData2 (out,newFlags) data

        Ok {
            DPath = dataPath;
            MEMState = getCpuMem cpuData 
        }