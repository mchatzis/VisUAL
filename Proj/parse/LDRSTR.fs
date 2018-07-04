module LDRSTR

    open CommonData
    open CommonLex
    open System.Text.RegularExpressions

/// ---------------SPEC SETUP-----------------

    /// Literal Type for offsets
    type Literal = {K: uint32; R: int}
    /// Shift Type for offsets
    type Shift =
        | LSL
        | ASR
        | LSR
        | ROR
    
    /// Memory Instruction Class
    type MemInstrClass = LDR | STR
    /// Memory Instruction Offset
    /// Can be a Literal, a Register, or a Shifted Register
    type MemInstrOffset =
        | N of Literal
        | R of RName
        | RSN of RName * Shift * Literal
        | RRX of RName
    type MemInstrPN =
        | PLUS
        | MINUS
    /// Memory Instruction Index
    /// PRE for update before, POST for update after
    type MemInstrIndex =
        | PRE
        | POST
        | NA

    /// Memory Instruction Attributes
    /// Self Explanatory
    type Instr =  {
        Class: MemInstrClass
        IsBytes: bool
        DstSrcReg: RName
        AddrReg: RName
        Offset: MemInstrPN * MemInstrOffset
        Index: MemInstrIndex
    }

    /// parse error (dummy, but will do)
    type ErrInstr = string

    /// Memory Instruction Specification
    let memSpec = {
        InstrC = MEM
        Roots = ["LDR";"STR"]
        Suffixes = [""; "B"]
    }


/// ---------------HELPER FUNCTIONS-----------------

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    /// Regex matcher that returns groups on success
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
    
    let litValue {K=k ; R=r} =
        let rotVal2 u r =
            let n = (r &&& 0xF)*2 //(keep n >= 0, % would not do this)
            (u % 256 >>> n) ||| (u % 256 <<< 32 - n)
        rotVal2 (k|>int) r

/// ---------------VALIDATE FUNCTIONS----------------- 

    /// Verify and forms Literal, otherwise return None
    let checkAndMakeLiteral (lit:uint32) =
        let rotate (k:uint32) n = (k >>> n) ||| (k <<< 32 - n),n
        [0..2..30] 
        |> List.map (rotate 0xFFu)
        |> List.tryFind (fun (mask,_) -> (mask &&& lit) = lit)
        |> Option.map (fun (_,n) -> { K=(rotate lit (32 - n)) |> fst; R=n/2})
    
    /// Verify and forms 12-bit Value, otherwise return None
    let checkAndMakeVal (value:string) =
       match value.Trim() with
       | Regex @"^([0-9]+)$" [VAL]
            -> VAL |> uint32 |> Ok
       | Regex @"^(0[xX][0-9a-fA-F]+)$" [VAL]
            -> VAL |> uint32 |> Ok
       | Regex @"^(0b[01]+)$" [VAL]
            -> VAL |> uint32 |> Ok
       | "" -> 0u |> Ok
       | _ -> Error "Not valid expression for #"

    /// Verify and forms 4-bit Shift Value, otherwise return None
    let checkAndMakeSVal (sval:uint32) =
        sval |> checkAndMakeLiteral

    /// Checks and makes plus/minus indicator
    let checkPN pn =
        match pn with
        | "-" -> MINUS
        | _ -> PLUS

    /// Checks if inputs result in valid Offset type, otherwise return Error
    let checkValidN (Ra, Rb, (pn, v), pre) =
        let checkVal = Result.map checkAndMakeLiteral v
        Result.bind (fun x ->
            match x with
            | Some l -> (Ra, Rb, (pn, l |> N), pre) |> Ok
            | _ -> Error "Literal is not valid"
        ) checkVal

    /// Checks if inputs result in valid Offset type, otherwise return Error
    let checkValidR (Ra, Rb, (pn, Rc), pre) =
        (Ra, Rb, (pn, Rc |> R), pre) |> Ok

    /// Checks if inputs result in valid Offset type, otherwise return Error
    let checkValidRSN (Ra, Rb, (pn, (Rc, SHFT, sval)), pre) =
        let matchShift =
            match SHFT with
            | "LSL" -> LSL |> Some
            | "ASR" -> ASR |> Some
            | "LSR" -> LSR |> Some
            | "ROR" -> ROR |> Some
            | _ -> None
        let checkSVal = Result.map checkAndMakeSVal sval
        Result.bind (fun x ->
            match SHFT, matchShift, x with
            | "RRX", _, _ -> (Ra, Rb, (pn, Rc |> RRX), pre) |> Ok
            | _, Some s, Some l -> (Ra, Rb, (pn, (Rc, s, l)|> RSN), pre) |> Ok
            | _, _, _ -> Error "Shift operands are incorrect"
        ) checkSVal

    /// Checks if parsed instruction is valid, otherwise return Error
    let checkValidParse x =
        match x with
        | Ok k -> Ok k
        | Error e -> Error e

/// ---------------PARSE FUNCTIONS-----------------

    /// Parses a string of operands, checking valid inputs by Regex matching
    /// Otherwise, returns an Error with specific information
    let parseMemOps (ops: string) =
        let preCheck = ops.EndsWith("!") |> (fun x -> if x then PRE else NA)
        match ops with
        | Regex @"(R[0-9]|R1[0-5]|PC|LR|SP) *, * \[ *(R[0-9]|R1[0-5]|PC|LR|SP)(,.*)?\]!?(.*)$" [Ra; Rb; Inner; Outer] ->
            match Inner.Trim(), Outer.Trim() with
            | Regex @"^(?!.)" [], Regex @"^(?!.)" []
                -> (regNames.[Ra], regNames.[Rb], (PLUS, "0" |> checkAndMakeVal), NA) |> checkValidN
            | Regex @"^, *#(\+|-)?(.+)$" [pn; VAL], Regex @"^(?!.)" []
                -> (regNames.[Ra], regNames.[Rb], (pn |> checkPN, VAL |> checkAndMakeVal), preCheck) |> checkValidN
            | Regex @"^, *(\+|-)?(R[0-9]|R1[0-5]|PC|LR|SP)$" [pn; Rc], Regex @"^(?!.)" []
                -> (regNames.[Ra], regNames.[Rb], (pn |> checkPN, regNames.[Rc]), preCheck) |> checkValidR
            | Regex @"^, *(\+|-)?(R[0-9]|R1[0-5]|PC|LR|SP) *, *([A-Z]{3}) *(?:#(.+))?$" [pn; Rc; SHFT; SVAL], Regex @"^(?!.)" []
                -> (regNames.[Ra], regNames.[Rb], (pn |> checkPN, (regNames.[Rc], SHFT, SVAL |> checkAndMakeVal)), preCheck) |> checkValidRSN
            | Regex @"^(?!.)" [], Regex @"^, *#(\+|-)?(.+)$" [pn; VAL]
                -> (regNames.[Ra], regNames.[Rb], (pn |> checkPN, VAL |> checkAndMakeVal), POST) |> checkValidN
            | Regex @"^(?!.)" [], Regex @"^, *(\+|-)?(R[0-9]|R1[0-5]|PC|LR|SP)$" [pn; Rc]
                -> (regNames.[Ra], regNames.[Rb], (pn |> checkPN, regNames.[Rc]), POST) |> checkValidR
            | Regex @"^(?!.)" [], Regex @"^, *(\+|-)?(R[0-9]|R1[0-5]|PC|LR|SP) *, *([A-Z]{3}) *(?:#(.+))?$" [pn; Rc; SHFT; SVAL]
                -> (regNames.[Ra], regNames.[Rb], (pn |> checkPN, (regNames.[Rc], SHFT, SVAL |> checkAndMakeVal)), POST) |> checkValidRSN
            | _ -> Error "Inner/Outer Operand format does not match"
        | _ -> Error "Operand format does not match"

    /// Parses memory instruction from root, suffix, and operands
    let parseMemInstr (root, suffix, ops) =
            let getClass =
                match root with
                | "LDR" -> LDR
                | "STR" -> STR
            let getIsBytes =
                match suffix with
                | "" -> false
                | "B" -> true
            parseMemOps ops
            |> Result.map (
                fun (DSR, AR, OFF, I) ->
                {
                    Class = getClass;
                    IsBytes = getIsBytes;
                    DstSrcReg = DSR;
                    AddrReg = AR;
                    Offset = OFF;
                    Index = I
                }) |> checkValidParse

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let parse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
            // this does the real work of parsing
            let buildInstr = 
                match instrC with
                | MEM -> parseMemInstr (root, suffix, ls.Operands)
                | _ -> Error "No match for Instruction Class"
                |> Result.map (
                    fun x ->
                    {
                        PInstr = x;
                        PLabel = None;
                        PSize = 4u;
                        PCond = pCond
                    })
            buildInstr
        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'

/// ---------------EXECUTE FUNCTIONS-----------------
    type CompleteData<'INS> = {Fl: Flags; Regs:Map<RName,uint32>; MM:MemoryArgument}

    /// Peform shift given, value, shift type, and value to shift by
    let basicFlexOp2 (mIO: MemInstrOffset) (cpuData: CompleteData<Instr>) : uint32 =
        match mIO with
        | N l -> (litValue l) |> uint32
        | R reg -> Map.find reg cpuData.Regs
        | RSN (reg, shift, sval) ->
            let regVal = Map.find reg cpuData.Regs
            let shiftVal = litValue sval
            match shift with
            | ASR | LSR -> (>>>) regVal shiftVal
            | LSL -> (<<<) regVal shiftVal
            | ROR -> (>>>) regVal shiftVal ||| (<<<) regVal (32-shiftVal)
        | RRX reg ->
            let regVal = Map.find reg cpuData.Regs
            (>>>) regVal 1 ||| if cpuData.Fl.C then (<<<) 1ul 30 else 0ul

    /// Main function to execute memory instruction
    let execMemInstr (instr: Instr) (cpuData: CompleteData<Instr>) =
        let exec cl ib dsr addr off idx =
            let pn, offVal = off |> (function| pn, o -> pn, basicFlexOp2 o cpuData)
            let bytesMask = ib |> (function| true -> 0xFFu | false -> 0xFFFFFFFFu)
            let newAddrVal =
                cpuData.Regs.TryFind addr
                |> Option.map (fun x ->
                    match pn with
                    | PLUS -> x + offVal
                    | MINUS -> x - offVal
                )
            let preAddrVal = 
                match idx with
                | POST -> cpuData.Regs.TryFind addr
                | _ -> newAddrVal
            let processDat x y =
                match cl with
                | LDR ->
                    match cpuData.MM.TryFind (WA x) with
                    | Some (DataMem dat) ->
                        {cpuData with Regs = cpuData.Regs.Add(dsr, (dat &&& bytesMask));}
                        |> Ok
                    | _ -> Error "Memory address is not valid"
                | STR ->
                    match cpuData.Regs.TryFind dsr with
                    | Some dat -> 
                        {cpuData with MM = cpuData.MM.Add ((WA x), DataMem (dat &&& bytesMask));}
                        |> Ok
                    | _ -> Error "Memory address is not valid"
                |> Result.map (fun x ->
                    match idx with
                    | NA -> x
                    | _ -> {x with Regs = x.Regs.Add(addr, y);})
            match preAddrVal, newAddrVal with
            | Some x, Some y when (x%4u)<>0u || (y%4u)<>0u ->
                if not ib then Error "Invalid Byte Addressing" else processDat x y
            | Some x, Some y -> processDat x y    
            | _, _ -> Error "Register not found"

        let cl, ib, dsr, addr, off, idx =
            instr.Class, instr.IsBytes, instr.DstSrcReg, instr.AddrReg, instr.Offset, instr.Index
        exec cl ib dsr addr off idx

    let parseThenExec (ld, cpuData) =
        parse ld |> Option.map(fun x ->
            match x with
            | Ok k -> execMemInstr k.PInstr cpuData
            | Error e -> Error e
        )
    
    let execute (instr: Instr) (cpu: CPUState) =
        let cpuData = {Fl=cpu.DPath.Fl; Regs=cpu.DPath.Regs; MM=cpu.MEMState}
        match execMemInstr instr cpuData with
        | Ok x -> Ok {MEMState = x.MM; DPath = {Fl=x.Fl; Regs=x.Regs}}
        | Error e -> Error e
    
    let (|IMatch|_|)  = parse