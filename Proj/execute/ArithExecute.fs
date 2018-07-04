module ArithExecute
    open CommonData
    open CommonLex
    open Data
    open Arith
    open FlexOp2

    //Get value of cpu data
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
    let simulateArith (instruction:Instr) (cpuData:DataPath) =
        createCpuData (math instruction cpuData) instruction.OutputReg cpuData //If Suffix set call instructions that set flags

    // Given a parsed instruction extract instruction and simulate used for testing only
    let parseError (parse: Result<Parse<Instr>,string> option) (cpuData:DataPath)=
        match parse with
        |Some instr->
            match instr with
            |Ok x->
                match x.PInstr with
                |y -> Ok (simulateArith y cpuData)
            |Error e-> Error e 
        |None -> Error "Instruction failed to parse"
