module CompExecute
    open CommonData
    open CommonLex
    open Data
    open Comp
    open FlexOp2
    open ArithExecute

    //Get Rname with value from a Result Monad
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
    let simulateComp (instruction:Instr) (cpuData:DataPath) =
        createCpuData (math instruction cpuData) cpuData //If Suffix set call instructions that set flags

    // Given a parsed instruction extract instruction and simulate used for testing only
    let parseError (parse: Result<Parse<Instr>,string> option) (cpuData:DataPath)=
        match parse with
        |Some instr->
            match instr with
            |Ok x->
                match x.PInstr with
                |y -> Ok (simulateComp y cpuData)
            |Error e-> Error e 
        |None -> Error "Instruction failed to parse"
