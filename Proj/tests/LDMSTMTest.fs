    
    module TestLDMSTM
    open CommonLex
    open CommonData
    open LDMSTM
    open System
    open EEExtensions
    
    //IMPORTANT!!!!!!!!!!!!!!!!
    //This file used to be fully functional. It has been updated from individual stage to
    //contain more tests. HOWEVER, an interface in CommonTop had to change and this file
    //does not comply with the interface anymore. However, its job has been done, as it has 
    //tested the LDMSTM module to all cornercases. The LDMSTM did not change since then so it
    //is logical to expect it to still work. To conclude, don't include it in project since it
    //is here simple to demonstrate that the testing has been done. 


(*
     ------------------------------------------- A FEW COMENTS ON THIS TESTING FILE----------------------------------
     Testing is done on a verbose way. Parsing and Execution functions are tested seperately. Parsing is kind of neat,
     in the sense that a top level function checks if actual = expected, where expected comes from user specification.
     Execution testing is more boilerplate code. What it does is the following. User defines memory, registers and 
     how the change after the instruction manually for each individual test. The tests have been numbered using comments
     and are all based on the following scenario:
    
     Memory: 0x0 - 0xFCU -> Reserved for Code
             0x100 - 0x200 -> Holds uint32 equal to its address
     
     Instruction: Pointer used: R13
                  Register List used: R4, R5
     
     What changes is the SMT/LDM and The Suffixes (EA,ED,FA,FD)
     Also, errors like CannotAccessCodeMem or WANotDivBy4 are tested

     1. Parsing
     Any valid syntax instruction can be successfully parsed (has been tested)
     Invalid syntax instruction can be detected and given the appropriate error return
     However, not all invalid syntax opcodes have been tested. 
     The invalid syntax instructions that have been tested are the ones that might confuse the string regular expression match 
     and the tokenize functions. Since they were tested with unit tests the are limited to my imagination of 
     possible errors (having coded myself the parsed function I could imagine cases in which it would fail) 
     but this of course does not guarantee obscure bugs which can be found by randomizing input
    2. Execution
     All possible variations of LDM/STM and EA,ED,FA,FD have been tested
     Errors: CannotAccessCodeMem and WANotDivBy4 have been tested
     Randomized instructions which can reveal obscure errors I cannot imagine have not being used in testing
    *)

    
    ///A dummy instruction
    let dummy= {OperationalCode = LDM,EA; StackPointer= R1,false; OpRegs = []}


    let expectedResult error mems (data:DataPath) point= 
        match error with
        |Some e -> e |> Error
        |None -> (mems,data) |> Ok
    
    //INITIALIZING - Create memory map and dataPath
    let joinMaps (first:Map<'a,'b>) (second:Map<'a,'b>) = 
        Map(Seq.concat [ (Map.toSeq first) ; (Map.toSeq second) ])
    //let WA(uint32) contain the address location of the lowest address byte in the word
    let mems:MachineMemory<Instr> = 
            let codeMap =
                [0u..0x4u..0xFCu]
                |> List.map (fun a -> WA(a) , Code(dummy))
                |> Map.ofList
        
            let dataMap = 
                [0x100u..0x4u..0x200u]
                |> List.map (fun a -> WA(a),DataLoc(a))
                |> Map.ofList
        
            joinMaps codeMap dataMap
    let regs = 
            Map.ofList [ 
                R0,0u ; R1,1u ; R2,2u ; R3,3u ; R4,4u ; R5,5u
                R6,6u ; R7,7u ; R8,8u ; R9,9u ; R10,10u ; R11,11u ; 
                R12,12u ; R13,0x100u ; R14,14u ; R15,8u; 
                ]     
    let dataPath:DataPath=
            {Fl={N=false;C=false;Z=false;V=false};Regs=regs}

    
    //SPECIFY MEMS AND REGISTERS before and after each corresponding instruction for each test!

    //1
    //Correct and Valid instruction, No Update
    let testInstrEA = {OperationalCode = STM,EA; StackPointer= R13,false; OpRegs = [R4;R5]}
    let memsAfter1 = 
            let mems' = mems.Add(WA(0x100u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x104u),DataLoc(5u))
            mems''
    let regsAfter1 = 
        regs
    let dataPathAfter1 = 
        dataPath

    //2
    //Correct and Valid instruction, Update
    let testInstrEA' = {OperationalCode = STM,EA; StackPointer= R13,true; OpRegs = [R4;R5]}
    let memsAfter2 = 
            let mems' = mems.Add(WA(0x100u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x104u),DataLoc(5u))
            mems''
    let regsAfter2 = 
        regs.Add(R13,0x108u)
    let dataPathAfter2 = 
        {dataPath with Regs = regsAfter2}

    //3
    //Correct but Invalid Instruction, Accessing Code Memory
    let testInstrAccessError = {OperationalCode = STM,EA; StackPointer= R4,true; OpRegs = [R4;R5]}
    
    //4
    //Correct but Invalid Instruction, Accessing Non-Valid Word Address
    let testInstrNotDivError = {OperationalCode = STM,EA; StackPointer= R1,true; OpRegs = [R4;R5]}


    //Checking Variations of Suffixes...

    //5
    //FD Suffix - Correct and Valid instruction, No Update, 
    let testInstrFD = {OperationalCode = STM,FD; StackPointer= R13,false; OpRegs = [R4;R5]}
    let memsAfter5 = 
            let mems' = mems.Add(WA(0x100u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x104u),DataLoc(5u))
            mems''
    let regsBefore5 = regs.Add(R13,0x108u)
    let regsAfter5 = 
        regsBefore5
    let dataPathBefore5 = 
        {dataPath with Regs = regsBefore5}
    let dataPathAfter5 = dataPathBefore5

    //5B
    //FD Suffix - Correct and Valid instruction, Update 
    let testInstrFD' = {OperationalCode = STM,FD; StackPointer= R13,true; OpRegs = [R4;R5]}
    let memsAfter5B = 
            let mems' = mems.Add(WA(0x100u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x104u),DataLoc(5u))
            mems''
    let regsBefore5B = regs.Add(R13,0x108u)
    let regsAfter5B = 
        regsBefore5B.Add(R13,0x100u)
    let dataPathBefore5B = 
        {dataPath with Regs = regsBefore5B}
    let dataPathAfter5B = 
        {dataPath with Regs = regsAfter5B}

    //6
    //FA Suffix - Correct and Valid instruction, No Update, 
    let testInstrFA = {OperationalCode = STM,FA; StackPointer= R13,false; OpRegs = [R4;R5]}
    let memsAfter6 = 
            let mems' = mems.Add(WA(0x104u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x108u),DataLoc(5u))
            mems''
    let regsBefore6 = regs
    let regsAfter6 = 
        regsBefore6
    let dataPathBefore6 = 
        {dataPath with Regs = regsBefore6}
    let dataPathAfter6 = dataPathBefore6

    //7
    //FA Suffix - Correct and Valid instruction, Update asserted 
    let testInstrFA' = {OperationalCode = STM,FA; StackPointer= R13,true; OpRegs = [R4;R5]}
    let memsAfter7 = 
            let mems' = mems.Add(WA(0x104u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x108u),DataLoc(5u))
            mems''
    let regsBefore7 = regs
    let regsAfter7 = 
        regsBefore7.Add(R13,0x108u)
    let dataPathBefore7 = 
        {dataPath with Regs = regsBefore7}
    let dataPathAfter7 = 
        {dataPath with Regs = regsAfter7}

    //8
    //ED Suffix - Correct and Valid instruction, No Update 
    let testInstrED = {OperationalCode = STM,ED; StackPointer= R13,false; OpRegs = [R4;R5]}
    let memsAfter8 = 
            let mems' = mems.Add(WA(0x104u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x108u),DataLoc(5u))
            mems''
    let regsBefore8 = regs.Add(R13,0x108u)
    let regsAfter8 = 
        regsBefore8
    let dataPathBefore8 = 
        {dataPath with Regs = regsBefore8}
    let dataPathAfter8 = dataPathBefore8

    //9
    //ED Suffix - Correct and Valid instruction, Update asserted
    let testInstrED' = {OperationalCode = STM,ED; StackPointer= R13,true; OpRegs = [R4;R5]}
    let memsAfter9 = 
            let mems' = mems.Add(WA(0x104u),DataLoc(4u))
            let mems'' = mems'.Add(WA(0x108u),DataLoc(5u))
            mems''
    let regsBefore9 = regs.Add(R13,0x108u)
    let regsAfter9 = 
        regsBefore9.Add(R13,0x100u)
    let dataPathBefore9 = 
        {dataPath with Regs = regsBefore9}
    let dataPathAfter9 = 
        {dataPath with Regs = regsAfter9}

    //STM done, now going to check LDM

    //10
    //EA - Correct and Valid instruction, No Update
    let testInstrEA_LDM = {OperationalCode = LDM,EA; StackPointer= R13,false; OpRegs = [R4;R5]}
    let regsBefore10 = regs.Add(R13,0x108u)
    let regsAfter10 = 
        let map' = regsBefore10.Add(R5,0x104u)
        map'.Add(R4,0x100u)
    let dataPathBefore10 = 
        {dataPath with Regs = regsBefore10}
    let dataPathAfter10 = 
        {dataPath with Regs = regsAfter10}

    
    //11
    //ΕΑ - Correct and Valid instruction, Update
    let testInstrEA_LDM' = {OperationalCode = LDM,EA; StackPointer= R13,true; OpRegs = [R4;R5]}
    let regsBefore11 = regs.Add(R13,0x108u)
    let regsAfter11 = 
        let map' = regsBefore10.Add(R5,0x104u)
        let map'' = map'.Add(R13,0x100u)
        map''.Add(R4,0x100u)
    let dataPathBefore11 = 
        {dataPath with Regs = regsBefore11}
    let dataPathAfter11 = 
        {dataPath with Regs = regsAfter11}

    //12
    //Correct but Invalid Instruction, Accessing Code Memory
    let testInstrAccessError_LDM = {OperationalCode = LDM,EA; StackPointer= R4,true; OpRegs = [R4;R5]}
    
    //13
    //Correct but Invalid Instruction, Accessing Non-Valid Word Address
    let testInstrNotDivError_LDM = {OperationalCode = LDM,EA; StackPointer= R1,true; OpRegs = [R4;R5]}


    //Checking Variations of Suffixes...

    //14
    //FD Suffix - Correct and Valid instruction, No Update, 
    let testInstrFD_LDM = {OperationalCode = LDM,FD; StackPointer= R13,false; OpRegs = [R4;R5]}
    let regsBefore14 = regs.Add(R13,0x100u)
    let regsAfter14 = 
        let map' = regsBefore14.Add(R5,0x104u)
        map'.Add(R4,0x100u)
    let dataPathBefore14 = 
        {dataPath with Regs = regsBefore14}
    let dataPathAfter14 = 
        {dataPath with Regs = regsAfter14}
    
    //15
    //FD Suffix - Correct and Valid instruction, Update 
    let testInstrFD_LDM' = {OperationalCode = LDM,FD; StackPointer= R13,true; OpRegs = [R4;R5]}
    let regsBefore15 = regs.Add(R13,0x100u)
    let regsAfter15 = 
        let map' = regsBefore15.Add(R5,0x104u)
        let map'' = map'.Add(R13,0x108u)
        map''.Add(R4,0x100u)
    let dataPathBefore15 = 
        {dataPath with Regs = regsBefore15}
    let dataPathAfter15 = 
        {dataPath with Regs = regsAfter15}

    //16
    //FA Suffix - Correct and Valid instruction, No Update, 
    let testInstrFA_LDM = {OperationalCode = LDM,FA; StackPointer= R13,false; OpRegs = [R4;R5]}
    let regsBefore16 = regs.Add(R13,0x108u)
    let regsAfter16 = 
        let map' = regsBefore16.Add(R5,0x108u)
        map'.Add(R4,0x104u)
    let dataPathBefore16 = 
        {dataPath with Regs = regsBefore16}
    let dataPathAfter16 = 
        {dataPath with Regs = regsAfter16}

    //17
    //FA Suffix - Correct and Valid instruction, Update asserted 
    let testInstrFA_LDM' = {OperationalCode = LDM,FA; StackPointer= R13,true; OpRegs = [R4;R5]}
    let regsBefore17 = regs.Add(R13,0x108u)
    let regsAfter17 = 
       let map' = regsBefore17.Add(R5,0x108u)
       let map'' = map'.Add(R13,0x100u)
       map''.Add(R4,0x104u)
    let dataPathBefore17 = 
        {dataPath with Regs = regsBefore17}
    let dataPathAfter17 = 
        {dataPath with Regs = regsAfter17}
    //18
    //ED Suffix - Correct and Valid instruction, No Update 
    let testInstrED_LDM = {OperationalCode = LDM,ED; StackPointer= R13,false; OpRegs = [R4;R5]}
    let regsBefore18 = regs.Add(R13,0x100u)
    let regsAfter18 = 
       let map' = regsBefore18.Add(R5,0x108u)
       map'.Add(R4,0x104u)
    let dataPathBefore18 = 
        {dataPath with Regs = regsBefore18}
    let dataPathAfter18 = 
        {dataPath with Regs = regsAfter18}

    //19
    //ED Suffix - Correct and Valid instruction, Update asserted
    let testInstrED_LDM' = {OperationalCode = LDM,ED; StackPointer= R13,true; OpRegs = [R4;R5]}
    let regsBefore19 = regs.Add(R13,0x100u)
    let regsAfter19 = 
       let map' = regsBefore19.Add(R5,0x108u)
       let map'' = map'.Add(R13,0x108u)
       map''.Add(R4,0x104u)
    let dataPathBefore19 = 
        {dataPath with Regs = regsBefore19}
    let dataPathAfter19 = 
        {dataPath with Regs = regsAfter19}


    ///Create list of Input into execute function, tupled with: expected result of the function
        
    let executionExpectedOutput =
        [
        //1
        (mems,dataPath,testInstrEA),expectedResult None memsAfter1 dataPathAfter1 R13;
        //2
        (mems,dataPath,testInstrEA'),expectedResult None memsAfter2 dataPathAfter2 R13;
        //3
         (mems,dataPath,testInstrAccessError),expectedResult (Some (CannotAccessCodeMem|> ExecutionErr)) mems dataPath R13;
        //4
         (mems,dataPath,testInstrNotDivError),expectedResult (Some (WANotDivBy4|> ExecutionErr)) mems dataPath R13;
        //5
        (mems,dataPathBefore5,testInstrFD),expectedResult (None) memsAfter5 dataPathAfter5 R13;
        //5B
        (mems,dataPathBefore5B,testInstrFD'),expectedResult (None) memsAfter5B dataPathAfter5B R13;
        //6
        (mems,dataPathBefore6,testInstrFA),expectedResult (None) memsAfter6 dataPathAfter6 R13;
        //7
        (mems,dataPathBefore7,testInstrFA'),expectedResult (None) memsAfter7 dataPathAfter7 R13;
        //8
        (mems,dataPathBefore8,testInstrED),expectedResult (None) memsAfter8 dataPathAfter8 R13;
        //9
        (mems,dataPathBefore9,testInstrED'),expectedResult (None) memsAfter9 dataPathAfter9 R13;
        //10
        (mems,dataPathBefore10,testInstrEA_LDM),expectedResult (None) mems dataPathAfter10 R13;
        //11
        (mems,dataPathBefore11,testInstrEA_LDM'),expectedResult None mems dataPathAfter11 R13;
        //12
         (mems,dataPath,testInstrAccessError_LDM),expectedResult (Some (CannotAccessCodeMem|> ExecutionErr)) mems dataPath R13;
        //13
         (mems,dataPath,testInstrNotDivError_LDM),expectedResult (Some (WANotDivBy4|> ExecutionErr)) mems dataPath R13;
        //14
        (mems,dataPathBefore14,testInstrFD_LDM),expectedResult (None) mems dataPathAfter14 R13;
        //15
        (mems,dataPathBefore15,testInstrFD_LDM'),expectedResult (None) mems dataPathAfter15 R13;
        //16
        (mems,dataPathBefore16,testInstrFA_LDM),expectedResult (None) mems dataPathAfter16 R13;
        //17
        (mems,dataPathBefore17,testInstrFA_LDM'),expectedResult (None) mems dataPathAfter17 R13;
        //18
        (mems,dataPathBefore18,testInstrED_LDM),expectedResult (None) mems dataPathAfter18 R13;
        //19
        (mems,dataPathBefore19,testInstrED_LDM'),expectedResult (None) mems dataPathAfter19 R13;
        ]
    
    let testExecutionTopFunction testList =
        
        let testFun ((mems',dataPath',testInstr),expected) =
            let actualOutput = 
                (execute dataPath' mems' testInstr) 
            not (actualOutput = expected) 
        
        let failedTests =
            testList
            |> List.filter testFun
        
        printfn "%A out of %A tests failed..." failedTests.Length testList.Length
        printfn "\n The tests that failed are the following %A" failedTests

        ()

    //TEST PARSING

    ///Creates user defined expected output after parsing
    let expectedParsedInstr error root suffix point update list =
        match error with
        |Some e -> e |> Error
        |None -> {OperationalCode = (root,suffix) ; StackPointer = point,update ; OpRegs = list} |> Ok

    ///Creates list of string to be inputed into parse function and the expected output
    let listCornerCases = 
        [
        //Basic simple correct STM 
        ("STMEAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM EA R13 true [R1;R2;R3;R4;R5];

        //Basic simple correct STM instruction
        ("LDMEAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None LDM EA R13 true [R1;R2;R3;R4;R5];

        //Opcode wrong 1
        ("STREAEQ","R13!,{R1-R5,R2}"),expectedParsedInstr (Some(OpCodeErr)) STM EA R13 true [R1;R2;R3;R4;R5];

        //Opcode wrong 2
        ("STMEFEQ","R13!,{R1-R5,R2}"),expectedParsedInstr (Some(OpCodeErr)) STM EA R13 true [R1;R2;R3;R4;R5];

        //Opcode wrong 3
        ("STMEA23","R13!,{R1-R5,R2}"),expectedParsedInstr (Some(OpCodeErr)) STM EA R13 true [R1;R2;R3;R4;R5];

        //Opcode wrong 4 (changing order of root <-> suffix
        ("EASTMEQ","R13!,{R1-R5,R2}"),expectedParsedInstr (Some(OpCodeErr)) STM EA R13 true [R1;R2;R3;R4;R5];

        //Opcode wrong 5 (changing order of everything
        ("EQSTMEA","R13!,{R1-R5,R2}"),expectedParsedInstr (Some(OpCodeErr)) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 1
        ("STMEAEQ","T13!,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidReg))) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 2
        ("STMEAEQ","R25!,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidReg))) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 3 - Pointer, cannot be R15/PC
        ("STMEAEQ","R15!,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidReg))) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 4 - Pointer, cannot be R15/PC
        ("STMEAEQ","PC!,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidReg))) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 5
        ("STMEAEQ","{R13!,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidReg))) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 6
        ("STMEAEQ","R!3,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidReg))) STM EA R13 true [R1;R2;R3;R4;R5];

        //First Operand Wrong 7
        ("STMEAEQ","!R13,{R1-R5,R2}"),expectedParsedInstr (Some(FstOperErr(InvalidFormat))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 1
        ("STMEAEQ","R13!,R1{-R5,R2}"),expectedParsedInstr (Some(SndOperErr(InvalidBrackets))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 2
        ("STMEAEQ","R13!,{R1-R5},R2"),expectedParsedInstr (Some(SndOperErr(InvalidBrackets))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 3
        ("STMEAEQ","R13!,{R1,-,R5,R2}"),expectedParsedInstr (Some(SndOperErr(InvalidListFormat))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 4
        ("STMEAEQ","R13!,{R1-R5,R2,}"),expectedParsedInstr (Some(SndOperErr(InvalidListFormat))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 5
        ("STMEAEQ","R13!,{R111-R5,R2}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 6 - {} cannot contain SP
        ("STMEAEQ","R13!,{R1-R5,R13}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 6 - {} cannot contain SP (LDM variation)
        ("LDMEAEQ","R13!,{R1-R5,R13}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) LDM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 7 - STM must not contain PC in {}
        ("STMEAEQ","R13!,{R1-R5,PC}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 8 - STM must not contain PC in {}
        ("STMEAEQ","R13!,{R1-R5,R15}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) STM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 9 - LDM must NOT contain PC IF it contains LR
        ("LDMEAEQ","R13!,{R1-R5,R14,R15}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) LDM EA R13 true [R1;R2;R3;R4;R5];

        //Second Operand Wrong 9 - LDM CAN contain PC if it does not contain LR
        ("LDMEAEQ","R13!,{R1-R5,R15}"),expectedParsedInstr None LDM EA R13 true [R1;R2;R3;R4;R5;R15];

        //Second Operand Wrong 10 - {} must not contain Pointer register IF WRITEBACK SUFFIX IS ASSERTED
        ("STMEAEQ","R1!,{R1-R5,R2}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) STM EA R13 true [R1;R2;R3;R4;R5];

        //CORRECT - {} can contain pointer because writeback suffix is NOT ASSERTED
        ("STMEAEQ","R1,{R1-R5,R2}"),expectedParsedInstr None STM EA R1 false [R1;R2;R3;R4;R5];

        //Second Operand Wrong 10 - {} must not contain Pointer register IF WRITEBACK SUFFIX IS ASSERTED -(LDM variation)
        ("LDMEAEQ","R1!,{R1-R5,R2}"),expectedParsedInstr (Some(SndOperErr(InvalidRegister))) STM EA R13 true [R1;R2;R3;R4;R5];

        //CORRECT - {} can contain pointer because writeback suffix is NOT ASSERTED -(LDM Variation)
        ("LDMEAEQ","R1,{R1-R5,R2}"),expectedParsedInstr None LDM EA R1 false [R1;R2;R3;R4;R5];

        //Correct Instructions with different Opcodes - 1
        ("STMEDEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM ED R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with different Opcodes - 2
        ("STMFAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM FA R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with different Opcodes - 3
        ("STMFDEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM FD R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 1
        ("STMIBEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM FA  R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 2
        ("STMIAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM EA R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 3
        ("STMDBEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM FD R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 4
        ("STMDAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None STM ED R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 1
        ("LDMIBEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None LDM ED  R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 2
        ("LDMIAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None LDM FD R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 3
        ("LDMDBEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None LDM EA R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with suffix aliases - 4
        ("LDMDAEQ","R13!,{R1,R2-R5}"),expectedParsedInstr None LDM FA R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with register aliases - 1
        ("STMDAEQ","SP!,{R1,R2-R5}"),expectedParsedInstr None STM ED R13 true [R1;R2;R3;R4;R5];

        //Correct Instructions with register aliases - 2
        ("STMDAEQ","LR!,{R1,R2-R5}"),expectedParsedInstr None STM ED R14 true [R1;R2;R3;R4;R5];
        
        //Correct Instructions with register aliases - 3
        ("STMDAEQ","R13!,{LR,R2-R5}"),expectedParsedInstr None STM ED R13 true [R2;R3;R4;R5;R14];

        //Correct Instruction with many registers in register list
        ("STMDAEQ","R13!,{R1-R2,R3,R2,R3-R4,R5,R6-R8,LR,R10}"),expectedParsedInstr None STM ED R13 true [R1;R2;R3;R4;R5;R6;R7;R8;R10;R14];
        ]

    
    let initializeLineData opcode operands=
            {LoadAddr= WA(4u); Label= None; SymTab=None; OpCode= opcode; Operands=operands}

     
    let testParseTopFunction testList =

        let testFun ((opcode,operand),expected) =
            let actualOutput = 
                match parse (initializeLineData opcode operand) with
                |Some a -> match a with 
                            |Ok parsed -> parsed.PInstr |> Ok
                            |Error e -> e |> Error
                |None ->  OpCodeErr |> Error 
            not (actualOutput = expected) 
        
        let failedTests =
            testList
            |> List.filter testFun
        
        printfn "%A out of %A tests failed..." failedTests.Length testList.Length
        printfn "\n The tests that failed are the following %A" failedTests
