namespace VisualTest

/// top-level code demonstrating how to run tests

open Comp
module ArithTests =

    open Expecto
    open CommonData
    open CommonLex
    open Arith
    open FlexOp2
    open Data
    open VCommon
    open Visual

    /// parameters setting up the testing framework
    /// WARNING: PostludeLength must be changed if Postlude is changed
    /// WARNING: global cache (CacheFileName) must be deleted if Postlude is changed
    /// Postlude can contain instructions to move CPU state (flags, memory locations) into rgeisters
    /// standard Postlude moves flags into R1
    /// Simulation reads VisUAL log and returns registers after test assembly code, and also after postlude
    let defaultParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"Proj\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"Proj\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"Proj\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=false;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    
    
    /// run an expecto test of VisUAL
    /// name - name of test
    let VisualUnitTest paras name src (flagsExpected:string) (outExpected: (Out * int) list) =
        testCase name <| fun () ->
            let flagsActual, outActual = RunVisualWithFlagsOut paras src
            Expecto.Expect.equal flagsActual (flagsExpected |> strToFlags) "Status flags don't match"
            let outRegsNoted = 
                outExpected 
                |> List.map fst
            let outActualNoted = 
                outActual.Regs 
                |> List.filter (fun (r,_) -> List.contains r outRegsNoted)
                |> List.sort
            Expecto.Expect.equal outActualNoted (outExpected |> List.sort) <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outActual.Regs src

    let VisualFrameworkTest paras =
        testCase "Framework test failed" <| fun () ->
            let parasExpected = 
                paras.InitRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual = RunVisualWithFlagsOut paras ""
            let outSorted = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            Expecto.Expect.equal flagsActual  paras.InitFlags "Status flags don't match"
            Expecto.Expect.equal outSorted parasExpected <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs" outActual.Regs


    type rType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    let rType2List (r:rType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]
      

    let VisualFrameworkRun (regs: rType,flags:Flags) =
        let performTest() =
            let initRegs = 
                rType2List regs
                |> List.map uint32
        
            let expectedRegs =
                initRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual = 
                    RunVisualWithFlagsOut { 
                        defaultParas with 
                            InitFlags=flags;
                            InitRegs=initRegs
                        } ""
            let actualRegs = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            let flagsOK = flagsActual = flags
            let regsOK = actualRegs = expectedRegs 
            if not flagsOK then 
                printfn "Framework error: Bad flags: %A" flagsActual
                System.Console.ReadKey() |> ignore
            if not regsOK then 
                printfn "Framework error: Bad registers %A" actualRegs
                System.Console.ReadKey() |> ignore
            flagsOK && regsOK
        match flags with
        | {FN=true;FZ=true} -> true // prevent test with imposisble input
        | _ -> performTest()
            
    let testParas = defaultParas
 
    //Set paras with C flag set
    let cSetParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"arith\Arith\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"arith\Arith\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"arith\Arith\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=true;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 

    //Test with c flag set
    let cTest = VisualUnitTest cSetParas
    //Set paras with z flag set
    let zSetParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"arith\Arith\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"arith\Arith\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"arith\Arith\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=true; FC=false;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    let zTest = VisualUnitTest zSetParas
    //Set paras with n flag set
    let nSetParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"arith\Arith\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"arith\Arith\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"arith\Arith\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=true;FZ=false; FC=false;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    let nTest = VisualUnitTest nSetParas
    //set paras with v flag set
    let vSetParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"arith\Arith\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"arith\Arith\VisualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"arith\Arith\VisualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=false;FV=true}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    let vTest = VisualUnitTest vSetParas
    let tTest = VisualUnitTest defaultParas
    //Generate cpu data with flags equal to n z c v
    let cpuData n z c v= 
        let flags = {N=n; C=c; Z=z; V=v}
        let reg = Map.ofList [ 
                    R0, 0u ; R1, 10u ; R2, 20u ; R3, 30u ; R4, 40u ; R5, 50u
                    R6, 60u ; R7, 70u ; R8, 80u ; R9, 90u ; R10, 100u ; R11, 110u ; 
                    R12, 120u ; R13, 130u ;R14, 140u ; R15, 0u ; 
        ]
        {Fl=flags; Regs = reg}
    
    //Return string of "nzcv" with 1 for true 0 for false from cpu data after simulation
    let unpackDataPathFlags (cpuData)=
        match cpuData with
        |Ok x ->
            match x with
            |{DPath = data; MEMState = _} -> 
                match data.Fl.N, data.Fl.Z,data.Fl.C,data.Fl.V with
                | (false, false, false, false) -> "0000"
                | (false, false, false, true) -> "0001"
                | (false, false, true, false) -> "0010"
                | (false, false, true, true) -> "0011"
                | (false, true, false, false) -> "0100"
                | (false, true, false, true) -> "0101"
                | (false, true, true, false) -> "0110"
                | (false, true, true, true) -> "0111"
                | (true, false, false, false) -> "1000"
                | (true,  false, false, true) -> "1001"
                | (true, false, true, false) -> "1010"
                | (true, false, true, true) -> "1011"
                | (true, true, false, false) -> "1100"
                | (true, true, false, true) -> "1101"
                | (true, true, true, false) -> "1110"
                | (true, true, true, true) -> "1111"
        |Error e-> "Error: "+e
            
            
    let accessibleMemState:MemoryArgument =
        [0x100u..0x4u..0x200u]
        |> List.map (fun a -> WA(a),DataMem(0u))
        |> Map.ofList


    //Get data of register in form [R (reg:int), int32]
    let unpackDataPathReg (cpuData:Result<CPUState,string>) reg=
        match cpuData with
        |Ok x->
            match x with
            |{DPath = data; MEMState = _} -> [R reg, data.Regs.[register reg]|>int]
        |Error e-> [R 0, 0]
    let symTab:SymbolTable option = Some (["", 0u] |> Map.ofList)
    //Make line Data for testing parse
    let makeLineData opcode operands =
                {
                    LoadAddr = WA 256u;
                    Label = Some "Label";
                    SymTab = symTab;
                    OpCode = opcode;
                    Operands = operands
                }

    //Completely simulate a line from parsing through to simulation
    let runLine root operand n z c v= parseError ((makeLineData root operand)|>parse) ({DPath = cpuData n z c v; MEMState = accessibleMemState})

    //////////////////////////////////////////////////////////////////////////////////
    ///                      Tests                                                 ///
    //////////////////////////////////////////////////////////////////////////////////

    //// Parsing Tests

    //Test that listLookup returns the correct values from its map lookup
    [<Tests>]
    let mapTest=
            let makeMapTest name inp outp = 
                testCase name <| fun () ->
                    Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
            Expecto.Tests.testList "MapTests"
                [
                    makeMapTest "opMap Add" (listLookup opMap "ADD") (Ok Add)
                    makeMapTest "opMap Sub" (listLookup opMap "SUB") (Ok Sub)
                    makeMapTest "revMap SBC" (listLookup revMap "SBC") (Ok false)
                    makeMapTest "revMap RSC" (listLookup revMap "RSC") (Ok true)
                    makeMapTest "carMap SBC" (listLookup carMap "ADC") (Ok Carry)
                    makeMapTest "carMap RSB" (listLookup carMap "RSB") (Ok NoCarry)
                    makeMapTest "revMap RSB" (listLookup revMap "RSB") (Ok true)
                ]

    //Check the getOperands function splits correctly
    [<Tests>]
    let operandsTest =
        let makeOperandsTest name inp outp = 
            testCase name <| fun () ->
                Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
        Expecto.Tests.testList "operandsTests"
            [
                makeOperandsTest "Operands1" (getOperands("R1,R2,R3").[0]) ("R1")
                makeOperandsTest "Operands2" (getOperands("R1,R2,R3").[1]) ("R2")
                makeOperandsTest "Operands3" (getOperands("R1,R2,R3").[2]) ("R3")
                makeOperandsTest "Operands4" (getOperands("R1,R2,R3,LSL#4").[3]) ("LSL#4")
            ]

    //Test the parse can return the correct output
    //Lots of edge case unit tests that all work
    [<Tests>]
    let parseTest = 
            let makeParseOutput op (suffix:Set) (out:RName) (in1:RName) (in2:OpTwo) flagInvert= 
                let removeOk x =
                    match x with
                    |Ok y-> y
                    |_ ->failwithf "no parse" 
                {
                Instruction=
                    {
                        Op = removeOk (listLookup opMap op)
                        Car = removeOk (listLookup carMap op)
                        Rev = removeOk (listLookup revMap op)
                    }
                Suffix= suffix
                OutputReg =out
                Input1 = in1
                Input2 = in2
                }
                    
            let makeParseTest name inp (outp:Instr)=
                let test = 
                    match inp with
                    |Some (Ok x) -> x.PInstr
                    |_ -> failwithf "Not valid instruction"
                testCase name <| fun () ->
                        Expect.equal test outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name test outp)
            Expecto.Tests.testList "parseTests"
                [
                    makeParseTest "parse1" ((makeLineData "ADD" "R1,R2,R3")|>parse) (makeParseOutput "ADD" false R1 R2 (R3|>Rm) false)
                    makeParseTest "parse2" ((makeLineData "ADDS" "R1,R2,R3")|>parse) (makeParseOutput "ADD" true R1 R2 (R3|>Rm) false)
                    makeParseTest "parse3" ((makeLineData "ADDSNE" "R1,R2,R3")|>parse) (makeParseOutput "ADD" true R1 R2 (R3|>Rm) false)
                    makeParseTest "parse4" ((makeLineData "ADDSNE" "R1,R2,R3,LSL#4")|>parse) (makeParseOutput "ADD" true R1 R2 ((R3,(LSL,({K=4u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) false)
                    makeParseTest "parse5" ((makeLineData "ADDSNE" "R1,R2,R3,LSL#0x1")|>parse) (makeParseOutput "ADD" true R1 R2 ((R3,(LSL,({K=1u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) false)
                    makeParseTest "parse6" ((makeLineData "ADDSNE" "R1,R2,R3,LSL#0xFF000000")|>parse) (makeParseOutput "ADD" true R1 R2 ((R3,(LSL,({K=255u;R=4},NoInvert)|>Num)|>NotRRX)|>Rs) false) 
                    makeParseTest "parse7" ((makeLineData "ADDSNE" "R1,R2,R3,LSL#0b101")|>parse) (makeParseOutput "ADD" true R1 R2 ((R3,(LSL,({K=5u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) false)   
                    makeParseTest "parse8" ((makeLineData "SUB" "R1,R2,#3")|>parse) (makeParseOutput "SUB" false R1 R2 (({K=3u;R=0},NoInvert)|>Immediate) false)
                    makeParseTest "parse9" ((makeLineData "RSB" "R1,R2,#0xF400")|>parse) (makeParseOutput "RSB" false R1 R2 (({K=61u;R=11},NoInvert)|>Immediate) false)
                    makeParseTest "parse10" ((makeLineData "SBC" "R1,R2,#0b1011")|>parse) (makeParseOutput "SBC" false R1 R2 (({K=11u;R=0},NoInvert)|>Immediate) false) 
                    makeParseTest "parse11" ((makeLineData "SBC" "R1,R2,#-0x5")|>parse) (makeParseOutput "ADC" false R1 R2 (({K = 5u; R = 0},Invert)|>Immediate) true)
                    makeParseTest "parse12" ((makeLineData "ADD" "R1,R2,#-0x5")|>parse) (makeParseOutput "SUB" false R1 R2 (({K = 5u; R = 0},Invert)|>Immediate) false) 
                ]
    
    //Tests that a flexOp 2 instruction can be made correctly or errors correctly
    //Obsolete in top level but important to know it did pass
    // [<Tests>]
    // let makeShiftTest=
    //     let makeMakeShiftTest name inp outp = 
    //         testCase name <| fun () ->
    //             Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
    //     Expecto.Tests.testList "makeShiftTests"
    //         [
    //             makeMakeShiftTest "MakeShiftTest1" (makeShift R5 "LSL" "#5") ((R5,(LSL,{K=5u; R=0}|>Num)|>NotRRX)|>Rs|>Ok)
    //             makeMakeShiftTest "MakeShiftTest3" (makeShift R3 "LSL" "R15") (Error "R15 is not a valid register for flexible Operand 2 ")
    //             makeMakeShiftTest "MakeShiftTest4" (makeShift R5 "LSL" "R4") ((R5,(LSL, R4|>Reg)|>NotRRX)|>Rs|>Ok)
    //         ]

    //// FlexOp2 Tests

    
    //Check consistency of flexOp2 literal and make literal
    [<Tests>]
    let flexOpLiteralPropertyTest = 
            testProperty "check literal and flexop2 output are consistent" <| fun(K:uint32, rotate:int, cpuData:DataPath) ->
                let rotateValue = rotate%16
                let baseValue =  (K|>int)%255|> uint32
                let num = ({K =baseValue;R=rotateValue}, NoInvert)|>Immediate
                let validUint = (flexOp2 num cpuData)
                let endLiteral = validUint|>makeLiteral
                match endLiteral with
                |Ok (x) -> Expect.equal validUint  (flexOp2 ((x)|>Immediate) cpuData)
                |_->failwith "Not Valid literal"

    //Check that flexOp2 can given a valid Op2 return the correct uint32
    //Multiple tests for each valid input trying to his as many edge cases as possible
    [<Tests>]
    let flexOpTest =
        let reg = Map.ofList [ 
                    R0, 0u ; R1, 1u ; R2, 2u ; R3, 3u ; R4, 4u ; R5, 5u
                    R6, 6u ; R7, 7u ; R8, 8u ; R9, 9u ; R10, 10u ; R11, 0x80000000u ; 
                    R12, 12u ; R13, 13u ;R14, 14u ; R15, 15u ; 
            ]
        let cpuData = {Fl={N=true; C=true; Z=true; V=true};Regs=reg}
        let cpuDataCarryFalse = {Fl={N=true; C=false; Z=true; V=true};Regs=reg}
        let makeFlexOpTest name inp outp =
            testCase name <| fun () ->
                Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
        Expecto.Tests.testList "flexOpTests"
            [
                makeFlexOpTest "flexOpTest Literal 1" (flexOp2 (({K=1u;R=5},NoInvert)|>Immediate) cpuData) 4194304u
                makeFlexOpTest "flexOpTest Literal 2" (flexOp2 (({K=69u;R=15},NoInvert)|>Immediate) cpuData) 276u
                makeFlexOpTest "flexOpTest Literal 3" (flexOp2 (({K=138u;R=2},NoInvert)|>Immediate) cpuData) 2684354560u
                makeFlexOpTest "flexOpTest Register 1" (flexOp2 (R1|>Rm) cpuData) cpuData.Regs.[R1]
                makeFlexOpTest "flexOpTest Register 2" (flexOp2 (R2|>Rm) cpuData) cpuData.Regs.[R2]
                makeFlexOpTest "flexOpTest Register 3" (flexOp2 (R4|>Rm) cpuData) cpuData.Regs.[R4]
                makeFlexOpTest "flexOpTest Register 4" (flexOp2 (R13|>Rm) cpuData) cpuData.Regs.[R13]
                makeFlexOpTest "flexOpTest LSL 1" (flexOp2 ((R2,(LSL,({K=5u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0b1000000u)
                makeFlexOpTest "flexOpTest LSL 2" (flexOp2 ((R10,(LSL,({K=1u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0b10100u)
                makeFlexOpTest "flexOpTest LSL 3" (flexOp2 ((R2,(LSL,R5|>Reg)|>NotRRX)|>Rs) cpuData)  (0b1000000u) 
                makeFlexOpTest "flexOpTest LSR 1" (flexOp2 ((R2,(LSR,({K=1u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0b1u)
                makeFlexOpTest "flexOpTest LSR 2" (flexOp2 ((R11,(LSR,({K=56u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0x0u)
                makeFlexOpTest "flexOpTest LSR 3" (flexOp2 ((R11,(LSR,R8|>Reg)|>NotRRX)|>Rs) cpuData)  (0x800000u)
                makeFlexOpTest "flexOpTest ASR 1" (flexOp2 ((R2,(ASR,({K=1u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0b1u)
                makeFlexOpTest "flexOpTest ASR 2" (flexOp2 ((R11,(ASR,({K=58u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0x0u)
                makeFlexOpTest "flexOpTest ASR 3" (flexOp2 ((R11,(ASR,R8|>Reg)|>NotRRX)|>Rs) cpuData)  (0xFF800000u) 
                makeFlexOpTest "flexOpTest ROR 1" (flexOp2 ((R2,(ROR,({K=1u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0b1u)
                makeFlexOpTest "flexOpTest ROR 2" (flexOp2 ((R8,(ROR,({K=58u;R=0},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0x200u)
                makeFlexOpTest "flexOpTest ROR 3" (flexOp2 ((R11,(ROR,R8|>Reg)|>NotRRX)|>Rs) cpuData)  (0x800000u)
                makeFlexOpTest "flexOpTest LSR to 0" (flexOp2 ((R11,(LSR,({K = 36u;R = 0;},NoInvert)|>Num)|>NotRRX)|>Rs) cpuData)  (0x0000000u)
                makeFlexOpTest "flexOpTest RRX 1" (flexOp2 ((R2,RRX)|>Rs) cpuDataCarryFalse)  (0b1u)
                makeFlexOpTest "flexOpTest RRX 2" (flexOp2 ((R5,RRX)|>Rs) cpuData)  (0x80000002u)
                makeFlexOpTest "flexOpTest RRX 3" (flexOp2 ((R1,RRX)|>Rs) cpuDataCarryFalse)  (0x0u)            
            ]
    
    
    //Check that make literal returns a correct litteral including when inverse is needed.
    //Testing worked without Symbol table the added functionality is yet to be fuly tested
    // [<Tests>]
    // let makeLiteralTest=
    //     let makeMakeLiteralTest name inp outp = 
    //         testCase name <| fun () ->
    //             Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
    //     Expecto.Tests.testList "makeLiteralTests"
    //         [
    //             makeMakeLiteralTest "literal 1" (numOrReg "#5") (Ok ({K = 5u; R = 0;}|>Num,false))
    //             makeMakeLiteralTest "literal 2" (numOrReg "#-5") (Ok ({K = 5u; R = 0;}|>Num,true))
    //             makeMakeLiteralTest "literal 3" (numOrReg "#0x80000000") (Ok ({K = 2u;R = 1;}|>Num,false))
    //             makeMakeLiteralTest "literal 4" (numOrReg "#-1") (Ok ({K = 1u; R = 0;}|>Num,true))
    //         ]
    
    //// Simulate Tests

    // A large number of mathematical property tests checking for continuity
    [<Tests>]
    let commutativeAddTest =
        testProperty "a+b is equivelant to b+a" <| fun (in1:uint32) (in2:uint32) (cpuData:DataPath)->
            add (in1) (in2) (+) NoCarry cpuData false Add= add (in2) (in1) (+) NoCarry cpuData false Add   
     
    [<Tests>]
    let addtestNoCarry =
        testProperty "add x y (+) false cpuData returns x+y,flags unchanged" <| fun (in1:uint32) (in2:uint32) (cpuData:DataPath)->
            add (in1) (in2) (+) Carry cpuData false Add= ((in1 + in2),cpuData.Fl)

    [<Tests>]        
    let addTestWithCarry =
        testProperty "add x y (+) true cpuData returns x+y+1 if cpuData.Fl.C is set,flags unchanged" <| fun (in1:uint32) (in2:uint32) (cpuData:DataPath)->
            let carryAdd =
                match cpuData.Fl.C with
                |true -> 1u
                |false -> 0u
            add (in1) (in2) (+) Carry cpuData false Add= ((in1 + in2+ carryAdd),cpuData.Fl)
    [<Tests>]
    let subtestNoCarry =
        testProperty "sub x y (-) false cpuData returns x-y,flags unchanged" <| fun (in1:uint32) (in2:uint32) (cpuData:DataPath)->
            sub (in1) (in2) (-) NoCarry cpuData false Sub= ((in1 - in2),cpuData.Fl)


    
    //Check the checkPositive function is able to determine if an input is positive
    [<Tests>]    
    let checkPositiveInTest = 
        let makeCheckPositiveInTest name inp outp = 
            testCase name <| fun () ->
                Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
        Expecto.Tests.testList "checkPositiveInTests"
            [
                makeCheckPositiveInTest "Positive 1" (checkPositiveIn 1u) (true)
                makeCheckPositiveInTest "Positive 2" (checkPositiveIn 0x7FFFFFFFu) (true)
                makeCheckPositiveInTest "zero" (checkPositiveIn 0u) (true)
                makeCheckPositiveInTest "Negative 1" (checkPositiveIn (-1|>uint32)) (false)
            ]
    
    //Check if less than 0 is valid
    [<Tests>]
    let checkLess0Test = 
        let makeCheckLess0Test name inp outp = 
            testCase name <| fun () ->
                Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
        Expecto.Tests.testList "checkPositiveInTests"
            [
                makeCheckLess0Test "less0 false 1" (checkLess0 1u) (false)
                makeCheckLess0Test "less0 false 2" (checkLess0 0x7FFFFFFFu) (false)
                makeCheckLess0Test "less 0 Zero" (checkLess0 0u) (false)
                makeCheckLess0Test "less 0 true 1" (checkLess0 0xFFFFFFFFu) (true)
            ]
    
    
    [<Tests>]
    let negativeNumberPropertyTestWithOutFlags = 
        testProperty "add x (-y) should be equal to sub x y." <| fun (in1:uint32) (in2:uint32) (cpuData:DataPath)->
            add (in1) (((-((in2)|>int))|>uint32)) (+) NoCarry cpuData false Add= sub (in1) (in2) (-) NoCarry cpuData false Sub

    
    [<Tests>]
    let negativeMinusNegativePropertyTestWithOutFlags = 
        testProperty "add x y should be equal to sub x (-y)." <| fun (in1:uint32) (in2:uint32) (cpuData:DataPath)->
            add (in1) (in2) (+) NoCarry cpuData false Add= sub (in1) (((-((in2)|>int))|>uint32)) (-) NoCarry cpuData false Sub

    //Check value read from cpuData is correct
    [<Tests>]
    let errorResultRNameTests=
        let makeErrorResultRNameTests name inp outp=
            testCase name <| fun () ->
                Expect.equal inp outp (sprintf "Test Name: '%s' Input: '%A' Expected Input: '%A'" name inp outp)
        Expecto.Tests.testList "errorResultRNameTests"
                [
                    makeErrorResultRNameTests "R15 +8 1" (errorResultRName (R15) (cpuData false false false false)) (8u)
                    makeErrorResultRNameTests "R13 1" (errorResultRName (R3) (cpuData false false false false)) (30u)
                ]
    
    
    //// Tests Against Visual
    
    [<Tests>]
    /// implements random property-based tests of the framework
    /// tests that read/write of registers and flags is consistent for random
    /// input values
    let frametests =        
        let fsConfig = {
                FsCheckConfig.defaultConfig with
                    replay = Some (0,0) // seed for RNG. Means that the same tests are done each run
                                        // replace by None for a random time-based seed and therefore
                                        // new tests each time that will not cache
                    maxTest = 100       // number of random tests
                }
        testPropertyWithConfig fsConfig "Flags and registers are preserved" VisualFrameworkRun


    //Randomly generate a list of tests.
    //randomly pick instruction if flags should be set.
    //pass into correct test type.
    //Check Visual output compared to whole simulation output.
    //Since parse is thourghly tested above it can be assumed to work here 
    //as such this is mainly testing the ability for my code to perform the maths as required

    //Issue in final build due to change to CPUdata path to allow for machine memory. 
    //This module never touches that data just passing the same values it recieves in so can assume the fact tests passed before hand that they still will.
    //As such this module fully tests the base functionality of the flexOp2 and maths commands and show they work.
    //Also since CMP and CMN are almost carbon copies of this execution just without updating the registers they can be assumed to work too.
    let compareToVisualList x test n z c v= 
        [0..x] 
        |> List.map (fun k ->
            let rnd = System.Random()
            let rnd2 = System.Random()
            let sSet = List.init (x) (fun _ -> rnd2.Next())
            let regList = List.init (x) (fun _ -> rnd.Next()) 
            let reg n' = regList.[n' % x]%13
            let n' = 1 + (k % 254)
            let instr =
                match regList.[n' % x] % 6 with
                |0-> "ADD"
                |1-> "ADC"
                |2-> "SUB"
                |3-> "SBC"
                |4-> "RSB"
                |5-> "RSC"
            let set =
                match sSet.[n' % x] % 2 with
                |0-> "S"
                |1-> ""
            test (sprintf "%s%s%d test" set instr k) (sprintf "%s%s R0, R%d, #%d" instr set (n'|>reg) n') (((runLine (sprintf "%s%s" instr set) (sprintf "R0,R%d,#%d" (n'|>reg) n') n z c v)|>unpackDataPathFlags)) (unpackDataPathReg (runLine (sprintf "%s%s" instr set) (sprintf "R0,R%d,#%d" (n'|>reg) n') n z c v) 0))
    
    //Check with inversion that the code still functions as expected
    let compareToVisualListNegativeIn x test n z c v= 
        [0..x] 
        |> List.map (fun k ->
            let rnd = System.Random()
            let rnd2 = System.Random()
            let sSet = List.init (x) (fun _ -> rnd2.Next())
            let regList = List.init (x) (fun _ -> rnd.Next()) 
            let reg n' = regList.[n' % x]%13
            let n' = 1 + (k % 254)
            let instr =
                match regList.[n' % x] % 4 with
                |0-> "ADD"
                |1-> "ADC"
                |2-> "SUB"
                |3-> "SBC"
            let set =
                match sSet.[n' % x] % 2 with
                |0-> "S"
                |1-> ""
            test (sprintf "%s%s%d test" instr set k) (sprintf "%s%s R0, R%d, #-%d" instr set (n'|>reg) n') (((runLine (sprintf "%s%s" instr set) (sprintf "R0,R%d,#-%d" (n'|>reg) n') n z c v)|>unpackDataPathFlags)) (unpackDataPathReg (runLine (sprintf "%s%s" instr set) (sprintf "R0,R%d,#-%d" (n'|>reg) n') n z c v) 0))
    
    //Check with random FlexOp instructions with shift
    let compareToVisualListFlexOp x test n z c v= 
        [0..x] 
        |> List.map (fun k ->
            let rnd = System.Random()
            let rnd2 = System.Random()
            let sSet = List.init (x) (fun _ -> rnd2.Next())
            let regList = List.init (x) (fun _ -> rnd.Next()) 
            let reg n' = regList.[n' % x]%13
            let n' = 1 + (k % 254)
            let instr =
                match regList.[n' % x] % 6 with
                |0-> "ADD"
                |1-> "ADC"
                |5-> "SUB"
                |4-> "SBC"
                |2 ->"RSB"
                |3 ->"RSC"
            let flexOp1 = 
                match sSet.[n' % x] % 5 with
                |0-> (sprintf " LSL #%d" n')
                |1-> (sprintf " LSR #%d" n')
                |2-> (sprintf " ASR #%d" n')
                |3-> (sprintf " ROR #%d" n')
                |4-> (sprintf " RRX")
            let flexOp2 = 
                match sSet.[n' % x] % 5 with
                |0-> (sprintf "LSL#%d" n')
                |1-> (sprintf "LSR#%d" n')
                |2-> (sprintf "ASR#%d" n')
                |3-> (sprintf "ROR#%d" n')
                |4-> (sprintf "RRX")

            let set =
                match sSet.[n' % x] % 2 with
                |0-> "S"
                |1-> ""
            test (sprintf "%s%s%d test" instr set k) (sprintf "%s%s R0, R%d, R%d, %s" instr set (n'|>reg) (n'|>reg) flexOp1) (((runLine (sprintf "%s%s" instr set) (sprintf "R0,R%d,R%d,%s" (n'|>reg) (n'|>reg) flexOp2) n z c v)|>unpackDataPathFlags)) (unpackDataPathReg (runLine (sprintf "%s%s" instr set) (sprintf "R0,R%d,R%d,%s" (n'|>reg) (n'|>reg) flexOp2) n z c v) 0))
    
    //Large number to make sure there is a large set of random tests covering most test cases
    let testNum = 3000

    //Tests with all the different flags. Only C flag should effect code run everything else is jus tincase.
    [<Tests>]
    let compareToVisualCSet = 
        testList "Many pointless tests C Set " (compareToVisualList testNum cTest false false true false)
    [<Tests>]
    let compareToVisualCNotSetFlags = 
        testList "Many pointless tests NoFlags " (compareToVisualList testNum tTest false false false false)
    [<Tests>]
    let compareToVisualNSet = 
        testList "Many pointless tests N Set " (compareToVisualList testNum nTest true false false false)
    [<Tests>]
    let compareToVisualZSet = 
        testList "Many pointless tests Z Set " (compareToVisualList testNum zTest false true false false)
    [<Tests>]
    let compareToVisualVSet = 
        testList "Many pointless tests V Set " (compareToVisualList testNum vTest false false false true)
    [<Tests>]
    let compareToVisualCSetNegative = 
        testList "Many pointless tests C Set Negative " (compareToVisualListNegativeIn testNum cTest false false true false)
    [<Tests>]
    let compareToVisualNoFlagSetNegative = 
        testList "Many pointless tests NoFlags Set Negative " (compareToVisualListNegativeIn testNum tTest false false false false)
    [<Tests>]
    let compareToVisualNoFlagSetFlexOp2 = 
        testList "Many pointless tests NoFlags Set FlexOp" (compareToVisualListFlexOp testNum tTest false false false false)
    [<Tests>]
    let compareToVisualCFlagSetFlexOp2 = 
        testList "Many pointless tests C Set FlexOp" (compareToVisualListFlexOp testNum cTest false false true false)