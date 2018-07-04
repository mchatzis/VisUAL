module LDMSTM
    open CommonLex
    open CommonData
    open EEExtensions
    open System

    type Root = LDM | STM
    ///Defines the direction of the stack pointer change
    type Suffix = EA |ED |FA |FD
    ///False means that stack pointer remains same after memory operation
    type UpdatePointer = bool

    type Tokens =
        |Reg of RName
        |Comma
        |Dash
        |ExclMark
        |End

    ///Instruction Class definition (This will be input to execution function,
    ///so Condition not needed since handled in top level)
    ///Contains all the necessary input information for the execution function
    type Instr = {  
        OperationalCode: Root*Suffix   
        StackPointer: RName*UpdatePointer 
        OpRegs: RName list
        }

    type FirstOperandError = InvalidReg | InvalidFormat
    type SecondOperandError = InvalidBrackets| InvalidListFormat| InvalidRegister
    type ExecutionError = CannotAccessCodeMem | WANotDivBy4
    /// high level type to capture all types of errors and give hint to user
    type Err = 
        |OpCodeErr
        |InvalidInstrFormat
        |FstOperErr of FirstOperandError
        |SndOperErr of SecondOperandError
        |ExecutionErr of ExecutionError
        |WrongInstClass
    
    ///The specific type of instructions in this module
    let memSpec = {
        InstrC = MEM
        Roots = ["LDM";"STM"]
        Suffixes = ["FA"; "IB"; 
                    "FD"; "IA"; 
                    "EA"; "DB"; 
                    "ED"; "DA"]
    }
    
    ///*Includes all possible suffixes and their aliases     
    let opCodeMap = [
                        
                        "STMEA",(STM,EA);  "STMDB",(STM,FD); 
                        "STMED",(STM,ED);  "STMDA",(STM,ED);
                        "STMFA",(STM,FA);  "STMIB",(STM,FA);
                        "STMFD",(STM,FD);  "STM",(STM,EA);    "STMIA",(STM,EA);
                        "LDMEA",(LDM,EA);  "LDMDB",(LDM,EA); 
                        "LDMED",(LDM,ED);  "LDMDA",(LDM,FA);
                        "LDMFA",(LDM,FA);  "LDMIB",(LDM,ED);
                        "LDMFD",(LDM,FD);  "LDM",(LDM,EA);    "LDMIA",(LDM,FD);
                        ] |> Map.ofList

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand memSpec

    ///Active pattern: Finds regular expression inside string
    let (|StringMatch|_|) regex str= 
        match String.regexMatch regex str  with 
        |None -> None 
        |Some (fstOccurence,rest)-> (fstOccurence,rest)|> Some

    ///Creates dummy instruction and updates it with the parsed opcode
    let parseOpCode root suffix =
        //Create Dummy instr
        let instr= {OperationalCode = LDM,EA; StackPointer= R1,false; OpRegs = []}
        let makeString = String.concat "" [root;suffix]
        match opCodeMap.TryFind(makeString) with
        |Some (root',suffix')-> Ok {instr with OperationalCode = root',suffix' }
        |None -> OpCodeErr |> Error

    ///Splits the two operands apart (Assumes white spaces have been removed in top level)
    ///Way it does it: Searches for the first comma
    let rec splitOp str= 
        match str with
        |StringMatch "^(.*?)," (outp,_) -> (outp,str.Substring(outp.Length)) |> Ok
        |_ ->  InvalidInstrFormat |> Error

    //Input an empty list and your operand string to get a list of tokens representing operand (Tokens list type)
    let rec tokenize tokenList op= 
        match op with
        |"" -> (tokenList @ [End]) |> Some
        |StringMatch @"^[r,R]\d{1,2}|^[a-zA-Z]{2}" (outp,_) -> 
                match regNames.TryFind(outp) with
                |None -> None
                |Some reg -> tokenize (tokenList @ [Reg(reg)]) (op.Substring(outp.Length))
        |StringMatch @"^," (outp,_) -> 
                tokenize (tokenList @ [Comma]) (op.Substring(outp.Length))
        |StringMatch @"^-" (outp,_) -> 
                tokenize (tokenList @ [Dash]) (op.Substring(outp.Length))
        |StringMatch @"^!" (outp,_) ->
                tokenize (tokenList @ [ExclMark]) (op.Substring(outp.Length))
        |_ -> None

    ///Update instruction state by parsing first operand
    let parseFstOp op1 instr =
        let rec parseFstOp' instr' toks =
            match toks with
            |Reg(R15)::rest -> InvalidReg |> FstOperErr |>  Error
            |Reg(reg)::Comma::[End] -> {instr' with StackPointer = (reg,false)} |> Ok
            |Reg(reg)::ExclMark::Comma::[End] -> {instr' with StackPointer = (reg,true)} |> Ok
            |_ -> InvalidFormat |> FstOperErr |> Error
            
        match tokenize [] op1 with
        |None -> InvalidReg |> FstOperErr |> Error 
        |Some toks -> toks |> parseFstOp' instr 
        
    ///Update instruction state by parsing second operand
    let parseSndOp op2 instr =
        
        ///Generates list of all registers contained in the range which is set by the input registers
        let constructRegRange reg1 reg2 =
             let constrRegList' a b =
                [a..b]
                |> List.map (fun a -> inverseRegNums.[a])
                
             match (regNums.[reg1], regNums.[reg2]) with
             |a,b when a = b -> [inverseRegNums.[a]]
             |a,b when a > b -> constrRegList' b a
             |a,b when a < b -> constrRegList' a b
             |_ -> failwithf "Failed to match any pattern"

        let removeCurlyBrackets op =
         match op |> String.toList with
         |'{' ::tl-> match List.rev(tl) with
                     |'}'::tl' -> List.rev(tl') |> List.toString |> Ok
                     |_ ->  InvalidBrackets  |> SndOperErr |> Error  
         |_ -> InvalidBrackets |> SndOperErr |> Error

        ///Find Registers that are not valid according to ARM specifications
        let screenForInvalidRegList (rList:RName list) = 
            
            let (pointer,update) = instr.StackPointer
            if not (rList = []) then
               if not (List.contains R13 rList) then
                  if  not (update = true  && List.contains pointer rList) then 
                     if not (fst(instr.OperationalCode) = LDM && (List.contains R15 rList && List.contains R14 rList)) then
                         if not (fst(instr.OperationalCode) = STM && (List.contains R15 rList)) then 
                            rList |> Ok
                         else InvalidRegister |> SndOperErr |> Error
                     else InvalidRegister |> SndOperErr |> Error
                  else InvalidRegister |> SndOperErr |> Error
               else InvalidRegister |> SndOperErr |> Error
            else InvalidListFormat |> SndOperErr |> Error 

        ///Does most of parsing
        let rec createRegList regList tokList =
                match tokList with 
                |[End] -> regList |> Ok
                |Reg(reg)::[End] -> regList @ [reg] |> Ok
                |Reg(reg)::Comma::rest when not (rest = [End])-> createRegList (regList @ [reg]) rest
                |Reg(reg1)::Dash::Reg(reg2)::Comma::rest when not (rest = [End])-> 
                        let regList' = regList @ (constructRegRange reg1 reg2)
                        createRegList regList' rest
                |Reg(reg1)::Dash::Reg(reg2)::rest -> 
                        let regList' = regList @ (constructRegRange reg1 reg2)
                        createRegList regList' rest
                |_ -> InvalidListFormat |> SndOperErr |> Error
         
        let  putIntoInstr toks =
            {instr with OpRegs = toks}
        
        let catchTokenizeErrors operand =
            match tokenize [] operand with
            |None -> InvalidRegister |> SndOperErr |> Error 
            |Some toks -> toks |> Ok

        op2
        |> removeCurlyBrackets
        |> Result.bind catchTokenizeErrors
        |> Result.bind (createRegList [])
        |> Result.map List.distinct
        |> Result.map (List.map (fun a -> regNums.[a]))
        |> Result.map List.sort
        |> Result.map (List.map (fun a -> inverseRegNums.[a]))
        |> Result.bind screenForInvalidRegList
        |> Result.map putIntoInstr
     
    ///Top-level parsing function
    let parse (ls: LineData) : Result<Parse<Instr>,Err> option =
        let parse' (instrC, (root,suffix,pCond)) =
            match instrC with
            |MEM -> 
                let (WA la) = ls.LoadAddr // address this instruction is loaded into memory

            
                let distributeForParsing (op1,op2)=
                    parseOpCode root suffix
                    |> Result.bind (parseFstOp op1)
                    |> Result.bind (parseSndOp op2)
            
                ///Parsed instruction
                let instr' =
                    ls.Operands
                    |> splitOp 
                    |> Result.bind distributeForParsing
            
                match instr' with
                    | Ok result -> 
                
                                { 
                                PInstr = result; 
                                // This is normally the line label as contained in
                                // ls together with the label's value which is normally
                                // ls.LoadAddr. Some type conversion is needed since the
                                // label value is a number and not necessarily a word address
                                // it does not have to be div by 4, though it usually is
                                PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 
                                PSize = 4u; 
                                PCond = pCond 
                                }   |> Ok

                    |Error e -> e |> Error
            |_ -> Error WrongInstClass    

        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = parse

    //This function needs explaining
    //It takes advantage of the common functionality of the different stack directions (EA,ED,FA,FD)
    //We have the two basicOperation functions that recursively store/load from/to memory/registers to registers/memory
    //They update the memory and register states and return them in the end
    //A function is given as an input to these two functions which is used to switch from Ascending to Descending stack pointer
    //Moreover, this function also increments the starting stack pointer or decrements it, to account for Empty and Full operation
    //Finally this function also changes the value of the final iteration return value to update the pointer correctly because 
    //otherwise the pointer in the end would be one more address up/down than desirable
    let execute (cpu:CPUState) (instr:Instr)=
        let dPath = cpu.DPath
        let memoryState = cpu.MEMState
        printfn "%A" cpu
        let rec basicStoreOperation (memState:MemoryArgument) (regState:Map<RName,uint32>) (pointer,ascendOrDescend,lastIteration) (regList:RName list)=
            match regList with
            |[] -> (memState,regState,pointer |> lastIteration) |> Ok
            |fstReg::rest -> let regContent = regState.Item(fstReg)
                             let memState' = memState.Add(WA(pointer),DataMem(regContent))
                             basicStoreOperation memState' regState (pointer|>ascendOrDescend ,ascendOrDescend,lastIteration) rest
                              
        let rec basicLoadOperation (memState:MemoryArgument) (regState:Map<RName,uint32>) (pointer,ascendOrDescend,lastIteration) (regList:RName list)=
            match regList with
            |[] -> (memState,regState,pointer |> lastIteration) |> Ok
            |fstReg::rest -> match memState.Item(WA(pointer)) with
                             |DataMem(memContent) -> 
                                    let regState' = regState.Add(fstReg,memContent)
                                    basicLoadOperation memState regState' (pointer|>ascendOrDescend ,ascendOrDescend,lastIteration) rest
        
        ///Function passed as an argument to the basic low level store and load function
        ///It sets the initial position of the pointer, its direction and whether it should be amended in the last iteration 
        let setPointDir point= 
            let increment (a:uint32) = a + 4u
            let decrement (a:uint32) = a - 4u
            let doNothing (a:uint32) = a
            match instr.OperationalCode with
            |(STM,EA) | (LDM,FD) -> (point, increment,doNothing)
            |(STM,ED) | (LDM,FA) -> (point, decrement,doNothing)
            |(STM,FA) | (LDM,ED) -> (point + 4u, increment,decrement)
            |(STM,FD) | (LDM,EA) -> (point - 4u, decrement,increment)

        ///Top level HOF responsible for breaking down top level datatypes into function arguments
        ///so that just the information needed passes to the basic low level function instead of the
        ///whole top level data type
        let memoryTopOperation pointer callMemFun = 
                
            ///reverses list of registers depending on Ascending/Descending and LDM/STM to account for 
            ///ALWAYS having higher registers at higher memory addresses.
            let regList' = 
                match instr.OperationalCode with
                |(STM,EA) |(STM,FA) |(LDM,FD) | (LDM,ED) -> instr.OpRegs
                |(STM,FD) |(STM,ED) | (LDM,EA) |(LDM,FA) -> List.rev instr.OpRegs
            
            ///decides whether to update pointer or not
            let wasReturned (mem,regs:Map<RName,uint32>,pointerRet) = 
                    ///INCREMENTS PC
                    let regs' = regs.Add(R15,(regs.Item(R15)|>uint32)+4u)
                    match instr.StackPointer with
                    |p,true -> {DPath = {dPath with Regs = regs'.Add(p,pointerRet)}; MEMState = mem }
                    |p,false -> {DPath = {dPath with Regs = regs'.Add(p,pointer)};  MEMState = mem}
            
            ///calls lower level load and store operations
            callMemFun memoryState dPath.Regs (setPointDir pointer) regList'
            |> Result.map wasReturned 

        //Allocating LDM and STM and passing pointer value, also catching Word Address not div by 4 error

        //Needed as for somereason this tupled for me may cause errors on other machines to test
        match instr.OperationalCode , dPath.Regs.Item(instr.StackPointer|>fst) with
        |(_,_),pointer when (not (pointer % 4u = 0u)) -> WANotDivBy4 |> ExecutionErr|> Error
        |(LDM,_),pointer-> 
            match memoryTopOperation pointer basicLoadOperation with
            |Error e -> Error e
            |Ok (x) -> Ok x 
        |(STM,_),pointer -> 
            match memoryTopOperation pointer basicStoreOperation with
            |Error e -> Error e
            |Ok (x) -> Ok x


    let (|Execute|_|) = execute|> Some




