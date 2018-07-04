////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////
module CommonTop

    open CommonLex
    open CommonData
    open LDMSTM
    open LDRSTR
    open Misc
    open Log
    open Comp
    open Move
    open Arith
    
    open EEExtensions
    open Arith
    open Arith
    open Data
    open Arith

    /// allows different modules to return different parse error info
    /// by default all return string so this is not needed
    /// NB - these are not yet implemented in sample code    

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of LDRSTR.Instr
        | ISTACK of LDMSTM.Instr
        | IARITH of Arith.Instr
        | ILOG of Log.Instr
        | IMOVE of Move.Instr
        | ICOMP of Comp.Instr
        | IMISC of Misc.Instr
        
        //etc
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of LDMSTM.Err
        | ERRIΑRITH of Arith.ErrInstr
        | ERRILOG of Log.ErrInstr
        | ERRICOMP of Comp.ErrInstr
        | ERRILDRSTR of LDRSTR.ErrInstr
        | ERRIMISC of Misc.ErrInstr
        | ERRIMOVE of Move.ErrInstr
        | ERRTOPLEVEL of string

    
    // Join two maps   
    let joinMaps (first:Map<'a,'b>) (second:Map<'a,'b>) = 
        Map(Seq.concat [ (Map.toSeq first) ; (Map.toSeq second) ])

        //THIS IS THE INTERFACE!!

    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible isntruction values combines with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrInstr
    /// Similarly IMatch here is combination of module IMatches
    let IMatch (ld: LineData) : Result<Parse<Instr>,ErrInstr> option =
        ///this function makes sure that the result <instruction,Error> type returned
        ///from each module is interfaced to this module's Instr type defined in line 14
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | LDMSTM.IMatch pa -> pConv ISTACK ERRIMEM pa
        | LDRSTR.IMatch pa -> pConv IMEM ERRILDRSTR pa
        | Arith.IMatch pa -> pConv IARITH ERRIΑRITH pa
        | Log.IMatch pa -> pConv ILOG ERRILOG pa

        | Move.IMatch pa -> pConv IMOVE ERRIMOVE pa
        | Comp.IMatch pa -> pConv ICOMP ERRICOMP pa
        | Misc.IMatch pa -> pConv IMISC ERRIMISC pa
        | _ -> None
    
    type CondInstr = Condition * Instr
    

    //Make the symbolTable
    let makeSymbolTable (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) = 
        
        let arrayIntoList ar =
            printfn "%A" ar
            ar |> Array.toList

        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
        /// remove comments from string
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands 
                        with Label=Some label}
                      |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
                | Some pa -> pa
            | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
        asmLine
        |> removeComment
        |> splitIntoWords
        |> arrayIntoList
        |> matchLine
        

    ///Actually parse each line 
    let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) =

        let arrayIntoList ar =
            printfn "%A" ar
            ar |> Array.toList

        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
        /// remove comments from string
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands 
                        with Label=Some label}
                      |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
                | Some pa -> pa
            | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
        asmLine
        |> removeComment
        |> splitIntoWords
        |> arrayIntoList
        |> matchLine

    
    ///A first pass over the data.
    ///Returns a map of all labels along with their position in code
    ///Position is absolute so line 6 is position 32
    let firstPass (list:string list) (loadAddr: WAddr)=
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = None
        }
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        let arrayIntoList ar =
            printfn "%A" ar
            ar |> Array.toList
        let matchLine words item=
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    match makeLineData opc operands |> IMatch with
                    |Some (Ok x) -> Some (None, (item*4|>uint32))
                    |_-> None
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> (Some(label, 8+item*4|>uint32),(item*4|>uint32))
        let tupled = 
            list
            |>List.map(fun x -> 
                matchLine (x|>removeComment|>splitIntoWords|>arrayIntoList) (List.findIndex(fun y -> y=x) list)
                )
        let optionMatch inp=
            match inp with
            |Some _ -> true
            |_ ->false
        let symTab (tupled:((((string*uint32) option)*uint32) list))=
            //printfn "Tupled: %A" tupled
            let symOpt = List.map(fun (x,_) -> x) tupled
            let sym = List.filter(optionMatch) symOpt
            sym
            |>List.map(fun (Some x)-> x)
            |>Map.ofList
        symTab tupled

    ///A second pass over the data
    ///Returns a list of maps where each map contains a label and the equivelant value if it is an expression
    let secondPass (symtab: SymbolTable) (list:string list) (loadAddr: WAddr) cpu=
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
            Label=None
            LoadAddr = loadAddr
            SymTab = None
        }
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        let arrayIntoList ar =
            printfn "%A" ar
            ar |> Array.toList
        let matchLine words=
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    match makeLineData opc operands |> IMatch with
                    |Some (Ok x) -> Some symtab
                    |_-> None
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> None
            | None, label :: opc :: operands -> 
                match opc with 
                |"EQU" ->
                    match { makeLineData opc operands with Label=Some label}|> IMatch with
                    |Some x ->
                        match x with
                        |Ok y -> 
                            match y.PInstr with
                            |IMISC i->
                                match i.OpRoot with
                                |"EQU" -> 
                                    match Misc.execute i cpu with
                                    |Error e -> Some (label, e|>uint32)
                                    |_ -> failwithf "Should not happen"
                                | _ -> None
                            | _ -> None
                        |_ -> None
                    |None -> None
                |_ -> None
            |_ -> None
        let a = List.map(fun x -> matchLine (x|>removeComment|>splitIntoWords|>arrayIntoList)) list
        let optionMatch inp=
            match inp with
            |Some _ -> true
            |_ ->false
        let sym = List.filter(optionMatch) a
        sym|>List.map(fun (Some x)-> x)|> List.map( fun (x,y)-> symtab.Add (x,y))
    
    //main function that performs parsing and execution
    //Will be called from [<Entrypoint>] with inputs 1. string list and 2. CPUState but for now 
    //just the string list is here to make it easier to test
    let run (userInput:string list) = 
        
        //a testing instruction for parsing
        let dummyInstr:Instr = 
            {OperationalCode = LDM,EA; StackPointer= R1,false; OpRegs = []} |> ISTACK
        //Initialize MACHINE STATE
        let memoryState:MachineMemory<Instr> =
            let codeMap =
                [0u..0x4u..0xFCu]
                |> List.map (fun a -> WA(a) , Code(dummyInstr))
                |> Map.ofList
        
            let dataMap = 
                [0x100u..0x4u..0x200u]
                |> List.map (fun a -> WA(a),DataLoc(0u))
                |> Map.ofList
            joinMaps codeMap dataMap
        let regState = 
            Map.ofList [ 
                R0,0u ; R1,0u ; R2,0u ; R3,0u ; R4,0u ; R5,0u
                R6,0u ; R7,0u ; R8,0u ; R9,0u ; R10,0u ; R11,0u ; 
                R12,0u ; R13,0x100u ; R14,0u ; R15,8u; 
                ] 
        let flagState =
            {N = false; C = false; Z = false; V = false}
        let dataPathState =
            {Fl = flagState; Regs = regState}
            
        ///Get the symbol table from first pass
        let symbTable= 
            firstPass userInput (WA(1u))
        //printfn "Symbol Table: %A" symbTable
        
        ///Get the sybol table after the secocnd pass
        let secondSym :SymbolTable = 
            let toAdd = secondPass (symbTable) userInput (WA(1u)) {DPath = dataPathState; MEMState = Map.empty}
            //Combine the map from second pass by using a fold. Then combine this map and the symbolTable map into a single symbol table map.
            List.fold(fun acc x -> Map.fold(fun acd key value -> Map.add key value acd) acc x) symbTable  toAdd 


        //folder function needed for List.fold following
        //The list.fold pretty much does matches the assembly code strings with their corresponding address in memory
        let folder state item =
            match state with
            |counter,lst-> (counter + 4u, (counter,item)::lst)
        let accessibleMemState:MemoryArgument =
            [0x100u..0x4u..0x200u]
            |> List.map (fun a -> WA(a),DataMem(0u))
            |> Map.ofList

        let parsedOutput =
                userInput
                |> List.fold folder (8u,[]) 
                |> snd
                |> List.rev
                |> List.map (fun (a,b) -> (WA(a), (parseLine (secondSym|>Some) (WA(a)) b)))

        
        //this error mapping function is needed as an interface from the different individual modules that return 
        //different errors. It makes all of them of type ErrInstr which is a type defined in this module
        let mapErr fMap execOutput =
            match execOutput with
            |Ok a -> a |> Ok 
            |Error e -> fMap e |> Error
                    
        
        
        /// A recursive function to run the execution of the parsed file. 
        /// Used the value in R15 to find the value of the instruction to run next.
        /// Looks this value up from the parsed list. 
        /// Determines if a condition state has been met.
        /// Calls the correct execute along with itself for the next cpuState, or calls itself again with R15 updated.
        /// Returns a cpuState or Error
        let rec exec (cpu:CPUState) (parsed:(WAddr*Result<Parse<Instr>,ErrInstr>) list) = 
            let parsedMap = parsed|>Map.ofList
            let matchCase case= 
                match case with
                |Ok a -> exec a parsed 
                |Error e -> e |> Error
            let getLocation CPUState=
                getCpuRegValue R15 (getCpuData CPUState)|>WA
            let updateR15 =
                let regValue reg= cpu.DPath.Regs.[reg]
                let reg = Map.ofList [ R0, regValue R0 ; R1, regValue R1 ; R2, regValue R2 ; R3, regValue R3 ; R4, regValue R4 ; R5, regValue R5;
                    R6, regValue R6 ; R7, regValue R7 ; R8, regValue R8 ; R9, regValue R9 ; R10, regValue R10 ; R11, regValue R11 ; 
                    R12, regValue R12 ; R13, regValue R13 ;R14, regValue R14 ; R15, (regValue R15) + 4u ; 
                    ]
                {DPath= {Regs = reg; Fl = cpu.DPath.Fl}; MEMState = cpu.MEMState}
            match parsed|>Map.ofList|>Map.tryFind (getLocation cpu) with
            |Some ins -> 
                match ins with
                |Ok x ->
                    match x.PCond, cpu with
                    |Cne, {DPath = {Fl = {Z=true}}} -> exec updateR15 parsed
                    |Ceq, {DPath = {Fl = {Z=false}}} -> exec updateR15 parsed
                    |Clo, {DPath = {Fl = {C=true}}} -> exec updateR15 parsed
                    |Chs, {DPath = {Fl = {C=false}}} -> exec updateR15 parsed
                    |Cpl, {DPath = {Fl = {N=true}}} -> exec updateR15 parsed
                    |Cmi, {DPath = {Fl = {N=false}}} -> exec updateR15 parsed
                    |Cvc, {DPath = {Fl = {V=true}}} -> exec updateR15 parsed
                    |Cvs, {DPath = {Fl = {V=false}}} -> exec updateR15 parsed
                    |Chi, {DPath = {Fl = {C=false; Z=true}}} -> exec updateR15 parsed
                    |Cls, {DPath = {Fl = {C=true}}} -> exec updateR15 parsed
                    |Cls, {DPath = {Fl = {Z=false}}} -> exec updateR15 parsed
                    |Clt, {DPath = {Fl = {V=true; N = true}}} -> exec updateR15 parsed
                    |Clt, {DPath = {Fl = {V=false; N = false}}} -> exec updateR15 parsed
                    |Cge, {DPath = {Fl = {V=true; N = false}}} -> exec updateR15 parsed
                    |Cge, {DPath = {Fl = {V=false; N = true}}} -> exec updateR15 parsed
                    |Cgt, {DPath = {Fl = {V=true; N = false; Z=true}}} -> exec updateR15 parsed
                    |Cgt, {DPath = {Fl = {V=false; N = true; Z=true}}} -> exec updateR15 parsed
                    |Cle, {DPath = {Fl = {V=false; N = false}}} -> exec updateR15 parsed
                    |Cle, {DPath = {Fl = {V=true; N = true}}} -> exec updateR15 parsed
                    |Cle, {DPath = {Fl = {Z=false}}} -> exec updateR15 parsed
                    |_ ->
                        match x.PInstr with
                        |ISTACK i -> matchCase (LDMSTM.execute cpu i |> (mapErr ERRIMEM))
                        |IMEM i -> matchCase (LDRSTR.execute i cpu|> (mapErr ERRILDRSTR))
                        |IARITH i -> matchCase (Arith.execute i cpu|> (mapErr ERRIΑRITH))
                        |ILOG i -> matchCase (Log.execute i cpu|> (mapErr ERRILOG))
                        |ICOMP i -> matchCase (Comp.execute i cpu|> (mapErr ERRICOMP))
                        |IMOVE i -> matchCase (Move.execute i cpu|> (mapErr ERRIMOVE))
                        |IMISC i -> matchCase (Misc.execute i cpu|> (mapErr ERRIMISC))
                        |_ -> failwithf "Something went terribly wrong"
                |Error e ->
                    match e with
                    |ERRIMEM(x) -> mapErr ERRIMEM (Error x)
                    |ERRILDRSTR(x) -> mapErr ERRILDRSTR (Error x)
                    |ERRIΑRITH(x) -> mapErr ERRIΑRITH (Error x)
                    |ERRILOG(x) -> mapErr ERRILOG (Error x)
                    |ERRICOMP(x) -> mapErr ERRICOMP (Error x)
                    |ERRIMOVE(x) -> mapErr ERRIMOVE (Error x)
                    |ERRIMISC(x) -> mapErr ERRIMISC (Error x)
                    |ERRTOPLEVEL(x) -> mapErr ERRTOPLEVEL (Error x)
            |None -> cpu|>Ok
                      
        exec {DPath = dataPathState; MEMState = accessibleMemState} parsedOutput
