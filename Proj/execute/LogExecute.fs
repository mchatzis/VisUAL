module LogExecute
    open CommonData
    open Log
    open Data
    open FlexOp2
    open ArithExecute
 
    //-----------------PERFORM FINAL OPERATIONS----------------------------//            
    let simulateLOG (instruction:Instr) (cpuData:DataPath) : DataPath = 
        //get unint32's from registers 
        let op2' = flexOp2 instruction.In2 cpuData 
        let op1'= getCpuRegValue instruction.In1 cpuData
        let out = match instruction.Class with 
                    | {Op = EOR; Not = _} -> op1' ^^^ op2'
                    | {Op = ORR; Not = _} -> op1' ||| op2'
                    | {Op = AND; Not = NoNot} -> op1' &&& op2'
                    | {Op = AND; Not = Not} -> op1' &&& (~~~op2') //for implementing BIC
        let outN = checkLess0 out
        let outZ = checkFor0 out
        let outC = 
            match workOutCarry instruction.In2 cpuData with
            |1u-> true
            |0u-> false
            |_ -> failwithf "Should never occur as workoutCarry only outputs 1u and 0u"


        let newFlags = match instruction.Suffix with 
                        | WriteFlags ->  {cpuData.Fl with N = outN; Z = outZ; C = outC}
                        | NoWriteFlags -> cpuData.Fl

        {
            Fl = newFlags;
            Regs = Map.add instruction.Dest out cpuData.Regs;
        }
