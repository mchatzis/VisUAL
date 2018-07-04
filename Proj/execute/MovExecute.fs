module MovExecute
    open CommonData
    open Move
    open Data
    open FlexOp2
    open ArithExecute

    let simulateMOV (instruction:Instr) (cpuData:DataPath) : DataPath = 
        //get unint32's from registers 
        let op2' = flexOp2 instruction.In2 cpuData 
        let out = match instruction.Class with 
                    | {Op = MOV; Not = NoNot} -> op2'
                    | {Op = MOV; Not = Not} -> ~~~ op2'
                    | {Op = TST; Not = _} -> (getCpuRegValue instruction.Dest cpuData) &&& op2'
                    | {Op = TEQ; Not = _} -> (getCpuRegValue instruction.Dest cpuData) ^^^ op2'

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
        let Regs = 
            match instruction.Class with
            |{Op = MOV; Not = _} ->  Map.add instruction.Dest out cpuData.Regs;
            |_ -> cpuData.Regs

        {
            Fl = newFlags;
            Regs = Regs;
        }
