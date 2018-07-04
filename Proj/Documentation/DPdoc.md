
# Log.fs : Module Log : Parsing and implementation of MOV, MVN, TEQ, TST
<h4 style = margin-left:40px;margin-bottom:0px> Specification for this modules instructions </h4>

```PHP      
let mooAndTestSpec = {
	InstrC = MOO
	Roots = ["MOV"; "MVN"; "TST"; "TEQ"]
	Suffixes = ["" ;"S" ] 
}

```

<h4 style = margin-left:40px;margin-bottom:0px> Line input formats </h4>

```PHP
(MOV/MVN){S}{cond} dest, op1 {, SHIFT_op #expression}
(TST/TEQ)   {cond} op1, op2  {, SHIFT_op #expression}
```
Due to a Similar parsing structure these four operations can be parsed the same. 
After seperation from the root, the remaining *opString* is split up by the seperating commas and stripped of white space before
the type of operands can be determined by the length of the opString array.
Each command **MOV** and **MVN** has the option of a Suffix which will set the *N*, *Z* and *C* Flags when appropriate.
For **TST** and **TEQ** however, the suffix *"S"* is implicit.
The *V* flag is **Always** preserved in this module.
**C** is only set when a carry occurs in the Shift op1 expression calculation ortherwise the Carry flag is preserved.

Errors throughout my indivdual contribution, where possible, will be in this format so the origin of the error is easier located and fixed.
```FSharp
///remove # and return uint32
let litStringToUint32 (op:String) = 
	match op with 
		| Prefix "#" suf -> Ok ((strReplAll suf '&' "0x") |> uint32)
       
		| _ -> Error (sprintf "Incorrect prefix on literal input, expecting '#', given %A: Log.litStringToUint32" op)
```

<h4 style = margin-left:40px;margin-top:10px> Summary of operations </h4>
<table style = "text-align:center; margin-left:20px; margin-right:20px;">
  <tr>
    <th>Root</th> <th>Suffix </th> <th>Comp for N<p>(Negative)</p></th> <th>Comp for Z<p>(Equal to)</p></th> 
			<th>Output<p>type</p></th><th>Handled<p>by func</p></th> <th> Notes</th>
  </tr>
  <tr>
    <td>MOV</td> <td>Sets flags</td> <td>op1 w/shift</td> <td>dest = op1 w/shift</td> <td>DataPath</td> <td>mov </td> <td> </td>
  </tr>
  <tr>
    <td>MVN</td> <td>Sets flags</td> <td >op1 w/shift</td> <td>dest = op1 w/shift</td> <td>DataPath</td> <td>mov </td><td>Inverted op1</td>
  </tr>
  <tr>
    <td>TEQ</td> <td>Implicit</td> <td>op1 EOR op2 w/shift</td> <td>op1 = op2 w/shift</td> <td>DataPath</td><td>teqst </td> <td> </td>
  </tr>
  <tr>
    <td>TST</td> <td>Implicit</td> <td>op1 AND op2 w/shift</td> <td>op1 = op2 w/shift</td> <td>DataPath</td><td>teqst</td> <td> </td>
  </tr>
</table>



<p style = margin-bottom:0px> 
	For the sake of neatness I have changed the order of the *Flag* type in commonData.fs to match that of VisUAL's representation.
</p>

```Fsharp
type Flags = { N: bool; Z: bool; C:bool; V:bool }
```
The mov function handles either MOV or MVN dependant on the boolean input doNot. This is seen also in teqst
with the boolean chosen as eq_st. The detailed operation of these functions is commented though them.

```FSharp
let mov (dest:RName) (op2:OpTwo) (doNot:bool) (cpuData:DataPath<'INS>) (suf:Set) : DataPath<'INS>

let teqst (op1:RName) (op2:OpTwo) (eq_st:bool) (cpuData:DataPath<'INS>) : DataPath<'INS> 
```

The most notable points of the these functions are 

1. Their output type: **DataPath** will be updates as the main calls each operation. It seems logical
	for all operations to return this structure so that each line can deal with it step by step in the top level without 
	any race conditions.  
2. The calculation of the Carry flag and it's parsing.

### flexOp2.fs : Module flexOp2 : Implementation of flexible operands 

```fsharp
///literal value = (K % 256) rotated right by (R % 16)*2. 
type Literal = {K: uint32; R: int}

///possible shifts
type ShiftOp = LSL | LSR | ASR | ROR 

///type thing = Number | regval
type Dat = Num of Literal | Reg of RName

///Shifted registers are either shifted by 
type Op2Option = NotRRX of ShiftOp*Dat | RRX  

///Op2 is either an immediatoe or two types of flexible op2, shifted register or an unshifted register
type OpTwo = Immediate of Literal | Rs of RName*Op2Option | Rm of RName

///Flexible operand2 returns tuple (data from flexOp:Uint32, Carry flag:bool) 
let flexOp2 (op2: OpTwo) (cpuData : DataPath<'INS>)  : uint32*bool = 
    match op2 with
        | Immediate x ->  (litToUint32 x, false)
        | Rm regName -> (Map.find regName cpuData.Regs, false)
        | Rs (regName, option) -> match option with
                                        | RRX -> (compRRX regName cpuData )               //RRX func
                                        | NotRRX (LSL, x) -> (compLSL regName cpuData x ) //LSL func
                                        | NotRRX (LSR, x) -> (compLSR regName cpuData x ) //LSR func
                                        | NotRRX (ASR, x) -> (compASR regName cpuData x ) //ASR func
                                        | NotRRX (ROR, x) -> (compROR regName cpuData x ) //ROR func
```
Although this function and the type structure is the center of the 2nd operand calculation. There are a lot of steps to get
both into and through the function. Once the op2 is isolated by the parser it must first be seperated into one of the three *OpTwo* types. 
As previously mentioned, matching the length of the opList with 2 means it must be a register and either a literal (Immediate) or a register (Rm).
Both of these have to be found from small functions in *Log.stringsToOpTwo*. Else if the opList has length three, it must be a register with a shift. 
This is where we now find the Op2Option that fortunatly after stripping all the zeros will always have a string length of 3. After this is isolated 
further matching in "Log.getData" distinguishes between the final literal or register value. 
During the writing of this type deconstruction. I had issues with parseing the Literal input. I used a combination of 3 functions to remove the *'#'* 
character from the string and deal with the irregular format of *'&'* to represent a hex *'0x"* in the end. Initial attempts were to split the string
into a *char array* and perform searches on it. This proved to be an issue when it was later converted back into a *string* and piped into the *uint32* 
type. An expetion would throw on the piping and I couldn't see any reason for it. Later used a Prefix finder and used a string mapping helper. 
*(See MISC HELPER FUCTIONS in Log.fs)*

### Program.fs - Entry point source file containing all tests
Once *flexOp2* was finally working, with simple tests done in *Program.testRs* using an all zero *DataPath* map and dummy line data, *testMOOAndTSTParse* 
allowed me to work through white space issues and previous mentioined prefixes to Literal inputs. 

##### The main testing however is done in *finalOpTest*: 

* MOV and MVN register checks: Compare data in final register based on the input expression. Used to confirm the function *Log.putInReg* was functioning
	correctly and that MVN would correctly invert the data before it went into the register. ALso ensures that no flags are changed when they shouldn't be.
```fsharp
MOVtst "MVN inverts" R0 (Immediate {K = 0x0u; R = 0}) true testCPUData true 0xFFFFFFFFu {N = true; Z = false; C = true; V = true}
```
* Flags Z and N checks: Looks at inputs with the suffix boolean set true for MVN and MOV or for the implicitly implied set for TEQ and TST. Looks at both 
	setting the correct flags logically in *mov* and *tsteq* and that both Z & N flags will overwrite without effecting the C and V flags. Tests
	*Log.setFlags* and it's helper function *Log.checkAndSet*. The testCpuData was changed to accomidate these tests so that situations could be created
	 which made these test cases possible. 
```fsharp
TEQtst "TST sets negative flag" R0 (Immediate {K = 0xFFu; R = 4}) false testCPUData_2 {N = true; Z = false; C = true; V = true}
```

* Flag C checks: A fairly big test. Checks the more recently added tupled carry flag output of *flexOp2* and its corresponding overwriting capabilites
	 when used in suffex set/implicit final operation functions. 
```fsharp
TEQtst "TST sets Carry flag #2" R0 (Rs (R0, NotRRX (LSR, Num {K = 1u; R = 0}))) false testCPUData_4 {N = false; Z = true; C = true; V = true}
```

```Javascript
[22:16:54 INF] EXPECTO? Running tests... <Expecto>
[22:16:55 INF] EXPECTO! 27 tests run in 00:00:00.4427901 - 27 passed, 0 ignored, 0 failed, 0 errored. Success! <Expecto>
```
# ARITH
## Instructions
- ADD
- SUB
- ADC
- SBC
- RSC
- RSB
---
## Add and subtract

All commands can have there name with a **suffix** and __condition__.
**Condition** type is already dealt with in common code. <br>
**Suffix** can be a boolean value of true or false depending on whether not it is set. **True** if set **False** if not.

The instruction type can be defined as a collection of 6 parts.
```F#
type Instr =  {
        Instruction: Arith //Math instruction split into constituent parts
        Suffix: Result<Set,string> //Suffix
        OutputReg: Result<RName,string> //Output location
        Input1: Result<RName,string> // Input location
        Input2: Result<OpTwo,string> //FlexOp2 input
        CarryFlagInvert: bool //Has inversion Occured
    }
```
Each of these three parts will be further dissected into their own types to allow for the information to me more easily used.

#### Maths Commands
###### Roots
All the **maths** roots have three basic functions they implement.
- Add or subtract
- Standard or Reverse
- Standard or Carry

Due to this  can create a type with a field of each of these functions.
```f#
type Arith= {
    Op: Result<Operation,string>
    Rev: Result<Reverse,string>
    Car: Result<Carry,string>
}
```
**Operation** would be a DU between ADD and SUB
**Reverse** would be a boolean. **True** is **Reverse** is set <br>
**Carry** would also be a boolean. **True** if **Carry** is set <br>

Can use maps for each case to assign each instruction with what is should return. Then can use a function taking in the map along with the root to determine the state of the Arith type.

###### Suffix

If the suffix is set this can easily be seen through a map look up as above.
The **suffix** will have a state of "S" or "". Anything else should return and error. Since the **suffix** is either set or not set it makes sense to use a boolean type for it.


###### Implementation
All instructions have 3 inputs. The three base inputs are:
- Ouput Register
- Input 1 - Must be a register
- Input 2 - Flexible operand 2
The flexible operand can be considered similar to the worksheet 3 tick.

The Operands are passed in as a string. All the operands will be separated by a ,
and as such a `String.Split ','` command will nicely separate each of the  operands. This will split up Input 2 if a flexible operand 2 is used and will need to be dealt with.

```F#
let x = operands.Split ','
```
The above code will have a list in x in the form `x =["ROut"; "Input1", "Input2", Flexible Operand 2]`. Each of these can then be indexed appropriately and can be added to the `Instr` type.

To deal with the flexible operand specification some simple string manipulation can be used. Firstly a check of whether the line has 3 or 4 instructions should be used. Then depending on the result of this we can return a `Result<string,string>`. This will return a string of either the register and shift as a single string or a single register or literal.

```F#
let in2 =
    match opList.Length with
    | 4 -> flexOp with shifted
    | 3 -> literal or reg
    | _ -> Error "Not a valid Flexible Operand 2 instruction"
```
**FlexOp2**
The second input for the mathematical functions will be a flexible operand 2 and as such it would make sense to define a type to deal with this.

To increase with simplicity at later stages Jack Pearson's type structure was adopted:
```F#
type Literal = {K: uint32; R: int}

//possible shifts
type ShiftOp = LSL | LSR | ASR | ROR

//type thing = Number | regval
type Dat = Num of Literal | Reg of RName

//Shifted registers are either shifted by
type Op2Option = NotRRX of ShiftOp*Dat | RRX  

//Op2 is either an immediatoe or two types of flexible op2, shifted register or an unshifted register
type OpTwo = Immediate of Literal | Rs of RName*Op2Option | Rm of RName

let validShiftOps = ["LSL", LSL; "LSR", LSR; "ASR", ASR; "ROR", ROR] |> Map.ofList
```
Using **Op2** as the type for input 2 requires for the manipulation of the string from above to this type.

It is required to be able to work out a single **uint32** value from a **flexOp2** instruction. As such some computation was required in **FlexOp2**. The main body of this determines the type of the **flexOp2** instruction and then performs it on data returning the **uint32** value.

```F#
let toUint32 (lit:Literal):uint32 = lit.K >>> (lit.R*2) ||| lit.K <<< (32 - (lit.R*2))

let performShift regOut shift cpuData (doShift:uint32->int->uint32) ror=
    let sValue=
        match shift with //Shift by reg or literal and get the uint32 value of that shift
        |Num literal -> ((toUint32 literal)|>int)
        |Reg register -> (cpuData.Regs.[register]|>int)%32
    let x = cpuData.Regs.[regOut] //Get uint32 value of data to be shifted
    match ror with
    |true-> doShift x sValue
    |false->if sValue > 31 then 0u else doShift x sValue //Perform shift on the uint32 value and return new value

let doROR a n =
    (a >>> n) ||| (a <<< (32-n))

//Return the uint32 value of a flexOp2 instruction
let flexOp2 op2 cpuData =
    match op2 with
    |Immediate literal -> toUint32 literal
    |Rm register -> cpuData.Regs.[register]
    |Rs (regOut, option)->
        match option with
        |NotRRX (shift, value)->
            match shift with
            |LSL -> performShift regOut value cpuData (<<<) false
            |LSR -> performShift regOut value cpuData (>>>) false
            |ASR -> performShift regOut value cpuData (fun a b -> (int a) >>> b |> uint32) false
            |ROR -> performShift regOut value cpuData doROR true
        |RRX->
            match cpuData.Fl.C with
            |true -> (cpuData.Regs.[regOut]>>>1) ||| (1u<<<31)
            |false -> cpuData.Regs.[regOut]>>>1
```
**Simulation**
The simulation takes the custom instruction type along with the current cpuData and returns the updated cpuData or an error code.

To do this it is important to isolate parts of the data stored in the instruction. The first part to isolate is the **Suffix** as this determines whether or not flags are updated.
The second part to isolate is the **OutputReg** as this is the only register (bar R15 if not output) that will be updated.

The math function:
```F#
let math (instruction:DP.Instr) (cpuData:DataPath) set=
    match instruction.Instruction, instruction.CarryFlagInvert with
    |{Op = Ok Add; Rev =_; Car =Ok carry }, invert -> add (errorResultRName instruction.Input1 cpuData) (errorResultFlexOp2 instruction.Input2 cpuData) (+) carry cpuData invert set Add //Add
    |{Op = Ok Sub; Rev =Ok false; Car =Ok carry }, invert -> sub (errorResultRName instruction.Input1 cpuData) (errorResultFlexOp2 instruction.Input2 cpuData) (-) carry cpuData invert set Sub //Normal sub
    |{Op = Ok Sub; Rev =Ok true; Car =Ok carry }, invert -> sub (errorResultFlexOp2 instruction.Input2 cpuData) (errorResultRName instruction.Input1 cpuData) (-) carry cpuData invert set Sub //Sub inputs reversed
    |{Op = Error e; Rev =_; Car =_ }, _ -> Error e
    |{Op = _; Rev =Error e; Car =_ }, _ -> Error e
    |{Op = _; Rev =_; Car =Error e }, _ -> Error e
```
This function determines whether to add or subtract along with the order of the inputs and the state of the carry flag. It also determines whether an instruction has been inverted due to using an inverse literal.

`add` and `sub` then perform either the addition or subtraction on the literals and returns the flags set either as before if they should not be updated or the new ones if they should.
This data is then packeged back into a DataPath.  


# Arith specification
Valid Arith instructions are:</br>
* `ADD`
* `ADC`
* `SUB`
* `SBC`
* `RSB`
* `RSC`

`Instr{S}{cond} {Rd}, Rn, Flexible Operand 2`</br>
Where:
* `Instr` is any valid arith instruction
* `S` is optional suffix to determine if flags should be changed. Case sensitive. "S" will work "s" will not. valid inputs are "S" or ""
* `cond` is optional condition (Not currently implemented and will be done at top level. Parsing is tested though)
* `Rd` is the destination register. the result will overwrite any data in here
* `Rn` is the register holding the first operand. This data will remain unchanged

The `ADD` instruction adds the values in `Rn` and `Flexible Operand 2`.

The `SUB` instruction subtracts the value of `Flexible Operand 2` from the value in `Rn`.

The `RSB` (Reverse Subtract) instruction subtracts the value in `Rn` from the value of `Flexible Operand 2`.

The `ADC` (Add with Carry) instruction adds the values in `Rn` and `Flexible Operand 2`, together with the `carry flag`.

The `SBC` (Subtract with Carry) instruction subtracts the value of `Flexible Operand2` from the value in `Rn`. If the carry flag is clear, the result is reduced by one.

The `RSC` (Reverse Subtract with Carry) instruction subtracts the value in `Rn` from the value of `Flexible Operand 2`. If the carry flag is clear, the result is reduced by one.

### Flexible Operand 2
The `Flexible Operand2` has two possible forms:
* *Literal*
* *Rm{, shift}*

Where:
* *Literal*
    - is an expression evaluating to a numeric constant. The constant must correspond to an 8-bit pattern rotated by an even number of bits within a 32-bit word.
    - Certain pairs of instructions (`ADD` and `SUB`, `ADC` and `SBC`) are equivalent except for the negation or logical inversion of *Literal*.
    If a value of *Literal* cannot be expressed as a rotated 8-bit pattern, but its logical inverse or negation could be, the assembler substitutes the other instruction of the pair and inverts or negates *literal*.
    - This is done in a way that:
        - `SBC R0, R0, #-3 = ADC R0, R0, #2`
        - `ADD R0, R0, #-3 = SUB R0, R0, #3`
    - Mathematical operations are not valid. eg `#2*3` is not a valid input
    - Input can be of type:
        - Decimal with # identifier eg #10 or #-10
        - Hex with #0x identifier eg #0xFF00 or #-0xFF00
        - Binary with #0b identifier eg #0b1100 or #-0b1100
* *Rm*
    - is the register holding the location of the data that can be shifted if set
* *shift*
    - Optional shift that can be applied to Rm. Can be one of:
        - `ASR n`
            - arithmetic shift right `n` bits. if n > 31 then 0 .
        - `LSL n`
            - logical shift left `n` bits. if n > 31 then 0.
        - `LSR n`
            - logical shift right `n` bits. if n > 31 then 0.
        - `ROR n`
            - rotate right `n` bits. n is any valid positive *Literal*.
        - `RRX`
            - rotate right one bit, with extend.
        - type </br>
            where:
            - type </br>
                is one of `ASR`, `LSL`, `LSR`, `ROR`.
            - Rs</br>
                is an ARM register supplying the shift amount. Only the least significant byte is used.

### Type Case Sensitivity
* All instructions must be written in full caps.
* All register must be prefixed by a **capital R, eg R10** otherwise they will be seen as errors.
* All literals must be prefixed with a # as shown above in literal spec.

### Testing
#### Plan
* Test parsing in detail.
* Test FlexOp2 in detail
* Then compare simulate to visual using parse and FlexOp2 to generate the data. Since they are both tested any errors will have to be in simulate. Use large number of random tests to increase likely hood of finding errors.

#### Tests
Tests are run using **VProgram.fs** as the entrypoint.
All tests are written in **VTests.fs**

|Test area |Specific Test     | Expected Outcome| Implemented|
|--------- |--------|----------|---|
|**Parsing**| Split Operands| Check that operands can be split on , into seperate Rd,Rn,Flexible Operand2 | Yes|
|**Parsing**| Return Custom Instr| Check that given lineData with InstrC parse returns the expected custom type| Yes|
|**Parsing**| FlexOp parsing| Check that given a shift it parses correctly| Yes|
|**FlexOp2**| Literal consistency| Check that literals are consistent with FlexOp| Yes|
|**FlexOp2**| FlexOp returns uint32| Given an type of Op2 the flexOp should return a literal of equivalent value| Yes(Although some more random testing would be nice)|
|**FlexOp2**| literals| Check that given a literal if inversion is required it happens correctly| Yes|
|**Simulate**|x+y = y+x| check basic add property| Yes|
|**Simulate**|Check add function| Add x y = x+y| Yes|
|**Simulate**|Check adc function| Adc x y = x+y+Carry| Yes|
|**Simulate**|Check add function| sub x y = x-y| Yes|
|**Simulate**|Check add function| sub x y = x+y+carry-1| Yes|
|**Simulate**|Check add and sub inverse| x+-y = x-y| Yes|
|**Simulate**|Check add function| Add x y = x+y| Yes|
|**Simulate**|Check 2sComplement| Check whether can determine if a number is 2s complement positive or negative| Yes|
|**Simulate**|Check add and sub inverse| x+y = x--y| Yes|
|**Simulate**|Get reg value| Should be able to get the uint32 value of reg without changing it| Yes|
|**Simularte**| Check Visual| Any valid instruction should return the same value as visual.| Yes|


The tests all pass and with such a large number of random tests along with carefully selected unit and property tests it is highly unlikely any errors have gone through the cracks.
```javascript
[20:05:10 INF] EXPECTO! 13,584 tests run in 01:00:08.2197093 - 13,584 passed, 0 ignored, 0 failed, 0 errored. Success! <Expecto>
```
sources used to help with specification:</br>
http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0204j/Cihbeage.html</br>
http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0204j/Cihcjfjg.html






