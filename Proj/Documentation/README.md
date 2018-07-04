# How to read this file?

The following file contains the functionality of the program at hand.
It is split into the various groups of instructions.
Note: The execute folder is practically useless for someone wanting to assemble our program,
since the execute functions is combined in the same module with the parse functions and can be found
in Parse folder. Hence, just Parse folder needed.

# Proud of
* Robustness of individual modules (thoroughly tested).
* Understanding and implementing interfaces of individual modules with Top level file.
* Having a working visual emulator, even if not error-free or fully operational.

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

# Logical Specification
Valid logical instructions are:
- `EOR`
- `AND`
- `ORR`
- `BIC`

`Instr{S}{Cond} {Rd,} Rn, Flexible Operand 2`
Where:
- `Instr` is any valid logical instruction
- `S` is optional suffix
- `cond` is optional condition case.
- `Rd` is the destination register and will be overwritten
- `Rn` is the register holding the first operand

The `EOR` instruction does a logical exclusive or on the bits of `Rn`and `Flexible Operand 2` placing result in `Rd`
The `AND` instruction does a logical and on the bits of `Rn`and `Flexible Operand 2` placing result in `Rd`
The `ORR` instruction does a logical or on the bits of `Rn`and `Flexible Operand 2` placing result in `Rd`
The `BIC` instruction does a logical AND on the bits of `Rn`and the inverse bits of`Flexible Operand 2` placing result in `Rd`

# Move Specification
Valid logical instructions are:
- `MOV`
- `MVN`


`Instr{S}{Cond} {Rd,} Flexible Operand 2`
Where:
- `Instr` is any valid logical instruction
- `S` is optional suffix
- `cond` is optional condition case.
- `Rd` is the destination register and will be overwritten

The `MOV` instructions places the values of `Flexible Operand 2` in `Rd`
The `MVN` instructions places the values of inverted bits of `Flexible Operand 2` in `Rd`

# Test Specification
Valid logical instructions are:
- `TEQ`
- `TST`

`Instr{Cond} Rn, Flexible Operand 2`
Where:
- `Instr` is any valid logical instruction
- `S` is optional suffix
- `cond` is optional condition case.
- `Rn` is the register holding the first operand

The `TEQ` instructions performs an exclusive or on the bits of  `Flexible Operand 2` in `Rd` setting flags as expected
The `TEQ` instructions performs an and on the bits of  `Flexible Operand 2` in `Rd` setting flags as expected

# Compare Specification
Valid logical instructions are:
- `CMP`
- `CPN`

`Instr{S}{Cond} Rn, Flexible Operand 2`
Where:
- `Instr` is any valid logical instruction
- `S` is optional suffix
- `cond` is optional condition case.
- `Rn` is the register holding the first operand

The `CMP` instructions performs a `SUBS` on  `Flexible Operand 2` in `Rd` setting flags as expected without changing registers
The `CPN` instructions performs a `ADDS` on  `Flexible Operand 2` in `Rd` setting flags as expected without changing registers

# EQU Specification
Valid EQU instructions are:
-`EQU`

`Label Instr #expression`
Where:
-`Instr` is EQU
-`label` is any valid label that isnt an instruction
-`expression`is any valid mathematical expression using integers, +,-,* and /

The `EQU` instruction assigns a label to a value for reference else where in the program.
An `EQU` should always be branched over or placed after an `END` command and should never be executed.
When referencing a label created by an `EQU` command a `#` is required before the label and the line must itself contain a label different to that of the `EQU` instruction. 

# Branch Specification
Valid branch instructions are:
-`B`
-`BL`

`Instr{Cond} label`
Where:
-`Instr` is any valid branch instruction
-`label` is any valid label in the code

The `B` instruction sets the value in `R15` to the address of the label instruction.
The `BL` instruction sets the value in `R15` to the address of the label instruction. and places address of next instruction into `R14`.

#End Specification
The `END` command will set `R15` to `1` ending the program.

# Flexible Operand 2 Specification
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



# LDR/STR specification

Valid instructions are:
* `LDR`
* `STR`

Variants:
`Instr{B}{cond} Rd, [Rn {, OFFSET}]`
`Instr{B}{cond} Rd, [Rn, OFFSET]!`
`Instr{B}{cond} Rd, [Rn], OFFSET`

Where:
* `Instr` is any valid arith instruction
* `B` is optional suffix to determine if byte addressing is enabled. Case sensitive. "B" will work "b" will not. valid inputs are "B" or ""
* `cond` is optional condition (Not currently implemented and will be done at top level. Parsing is tested though)
* `Rd` is the destination register. the result will overwrite any data in here
* `Rn` is the register holding the first operand. This data will remain unchanged
* `OFFSET` is the offset applied to the source address or destination address for load and store instructions respectively. It can be a register, a numerical expression, or a shifted register.

The `LDR` instruction loads the 32-bit word at the memory address defined by `Rn` and `OFFSET` into 'Rd'.

The `STR` instruction stores the 32-bit word in 'Rd' into the memory address defined by `Rn` and `OFFSET`.

The `LDRB` instruction loads an unsigned byte at the memory address defined by `Rn` and `OFFSET` into 'Rd'. (currently only working for least significant byte)

The `STRB` instruction stores the unsigned byte in 'Rd' into the memory address defined by `Rn` and `OFFSET`.

# LDM/STM specification

**A. Functional Instructions**

All instructions following the below form are fully functional and have been tested:
**op{addr_mode}{cond} Rn{!}, reglist** ,where:
* op -> 
    * LDM/STM
* addr_mode -> 
    * EA , ED , FA , FD &  aliases: DB , DA , IB , IA
* cond -> 
    * optional conditional code (see toplevel spec)
* Rn ->
    * Specifies the register on which the memory addresses are based.
* ! ->
    * Is an optional writeback suffix. If ! is present the final address, that is loaded from or stored to, is written back into Rn. 
* reglist ->
    * Is a list of one or more registers to be loaded or stored, enclosed in braces. It can contain register ranges. It must be comma       separated if it contains more than one register or register range, see *Examples*.

**B. Implemented Restrictions**

The following ARM specification's restrictions have been implemented fully:
- Rn must not be PC 
- reglist must not contain SP 
- in any STM instruction, reglist must not contain PC 
- in any LDM instruction, reglist must not contain PC if it contains LR 
- reglist must not contain Rn if you specify the writeback suffix.

**C. Simulation**

As claimed above, the above instructions can parse and simulate according to the ARM specification. The description of the simulation is out of the scope of this document and can be found in the [ARM Infocenter](http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0552a/BABCAEDD.html)

# Common Functionality

**Conditional Execution**
All instructions parse and simulate the 'S' suffix which sets the flags. 
Conditional execution is also fully functional for all instructions.
The above claim worked for all cases until now, but not the full set of combinations of instructions and flags has been tested.

# Type Case Sensitivity
* All instructions must be written in full caps.
* All register must be prefixed by a **capital R, eg R10** otherwise they will be seen as errors.
* All literals must be prefixed with a # as shown above in literal spec.

# Testing
## Plan
* Test parsing in detail.
* Test FlexOp in detail
* Execute code lines and see if they match to visual. Since the testing of parse and flexOp has occured any errors will be soley related to execution

## Tests
Please note that some of the tests require other sections of the code to be removed from the project due to conflicts between testing module and these sections.



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


# GUI specification

Implemented Features:
* Registers and flag interfaces
    - Registers and flags read and update correctly from a fake datapath
* Bin/Hex/Dec viewing of register contents
    - Button to cycle through Binary/Hex/Dec representations of register value
    - Issue: Negative values cause a problem when represented in binary
* Reset button
    - All registers and flags are set to 0 in the current state, this is then reflected in GUI 
* New file
    - Clears editor contents
* Open file
    - Reads txt file in local directory with filename set in input box, and imports to editor
* Save file
    - Writes txt file in local directory with filename set in input box with contents of editor. Creates file if doesn't exist

Partially implemented:
* Run editor
    - Will not compile due to Fable regex match/group collection issue. Lengthy fix as requires rewriting parts of emulator not to use Regex
    - Code to send editor contents and interface with emulator will work as updating from a test datapath works and calling the run function in pure F# works
* Current Instruction/Cycle
    - GUI elements exist. Code to read unwritten due to Fable issue impeding interface
* Initialising registers/flags state
    - All registers and flags can be set by user in GUI
    - This changes the current state and would be very useful for testing
    - Currently this state is not passed to emulator 
    - More protection to when this is accessible is needed
    - Plans to store/import preset states for testing not implemented

Not implemented:
* Error Handling
    - Displaying Error line and a message 
* Step forward/backward
    - Interface issue limited development




sources used to help with specification:</br>
http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0204j/Cihbeage.html</br>
http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0204j/Cihcjfjg.html </br>
http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0552a/BABCAEDD.html
