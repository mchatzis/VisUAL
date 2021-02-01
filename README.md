# ARM Assembly Language emulator
Usually, when we program in higher level languages (eg. *C++*), we do not care about how the code gets transformed from english words (e.g. *int my_int*) to **assembly code instructions (eg. *MOV R1 1*)** and finally to machine code (eg. *000000 00001 00010 00110 00000 100000*). It gets taken care of automatically.

This project's goal was to build a program that emulates what is happening behind the scenes at the assembly code level. More specifically...

[*ARM*](https://www.arm.com/products/silicon-ip-cpu?utm_source=google&utm_medium=cpc&utm_campaign=2019_enterprise-marketing_mk30_na_brand-solutions_bol_awa&utm_term=arm%20technology&gclid=Cj0KCQiA6t6ABhDMARIsAONIYywHOBIjWjvlhM2sMU7O_jBkCilNUscSFEx69NlLGgMOF4LDl5MNXSMaAkPgEALw_wcB) microprocessors use the ARM UAL instruction set, an assembly code language targeted at ARM devices. These instructions control the values of the registers of an ARM microprocessor and its memory registers. A sub-part of the whole ARM instruction set can be found in [this list](https://salmanarif.bitbucket.io/visual/supported_instructions.html) of instructions.

Finally, to give an example of an ARM instruction:
LDM  R8, {R0,R2,R9}  

Explanation: R8 is a register that contains the memory address we want to access. Instruction: Access that address and copy its contents to R0. Then increment the address to get the next value and copy it to R2. Then increment again to copy the next address to R9. 

For detailed documentation on the ARM instruction set visit this [*ARM Infocenter page*](https://developer.arm.com/documentation/dui0552/a/the-cortex-m3-instruction-set/memory-access-instructions/ldm-and-stm)

For documentation on the code found in this repository, please check the [documentation folder](https://github.com/mchatzis/VisUAL/tree/master/Proj/Documentation)

### Disclaimer

The project in this branch is not entirely mine. It was part of a group project for university.
It has been uploaded as a whole for the sake of completeness. The project was tested and worked at the time of presentation (around Feb 2018), but it has not been maintained since.

### PARTS THAT ARE MINE
___________________

I have created all the files relevant to the LDM and STM instructions. These are instructions that deal with memory management. Instructions that start with 'L' load from memory registers, while instructions that start with 'S' (eg. STM) store into memory registers. Please read on for further explanations.

Namely:
- LDM_STM.fs (path: Proj/parse/...)
- LDRSTR.fs (path: Proj/parse/...)
- LDMSTMTest.fs (path: Proj/tests/...)
- MemDoc.md (path: Proj/Documentation/...)

Two people contributed to the file below (Me: 50%, teammate: 50%, based on skeleton from our professor):
- CommonTop (path: Proj/common/...)

Explanation: The CommonTop is the module that connects all. I created the interfaces and function definition structures and my teamate filled in the function execution instructions. In other words, I focused on high level architecture while he focused on low-level implementation. It is also worth mentioning that our professor helped by giving us an idea of how the interfaces should look like in the end(skeleton).








