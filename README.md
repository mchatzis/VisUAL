# ARM Assembly Language Emulator
[*ARM*](https://www.arm.com/products/silicon-ip-cpu?utm_source=google&utm_medium=cpc&utm_campaign=2019_enterprise-marketing_mk30_na_brand-solutions_bol_awa&utm_term=arm%20technology&gclid=Cj0KCQiA6t6ABhDMARIsAONIYywHOBIjWjvlhM2sMU7O_jBkCilNUscSFEx69NlLGgMOF4LDl5MNXSMaAkPgEALw_wcB) microprocessors use the ARM UAL instruction set, an assembly code language targeted at ARM devices. These instructions control the values of the registers of an ARM microprocessor and its memory registers. This project emulates this process, which happends *behind the scenes* when a processor is programmed in a higher level language, like *C*. 

### Disclaimer

The project in this branch is not entirely mine. It was part of a group project for university.
It has been uploaded as a whole for the sake of completeness. The project worked at the time of presentation (around Feb 2018), but it has not been maintained since.

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


### ABOUT THE PROJECT AND MY EXPERIENCE
___________________________________
The project was in the field of software engineering, more specifically in the paradigm of functional programming in F#. The goal was to create an ARM UAL emulator. In simple words, the team had to build a program that visualized the processes happening in an ARM processor’s UAL unit for a given set of assembly code instructions. These can be split into two categories, memory operations and arithmetic operations. In the individual phase I was in charge of the memory operations and in the group stage I was in charge of the top-level architecture, more specifically creating a top-level file that would define all interfaces and connect all modules in a way that captures the complex process flow of an ARM UAL unit. The final touch was the testing of the whole program against a specified testbench and the connection to a UI. 








