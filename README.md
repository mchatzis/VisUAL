# Disclaimer

The project in this branch is not entirely mine. It was part of a group project for university.
It has been uploaded as a whole for the sake of completeness.

### PARTS THAT ARE MINE
___________________

I hace created all the files relevant to the LDM and STM operations:

- LDM_STM.fs (path: Proj/parse/...)
- LDRSTR.fs (path: Proj/parse/...)
- LDMSTMTest.fs (path: Proj/tests/...)
- MemDoc.md (path: Proj/Documentation/...)

Two people contributed to the file below (Me: 50%, teammate: 50%, based on skeleton from our professor):
- CommonTop (path: Proj/common/...)

Explanation: The CommonTop is the module that connects all. I created the interfaces and function definition structures and my teamate filled in the function execution instructions. In other words, I focused on high level architecture while he focused on low-level implementation. It is also worth mentioning that our professor helped by giving us an idea of how the interfaces should look like in the end(skeleton).


### ABOUT THE PROJECT AND MY EXPERIENCE
___________________________________
The project was in the field of software engineering, more specifically in the paradigm of functional programming in F#. It was a group project in university. The goal was to create an ARM UAL emulator. In simple words, the team had to build a program that visualized the processes happening in an ARM processor’s UAL unit for a given set of assembly code instructions. These can be split into two categories, memory operations and arithmetic operations. In the individual phase I was in charge of the memory operations and in the group stage I was in charge of the top-level architecture, more specifically creating a top-level file that would define all interfaces and connect all modules in a way that captures the complex process flow of an ARM UAL unit. The final touch was the testing of the whole program against a specified testbench and the connection to a UI. 








