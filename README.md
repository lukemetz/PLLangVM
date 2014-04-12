PLLangVM
========
Description:
Programming langauge implementation and design final project with llvm!

Team:
Luke Metz
Chris Lee



Instructions for Compiling
===========================

Dependencies
(1) LLVM  (llvc)
(2) Python (os, sys)

Compiling.
Code that will be compiled is called test.plg; however, it is simply an argument passed into the command for compilation. 

For Windows users, (plang.cmd)
plang [file] [-r to execute]

For Linux users, 
./plang [file]

For OSX users,
Sorry.

The Project
==========================
Programming langauge implementation and design final project with llvm using code from previous lectures. 

<b>Project structure:</b>
build
	llvm and c compilation outputs
src
	SML back-end for our language

run.py 
	Python script that runs the compilation commands

run.sml 
	SML script that handles the input/output of the .plg script. 

test.plg
	Test code written in our plang language. 

<b>What we have implemented:</b>
 Currently, we have basic mathematic operations implemented. 
 We have a naive implementation of Let, singl argument function declarations and function calling, and conditionals.
 
 Unfortunately as of now, we are only handling 32-bit integer types. 

<b>Next Steps:</b>
 Implement environments and closures using use of structures and pointers. 

 Implement data types using structures (hopefully, unions) with encoded type information. 