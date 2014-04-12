PLLangVM
========
<b>Description:</b><br>
Programming langauge implementation and design final project with llvm!

<b>Team:</b><br>
Luke Metz<br>
Chris Lee<br>



Instructions for Compiling
===========================

Dependencies<br>
(1) LLVM  (llvc)<br>
(2) Python (os, sys)<br>

<b><i>Compiling</i></b>
Code that will be compiled is called test.plg; however, it is simply an argument passed into the command for compilation. 

For Windows users, (plang.cmd)<br>
`plang [file] [-r to execute]`

For Linux users, <br>
`./plang [file]`

For OSX users, <br>
`Sorry.`

The Project
==========================
Programming langauge implementation and design final project with llvm using code from previous lectures. 

<b>Project structure:</b><br>
<ul>
<li>build</li>
 llvm and c compilation outputs<br>
<li>src</li>
 SML back-end for our language<br>
<li>run.py</li> 
 Python script that runs the compilation commands<br>
<li>run.sml</li>
 SML script that handles the input/output of the .plg script. <br>
<li>test.plg</li>
 Test code written in our plang language. <br>
 </ul>

<b>What we have implemented:</b>
Currently, we have basic mathematic operations implemented.<br> 
We have a naive implementation of Let, singl argument function declarations and function calling, and conditionals.<br>
 <br>
Unfortunately as of now, we are only handling 32-bit integer types. 

<b>Next Steps:</b>
Implement environments and closures using use of structures and pointers. <br>
Implement data types using structures (hopefully, unions) with encoded type information. <br>
