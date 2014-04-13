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

<b><i>Compiling</i></b><br>
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
 SML back-end for our language. The bulk of this is from your functional
programming language. We are mainly working on compile-llvm.sml.<br>
<li>run.py</li>
 Python script that runs the compilation commands<br>
<li>run.sml</li>
 SML script that handles the input/output of the .plg script. <br>
<li>test.plg</li>
 Test code written in our plang language. <br>
 </ul>

<b>What we have implemented:</b><br>
Currently, we have basic mathematic operations implemented.<br>
We have a naive implementation of Let, single argument function declarations and function calling, and conditionals.<br>
 <br>
Unfortunately as of now, we are only handling 32-bit integer types.

<b>Next Steps:</b><br>
Implement data types using llvm structures (hopefully, unions) with encoded type information. Our end goal in this respect is to have primitive dynamic type checking.<br>
Implement environments. These will be heap allocated structures that
contain pointers to what ever variables are in scope.
Finally, with this environment set up, we will implement closures and then currying. <br>
