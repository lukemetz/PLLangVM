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
(1) gcc (convert assembly generated by llvc to executable.)
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
We have a naive implementation of Let, single argument function declarations and function calling, and conditionals, multi argument functions via currying, closures, and first order functions.<br>
<br>
<h3> How it works </h3> </br>
<h4> Compiler </h4>
Keeping with existing designs we worked with in this class, our compiler
works recursively. At a top level, our compiler attempts to compile each
deceleration in `compileDecl`. Declarations have a expression that we
then compile with `compileE`. These two functions are the heart of our
program, and where all the magic happens
<h5> Count / Code Stack (cstack) / Symbol Environment sym_env</h5>
One annoying issue with recursive compilers is they have no real notion
of state or where they are in compiling. Because of this, chain along
state such that its passed in, possibly modified, and returned. The
three pieces of state we do this for are count, cstack, and sym_env.

Count is used to ensure that our llvm variables don't overlap in name.
When creating temporary values, we always number them with count.

Cstack is list of strings that represent code. Each function we do adds
to this cstack. By making this constantly passed and built vs gathered
all the way at the end, we are able to add new function declarations to the
beginning of cstack (like ones needed from anonymous functions),
while continuing with our code at the end of the list.

sym_env keeps track of symbols in our environment.
<h5> Symbol Environment </h5>
<p> The plang-llvm compiler keeps track of the function environment for the let and letfun expressions. In sml, the function environment is denoted by "sym_env" where:
 `sym_env: string * string list` 
 The tuples hold the symbol name and the llvm reference to that symbol. For variables, the symbol name is the variable name and the llvm reference is the register that holds that value. For functions, the symbol is the name of the function and the llvm reference is the global function name (i.e. @func_name). The compiler uses this function environment to pack and unpack the environment within the generated llvm code. Before we call a function, the environment is packed into a %value array and passed into the function. Once inside the function, the symbols are unpacked from the %value array in the order it was packed, recorded in the SML environment. This symbol environment allows us to keep track of scope within let and letfun calls. We are using the same names as whatever the function or variable is called in the inputted plang code. As a result for any temporary registers in the llvm generated code, we use underscores in the name, which are not allowed in the PLANG language. This prevents any conflicts between names. </p>
<p> Using the symbol environment, our compiler can implement currying by simply transforming multiple argument functions to a sequence of EFun expressions. The let and letfun expression are both essentially compiled down to EFun expressions, so we got currying for frees.</p>

<p> Environment </p>
<p> Currying</p>


<h4> Types </h4> </br>
The language we have implemented supports dynamic types. Currently we
only support 3 types, bools, integers, and functions. This limitation is
purely due to lack of time and avoidance of boring work.

These types are implemented via the %value type. `%value = type {i8, i32*}` The first value, the `i8`, denotes the type of the `%value`. In our code, 0 is a None type, 1 is an int type, 2 is a func type, and 3 is a
bool type. The second, `i32*`, is used to represent some pointer to some value, not necessarily a i32. For the case of a int, and boolean, the value is just a i32. For the function type however, it is a pointer to the %func_t type.
`%func_t = type {%value (%value*, %value) *, %value *}`
This type has function pointer as well as a environment list.

In our code, all functions have a `%value (%value*, %value)` syntax, Or a
function always returns a `%value`, and takes in a pointer to a `%value`
and a `%value`. The pointer to `%value`, the first argument, is the
environment. This is used only in closures that capture the environment.
This standardized api allows us to easily call both functions and
closures with similar syntax by just passing in a null pointer when an
environment is not needed.

<h4> Closures / first class functions </h4>
In the processes of mapping a function language to a more procedural
language, first class functions are always tricky. In our
implementation, when an anonymous function is created, a new function is
declared. This function has the name `@func_n`, where n is a counter
used to ensure no definitions overlap in name. When storing the
function, first that function pointer is obtained. Next, the size of the symbol environment is determined. This environment contains all of the variable names defined in visible
scopes. A array is then `malloc`-ed, filled, and cast to the `%value*`
type as to have a variable size. This pair, is then put into a `func_t`
type, and cast to a `i32*` type and stored in a `%value`.

When a closure is slated to be called, the values for the environment
and the function pointer are extracted from the `%value` type, bitcast
 to the correct types and called, passing in the environment stored as
the first argument of the function pointer.

<h4> Boiler Plate </h4>
As you might have guessed so far, our code has a lot of type casting and
annoying hoops ones has to jump through when working with data. To
combat this, we created a number of helper llvm functions that can be
found in src/boiler.ll. These functions abstract all of the annoying
type casting allowing the execution of our code to be a little more
readable. We have wrapping functions, functions that take some arbitrary
data type, and wrap them inside a %value. These include `@wrap_i32`,
`@wrap_i1`, `@wrap_func`. In addition to wrapping these values, we also
support extracting with `@extract_i1`, `@extract_i32`, And for
functions, `@extract_env` and `@extract_func`. These functions are set
to `alwaysinline` for speed.

In addition to these type conversions, we also have several common
primitives hard coded to work in %values. These include `@eq` (equality)
`@add`, `@sub`, `@slt` (less than), `@sgt` (greater than) ,
`@mul`. While these could be implemented via currying, we
chose to hardcode them here for a mixture of speed, and ease of
development.

We also have a few malloc helper functions, `@malloc_i32`, and
`@malloc_env` to avoid code duplication.

There also a few functions that wrap libc apis. These include `@malloc`,
and `@printf`. We expose `@printf` to the user via the `@print` function
which is of the standard function api.

<h3> Improvements </h3>
<h4>Memory Management</h4>
<h5> Leaks everywhere </h5>
Currently, our code leaks a lot. Everytime we create any `%value`, when
ever we do anything really, we allocate some memory on the heap and
never remove it. For a toy langauge, this doesn't really matter, but in
production this is unacceptable. LLVM supports basic garbage collection,
but we are not using this. In addition to this, many of our values can
avoid before we even get to garbage collection. Intermediate values for
example, ones that don't have any name accociated with them can simply
be freed once they are used.
<h5> Heap allocation </h5>
A huge slowdown in our code currently is the way we allocate memory.
Each time a `%value` is created, it is allocated on the heap via libc
`malloc`. `malloc` is quite bad at allocating small chunks of memory. We
also should be attempting to use stack allocated variables instead of
heap allocated ones. This will also give us a speed boost.

<h4> Optimizations </h4>
<b> Broken </b>
<b>Next Steps:</b><br>
Implement data types using llvm structures (hopefully, unions) with encoded type information. Our end goal in this respect is to have primitive dynamic type checking.<br>
Implement environments. These will be heap allocated structures that
contain pointers to what ever variables are in scope.
Finally, with this environment set up, we will implement closures and then currying. <br>


