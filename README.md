# RPN Calculator
**Reversed Polish Notation Calculator**
and interpreter of **PapajScript**  
Version X.X.X (Leviathan)  
October 5, 2020  
by Paul Lipkowski (RooiGevaar19) & his fiancée Rozalia (rozirogal) :heart: 

Since 11/24/2017, proudly written in FreePascal. :smile:

**Note**: The Leviathan version is a development one. It may be sometimes pretty unstable. The stable releases are found in other Git branches. You can obtain the latest version of RPN Calculator (**v.0.5.0**, nickname Yod) [here](https://github.com/RooiGevaar19/RPNCalculator/releases/tag/v050-stable).

## How to use it

### Console application
- Execute a command **rpn do** with a quoted PS expression (e.g. `rpn do "2 3 + 4 *"`). More info about expressions in `rpn expression` and `rpn functions`.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).
- If you need help, you can type `rpn help`.
- If you want to run an PS script file, then execute `rpn FILENAME` or `rpn run FILENAME`. If you want to include some input parameters that would be laid on the stack when script begins, 
then provide them after the FILENAME delimited by space, e.g. `rpn run FILENAME param1 param2 param3`. 
All these params are treated as PS expressions. For example `rpn run script.ppsc 2 3 4` executes script.rpn with input parameters of `2 3 4` being laid on the stack.
- If you want to run a REPL of PapajScript, then execute `rpn repl`.

### GUI Application
- Open an app executable.
- In order to compute an PS expression, just type it in the upper text box and click the "Count it!"-button. The result appears in the result box below.
- There is also a simple script running engine – just paste your PS script and click "Run script" to see its result.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).

## Requirements and installation
- Have installed
    * **Lazarus IDE** (version 1.6 or higher recommended – I use Lazarus 2.0.4 as of now) 
    * or **FreePascal Compiler (FPC)** (3.0.4 recommended) for Console app only – *alternative for Unix/Linux users only*
- There are three ways to build RPN Calculator
    * if you have Lazarus IDE and you use the GUI app,  
    then compile it by clicking "Run" or "Compile"
    * if you have Lazarus IDE and you are on a command shell or you don't use the GUI app,  
    then simply run `compile.sh` (in Unix/Linux systems) or `compile.bat` (in Windows systems) to compile the project
    * if you don't have Lazarus IDE and you use a Unix/Linux-like system,  
    then you may compile the project using `compileWithFPC.sh` (for Console app only)
- You can make a Desktop shortcut to a RPN REPL by running `installREPL.sh` (Unix/Linux) or `installREPL.bat` (Windows)

## PapajScript:

### About the language
**PapajScript** (PS) is an interpreted language being used in RPN Calculator. It has been developed while working on RPN Calculator. The PS's code aims to be compact and easily appendable.  
Its structure is based mostly on Reverse Polish Notation (with a handful of exceptions), which uses a stack when computing values. Therefore all the operations are being done on the stack. The PS's semi-stack is an extended version of a classic stack, as we can get an indirect access to the entities not being on the top of the semi-stack and we can programme it like this semi-stack can simulate the behavior of a queue. The entities are put on the stack and may be used from the semi-stack, however we can also store them in the named variables.  
Everything comes around the semi-stack and the entities. The entities may be numbers, text strings, logical expressions, functions, arrays or exceptions.  
The future enhancements of the language include an introduction of objects, better file management.

Find more about the language in this [wiki](https://github.com/RooiGevaar19/RPNCalculator/wiki).

Find basic functions [here](https://github.com/RooiGevaar19/RPNCalculator/wiki/Vanilla).

Find the examples of PapajScript code [here](https://github.com/RooiGevaar19/RPNCalculator/tree/leviathan/rpn_console/scripts).

### Constant numerical values:
- e.g. 2*π -> 2 Math.PI *

Name | Symbol | Approximated value | Programme Operand
---- | ------ | ------------------ | -----------------
Pi | π | 3.1415926535897 | Math.PI
Euler number | e | 2.7182818284590 | Math.EU
Golden number | φ | 1.6180339887498 | Math.FI
Euler-Mascheroni constant | γ | 0.5772156649015 | Math.EM

### Char/string constants (about to be obsolete)
- `\n` new line
- `\t` horizontal tab
- `\\` a backslash
- if a char or string that is already a built-in function, then use `\` before it (e.g. `<=` - equal or less - then use `\<=`)

### Other constants
- `TRUE` and `FALSE` for boolean types
- `NULL`
- `EXC` (an empty unraised exception)

### Commands for type casting
**Change of syntax for type casting from 0.4.2 onwards**

Syntax: `X command`
 
Programme Operand | Purpose
----------------- | -------
toString | convert to a string
toNumber | convert to a number
toArray | build an array from N elements of the stack
toException | build an exception (if X is a string, then it creates an unraised exception with this string message)
raiseException | same as above, but it raises exception too

### Commands for numeric expressions
- number1 number2 function

| Name    | Purpose                 |
|:-------:| ----------------------- |
| +       | add                     |
| -       | substract               |
| *       | multiply                |
| /       | divide                  |
| ^       | power                   |
| pow     | power                   |
| root    | root                    |
| div     | integer division (`-5 3 div` returns `-1`) |
| mod     | modulo (`-5 3 mod` returns `-2`)           |
| cdiv    | integer division (`-5 3 div` returns `-2`) _removed so far_ |
| cmod    | modulo (`-5 3 div` returns `1`) _removed so far_ |

- number1 function

Programme Operand | Purpose
----------------- | -------
++ | increment
inc | increment
-- | decrement
dec | decrement
sqrt | suqare root
trunc | truncate
makeChar | assuming N1 is an ASCII code, it returns a char from the ASCII table
getAscii | Return an ASCII code number of a char S1 (it must be a single char)
random | Return a random integer from a range [0; N1-1]

- function

Programme Function | Purpose
------------------ | -------
rand | Return a random real from a range [0; 1)

### Advanced mathematical functions
**Note**: If you do not want to type `Math.` every time, just type `@use(Math)` before.

Visit [here](https://github.com/RooiGevaar19/RPNCalculator/wiki/Math) for Math package functions. 

### Functions for string expressions

**Note**: If you do not want to type `String.` every time, just type `@use(String)` before.

Visit [here](https://github.com/RooiGevaar19/RPNCalculator/wiki/String-(package)) for String package functions. 

### Commands for arrays

### Advanced array functions
**Note**: If you do not want to type `Array.` every time, just type `@use(Array)` before.

Visit [here](https://github.com/RooiGevaar19/RPNCalculator/wiki/Array-(package)) for Array package functions. 

**Note**: Array indexes start from 0, i.e. a N-elements array has indexes of [0..N-1] 

### Other built-in functions

Programme Operand | Syntax | Purpose
----------------- | ------ | -------
callIf | *bool1* *func1* **callIf** | Calls a function whether *bool1* = `true`
callUnless | *bool1* *func1* **callUnless** | Calls a function whether *bool1* = `false`

### Stack operations

#### Stack aggregating functions
- <number of top elements> operand

Programme Operand | Name
----------------- | ----
sum | sum of values
product | product of values
count | amount of the values put on the stack (stack's size)
size | as above, but don't clear the stack
all | as above
avg | mean of the values
max | maximum value of the values
min | minimum value of the values
median | median
rev | reverse the top N elements of the stack

_to be expanded to arrays_

**Examples:**
- `5 times 2 sum` sums five 2's
- `5 times scan all sum` sums five numbers scanned by an input
- `2 3 4 5 6 2 avg` brings a result of 5.5, as avg(5,6) = 5.5. The bottom values stay on the stack and now the stack looks like `2 3 4 5.5`.
- `2 3 4 5 6 all avg` brings a result of 4, as avg(2,3,4,5,6) = 4.
- `5 3 8 10 32.5 4 Math.sin 2 2 + 5 10 all sum` sums all values previously put on the stack

**Protips and notes:**
- If you want to aggregate all stack, then just type as a number of top elements a value of `all` or `size`, e.g. `all sum` or `size stddev`.
- All aggregating functions are vulnerable to autoclear, except for `count` (which always clears the stack) and `size`/`all` (which don't remove anything).

#### Stack manipulators

Programme Operand | Name
----------------- | ----
times | do something N1 times
size | Get the size of current stack without clearing that stack
all | As above
clone | Clone the value being on the top of the stack
keep | Keeps the top n values on the stack
copy | Copies the top n values on the stack and puts them on the stack
sort | Sorts a stack according to numerical values
strsort | Sorts the strings alphabetically
mcopy | Copies the top n values on the stack and puts them on the stack in reversed order
rem | Remove a value from the top of the stack
clear | Clear entire stack
seq | generates an arithmetical sequence from A to B and puts it on the stack (syntax: `A STEP B seq`)
gseq | generates a geometical sequence from A to B and puts it on the stack (syntax: `A STEP B gseq`)
seql | generates an arithmetical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N seq`)
gseql | generates a geometrical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N gseq`)
rev | reverses the top N elements of the stack
reverse | reverses the entire stack

**Examples:**
- `1 1 8 seq` generates "1 2 3 4 5 6 7 8"
- `1 3 8 seql` generates "1 4 7 10 13 16 19 22"
- `8 2 1 gseq` generates "8 4 2 1"
- `8 -1 10 gseql` generates "8 -8 8 -8 8 -8 8 -8 8 -8"
- `1 2 3 4 reverse` transforms into "4 3 2 1"
- `5 10 times clone` generates "5 5 5 5 5 5 5 5 5 5 5"
- `5 4 3 2 1  2 keep` results in a stack of "2 1"
- `5 4 3 2 1  2 copy` results in a stack of "5 4 3 2 1 2 1"
- `5 4 3 2 1  2 mcopy` results in a stack of "5 4 3 2 1 1 2"
- `5 4 3 2 1  2 all sort` results in a stack of "1 2 2 3 4 5"
- `5 4 3 1 7 2  3 rev` results in a stack of "5 4 3 2 7 1"

**Protips:**
- `size copy` (or `all copy`) replicates the stack (e.g. `1 2 3 4 size copy` results in "1 2 3 4 1 2 3 4"), and `size 2 / keep` halves the stack and lets the new one stay, e.g. after you don't need a replication anymore. If you want to keep the "old stack", just use `reverse size 2 / keep reverse` - assuming the sizes of "old stack" and the "new stack" are the same.
- `size mcopy` creates a mirrored stack (e.g. `1 2 3 4 size mcopy` results in "1 2 3 4 4 3 2 1")

### Language syntax features

#### Condtitional instructions (if-else)
**Old RPN syntax:** `B1 ? if: I1 else: I2`
The question mark checks if an expression B1 is true or its numerical value is equal to 0.
The `if:` launches the next instruction only when the recent ?-check was successful.
The `else:` launches the next instruction only when the recent ?-check was unsuccesful.

**C-like syntaxes:** 
- `if ( B1 ) I1` 
- `if ( B1 ) I1 else I2`
- `if ( B1 ) I1 elif ( B2 ) I2`
- `if ( B1 ) I1 elif ( B2 ) I2 else I3`
- `if ( B1 ) I1 elif ( B2 ) I2 elif ( B3 ) I3 ... else IN`
- `if ( B1 ) I1 elif ( B2 ) I2 elif ( B3 ) I3 ... elif ( BN ) IN`  
The question mark checks if an expression B1 is true or its numerical value is equal to 0.
The `if` launches the next instruction only when the recent condition check was successful.
The `else` launches the next instruction only when the recent condition check was unsuccesful.
The `elif` launches the next condition if a check of the previous one was not successful.

**Examples**
- `scan toNumber 2 mod ? if: { "This number is even." println } else: { "This number is odd." println }`
- `scan toNumber 5 > ? if: { "This number is greater than 5" println }`
- `scan toNumber 5 <= ? else: { "This number is greater than 5" println }`
- `scan toNumber 10 mod ? if: 1 else: 0`
- `if ( scan toNumber 2 mod ) { "This number is even." println } else { "This number is odd." println }`
- `if ( scan toNumber 5 > ) { "This number is greater than 5" println }`
- `if ( scan toNumber 5 <= not ) { "This number is greater than 5" println }`
- `if ( scan toNumber 10 mod ) 1 else 0`
- `if ( x 1 = ) { "X is equal to 1" println } elif ( x 2 = ) { "X is equal to 2" println } else { "X is neither equal to 1 nor equal to 2" println }`

*These styles of conditionals are not made in a RPN philosophy, but they were introduced as an alternative. You can use the RPN-like functions of callIf and callUnless instead*

#### Loops
- `N1 times { I1 }` 
- `while (B1) { I1 }`
- `do { I1 } while ( B1 )`
- `do { I1 } until ( B1 )`
- `for ( I1 ; B1 ; I2 ) { I3 }`
- `for ( ENT1 : ARR1 ) { I1 }`

**Examples**
- `10 times { 100 random }` which generates 10 numbers from a range [0..99] (note: we must know the exact amount of iterations here)
- `1 >i while ( i 10 <= ) { 100 random i ++ >i }` which also generates 10 numbers from a range [0..99]
- `1 >i do { 100 random i ++ >i } while ( i 10 <= )` which also generates 10 numbers from a range [0..99]
- `1 >i do { 100 random i ++ >i } until ( i 10 > )` which also generates 10 numbers from a range [0..99]
- `for ( 1 >i ; i 10 <= ; i ++ >i ) { 100 random }` which also generates 10 numbers from a range [0..99]
- `for ( i : T ) { i println } ` prints an array content

#### Data types
**Current data types:**
_**(Capitalized type names from 0.4.2 onwards)**_
- `Number`, e.g. `5, 2, -10, 20.5`
- `String`, e.g. `"Hello world!", "Shalom!", "h"`
- `Boolean`, e.g. `TRUE, FALSE`
- `Null`
- `Function`, e.g. `fun{ -1 * }`
- `Exception`, e.g. `EXC` (as of now just basic ones, _to be improved_)
- `Array`, e.g. `[]`

**Planned for the future**
- `Object`
- `File`

#### Variables

- `abc xyz vset` or `abc >xyz` – Move an "abc" to a var "xyz". If the variable does not exist, then create it with this value.
- `xyz vget` or `$xyz` – Put either the var "xyz" or NULL on the stack, depending if the variable exists or not.
- `xyz vexists` or `?xyz` – Return true or false, depending if var "xyz" exists.
- `xyz vdestroy` or `~xyz` – Destroy a variable "xyz" if exists.
- `xyz vcall` or `@@xyz` – If the var is a function, then call it directly.
- `xyz` – If the XYZ is a function, then call it, otherwise put XYZ on stack.

#### Functions

Syntax: 
- `fun{ <set_of_instructions> }`
- `function{ <set_of_instructions> }`
- `fun { <set_of_instructions> }`
- `function { <set_of_instructions> }`

**Notes**
- The functions use the main stack, they may get the parameters from there and they manipulate it.
- To call a function being on the stack, use an operand `call`.
- If a function is a variable, then you may call it directly either via `vcall` or via `@@vname` (where vname is a name of the function).

### Other operands and directives

#### Exception management
- raiseException - (syntax `X raiseException`) if X is a string, then raise an exception with a X message (np. `"Error!" raiseException`). If X is an exception, then this command raises it. An expression of `EXC excraise` raises an empty exception.

#### Input-output operands

Operand  | Purpose
-------- | -------
type | Check the type of the value being on the top of the stack
scan | Scan 1 value from an input (e.g. standard input) and add it on the top of the stack of values
scannum | Scan 1 numerical value from input
scanstr | Scan 1 string value from input
toString | Convert anything to string
toNumber | Convert anything to number
print | Print a value being on the top of the stack
println | Same as above and end the line.
rprint | Print a value being on the top of the stack and remove it from this stack.
rprintln | Same as above and end the line.
status | Print the stack.
statusln | Same as above and end the line.
statusfull | Print the stack in a more "beautiful" and detailed way.
newln | Start a new line


**Examples:**
- `scan scan +` scans 2 values and adds them
- `scan 3 random +` scans a value, generates a random number from a range [0..3] and adds the numbers
- `"Hello world!" type` puts "string" on the top of the stack
- `"Hello world!" println` prints "Hello world!"

#### Console and program manipulators

Programme Operand | Purpose
----------------- | -------
Console.textColor | Sets a color of a console text output
Console.textBackground | Sets a background color of a console text output
Console.gotoXY | Moves a cursor to N1,N2 position
Console.clearScreen | Clears a screen
Console.clrscr | As above
Console.startSound | Plays a sound of a frequency of N1 Hz
Console.stopSound | Stops a sound
Console.delay | freezes a program for N1 milliseconds

**Examples:**
- `440 startSound` keeps playing a sound of 440 Hz
- `500 delay` freezes a program for 500 milliseconds (0.5s)
- `1 1 gotoXY` moves a cursor to a upper-left edge of a console window (1,1)

#### Files management directives

Operand | Purpose
------- | -------
@source("FNAME") | Use an another RPN script and execute its code directly on the main stack

**Notes:**
- FNAME is a path to a script file, must be quoted
- Any input/output instructions in a script included by a `@source` command work with the same rights as the instructions in the main script. It means that `clear` clears the main stack and `scan` stops the main program to obtain a value

**Protips:**
- `@source` is recommended when you use a file that consists only of values and you want to put them on the stack.

*to be extended and rebuilt completely*

#### Parsing directives

| Operand              | Purpose                                                                                                 |
|:-------------------- | ------------------------------------------------------------------------------------------------------- |
| @silent(BOOL)        | Don't print the final stack output (when BOOL=TRUE). BOOL=false by default.                             |
| @silent              | The same as @silent(TRUE)                                                                               |
| @real                | Output is a decimal (set by default)                                                                    |
| @milli               | Output is a decimal with fixed precision of 3 digits                                                    |
| @float               | Output is a decimal with fixed precision of 6 digits                                                    |
| @double              | Output is a decimal with fixed precision of 15 digits                                                   |
| @int                 | Output is rounded to an integer value.                                                                  |
| @decimal             | Output is a decimal number with thousands separator                                                     |
| @scientific          | Output is in a scientific notation (e.g. 2,137 -> 2.137E+03)                                            |
| @scientific1         | Output is in a scientific notation (fixed precision of 15 digits, e.g. 2,137 -> 2.1370000000000000E+03) |
| @money               | Output is a decimal with fixed precision of 2 digits                                                    |
| @amoney              | Output is a decimal with thousands separator and a fixed precision of 2 digits                          |
| @autoclear(BOOL)     | Stack is wisely cleared after every operation (BOOL=true by default)                                    |
| @sorttype(STYPE)     | Set a type of sorting a stack                                                                           |
| @casesensitive(BOOL) | Whether it does matter if you type a command like THIS, or this, or tHiS                                |
| @useshell(SHELL)     | Specify what shell do you want to use                                                                   |
| @use(PACKAGE)        | Use a package                                                                                           |
| @unuse(PACKAGE)      | Stop using a package                                                                                    |
| //                   | One-line comment (only parsing text files)                                                              |

*BOOL* – available values are `TRUE` or `FALSE` (or also `true`/`false`, for compatibility with older scripts)

*STYPE* – available values:
- `BUBBLESORT` or `BSORT` or `0` - bubblesort
- `QUICKSORT` or `QSORT` or `1` - quicksort (*set by default*)
- `MERGESORT` or `MSORT` or `2` - mergesort
- `RANDOMSORT` or `RSORT` or `3` or `BOGOSORT` - random sort/bogosort (_for devils and masochists only, use at **your own** risk_)

*SHELL* – available values:
- `BASH` - use /bin/bash shell (*set by default on UNIX systems*)
- `CMD` - use cmd.exe shell (*set by default on Windows systems*)
- `PWSH` or `POWERSHELL` - use PowerShell
- `SH` - use /bin/sh shell
- `ZSH` - use /bin/zsh shell

*PACKAGE* – available packages:
- `String` with string functions
- `Math` with mathematical functions
- `Array` with array functions 
- `Console` with console utils
- `File` with file I/O functions (*coming soon*) 
- `System` with system functions (*coming soon*)

**Examples:**
- `@int 14.89` prints a stack with "15"
- `@int 14.89 trunc` prints stack with "14"
- `@silent @int 14.89 trunc` doesn't print the stack
- `2 3 +` prints a stack with "5"
- `@autoclear(TRUE) 2 3 +` prints a stack with "5" - as 2 and 3 were removed automatically after usage
- `@autoclear(FALSE) 2 3 +` prints a stack with "2 3 5"
- `@use(String)` allows using String commands with their shorter forms, e.g. `concat` instead of `String.concat`.

**Notes**
- Disabling autoclear does not apply to instructions of `times`, `rprint`, `rprintln`, `++`, `--`, stack manipulators and stack aggregating functions
- `inc`, `dec` do the same thing as `++`, `--` mentioned above and are vulnerable to @autoclear(FALSE) (*protip*)

## REPL Commands
- `\` at the very end of the input line allows for multi-line commands 
- `\autoreset:BOOL` - allow REPL to reset the stack every move (BOOL=true/false, false by default)
- `\autoreset` - same as `\autoreset:true`
- `\reset` - reset the REPL.
- `\export:FILE` - export your history to a file (FILE is a relative or absolute path)
- `\history` - display the previously typed commands
- `\hclear` - clear all history
- `\hclear:N` - repeat the N-th command of history (N >= 1)
- `\import:FILE` - run a file (FILE is a relative or absolute path)
- `\!!` - repeat last command
- `\!N` - execute an N-th command of `\history` (N >= 1)
- `\help` - display the help.
- `\q` or `\quit` - exit the REPL.

## Languages support for the GUI application (Linux)
- :uk: **English** - *default*
- :denmark: **Danish** (Dansk)
- :poland: **Polish** (Polski)
- :israel: **Hebrew** (עברית)
- :de: German (Deutsch) - *to be implemented*
- 🇳🇱 Dutch (Nederlands) - *to be implemented*
- 🇿🇦 Afrikaans (Afrikaans) - *to be implemented*


## Releases history

Version | Version Name | Date of Release | Improvements
------- | ------------ |:---------------:| ------------
0.1.0 | Aleph | 11/24/2017 | Basic version
0.2.0 | Bet | 11/27/2017 | Improved computing power of integer values
0.2.1 | Gimel | 12/10/2017 | Unary operands
0.3.0 | Dalet | 01/12/2018 | Detect system language (GUI, Linux), fix of some bugs, stack operations
0.3.1 | Hey | 01/24/2018 | More operands (e.g. GCD, LCM, more stack operations), Danish language for GUI
0.4.0 | Vav | 04/26/2018 | Core improvements for console app, blocks of instructions, parsing script files, string and numbers management, more stack commands and other various abilities *and more*
0.4.1 | Zain | 11/08/2018 | Reconstruction and optimization, RPN logo, creating own functions, REPL, variables, more string functions
0.4.2 | Chet | 05/16/2019 | postfix calls, further code reconstruction
0.4.3 | Tet | 05/15/2020 | Hebrew language for GUI, syntax changes, Arrays, Packages and more
0.5.0 | Yod | 08/14/2020 | New generation code, more packages and more
0.5.1 | Khaf | soon | Array mapping and reducing, dataframes, more packages, file management, objects and more
X.X.X | Leviathan | one eternity later | Development Edition, may be sometimes pretty unstable
