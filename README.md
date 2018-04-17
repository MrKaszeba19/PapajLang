# RPN Calculator
**Reversed Polish Notation Calculator**  
Version X.X.X (Leviathan)  
April 17, 2018  
by Paul Lipkowski (RooiGevaar19)  

Since 11/24/2017, proudly written in FreePascal. :smile:

> Pascal is not dead. It's just a Force that remains forever, just like it does in Luke Skywalker.

## About RPN
**Reverse Polish Notation** (RPN) is a mathematical notation in which operators follow their operands. It allows to evaluate the mathematical expressions without using parentheses.

**More info:** https://en.wikipedia.org/wiki/Reverse_Polish_notation

### Examples of RPN expressions

Ordinary expression | RPN Expression
------------------- | --------------
5 | 5
2 + 3 | 2 3 +
(2.5 * 2) + 5 | 2.5 2 * 5 +
-4 * (2 / 3) | -4 2 3 / *
(2 + 5) * (11 - 7) | 2 5 + 11 7 - *
((8 - 2) / 3 + (1 + 4) * 2) / 6 | 8 2 - 3 / 1 4 + 2 * + 6 /

## How to use it

### Console application
- Execute a command **rpn** with a quoted RPN expression (e.g. `rpn "2 3 + 4 *"`). More info about expressions in `rpn expression` and `rpn operands`.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).
- In order to specify your output, you can execute rpn with a flag (e.g. `rpn "2 3.4 + 4.5 *" -i` provides an output of rounded integer). Type a command `rpn flags` to check out the available flags in this program.
- If you need help, you can type `rpn help`.
- If you want to parse an RPN script file, then execute `rpn parse FILENAME`.

**Flags are working now, but due to existence of the directives, they are supposed to be removed**

### GUI Application
- Open an app executable.
- In order to compute an RPN expression, just type it in the upper text box and click the "Count it!"-button. The result appears in the result box below.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).

## Implemented operations:

### Available constant values:
- e.g. 2*π -> 2 PI *

Name | Symbol | Approximated value | Programme Operand
---- | ------ | ------------------ | -----------------
Pi | π | 3.1415926535897 | PI
Euler number | e | 2.7182818284590 | EU
Golden number | φ | 1.6180339887498 | FI

### Binary operations
- expr1 expr2 operand

| Name    | Programme Operand       |
|:-------:| ----------------------- |
| +       | add                     |
| -       | substract               |
| *       | multiply                |
| /       | divide                  |
| ^       | power                   |
| pow     | power                   |
| root    | root                    |
| log     | logarithm               |
| div     | integer division (`-5 3 div` returns `-1`) |
| mod     | modulo (`-5 3 mod` returns `-2`)           |
| cdiv    | integer division (`-5 3 div` returns `-2`) |
| cmod    | modulo (`-5 3 div` returns `1`)            |
| choose  | Binomial coefficient                       |
| gcd     | Greatest Common Divisor                    |
| lcm     | Least Common Multiplier                    |

### Unary operations
- expr0 operand

Programme Operand | Name
----------------- | ----
++ | increment
inc | increment
-- | decrement
dec | decrement
abs | absolute value
sqrt | suqare root
exp | exponential
! | factorial
fact | factorial
sin | sine
cos | cosine
tan | tangent
cot | cotangent
sec | secant
csc | cosecant
ln | natural logarithm
fib | Fibonacci number
trunc | truncate
round | round
times | do the following operand/value N times (N is an integer value)

**Examples:**
- `5 times 2 sum` sums five 2's
- `5 times scan sum` sums five numbers scanned by an input

### Stack operations

#### Stack aggregating functions
- If you already own some values on the stack, e.g. after solving some minor expressions (`2 3 +  9 5 -` leaves 5 and 4 on the stack respectively), you may use stack operations on them by taking **ALL** its values.

**Notes:**
- Values themselves are put on the stack when solving the expression.
- The stack operations clear entire stack after execution.
- The stack aggregating functions will clear entire stack, even though autoclear is disabled.

Programme Operand | Name
----------------- | ----
sum | sum of all values
product | product of all values
count | amount of the values put on the stack (stack's size)
avg | mean of the values put on the stack
max | maximum value of the values put on the stack
min | minimal value of the values put on the stack

#### Stack manipulators

Programme Operand | Name
----------------- | ----
size | Get the size of current stack without clearing that stack
clone | Clone the value being on the top of the stack
keep | keeps the top n values on the stack
copy | copies the top n values on the stack and puts them on the stack
sort | Sorts a stack
mcopy | copies the top n values on the stack and puts them on the stack in reversed order
rem | Remove a value from the top of the stack
clear | Clear entire stack
seq | generates an arithmetical sequence from A to B and puts it on the stack (syntax: `A STEP B seq`)
gseq | generates a geometical sequence from A to B and puts it on the stack (syntax: `A STEP B gseq`)
seql | generates an arithmetical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N seq`)
gseql | generates a geometrical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N gseq`)
rev | reverses the stack

**Examples:**
- `5 3 8 10 32.5 4 sin 2 2 + 5 10 sum` sums all values previously put on the stack
- `1 1 8 seq` generates "1 2 3 4 5 6 7 8"
- `1 3 8 seql` generates "1 4 7 10 13 16 19 22"
- `8 2 1 gseq` generates "8 4 2 1"
- `8 -1 10 gseql` generates "8 -8 8 -8 8 -8 8 -8 8 -8"
- `1 2 3 4 rev` transforms into "4 3 2 1"
- `5 10 times clone` generates "5 5 5 5 5 5 5 5 5 5 5"
- `5 4 3 2 1 2 keep` results in a stack of "2 1"
- `5 4 3 2 1 2 copy` results in a stack of "5 4 3 2 1 2 1"
- `5 4 3 2 1 2 mcopy` results in a stack of "5 4 3 2 1 1 2"
- `5 4 3 2 1 2 sort` results in a stack of "1 2 2 3 4 5"

**Protips:**
- `size copy` replicates the stack (e.g. `1 2 3 4 size copy` results in "1 2 3 4 1 2 3 4"), and `size 2 / keep` halves the stack and lets the new one stay, e.g. after you don't need a replication anymore. If you want to keep the "old stack", just use `rev size 2 / keep rev` - assuming the sizes "old stack" and the "new stack" are the same.
- `size mcopy` creates a mirrored stack (e.g. `1 2 3 4 size mcopy` results in "1 2 3 4 4 3 2 1")

### Other operands ans directives

#### Input-output operands

Operand  | Purpose
-------- | -------
scan | Scan 1 value from an input (e.g. standard input) and add it on the top of the stack of values
rand | Generate a random integer value within a range [0..N-1]
print | Print a value being on the top of the stack
println | Same as above and end the line.
rprint | Print a value being on the top of the stack and remove it from this stack.
rprintln | Same as above and end the line.
status | Print the stack.
statusln | Same as above and end the line.
Xn | Do the next thing n-times. ('n' is a constant integer value, n >= 1)
X* | Do the next thing until the end of input (very risky and permitted only in console app)


**Examples:**
- `scan scan +` scans 2 values and adds them
- `X2 scan +` equivalent of the expression above
- `X* scan sum` read all values from an input and sums them
- `scan 3 rand +` scans a value, generates a random number from a range [0..3] and adds the numbers

*to be extended*

#### Files management directives

Operand | Purpose
------- | -------
@source="FNAME" | Use an another RPN script and execute its code directly on the main stack

**Notes:**
- FNAME is a path to a script file, must be quoted
- Any input/output instructions in a script included by a `@source` command work with the same rights as the instructions in the main script. It means that `clear` clears the main stack and `scan` stops the main program to obtain a value

**Protips:**
- `@source` is recommended when you use a file that consists only of values and you want to put them on the stack.


#### Parsing directives

| Operand         | Purpose                                                                                                 |
|:--------------- | ------------------------------------------------------------------------------------------------------- |
| #silent         | Don't print the final stack output (it does not affect the outputs invoked by script before)            |
| #real           | Output is a decimal (set by default)                                                                    |
| #milli          | Output is a decimal with fixed precision of 3 digits                                                    |
| #float          | Output is a decimal with fixed precision of 6 digits                                                    |
| #double         | Output is a decimal with fixed precision of 15 digits                                                   |
| #int            | Output is rounded to an integer value.                                                                  |
| #decimal        | Output is a decimal number with thousands separator                                                     |
| #scientific     | Output is in a scientific notation (e.g. 2,137 -> 2.137E+03)                                            |
| #scientific1    | Output is in a scientific notation (fixed precision of 15 digits, e.g. 2,137 -> 2.1370000000000000E+03) |
| #money          | Output is a decimal with fixed precision of 2 digits                                                    |
| #amoney         | Output is a decimal with thousands separator and a fixed precision of 2 digits                          |
| #autoclear=BOOL | Stack is wisely cleared after every operation (BOOL=true by default)                                    |
| #sorttype=STYPE | Set a type of sorting a stack                                                                           |
| //              | One-line comment (only parsing text files)                                                              |

*BOOL* available values are `true` or `false`
*STYPE* available values are
- `qsort` - quicksort
- `msort` - mergesort
- `bsort` - bubblesort
- `rsort` - random sort/bogosort (_for devils and masochists only, use at **your own** risk_)


**Examples:**
- `#int 14.89` prints a stack with "15"
- `#int trunc 14.89` prints stack with "14"
- `#silent #int trunc 14.89` doesn't print the stack
- `2 3 +` prints a stack with "5"
- `#autoclear=true 2 3 +` prints a stack with "5" - as 2 and 3 were removed automatically after usage
- `#autoclear=false 2 3 +` prints a stack with "2 3 5"

**Notes**
- Disabling autoclear does not apply to instructions of `times`, `rprint`, `rprintln`, `++`, `--`, stack manipulators and stack aggregating functions
- `inc`, `dec` do the same thing as `++`, `--` mentioned above and are vulnerable to autoclear=false (*protip*)

## Languages support for the GUI application (Linux)
- :uk: **English** - *default*
- :denmark: **Danish** (Dansk)
- :poland: **Polish** (Polski)
- :de: German (Deutsch) - *to be implemented*
- 🇿🇦 Afrikaans (Afrikaans) - *to be implemented*
- :israel: Hebrew (עברית) - *to be implemented*

## Improvements

Version | Version Name | Date of Release | Improvements
------- | ------------ |:---------------:| ------------
0.1.0 | Aleph | 11/24/2017 | Basic version
0.2.0 | Bet | 11/27/2017 | Improved computing power of integer values
0.2.1 | Gimel | 12/10/2017 | Unary operands
0.3.0 | Dalet | 01/12/2018 | Detect system language (GUI, Linux), fix of some bugs, stack operations
0.3.1 | Hey | 01/24/2018 | More operands (e.g. GCD, LCM, more stack operations), Danish language for GUI
0.4.0 | Vav | soon | Core improvements for console app, more stack commands and other various abilities *and more*
X.X.X | Leviathan | one eternity later | Development Edition, may be sometimes pretty unstable
