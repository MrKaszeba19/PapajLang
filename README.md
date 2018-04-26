# RPN Calculator
**Reversed Polish Notation Calculator**  
Version X.X.X (Leviathan)  
April 26, 2018  
by Paul Lipkowski (RooiGevaar19)  

Since 11/24/2017, proudly written in FreePascal. :smile:

## About RPN
**Reverse Polish Notation** (RPN) is a mathematical notation in which operators follow their operands. It allows to evaluate the mathematical expressions without using parentheses.

**More info:** https://en.wikipedia.org/wiki/Reverse_Polish_notation

### Examples of simple RPN expressions

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

### Available constant numerical values:
- e.g. 2*π -> 2 PI *

Name | Symbol | Approximated value | Programme Operand
---- | ------ | ------------------ | -----------------
Pi | π | 3.1415926535897 | PI
Euler number | e | 2.7182818284590 | EU
Golden number | φ | 1.6180339887498 | FI


### Operands for numeric expressions
- number1 number2 operand

| Name    | Purpose                 |
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

- number1 operand

Programme Operand | Purpose
----------------- | -------
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
times | do something N1 times
tostring | convert to a string
floor | floor
ceiling | ceiling

### Operands for string expressions

- string1 string2 operand

Programme Operand | Purpose
----------------- | -------
bind | Concatenates two strings with a delimiter of a space char
crush | Splits a string S1 to single chars
concat | Concatenates two strings
dechar | Removes all chars of S2 from a string S1
occur | Returns a count of occurences of a substring S2 in a string S1
pos | Returns an index of the first occurence of a substring S2 in a string S1
splitby | Splits a string S1 according to a char of S2 (and a space char too)
strremove | Removes the first occurence a substring S2 from a string S1

- string1 string2 string3 operand

Programme Operand | Purpose
----------------- | -------
bindby | Concatenates two strings S1 S2 with a delimiter of a S3 delimiter

- string1 number1 operand

Programme Operand | Purpose
----------------- | -------
crushby | Splits a string S1 to the parts of the same length of N1 (except for the last part, if len(S1) is not divisible by N1)
leftstr | Extracts a substring from a string S1 of the first N1 letters
rightstr | Extracts a substring from a string S1 of the last N1 letters

- string1 number1 number2 operand

Programme Operand | Purpose
----------------- | -------
strbetween | Extracts a substring from a string S1 positioned between N1 and N2
strremove | Removes a substring from a string S1, starting in N1 and being N2 chars long
substr | Extracts a substring from a string S1, starting in N1 and being N2 chars long

- string1 string2 number1 operand

Programme Operand | Purpose
----------------- | -------
npos | Returns an index of the N1-th occurence of a substring S2 in a string S1
strinsert | Inserts a string S2 into the string S1 in the position N1, making the following chars shift right

- string1 operand

Programme Operand | Purpose
----------------- | -------
val | Converts to a number (if possible)
len | Gets string length (does not remove the used string and is invulnerable to autoclear)
length | Gets string length (vulnerable to autoclear, not recommended)
ltrim | Eliminates whitespace on the left side
rtrim | Eliminates whitespace on the right side
trim | Eliminates whitespace on the both left and right sides
despace | Eliminates space chars
onespace | Eliminates combo space chars (e.g. "2   3" -> "2 3")
split | Splits a string according to space chars
strparse | Parses a string like it was a set of RPN commands


**Examples:**
- `5 times 2 sum` sums five 2's
- `5 times scan all sum` sums five numbers scanned by an input

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


**Examples:**
- `2 3 4 5 6 2 avg` brings a result of 5.5, as avg(5,6) = 5.5. The bottom values stay on the stack and now the stack looks like `2 3 4 5.5`.
- `2 3 4 5 6 all avg` brings a result of 4, as avg(2,3,4,5,6) = 4.
- `5 3 8 10 32.5 4 sin 2 2 + 5 10 all sum` sums all values previously put on the stack

**Protips and notes:**
- If you want to aggregate all stack, then just type as a number of top elements a value of `all` or `size`, e.g. `all sum` or `size stddev`.
- All aggregating functions are vulnerable to autoclear, except for `count` (which always clears the stack) and `size`/`all` (which don't remove anything).

#### Stack manipulators

Programme Operand | Name
----------------- | ----
size | Get the size of current stack without clearing that stack
all | As above
clone | Clone the value being on the top of the stack
keep | Keeps the top n values on the stack
copy | Copies the top n values on the stack and puts them on the stack
sort | Sorts a stack
mcopy | Copies the top n values on the stack and puts them on the stack in reversed order
rem | Remove a value from the top of the stack
clear | Clear entire stack
seq | generates an arithmetical sequence from A to B and puts it on the stack (syntax: `A STEP B seq`)
gseq | generates a geometical sequence from A to B and puts it on the stack (syntax: `A STEP B gseq`)
seql | generates an arithmetical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N seq`)
gseql | generates a geometrical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N gseq`)
rev | reverses the stack

**Examples:**
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
- `size copy` (or `all copy`) replicates the stack (e.g. `1 2 3 4 size copy` results in "1 2 3 4 1 2 3 4"), and `size 2 / keep` halves the stack and lets the new one stay, e.g. after you don't need a replication anymore. If you want to keep the "old stack", just use `rev size 2 / keep rev` - assuming the sizes "old stack" and the "new stack" are the same.
- `size mcopy` creates a mirrored stack (e.g. `1 2 3 4 size mcopy` results in "1 2 3 4 4 3 2 1")

### Other operands ans directives

#### Input-output operands

Operand  | Purpose
-------- | -------
type | Check the type of the value being on the top of the stack
scan | Scan 1 value from an input (e.g. standard input) and add it on the top of the stack of values
scannum | Scan 1 numerical value from input
scanstr | Scan 1 string value from input
rand | Generate a random integer value within a range [0..N-1]
print | Print a value being on the top of the stack
println | Same as above and end the line.
rprint | Print a value being on the top of the stack and remove it from this stack.
rprintln | Same as above and end the line.
status | Print the stack.
statusln | Same as above and end the line.
newln | Start a new line
Xn | Do the next thing n-times. ('n' is a constant integer value, n >= 1)
X* | Do the next thing until the end of input (very risky and permitted only in console app)
tilleof | As above


**Examples:**
- `scan scan +` scans 2 values and adds them
- `X2 scan +` equivalent of the expression above
- `X* scan all sum` read all values from an input and sums them
- `scan 3 rand +` scans a value, generates a random number from a range [0..3] and adds the numbers
- `"Hello world!" type` puts "string" on the top of the stack
- `"Hello world!" println` prints "Hello world!"

*to be extended*

#### Files management directives

Operand | Purpose
------- | -------
@source("FNAME") | Use an another RPN script and execute its code directly on the main stack

**Notes:**
- FNAME is a path to a script file, must be quoted
- Any input/output instructions in a script included by a `@source` command work with the same rights as the instructions in the main script. It means that `clear` clears the main stack and `scan` stops the main program to obtain a value

**Protips:**
- `@source` is recommended when you use a file that consists only of values and you want to put them on the stack.


#### Parsing directives

| Operand              | Purpose                                                                                                 |
|:-------------------- | ------------------------------------------------------------------------------------------------------- |
| @silent              | Don't print the final stack output (it does not affect the outputs invoked by script before)            |
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
| //                   | One-line comment (only parsing text files)                                                              |

*BOOL* available values are `true` or `false`
*STYPE* available values are
- `BUBBLESORT` or `BSORT` or `0` - bubblesort
- `QUICKSORT` or `QSORT` or `1` - quicksort (*set by default*)
- `MERGESORT` or `MSORT` or `2` - mergesort
- `RANDOMSORT` or `RSORT` or `3` or `BOGOSORT` - random sort/bogosort (_for devils and masochists only, use at **your own** risk_)


**Examples:**
- `#int 14.89` prints a stack with "15"
- `#int trunc 14.89` prints stack with "14"
- `#silent #int trunc 14.89` doesn't print the stack
- `2 3 +` prints a stack with "5"
- `@autoclear(true) 2 3 +` prints a stack with "5" - as 2 and 3 were removed automatically after usage
- `@autoclear(false) 2 3 +` prints a stack with "2 3 5"

**Notes**
- Disabling autoclear does not apply to instructions of `times`, `rprint`, `rprintln`, `++`, `--`, stack manipulators and stack aggregating functions
- `inc`, `dec` do the same thing as `++`, `--` mentioned above and are vulnerable to @autoclear(false) (*protip*)

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
0.4.0 | Vav | soon | Core improvements for console app, blocks of instructions, parsing script files, string and numbers management, more stack commands and other various abilities *and more*
X.X.X | Leviathan | one eternity later | Development Edition, may be sometimes pretty unstable
