# RPN Calculator
**Reversed Polish Notation Calculator**  
Version 0.3.1 (Hey)  
January 24, 2018  
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
- In order to specify your output, you can execute rpn with a flag (e.g. `rpn "2 3.4 + 4.5 *" -i` provides an output of rounded integer). Type a command `rpn help` to check out the available flags in this program. 
- If you need help, you can type `rpn help`.

**Flags' functionalities don't work right now, they will be reimplemented and improved by the version of 0.4.0**

### GUI Application
- Open an app executable.
- In order to compute an RPN expression, just type it in the upper text box and click the "Count it!"-button. The result appears in the result box below. 
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).

## Implemented operations:

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
times | do the following operand/value N times (N is an integer value, N >= 1)

*to be extended*

### Stack operations
- If you already own some values on the stack, e.g. after solving some minor expressions (`2 3 +  9 5 -` leaves 5 and 4 on the stack respectively), you may use stack operations on them by taking **ALL** its values.


**Note 1:** Values themselves are put on the stack when solving the expression.
**Note 2:** The stack operations clear entire stack after execution.

Programme Operand | Name 
----------------- | ----
sum | sum of all values
product | product of all values
count | amount of the values put on the stack (stack's size)
avg | mean of the values put on the stack
max | maximum value of the values put on the stack
min | minimal value of the values put on the stack
seq | generates an arithmetical sequence from A to B and puts it on the stack (syntax: `A STEP B seq`)
gseq | generates a geometical sequence from A to B and puts it on the stack (syntax: `A STEP B gseq`)
seql | generates an arithmetical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N seq`)
gseq | generates a geometical sequence of N numbers and puts it on the stack (syntax: `BEGIN STEP N gseq`)

**Examples:** 
- `5 3 8 10 32.5 4 sin 2 2 + 5 10 sum` sums all values previously put on the stack
- `1 1 8 seq` generates "1 2 3 4 5 6 7 8"
- `1 3 8 seql` generates "1 4 7 10 13 16 19 22"
- `8 2 1 gseq` generates "8 4 2 1"
- `8 -1 10 gseql` generates "8 -8 8 -8 8 -8 8 -8 8 -8"

*to be extended*

### Operands without any arguments
Those operands may 

| Operand | Purpose                                                                                                         |
|:-------:| --------------------------------------------------------------------------------------------------------------- |
| scan    | Scan 1 value from an input (e.g. standard input) and add it on the top of the stack of values                   |
| Xn      | Do the next thing n-times. ('n' is a constant integer value, n >= 1)                                            |
| X*      | Do the next thing until the end of input (very risky and permitted only in console app, *to be replaced*)       |

*to be extended*

**Examples:** 
- `scan scan +` scans 2 values and adds them
- `X2 scan +` equivalent of the expression above
- `X* scan sum` read all values from an input and sums them

### Available constant values:
- e.g. 2*π -> 2 PI *

Name | Symbol | Approximated value | Programme Operand
---- | ------ | ------------------ | -----------------
Pi | π | 3.1415926535897 | PI
Euler number | e | 2.7182818284590 | EU
Golden number | φ | 1.6180339887498 | FI

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
0.2.1 | Gimel | 12/01/2017 | Unary operands
0.3.0 | Dalet | 01/12/2018 | Detect system language (GUI, Linux), fix of some bugs, stack operations
0.3.1 | Hey | 01/24/2018 | More operands (e.g. GCD, LCM, more stack operations), Danish language for GUI
0.4.0 | Vav | soon | Core improvements for console app *and more*
X.X.X | Leviathan | one eternity later | Development Edition, may be sometimes unstable