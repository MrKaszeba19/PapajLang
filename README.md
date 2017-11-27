# RPN Calculator
Reversed Polish Notation Calculator

Version 0.2.0

November 24, 2017

by Paul Lipkowski (RooiGevaar19)

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
- Execute a command **rpn** with a quoted RPN expression (e.g. _rpn "2 3 + 4 *"_).
- Remember that all values and operands must be delimited with 1 space char.
- In order to specify your output, you can execute rpn with a flag (e.g. _rpn "2 3.4 + 4.5 *" -i_ provides an output of rounded integer). Type a command _rpn expression_ to check out the available flags in this program. 
- If you need help, you can type _rpn help_.

### GUI Application
- Open an app executable.
- In order to compute an RPN expression, just type it in the upper text box and click the "Count it!"-button. The result appears in the result box below. 
- Remember that all values and operands must be delimited with 1 space char.

## Implemented operations:

### Binary operations
- expr1 expr2 operand

Name | Programme Operand
---- | -----------------
add | +
substract | -
multiply | *
divide | /
power | ^
power | pow
root | root
logarithm | log

### Unary operations
- expr0 operand

*to be implemented*

### Available constant values:
- e.g. 2*π -> 2 PI *

Name | Symbol | Approximated value | Programme Operand
---- | ------ | ------------------ | -----------------
Pi | π | 3.1415926535897 | PI
Euler number | e | 2.7182818284590 | EU
Golden number | φ | 1.6180339887498 | FI

## Languages
- **English** (*default*)
- Polish (Polski, *to be implemented*) 

## Improvements

Version | Version Name | Improvements
------- | ------------ | ------------
0.1.0 | Aleph | basic version
0.2.0 | Bet | improved computing power of integer values