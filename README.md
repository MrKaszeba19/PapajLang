# RPN Calculator
**Reversed Polish Notation Calculator**
and interpreter of **PapajScript**  
Version X.X.X (Leviathan)  
November 12, 2021  
by Paul Lipkowski (RooiGevaar19) & his fiancée Rosie (rozirogal) :heart: 

Since 11/24/2017, proudly written in FreePascal. :smile:

**Note**: The Leviathan version is a development one. It may be sometimes pretty unstable. The stable releases are found in other Git branches. You can obtain the latest version of RPN Calculator (**v.0.5.1**.2, nickname Khaf) [here](https://github.com/RooiGevaar19/RPNCalculator/releases).

## How to use it

### Console application
- Execute a command **rpn do** with a quoted PS expression (e.g. `rpn do "2 3 + 4 *"`). More info about expressions in `rpn expression` and `rpn functions`.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).
- If you need help, you can type `rpn help`, `rpn -h` or `rpn --help`.
- If you want to run an PS script file, then execute `rpn FILENAME` or `rpn run FILENAME`. 
- If you want to include some input parameters that would be laid on the stack when script begins, then provide them after the FILENAME delimited by space, e.g. `rpn run FILENAME param1 param2 param3`. All these params are treated as PS expressions. For example `rpn run script.ppsc 2 3 4` executes script.rpn with input parameters of `2 3 4` being laid on the stack. (**_Runtime params are out of use as of now_**) 
- If you want to run a REPL of PapajScript, then execute `rpn repl`.
- If you want to load all pre-built packages on application start, then use flag `-L` or `--load-all`, e.g. `rpn do '2 PI * sin' -L` or `rpn repl --load-all`

### GUI Application
- Open an app executable.
- In order to compute an PS expression, just type it in the upper text box and click the "Count it!"-button. The result appears in the result box below.
- There is also a simple script running engine – just paste your PS script or load it from file and click "Run script" to see its result.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).

## Requirements and installation
- Have installed
    * **Lazarus IDE** (version 1.6 or higher recommended – I use Lazarus 2.0.4 as of now) 
    * or **FreePascal Compiler (FPC)** (3.0.4 recommended) for Console app only
- There are three ways to build RPN Calculator
    * if you have Lazarus IDE and you use the GUI app,  
    then compile it by clicking "Run" or "Compile"
    * if you have Lazarus IDE and you are on a command shell or you don't use the GUI app,  
    then simply run `compile.sh` (in Unix/Linux systems) or `compile.bat` (in Windows systems) to compile the project
    * if you don't have Lazarus IDE and you use a Unix/Linux-like system,  
    then you may compile the **console app** project using `compileWithFPC.sh` (Unix/Linux) or `compileWithFPC.bat` (Windows)
- You can make a Desktop shortcut to a RPN REPL by running `installREPL.sh` (Unix/Linux) or `installREPL.bat` (Windows)
- (for Unix/Linux users) If you want to have `rpn` available in your bash shell, you can either
    * modify your $PATH
    * run `installBash.sh` to add `rpn` executable to your $PATH automatically

## PapajScript:

### About the language
**PapajScript** (PS) is an interpreted language being used in RPN Calculator. It has been developed while working on RPN Calculator. The PS's code aims to be compact and easily appendable.  
Its structure is based mostly on Reverse Polish Notation (with a handful of exceptions), which uses a stack when computing values. Therefore all the operations are being done on the stack. The PS's semi-stack is an extended version of a classic stack, as we can get an indirect access to the entities not being on the top of the semi-stack and we can programme it like this semi-stack can simulate the behavior of a queue. The entities are put on the stack and may be used from the semi-stack, however we can also store them in the named variables.  
Everything comes around the semi-stack and the entities. The entities may be numbers, text strings, logical expressions, functions, arrays or exceptions.  
The future enhancements of the language include an introduction of objects and better file management.

Find more about the language in this [wiki](https://github.com/RooiGevaar19/RPNCalculator/wiki).

Find basic functions [here](https://github.com/RooiGevaar19/RPNCalculator/wiki/Vanilla).

Find the examples of PapajScript code [here](https://github.com/RooiGevaar19/RPNCalculator/tree/leviathan/scripts).

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

## Languages support for the GUI application
- :uk: **English** – *default*
- :south_africa: **Afrikaans** (Afrikaans)
- :denmark: **Danish** (Dansk)
- :netherlands: **Dutch** (Nederlands)
- :fr: **French** (Français)
- :de: **German** (Deutsch)
- :israel: **Hebrew** (עברית)
- :it: **Italian** (Italiano)
- :black_heart::yellow_heart: **Kashubian** (Kaszëbsczi) - standard and alternative orthographies (both Latin and Cyrilic)
- :poland: **Polish** (Polski)
- :ru: **Russian** (Русский)
- :morocco: Arabic (العربية) – *to be implemented*
- :israel: Yiddish (יידיש) – *to be implemented*
- :norway: Norwegian (Norsk bokmål) – *to be implemented*
- :norway: Norwegian (Norsk nynorsk) – *to be implemented*
- :sweden: Swedish (Svenska) – *to be implemented*
- :yellow_heart::blue_heart: Silesian (Ślůnsko godka) – *to be implemented*
- :macedonia: Macedonian (Македонски) – *to be implemented*
- :slovenia: Slovene (Slovenščina) – *to be implemented*
- :croatia: Croatian (Hrvatski) – *to be implemented*
- :serbia: Serbian (Српски, Srpski) – *to be implemented*
- :portugal: Portuguese (Português) – *to be implemented*
- :brazil: Brazilian Portuguese (Português do Brasil) – *to be implemented*
- :yellow_heart::heart: Catalan (Català) – *to be implemented*
- :es: Spanish (Español, Castellano) – *to be implemented*
- :greece: Greek (Ελληνικά) – *to be implemented*
- :slovakia: Slovak (Slovenčina) – *to be implemented*
- :czech_republic: Czech (Čeština) – *to be implemented*



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
0.5.1 | Khaf | 03/21/2021 | Array mapping and reducing, Date/time namagement, Mathematical utilities, eliminating bugs and more
0.5.2 | Lamed | soon | Environment rebuild, more efficiency, better memory management, less bugs, dataframes, file management and more
X.X.X | Leviathan | one eternity later | Development Edition, may be sometimes pretty unstable
