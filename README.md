# Papaj
**Reversed Polish Notation Calculator**
and interpreter of **PapajScript**  
Version X.X.X (Leviathan)  
May 16, 2024  
by Paul Lipkowski (MrKaszëba)

Since November 24, 2017 written in FreePascal. :smile:

**Note**: The Leviathan version is a development one. It may be sometimes pretty unstable. The stable releases are found in other Git branches. You can obtain the latest version of Papaj (**v.0.5.3**, nickname Mem) [here](https://github.com/MrKaszeba19/PapajLang/releases).

## How to use it

### Console application
- Execute a command **papaj do** with a quoted PS expression (e.g. `papaj do "2 3 + 4 *"`). More info about expressions in `papaj expression` and `papaj functions`.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).
- If you need help, you can type `papaj help`, `papaj -h` or `papaj --help`.
- If you want to run an PS script file, then execute `papaj FILENAME` or `papaj run FILENAME`. 
- If you want to include some input parameters, then provide them after the FILENAME delimited by space, e.g. `papaj run FILENAME param1 param2 param3`. All these params are treated as PS strings. For example `papaj run script.ppsc 2 3 4` executes *script.ppsc* with input parameters of `2 3 4` being strings wrapped into the array called `Params`. 
- If you want to run a REPL of PapajScript, then execute `papaj repl`.
- If you want to load all pre-built packages on application start, then use flag `-L` or `--load-all`, e.g. `papaj do '2 PI * sin' -L` or `papaj repl --load-all`
- If you want to pause the application after script execution in order to exit manually, then use flag `-P` or `--pause`, e.g. `papaj do '2 2 +' -P` or `papaj script.ppsc --pause`.
- If you need app version, you can type `papaj version`, `papaj -v` or `papaj --version`.

### GUI Application
- Open the app executable.
- In order to compute a simple PS expression, just type it in the upper text box and click the "Count it!" button. The result appears in the result box below.
- There is also a simple script running engine – just paste your PS script or load it from file and click "Run script" to see its result.
- Remember that all values and operands must be delimited with at least 1 whitespace char (e.g. space bar).
- There is a possibility of executing the PS script in an external terminal window (it is an experimental feature)
    * Linux users are required to have `papaj` console app in the **$PATH** or the same directory as the GUI app. It is recommended to have `xterm` installed, if the external terminal window does not appear (unless you use a Debian-like system, or any other Linux that uses Xfce, GNOME/Cinnamon or KDE desktop).
    * Windows users are required to have `papaj` console app in the same directory as the GUI app

## Requirements and installation
- Supported systems:
    * **Linux** (32 and 64 bit)
    * **Windows** (32 and 64 bit) – full support for both GUI and console apps is provided for Windows 2000 and newer OSes. 
    * **FreeBSD** (64 bit)
    * **MacOS** (ARM64) - partial GUI app support for Apple chips
- Have installed
    * **Lazarus IDE** (version 1.6 or higher recommended) 
    * or just **FreePascal Compiler (FPC)** (3.0.4 or newer recommended) in order ot compile just Console app
    * if you use a non-Debian-like Linux, then **xterm** *might be recommended* 
    * **bash** shell, if you use Linux or FreeBSD
- There are three ways to build papaj Calculator
    * if you have Lazarus IDE and you use the GUI app,  
    then compile it by clicking "Run" or "Compile"
    * if you have Lazarus IDE and you are on a command shell or you don't use the GUI app,  
    then simply run `compile.sh` (in Linux and FreeBSD) or `compile.bat` (in Windows) to compile the project
    * if you don't have Lazarus IDE,  
    then you may compile the **console app** project using `compileWithFPC.sh` (Linux or FreeBSD with Bash shell) or `compileWithFPC.bat` (Windows). 
- You can make a Desktop shortcut to a Papaj REPL by running `installREPL.sh` (Linux, FreeBSD) or `installREPL.bat` (Windows)
- (for Linux/FreeBSD users) If you want to have `papaj` available in your shell, you can either
    * modify your shell $PATH
    * run `installLocal.sh` to add `papaj` executable to your local bash **$PATH** automatically
    * run `installGlobal.sh` to add `papaj` executable to your global path (`/bin`). Then you are able to execute PS scripts directly (you must provide a `#!/bin/papaj` shebang at the beginning of your script file). The `installGlobal.sh` script requires root privileges to be executed. It requires bash shell to be executed, but the papaj app can be accessed via any shell afterwards.

## PapajScript:

### About the language
**PapajScript** (PS) is an interpreted programming language. It was being developed while working on a RPN calculator project. 
The PS's code aims to be compact and easily appendable.  
Its structure is based mostly on Reverse Polish Notation (with a handful of exceptions), which uses a stack when computing values. 
Therefore all the operations are being done on the stack. 
The PS's semi-stack is an extended version of a classic stack, 
as we can get an indirect access to the entities not being on the top of the semi-stack and we can programme it like this semi-stack can simulate the behavior of a queue. 
The entities are put on the stack and may be used from the semi-stack, however we can also store them in the named variables.  
Everything comes around the semi-stack and the entities. The entities may be numbers, text strings, logical expressions, functions, arrays or exceptions.  
The future enhancements of the language include an introduction of OOP and better file management.

Find more about the language in this [wiki](https://github.com/MrKaszeba19/PapajLang/wiki).

Find basic functions [here](https://github.com/MrKaszeba19/PapajLang/wiki/Vanilla).

Find the examples of PapajScript code [here](https://github.com/MrKaszeba19/PapajLang/tree/leviathan/scripts).

## REPL Commands
- `\` at the very end of the input line allows for multi-line commands 
- `\autoreset:BOOL` - allow REPL to reset the stack every move (BOOL=true/false, false by default)
- `\autoreset` - same as `\autoreset:true`
- `\reset` - reset the REPL.
- `\export:FILE` - export your history to a file (FILE is a relative or absolute path)
- `\history` - display the previously typed commands
- `\hclear` - clear all history
- `\hclear:N` - clear the N-th command from history (N >= 1)
- `\import:FILE` - run a file (FILE is a relative or absolute path)
- `\!!` - repeat the previous command
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
- :croatia: **Croatian** (Hrvatski)
- :israel: **Hebrew** (עברית)
- :it: **Italian** (Italiano)
- :black_heart::yellow_heart: **Kashubian** (Kaszëbsczi) - standard and alternative orthographies (both Latin and Cyrilic)
- :macedonia: **Macedonian** (Македонски)
- :norway: **Norwegian** (Norsk) - both bokmål and nynorsk
- :poland: **Polish** (Polski)
- :portugal: **Portuguese** (Português)
- :brazil: **Brazilian Portuguese** (Português do Brasil)
- :ru: **Russian** (Русский)
- :serbia: **Serbian** (Српски, Srpski) – both Cyrilic and Latin
- :slovenia: **Slovene** (Slovenščina)
- :sweden: **Swedish** (Svenska)
- :morocco: Arabic (العربية) – *to be implemented*
- :israel: Yiddish (יידיש) – *to be implemented*
- :yellow_heart::blue_heart: Silesian (Ślůnsko godka) – *to be implemented*
- :yellow_heart::heart: Catalan (Català) – *to be implemented*
- :es: Spanish (Español, Castellano) – *to be implemented*
- :greece: Greek (Ελληνικά) – *to be implemented*
- :slovakia: Slovak (Slovenčina) – *to be implemented*
- :czech_republic: Czech (Čeština) – *to be implemented*
- :ukraine: Ukrainian (Українська) – *to be implemented*



## Releases history

Version | Version Name | Date of Release | Improvements
------- | ------------ |:---------------:| ------------
0.1.0 | Aleph | 2017-11-24 | Basic version
0.2.0 | Bet | 2017-11-27 | Improved computing power of integer values
0.2.1 | Gimel | 2017-12-10 | Unary operands
0.3.0 | Dalet | 2018-01-12 | Detect system language (GUI, Linux), fix bugs, stack operations
0.3.1 | Hey | 2018-01-24 | More operands (e.g. GCD, LCM, more stack operations), Danish language for GUI
0.4.0 | Vav | 2018-04-26 | Core improvements for console app, blocks of instructions, parsing script files, string and numbers management, more stack commands and other various abilities *and more*
0.4.1 | Zain | 2018-11-08 | Reconstruction and optimization, RPN logo, creating own functions, REPL, variables, more string functions
0.4.2 | Chet | 2019-05-16 | postfix calls, further code reconstruction
0.4.3 | Tet | 2020-05-15 | Hebrew language for GUI, syntax changes, Arrays, Packages and more
0.5.0 | Yod | 2020-08-14 | Improved syntax, more packages
0.5.1 | Khaf | 2021-03-21 | Array mapping and reducing, Date/time namagement, Mathematical utilities, eliminating bugs and more
0.5.2 | Lamed | 2021-12-18 | Rebuild of PS Environment, more GUI languages, runtime arguments, Mathematical and Console utilities, fix bugs
0.5.3 | Mem | 2023-04-08 | Locales, number systems, environment optimization
0.5.4 | Nun | by the end of 2024 | Complex numbers, polynomials, memory management, new name
0.5.5 | Samech | soon | Dataframes, matrices, files management
X.X.X | Leviathan | one eternity later | Development Edition, may be sometimes pretty unstable
