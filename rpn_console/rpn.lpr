program rpn;
{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

//{$ZEROBASEDSTRINGS ON}

//{$R *.res}
//{$IFDEF MSWINDOWS}
//{$R project.rc} 
//{$ENDIF}


uses Unit2, UnitREPL, //UnitEnvironment, 
//StrUtils, 
{$IFDEF UNIX}
BaseUnix,
{$ENDIF}
SysUtils;

{$IFDEF UNIX}
procedure HandleSigInt(aSignal: LongInt); cdecl;
begin
    Writeln();
    Writeln(StdErr, 'RPN Calculator has been halted by SIGINT.');
    Halt(1);
end;
{$ENDIF}

procedure show_version();
begin
    writeln('RPN CALCULATOR - PapajScript Interpreter.'); 
    writeln('Version X.X.X (Leviathan). May be more unstable than usual. 3:)');
    writeln('Paul Lipkowski & his fiancee Rozalia. October 1, 2020.');
    writeln('Since 11/24/2017. Proudly written in FreePascal. :)');
    writeln('');
end;

procedure show_help();
begin
    writeln('SYNTAX: rpn do "quoted_PS_code"');
    writeln('        rpn FILENAME');
    writeln('');
    writeln('Run ''rpn help'' or ''rpn'' to display this again.');
    writeln('Run ''rpn expression'' to obtain info about making RPN expressions.');
    writeln('Run ''rpn functions [page]'' to obtain info about available functions.');
    writeln('Run ''rpn packages'' to check the packages for PapajScript');
    writeln('Run ''rpn run (FILENAME)'' to run a PS script file');
    writeln('Run ''rpn repl'' to run a REPL for PapajScript');
    writeln('');
    writeln('More help at github.com/RooiGevaar19/RPNCalculator');
end;

procedure show_expressions;
begin
	writeln('EXPRESSIONS: ');
	writeln('Remember - the expression in console mode must be a "quoted" string');
	writeln('and each operation must be separated by 1 space.');
	writeln('');
	writeln('Type ''rpn functions'' to check out the available operands.');
    writeln('Type ''rpn packages'' to check the packages for PapajScript');
	writeln('');
	writeln('MATHEMATICAL EXAMPLES');
	writeln(' 6              -> 6');
	writeln(' 2+3            -> 2 3 +');
	writeln(' (12-6)/3.5     -> 12 6 - 3.5 /');
	writeln(' 5*(2-9)        -> 5 2 9 - *');
	writeln(' (12-4)/(4^0.5) -> 12 4 - 4 0.5 ^ /');
	writeln(' (2*PI)^E       -> 2 Math.PI * Math.EU ^');
	writeln(' sin(5) + 1     -> 5 Math.sin 1 +');
end;

procedure show_operands;
begin
	writeln('FUNCTIONS:');
	writeln('Page 1: Binary functions');
	writeln('Page 2: Unary functions');
	writeln('Page 3: Stack functions');
	writeln('Page 4: Constants');
	writeln('Page 5: Parsing directives');
    writeln('Page 6: Variables and data types');
    writeln('Page 7: Conditionals and custom functions');
	writeln('Page 8: Other');
	writeln;
	writeln('Type ''rpn functions [page_num]'' to obtain info about specific functions, e.g. ''rpn functions 1''');
	writeln('Type ''rpn functions all'' to print all pages at once.');
    writeln('Type ''rpn packages'' to check the packages for PapajScript');
end;

// binary
procedure show_operands1();
begin
	writeln('Binary functions model: (expr1) (expr2) (function), e.g. 2 4 +');
    writeln('Available binary functions:');
    writeln('       +       -       *       /     div');
    writeln('       ^     pow    root     log     mod');
end;

// unary
procedure show_operands2();
begin
    writeln('Unary functions model: (expr0) (function), e.g. 2 sin');
    writeln('Available unary functions:');
    writeln('     inc     dec      ++      --');
    writeln('     abs    sqrt   trunc');
    writeln();
    writeln(' X times - do the next thing X times');
    writeln('  X keep - Keep the top n values on the stack (e.g. "2 3 1 4 5 3 keep" results in a stack of "1 4 5")');
    writeln('  X copy - Copy the top n values on the stack (e.g. "2 3 1 4 5 3 copy" results in a stack of "2 3 1 4 5 1 4 5")');
    writeln(' X mcopy - Copy the top n values on the stack in reversed order (e.g. "2 3 1 4 5 3 mcopy" results in a stack of "2 3 1 4 5 5 4 1")');
end;

// stacks
procedure show_operands3();
begin
	writeln('Stack operations model (set of expressions put on the stack) (amount of top elements) (operand),'); 
	writeln('e.g. 5 2 3 + 7 all sum => 5 5 7 all sum => 17');
	writeln('Available stack operations:');
	writeln('       sum   product     count       avg');
	writeln('       min       max');
	writeln('Stack generators:');
	writeln('     seq    gseq    seql   gseql');
	writeln('Stack transformations:');
	writeln('   clone     rev');
end;

// constants
procedure show_operands4();
begin
    writeln('Available constants: ');
    writeln('      Math.PI = ~3.1415926535897');
	writeln('      Math.EU = ~2.7182818284590');
	writeln('      Math.FI = ~1.6180339887498');
end;

procedure show_operands5();
begin
	writeln('Data directives: ');
	writeln('@real        @milli       @float');
	writeln('@double      @int         @decimal');
	writeln('@scientific  @scientific1 @money');
	writeln('@amoney');
	writeln('Parsing directives:');
	writeln('  @autoclear(BOOL)  : Stack is wisely cleared after every operation (BOOL=true by default)');
	writeln('            (TRUE)  : After "2 3 +" the stack is "5", as 2 and 3 were removed after usage. ');
	writeln('            (FALSE) : After "2 3 +" the stack is "2 3 5", as 2 and 3 stay on the stack.');
    writeln('  @silent(BOOL)     : Print final stack output or not, depending if BOOL is TRUE or FALSE (false by default.)');
    writeln('  @silent           : Work the same way as @silent(TRUE)');
	writeln('  @sorttype(VAL)    : Choose a sorting algorithm (VAL=[0..3], 1 is default, 3 is risky)');
	writeln('  @source("FNAME")  : Use an another RPN script and execute its code directly on the main stack ("FNAME" is a quoted path to a filename)');
    writeln('  @use(PKG)         : Use the package.');
    writeln('  @unuse(PKG)       : Stop using the package.');
end;

procedure show_operands6();
begin
    writeln('Variables: ');
    writeln('    LONG FORM :  SHORT F : EXPLANATION ');
    writeln(' abc xyz vset : abc >xyz : Move an "abc" to a var "xyz".');
    writeln('     xyz vget :     $xyz : Put either the var "xyz" or NULL on the stack.');
    writeln('  xyz vexists :     ?xyz : Return true or false, depending if var "xyz" exists.');
    writeln(' xyz vdestroy :     ~xyz : Destroy a variable "xyz".');
    writeln('    xyz vcall :    @@xyz : If the var is a function, then call it directly.');
    writeln();
    writeln('Data types: ');
    writeln('Number    String    Boolean   NULL');
    writeln('Function  Exception ');
end;

procedure show_operands7();
begin
    writeln('Conditionals: ');
    writeln(' <expression> ?            : Check if an expression if true or equal to zero.');
    writeln(' if: { set_of_commands }   : If the last check returned true, then execute the set_of_commands');
    writeln(' else: { set_of_commands } : If the last check returned false, then execute the set_of_commands');
    writeln();
    writeln('Functions'' syntax: fun{ set_of_commands }');
    writeln('Execute the functions via "call" command or via "vcall" or @@var (var - var name).');
end;

procedure show_operands8();
begin
    writeln('Other operations: ');
    writeln('      rand : Get a random integer value from 0 to N-1 (execute "N rand")');
    writeln('      scan : Scan any value from input');
    writeln('  toNumber : Convert anything to number if possible');
    writeln('  toString : Convert anything to string');
    writeln('     print : Print a value being on the top of the stack');
    writeln('   println : Same as above and end the line.');
    writeln('     clone : Clone the value being on the top of the stack');
    writeln('       rem : Remove a value from the top of the stack');
    writeln('      keep : Keep the top n values on the stack (e.g. "2 3 1 4 5  3 keep" results in a stack of "1 4 5")');
    writeln('        // : One-line comment (when parsing files)');
end;

procedure show_packages();
begin
    writeln('Packages:');
    writeln('Page 1: Array');
	writeln('Page 2: Math');
	writeln('Page 3: String');
    writeln('Page 4: Console');
	writeln;
    writeln('Type ''@use(package_name)'' in code to use the package.');
    writeln('Type ''@unuse(package_name)'' in code to stop using the package.');
    writeln('Type ''package_name.function_name'' to use a single function from it.');
	writeln('Type ''rpn packages [page_num]'' to obtain info about specific packages, e.g. ''rpn packages 1''');
    writeln('Type ''rpn packages all'' to obtain info about all packages.');
end;

procedure show_packages1();
begin
    writeln('Array package functions:');
    writeln('   crush   getAt   setAt');
    writeln('    push     pop  pushAt');
    writeln('   popAt   shift  length');
end;

procedure show_packages2();
begin
    writeln('Math package functions:');
    writeln(' - Unary');
    writeln('     exp      ln       !    fact   floor ceiling');
    writeln('     sin     cos     tan     csc     sec     cot');
    writeln('  arcsin  arccos  arctan  arccot   round');
    writeln(' - Binary');
    writeln('  choose     gcd     lcm');
    show_operands4();
end;

procedure show_packages3();
begin
    writeln('String package functions:');
    writeln('  between    bind  bindBy   concat       crush      crushBy');
	writeln('   dechar despace  insert     left nthPosition       occurs');
	writeln(' onespace     pad padLeft padRight    position positionFrom');
	writeln('   remove replace   right      run       split      splitBy');
	writeln('substring  system    trim trimLeft   trimRight');
end;

procedure show_packages4();
begin
    writeln('Console package functions:');
    writeln('          clrscr     clearScreen             whereX          whereY            gotoXY');
    writeln('  textBackground       textColor textBackgroundANSI   textColorANSI textBackgroundRGB');
    writeln('    textColorRGB       textReset           textBold      textItalic     textUnderline');
    writeln('       textBlink   textFastBlink        textInverse     textBoldOff     textItalicOff');
    writeln('textUnderlineOff    textBlinkOff   textFastBlinkOff  textInverseOff        runCommand');
    writeln('      startSound       stopSound         runCommand');
end;


function read_file(filename : String) : String;
var
    fun, S : String;
    fp     : Text;
begin
    fun := '';
    assignfile(fp, filename);
        reset(fp);
        while not eof(fp) do
        begin
            readln(fp, S);
            //if (S <> '') then S := cutCommentMultiline(S);
            //S := DelChars(S, #13);
            //S := DelChars(S, #10);
            S := trim(S);
            fun := fun + #10 + S;
        end;
        closefile(fp);
        //writeln(fun);
    read_file := PS_parseString(fun);
end;

function read_file_params(filename, params : String) : String;
var
    fun, S : String;
    fp     : Text;
begin
    fun := '';
    assignfile(fp, filename);
        reset(fp);
        while not eof(fp) do
        begin
            readln(fp, S);
            //if (S <> '') then S := cutCommentMultiline(S);
            fun := fun + #10 + S;
        end;
        closefile(fp);
    read_file_params := PS_parseString(params+fun);
end;

// main

var
   x, y : String;
   i    : Integer;

{$R *.res}

begin
    {$IFDEF UNIX}
    if FpSignal(SigInt, @HandleSigInt) = signalhandler(SIG_ERR) then begin
        Writeln('Failed to install signal error: ', fpGetErrno);
        Halt(1);
    end;
    {$ENDIF}
    randomize();
	case ParamCount of
		0 : begin
			show_version();
     		writeln('No arguments provided - run ''rpn help''');
		end;
		1 : begin
			case ParamStr(1) of
     			'help' : begin
     				show_version();
     				show_help();
     			end;
                'repl' : begin
                    show_version();
                    PS_runREPL();
                end;
     			'expression' : begin
     				show_version();
     				show_expressions();
     			end;
     			'functions' : begin
     				show_version();
     				show_operands();
     			end;
                'packages' : begin
     				show_version();
     				show_packages();
     			end
     			else begin
     				try
                        x := read_file(ParamStr(1));
        				if (x <> '') then writeln(x);
              		except
              			On E : Exception do
                 		begin
                      		writeln(StdErr, E.ToString);
                 		end;
              		end;
     			end;
     		end;
		end;
        2 : begin
            case ParamStr(1) of
                'functions' : begin
                    show_version();
                    case ParamStr(2) of
                        '1' : show_operands1();
                        '2' : show_operands2();
                        '3' : show_operands3();
                        '4' : show_operands4();
                        '5' : show_operands5();
                        '6' : show_operands6();
                        '7' : show_operands7();
                        '8' : show_operands8();
                        'all': begin 
                            show_operands1(); writeln();
                            show_operands2(); writeln();
                            show_operands3(); writeln();
                            show_operands4(); writeln();
                            show_operands5(); writeln();
                            show_operands6(); writeln();
                            show_operands7(); writeln();
                            show_operands8();
                        end;
                    end;
                end;
                'packages' : begin
                    show_version();
                    case ParamStr(2) of
                        '1' : show_packages1();
                        '2' : show_packages2();
                        '3' : show_packages3();
                        '4' : show_packages4();
                        'all': begin 
                            show_packages1(); writeln();
                            show_packages2(); writeln();
                            show_packages3(); writeln();
                            show_packages4(); 
                        end;
                    end;
                end;
                'do' : begin
                    try
                        x := PS_parseString(ParamStr(2));
                        if (x <> '') then writeln(x);
                    except
                        On E : Exception do
                        begin
                            writeln(StdErr, E.ToString);
                        end;
                    end;
                end;
                'run' : begin
                    try
                        x := read_file(ParamStr(2));
                        if (x <> '') then writeln(x);
                    except
                        On E : Exception do
                        begin
                            writeln(StdErr, E.ToString);
                        end;
                    end;
                end;
            end;
        end
		else begin
			case ParamStr(1) of
                'functions' : begin
                    show_version();
                    case ParamStr(2) of
                        '1' : show_operands1();
                        '2' : show_operands2();
                        '3' : show_operands3();
                        '4' : show_operands4();
                        '5' : show_operands5();
                        '6' : show_operands6();
                        '7' : show_operands7();
                        '8' : show_operands8();
                        'all': begin 
                            show_operands1(); writeln();
                            show_operands2(); writeln();
                            show_operands3(); writeln();
                            show_operands4(); writeln();
                            show_operands5(); writeln();
                            show_operands6(); writeln();
                            show_operands7(); writeln();
                            show_operands8();
                        end;
                    end;
                end;
                'run' : begin
                    try
                        y := '';
                        for i := 3 to ParamCount do y := y + ParamStr(i) + ' ';
                        x := read_file_params(ParamStr(2), y);
                        if (x <> '') then writeln(x);
                    except
                        On E : Exception do
                        begin
                            writeln(StdErr, E.ToString);
                        end;
                    end;
                end;
            end;
		end;
	end;
	
end.
