program rpn;
{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

//{$ZEROBASEDSTRINGS ON}

//{$R *.res}
//{$IFDEF MSWINDOWS}
//{$R project.rc} 
//{$ENDIF}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    {$IFDEF MSWINDOWS}
    windows,
    {$ENDIF}
    Classes, CustApp,
    Unit2, UnitREPL, RPNAbout,
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

type
    RPNC = class(TCustomApplication)
    protected
        procedure DoRun; override;
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;
        procedure WriteHelp; virtual;
end;


function convertToMDY(date : String) : String;
begin
    Result := Copy(date, 6, 2)+'/'+Copy(date, 9, 2)+'/'+Copy(date, 1, 4);
end;

procedure show_version();
begin
    writeln('RPN CALCULATOR - PapajScript Interpreter.'); 
    if (RPN_update <= 0) 
        then write('Version '+RPN_version+' ('+RPN_codename+') for '+RPN_targetCPU+'.')
        else write('Version '+RPN_version+' ('+RPN_codename+'), update #'+IntToStr(RPN_update)+' for '+RPN_targetCPU+'.');
    //if (RPN_isStable)
    //    then writeln(' Gen'+IntToStr(RPN_generation)+' build.')
    //    else writeln(' May be more unstable than usual. 3:)');
    if (not RPN_isStable) then writeln(' May be more unstable than usual. 3:)');
    writeln('by Paul Lipkowski & his fiancee Rosie. ');
    //if (RPN_updated = '')
    //    then writeln('Released on '+RPN_date+'.')
    //    else writeln('Released on '+RPN_date+', updated on '+RPN_updated+'.');
    if (RPN_updated = '')
        then writeln('Released on '+convertToMDY(RPN_date)+'.')
        else writeln('Released on '+convertToMDY(RPN_date)+', updated on '+convertToMDY(RPN_date)+'.');
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
    writeln('  SYNTAX : EXPLANATION ');
    writeln('   "abc" >xyz : Move an "abc" to a var "xyz".');
    writeln(' "abc" -> xyz : As above');
    writeln('"abc" -> $xyz : As above');
    writeln('         $xyz : Put either the var "xyz" or NULL on the stack.');
    writeln('         ?xyz : Return true or false, depending if var "xyz" exists.');
    writeln('         ~xyz : Destroy a variable "xyz".');
    writeln('        @@xyz : If the var is a function, then call it directly.');
    writeln('          xyz : If xyz is a function, then call it. Otherwise put it on the stack.');
    writeln();
    writeln('Data types: ');
    writeln('Number    String    Boolean   NULL');
    writeln('Function  Exception Date      Array');
end;

procedure show_operands7();
begin
    writeln('Conditionals: ');
    writeln(' if (expression) { set_of_commands }');
    writeln(' else { set_of_commands }');
    writeln();
    writeln('Functions'' syntax: function { set_of_commands }');
    writeln('                    function (args) { set_of_commands }');
    writeln('Execute the functions via "call" command or simply "VAR" (if a function is stored in a VARiable).');
end;

procedure show_operands8();
begin
    writeln('Other operations: ');
    writeln('      rand : Get a random real value from the range [0; 1) (execute "rand")');
    writeln('    random : Get a random integer value from 0 to N-1 (execute "N random")');
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
	writeln('Page 2: Number and Math');
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
    writeln('Number package functions:');
    writeln('factorial     round     floor   ceiling');
    writeln('      sqr       cub      cbrt       abs');
    writeln('Math package functions:');
    writeln(' - Unary');
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


function read_file(filename : String; LoadAll : Boolean = False; startFrom : LongInt = -1) : String;
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
        S := trim(S);
        fun := fun + #10 + S;
    end;
    closefile(fp);
    read_file := PS_parseString(fun, LoadAll, startFrom);
end;

function read_file_params(filename, params : String; LoadAll : Boolean = False; startFrom : LongInt = -1) : String;
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
    read_file_params := PS_parseString(params+fun, LoadAll, startFrom);
end;

// main

procedure RPNC.DoRun;
var
    x       : String;
    LoadAll : Boolean = False;
    Pause   : Boolean = False;
begin
    //{$IFDEF UNIX}
    //if FpSignal(SigInt, @HandleSigInt) = signalhandler(SIG_ERR) then begin
    //    Writeln('Failed to install signal error: ', fpGetErrno);
    //    Halt(1);
    //end;
    //{$ENDIF}
    randomize();

    if ParamCount = 0 then
    begin
        show_version();
     	writeln('No arguments provided - run ''rpn help'' or ''rpn -h'' or ''rpn --help''');
    end else begin
        case ParamStr(1) of
            'do' : begin
                if HasOption('L', 'load-all') then begin
                    LoadAll := True;
                end;
                if HasOption('P', 'pause') then begin
                    Pause := True;
                end;
                try
                    x := PS_parseString(ParamStr(2), LoadAll, 3);
                    if (x <> '') then writeln(x);
                except
                    On E : Exception do
                    begin
                        writeln(StdErr, E.ToString);
                    end;
                end;
                if (Pause) then readln();
            end;
     		'expression' : begin
     			show_version();
     			show_expressions();
     		end;
     		'functions' : begin
     			show_version();
                if ParamCount = 1 then
                begin
                    show_operands();
                end else begin
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
     		end;
            'help' : begin
     			show_version();
     			show_help();
     		end;
            'packages' : begin
     			show_version();
                if ParamCount = 1 then
                begin
                    show_packages();
                end else begin
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
     		end;
            'repl' : begin
                LoadAll := False;
                if HasOption('L', 'load-all') then begin
                    LoadAll := True;
                end;
                if HasOption('P', 'pause') then begin
                    Pause := True;
                end;
                show_version();
                PS_runREPL(LoadAll);
                if (Pause) then readln();
            end;
            'run' : begin
                LoadAll := False;
                if HasOption('L', 'load-all') then begin
                    LoadAll := True;
                end;
                if HasOption('P', 'pause') then begin
                    Pause := True;
                end;
                try
                    x := read_file(ParamStr(2), LoadAll, 3);
                    if (x <> '') then writeln(x);
                except
                    On E : Exception do
                    begin
                        writeln(StdErr, E.ToString);
                    end;
                end;
                if (Pause) then readln();
            end
     		else begin
                LoadAll := False;
                if HasOption('L', 'load-all') then begin
                    LoadAll := True;
                end;
                if HasOption('P', 'pause') then begin
                    Pause := True;
                end;
     			try
                    x := read_file(ParamStr(1), LoadAll, 2);
        			if (x <> '') then writeln(x);
            	except
            		On E : Exception do
             		begin
                  		writeln(StdErr, E.ToString);
             		end;
            	end;
                if (Pause) then readln();
     		end;
        end;
    end;
    Terminate;
end;

constructor RPNC.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    StopOnException:=True;
    DefaultFormatSettings.DecimalSeparator := '.';
end;

destructor RPNC.Destroy;
begin
    inherited Destroy;
end;

procedure RPNC.WriteHelp;
begin
    show_version();
    show_help();
end;

var App : RPNC;

{$R *.res}

begin
    App := RPNC.Create(nil);
    App.Title := 'RPN Calculator - PapajScript Interpreter';
    App.Run;
    App.Free;
    //{$IFDEF MSWINDOWS}
    //Sleep(500);
    //{$ENDIF}
    //readln();
end.
