unit UnitEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, 
  UnitEntity, UnitStack, UnitVariables;


const
    MNORM = 0;
    MIF = 1;
    MFUN = 2;
    MWHILE = 3;
    MDOWHILE = 4;
    MDOUNTIL = 9;
    MDO = 8;
    MFOR = 10;
    MFOR1 = 5;
    MFOR2 = 6;
    MELIF = 7;

type PSReaderMode = (
    M_NORM,
    M_IF,
    M_FUN,
    M_WHILE,
    M_DOWHILE,
    M_DOUNTIL,
    M_DO,
    M_FOR,
    M_FOREACH,
    M_FOR2,
    M_ELIF,
    M_UNKNOWN
);

type PSCmdType = (
    _EVAL,
    _JUMP,
    _PUSH,
    _POP,
    _DIR,
    _CALL,
    _DO,
    _SET,
    _GET,
    _KILL,
    _CREATE,
    _BREAK,
    _CONT,
    _CAST,
    _CALC,
    _TEST,
    _SCAN,
    _CLONE,
    _QSHIFT,
    _PRINT,
    _SIZE,
    _EXIT
);

type PSCmdType2 = (
    _LVAR,
    _GVAR,
    _ARRAY,
    _FUNC,
    _EXPR,
    _IF,
    _FOR,
    _FOR2,
    _FOR3,
    _WHILE,
    _TIMES,
    _DWHILE,
    _DUNTIL,
    _ADD,
    _SUB,
    _MUL,
    _DIV,
    _IDIV,
    _MOD,
    _CIDIV,
    _CMOD,
    _POW,
    _DEC,
    _INC,
    _EQ,
    _NEQ,
    _GT,
    _LT,
    _GE,
    _LE,
    _NOT,
    _AND,
    _OR,
    _XOR,
    _LN,
    _NONE
);

type TStringArrArray = array of TStringArray;

type PSCommand = record
    Name          : PSCmdType;
    Name2         : PSCmdType2;
    IntParam      : LongInt;
    IntParam2     : LongInt;
    IntParam3     : LongInt;
    IntParam4     : LongInt;
    StrParam      : String;
    StrParam2     : String;
    StrParam3     : String;
    EntityRelated : Entity;
end;

type PSListOfCommands = array of PSCommand;

//type PSConditional = object
//    public
//        Condition   : LongInt;
//        Instruction : LongInt;
//        constructor Create();
//        destructor Destroy();
//end;

type PSConditional = record
    Condition   : LongInt;
    Instruction : LongInt;
end;

type PSListOfConditionals = object
    public
        MainItem  : PSConditional; 
        ElifItems : array of PSConditional;
        ElseItem  : LongInt;
        constructor Create();
        destructor Destroy();
end;

//type PSCommandDB = array of PSListOfCommands;
type PSCommandDB = object
    public
        Commands     : array of PSListOfCommands;
        Conditionals : array of PSListOfConditionals;
        constructor Create();
        destructor Destroy();
end;

type TParams = array of String;

type PSEnvironment = object
    private
        Scripts : array of PSCommandDB;
        procedure checkExceptions(var db : PSCommandDB);
        procedure params_assign(Params : TParams = Default(TParams));   
        procedure makeListOfCommands(L : TStringArray; var db : PSCommandDB; Args : TStringArray = Default(TStringArray));
        //procedure buildCommands(input : String; var db : PSCommandDB);
        //function makeListOfCommands(L : TStringArray; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
        function buildCommands(input : String) : PSCommandDB;
        procedure wrapArray(var db : PSCommandDB; at : LongInt);
        procedure variablePutOrRun(guess : String; var db : PSCommandDB);
        procedure doIf(var db : PSCommandDB; at : LongInt);
        procedure doTimes(var db : PSCommandDB; at : LongInt; n : LongInt);
        procedure doWhile(var db : PSCommandDB; cond : LongInt; inst : LongInt);
        procedure doUntil(var db : PSCommandDB; cond : LongInt; inst : LongInt);
        procedure doFor1(var db : PSCommandDB; init : LongInt; cond : LongInt; incr : LongInt; inst : LongInt);
        procedure doFor2(var db : PSCommandDB; init : String; indx : String; item : String; arry : LongInt; inst : LongInt);
        procedure doDoWhile(var db : PSCommandDB; cond : LongInt; inst : LongInt);
        procedure doDoUntil(var db : PSCommandDB; cond : LongInt; inst : LongInt);
        //procedure doFunction(var db : PSCommandDB; at : LongInt);
        procedure doFunction(at : LongInt);
        procedure variablePutOrRun(var db : PSCommandDB; guess : String);
        procedure evaluate(var db : PSCommandDB; input : String);
        procedure executeSet(var db : PSCommandDB; at : LongInt = 0);
        procedure executeCommands(cmds : PSCommandDB);
        procedure executePSCode(input : String);
    public
        Stack     : StackDB;
        Settings  : TSettings;
        Variables : VariableDB;
        AutoReset : Boolean;
        constructor Create(LoadAll : Boolean = False);
        constructor Create(sets : TSettings);
        destructor Destroy;
        procedure runFromString(input : String);
        procedure runFromFile(input : String);
        procedure assignParams(startFrom : LongInt = 0);
end; 

function cutCommentMultiline(input : String) : String;
function cutCommentEndline(input : String) : String;
function cutShebang(input : String) : String;
function checkParentheses(input : String) : ShortInt;

{$I FunctionsHeaders.fph}

implementation

//uses Unit5, DateUtils, StringUtils, Math;
uses Unit5, MathUtils, Math, DTUtils, ArrayUtils, StringUtils, ConsoleUtils,
    {$IFDEF MSWINDOWS}
		ShellApi, crt,
    {$ELSE}
        UnixCrt,
 	{$ENDIF}
    DateUtils;

var
	Steps : Integer;

{$I FunctionsImpl.fph}

// HELPFUL THINGS

// FUNCTION

function checkWord(input : String; check : String; len : LongInt) : Boolean;
begin
    Result := (LeftStr(input, len) = check) and (not isValidForVariables(RightStr(input, Length(input)-len)));
end;

procedure wrapArgs(args : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
var
    L : TStringArray;
    i : LongInt;
begin
    L := args.Split(' ');
    for i := Length(L)-1 downto 0 do
    begin
        if (L[i][1] = '$') then L[i] := RightStr(L[i], Length(L[i])-1);
        if isValidForVariables(L[i])
            then vardb.setLocalVariable(L[i], stack_pop(pocz[sets.StackPointer]))
            else raiserror('EVariable:CSetInvalid: Invalid variable string at "'+L[i]+'"');

    end;
end;

procedure PSEnvironment.variablePutOrRun(guess : String; var db : PSCommandDB);
var
    EntEax : Entity;
begin
    EntEax := Variables.getVariable(guess);
    if (EntEax.EntityType = TFUN) then
    begin
        doFunction(trunc(EntEax.Num));
    end else begin
        stack_push(Stack[Settings.StackPointer], EntEax);
    end;
end;


// EVALUATION

function getPackage(input : String) : String;
var
    position : LongInt; 
begin
    position := Pos('.', input);
    if (position >= 2)
        then Result := LeftStr(input, position-1)
        else Result := '';
end;

function searchThroughNamespacesExplicit(i : String; var env : PSEnvironment; var db : PSCommandDB) : Boolean;
begin
    Result := False;
    case getPackage(i) of
        'Array'   : if (env.Settings.Packages.UseArray)   then Result := lib_arrays(i, env, db);
        'Console' : if (env.Settings.Packages.UseConsole) then Result := lib_datetime(i, env, db);
        'Date'    : if (env.Settings.Packages.UseDate)    then Result := lib_consolemanipulators(i, env, db);
        'Math'    : if (env.Settings.Packages.UseMath)    then Result := lib_math(i, env, db);
        'Number'  : if (env.Settings.Packages.UseNumber)  then Result := lib_numbers(i, env, db);
        'String'  : if (env.Settings.Packages.UseString)  then Result := lib_strings(i, env, db);
        //else begin
        //    Result := vardb.isVarAssigned(i);
        //    if Result then runFromString(i, pocz, Steps, sets, vardb);
        //end;
    end;
    
end;

function searchThroughNamespacesImplicit(i : String; var env : PSEnvironment; var db : PSCommandDB) : Boolean;
begin
    Result := True;
    if (not env.Settings.Packages.UseNumber) or (not lib_numbers(concat('Number.',i), env, db)) then
    if (not env.Settings.Packages.UseMath) or (not lib_math(concat('Math.',i), env, db)) then
    if (not env.Settings.Packages.UseString) or (not lib_strings(concat('String.',i), env, db)) then
    if (not env.Settings.Packages.UseArray) or (not lib_arrays(concat('Array.',i), env, db)) then
    if (not env.Settings.Packages.UseConsole) or (not lib_consolemanipulators(concat('Console.',i), env, db)) then
    if (not env.Settings.Packages.UseDate) or (not lib_datetime(concat('Date.',i), env, db)) then
        Result := False;
end;

function cutCommentMultiline(input : String) : String;
var
    pom         : String;
    togglequote : Boolean;
    commentmode : Boolean;
    i           : LongInt;
begin
    pom := '';
    togglequote := false;
    commentmode := false;
    i := 0;
    while i <= Length(input) do 
    begin
        //if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
        if (not commentmode) and (input[i] = '"') then 
            if (i > 0) and (input[i-1] <> '\')
                then togglequote := not (togglequote)
                else if (i = 0) then togglequote := not (togglequote);
        if (not commentmode) and (not togglequote) and ((input[i] = '/') and (input[i+1] = '*')) then commentmode := true;
        if (not commentmode) then pom := concat(pom, input[i]);
        if     (commentmode) and (not togglequote) and ((input[i] = '*') and (input[i+1] = '/')) then 
        begin
            i := i + 2; 
            commentmode := false;
        end else begin
            i := i + 1;
        end;
    end;
    Result := trim(pom);
end;

function cutCommentEndline(input : String) : String;
var
    pom         : String;
    togglequote : Boolean;
    commentmode : Boolean;
    i           : LongInt;
begin
    pom := '';
    togglequote := false;
    commentmode := false;
    i := 0;
    while i <= Length(input) do 
    begin
        //if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
        if (not commentmode) and (input[i] = '"') then 
            if (i > 0) and (input[i-1] <> '\')
                then togglequote := not (togglequote)
                else if (i = 0) then togglequote := not (togglequote);
        if (not commentmode) and (not togglequote) and ((input[i] = '/') and (input[i+1] = '/')) then commentmode := true;
        if (not commentmode) then pom := concat(pom, input[i]);
        if     (commentmode) and (not togglequote) and (input[i] = #10) then 
        begin
            i := i + 1; 
            commentmode := false;
        end else begin
            i := i + 1;
        end;
    end;
    Result := trim(pom);
end;

function correctParentheses(input : String) : String;
var
    pom         : String;
    togglequote : Boolean;
    i           : LongInt;
begin
    pom := '';
    togglequote := false;
    i := 0;
    while i <= Length(input) do 
    begin
        //if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
        if (input[i] = '"') then 
            if (i > 0) and (input[i-1] <> '\')
                then togglequote := not (togglequote)
                else if (i = 0) then togglequote := not (togglequote);
        if (input[i] in [']', '}', ')']) and (input[i+1] <> ' ') then
        begin
            if togglequote 
                then pom := pom + input[i]
                else pom := pom + input[i] + ' ';
        end else if (input[i] in [';', ':']) and ((input[i+1] <> ' ') or (input[i-1] <> ' ')) then begin
            if togglequote 
                then pom := pom + input[i]
                else pom := pom + ' ' + input[i] + ' ';
        end else begin
            pom := concat(pom, input[i]);
        end;
        i := i + 1;
    end;
    //writeln(pom);
    Result := trim(pom);
end;

function cutShebang(input : String) : String;
var
    pom         : String;
    togglequote : Boolean;
    commentmode : Boolean;
    i           : LongInt;
begin
    pom := '';
    togglequote := false;
    commentmode := false;
    i := 0;
    while i <= Length(input) do 
    begin
        //if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
        if (not commentmode) and (input[i] = '"') then 
            if (i > 0) and (input[i-1] <> '\')
                then togglequote := not (togglequote)
                else if (i = 0) then togglequote := not (togglequote);
        if (not commentmode) and (not togglequote) and (input[i] = '#') then commentmode := true;
        if (not commentmode) then pom := concat(pom, input[i]);
        if     (commentmode) and (not togglequote) and (input[i] = #10) then 
        begin
            i := i + 1; 
            commentmode := false;
        end else begin
            i := i + 1;
        end;
    end;
    Result := trim(pom);
end;

function checkParentheses(input : String) : ShortInt;
var
    v1, v2, v3 : LongInt;
    i          : LongInt;
    comment    : Boolean;
begin
    Result := 1;
    comment := False;
    i := 1;
    v1 := 0; v2 := 0; v3 := 0;
    while i <= Length(input) do 
    begin
        if (input[i] = '"') then
        begin
            if not ((i > 0) and (input[i-1] = '\')) 
                then comment := not comment;
        end else if not comment then begin
            case input[i] of
                '{' : begin
                    v1 := v1 + 1;
                end; 
                '}' : begin
                    v1 := v1 - 1;
                    if (v1 < 0) then begin
                        Result := 0;
                        Break;
                    end;
                end; 
                '(' : begin
                    v2 := v2 + 1;
                end; 
                ')' : begin
                    v2 := v2 - 1;
                    if (v2 < 0) then begin
                        Result := 0;
                        Break;
                    end;
                end; 
                '[' : begin
                    v3 := v3 + 1;
                end; 
                ']' : begin
                    v3 := v3 - 1;
                    if (v3 < 0) then begin
                        Result := 0;
                        Break;
                    end;
                end; 
            end;
        end;
        i := i + 1;
    end;
    if (v1 <> 0) or (v2 <> 0) or (v3 <> 0) then Result := 0;
    if (comment) then Result := -1; 
end;

function checkLevelChar(input : Char) : Integer;
begin
         if (input = '{') then Result := 1
    else if (input = '[') then Result := 1
    else if (input = '(') then Result := 1
	else if (input = '}') then Result := -1
    else if (input = ']') then Result := -1
    else if (input = ')') then Result := -1
    else Result := 0;
    //writeln(input, ': ', Result);
end;

procedure checkWordString(var str : String; var quoted : Boolean; var nesttx : String; var nestlv : LongInt);
var
    index  : LongInt;
begin
    if (Length(str) > 0) then 
    begin
        if (str[1] = '"') then quoted := not quoted;
        if not quoted then nestlv := nestlv + checkLevelChar(str[1]);
        nesttx := nesttx + str[1];
        for index := 2 to Length(str) do
        begin
            if (str[index] = '"') and (str[index-1] <> '\') then 
            begin
                //writeln('quote'); 
                quoted := not quoted;
            end;
            if not quoted then nestlv := nestlv + checkLevelChar(str[index]);
            nesttx := nesttx + str[index];
        end;
    end;
end;

function getScopedString(var L : TStringArray; var cursor : Integer; initStr : String = '') : String;
var
    nestlv : LongInt;
    nesttx : String;
    quoted : Boolean = False;
begin
    nestlv := 1;
    //writeln(initstr, Length(L[cursor]):8, nestlv:8, quoted:8);
    checkWordString(initstr, quoted, nesttx, nestlv);
    nesttx := initstr + ' ';
    Inc(cursor);
    while (nestlv > 0) and (cursor < Length(L)) do begin
        //writeln(L[cursor]:16, Length(L[cursor]):8, nestlv:8, quoted:8);
        checkWordString(L[cursor], quoted, nesttx, nestlv);
        nesttx := nesttx + ' ';
        Inc(cursor);
    end;
    //writeln(nesttx);
    if (Length(nesttx) > 0) then 
    begin
        nesttx := LeftStr(nesttx, Length(nesttx)-1);
        if (RightStr(nesttx, 1)[1] in ['}', ')', ']']) then nesttx := LeftStr(nesttx, Length(nesttx)-1); 
    end;
    Dec(cursor);
    Result := trimLeft(nesttx);
end;

function getQuotedString(var L : TStringArray; var cursor : Integer) : String;
var
    nesttx : String;
begin
    nesttx := L[cursor];
	Inc(cursor);
	repeat
		nesttx := nesttx + ' ' + L[cursor];
        //writeln(L[cursor], ' ', cursor);
        Inc(cursor);
	until ((RightStr(L[cursor-1], 1) = '"') and (RightStr(L[cursor-1], 2) <> '\"')) or (cursor-1 >= Length(L));
    Dec(cursor);
    //writeln(nesttx);
    if (RightStr(nesttx, 1) = '"')
        then Result := nesttx
        else raiserror('ESyntax:CQuotes: Wrong amount of quotation marks. Quotes are not closed.');
end;

// ==================================================================
// NEW ENVIRONMENT

// constructors and destructors

// to remove
//constructor PSConditional.Create();
//begin
//    SetLength(Condition, 0);
//    SetLength(Instruction, 0);
//end;
//
//destructor PSConditional.Destroy();
//begin
//    SetLength(Condition, 0);
//    SetLength(Instruction, 0);
//end;

constructor PSListOfConditionals.Create();
begin
    //SetLength(ElseItem, 0);
    SetLength(ElifItems, 0);
    ElseItem := -1;
end;

destructor PSListOfConditionals.Destroy();
begin
    //SetLength(ElseItem, 0);
    SetLength(ElifItems, 0);
end;

constructor PSCommandDB.Create();
begin
    SetLength(Commands, 0);
    SetLength(Conditionals, 0);
end;

destructor PSCommandDB.Destroy();
begin
    SetLength(Commands, 0);
    SetLength(Conditionals, 0);
end;

constructor PSEnvironment.Create(LoadAll : Boolean = False);
begin
	SetLength(Self.Scripts, 0);
    SetLength(Self.Stack, 1);
	Self.Stack[0] := stack_null();
	Self.Settings := default_settings(LoadAll);
    Self.Variables.Create;
    Self.AutoReset := False;
end;

constructor PSEnvironment.Create(sets : TSettings);
begin
    SetLength(Self.Scripts, 0);
	SetLength(Self.Stack, 1);
	Self.Stack[0] := stack_null();
	Self.Settings := sets;
    Self.Variables.Create;
    Self.AutoReset := False;
end;

destructor PSEnvironment.Destroy;
var
	i : LongInt;
begin
    SetLength(Self.Scripts, 0);
    Self.Variables.Destroy;
	for i := 0 to Length(Self.Stack)-1 do stack_clear(Self.Stack[i]);
	SetLength(Self.Stack, 0);
end;

// args engine

function params_obtain(from : LongInt) : TParams;
var
    i, j : LongInt;
    x    : TParams;
begin
    j := 0;
    SetLength(x, 0);
    for i := from to ParamCount do
    begin
        if isNotFlagParam(ParamStr(i)) then
        begin
            SetLength(x, j + 1);
            x[j] := ParamStr(i);
            j := j + 1;
        end;
    end;
    Result := x;
end;

procedure PSEnvironment.params_assign(Params : TParams = Default(TParams));
var
    i      : LongInt;
    stk    : LongInt;
    str    : String;
    ArrEcx : Entity;
begin
    if Length(Params) > 0 then
    begin
        stack_push(Stack[Settings.StackPointer], buildNewEmptyArray(Stack, Settings));
        ArrEcx := stack_pop(Stack[Settings.StackPointer]);
        stk := Settings.StackPointer;
        Settings.StackPointer := trunc(ArrEcx.Num);
        for i := 0 to Length(Params)-1 do
        begin
            stack_push(Stack[Settings.StackPointer], buildString(string_fromC(Params[i])));
        end;
        Variables.setLocalVariable('Params', ArrEcx);
        Settings.StackPointer := stk;
    end else begin
        Variables.setLocalVariable('Params', buildNewEmptyArray(Stack, Settings));
    end;
end;

procedure PSEnvironment.assignParams(startFrom : LongInt = 0);
begin
    params_assign(params_obtain(startFrom));
end;

// run code

procedure PSEnvironment.runFromString(input : String);
begin
    if (Length(input) > 0) then 
    begin
        input := cutShebang(input);
        input := cutCommentMultiline(input);
        input := cutCommentEndline(input);
        input := correctParentheses(input);
    end;
    case checkParentheses(input) of 
        -1 : raiserror('ESyntax:CQuotes: Wrong amount of quotation marks. Quotes are not closed.');
        0  : raiserror('ESyntax:CLevels: Wrong amount of braces and/or parentheses.');
        1  : begin
            executePSCode(input);
            //Self.Stack := parseOpen(input, Self.Stack, Self.Settings, Self.Variables);
        end;
    end;
end;

procedure PSEnvironment.runFromFile(input : String);
var
    fun, S : String;
    fp     : Text;
begin
    fun := '';
    assignfile(fp, input);
    reset(fp);
    while not eof(fp) do
    begin
        readln(fp, S);
        S := trim(S);
        fun := fun + #10 + S;
    end;
    closefile(fp);
    runFromString(fun);
end;

//------------------------------------------------------
// code reader
// code evaluation engine

function getScopedCommands(var L : TStringArray; var cursor : Integer; initStr : String = ''; nestlv : LongInt = 1) : TStringArray;
var
    M      : TStringArray;
    index  : LongInt = 0;
    nesttx : String;
    quoted : Boolean = False;
begin
    SetLength(M, 1);
    if (initStr <> '') then
    begin
        //writeln('simpli', #9, cursor, #9, 0, #9, index, #9, initstr, #9, nestlv);
        checkWordString(initstr, quoted, nesttx, nestlv);
        SetLength(M, 1);
        M[0] := initstr;
    end;
    Inc(index);
    Inc(cursor);
    //while (nestlv > 0) and (cursor < Length(L)) and (L[cursor] <> '') do begin
    while L[cursor] = '' do Inc(cursor);
    while (nestlv > 0) and (cursor < Length(L)) do begin
        while L[cursor] = '' do Inc(cursor);
        //writeln('simpl', #9, cursor, #9, 0, #9, index, #9, L[cursor], #9, nestlv);
        checkWordString(L[cursor], quoted, nesttx, nestlv);
        SetLength(M, index+1);
        M[index] := L[cursor];
        Inc(index);
        Inc(cursor);
        while L[cursor] = '' do Inc(cursor);
        //writeln('next', #9, L[cursor]);
    end;
    //writeln(nesttx);
    if (RightStr(M[index-1], 1)[1] in ['}', ')', ']']) then M[index-1] := LeftStr(M[index-1], Length(M[index-1])-1); 
    Dec(cursor);
    //writeln('simple list');
    //write('0: ');
    //for index := 0 to Length(M)-1 do
    //    write(M[index], #9);
    //writeln();
    Result := M;
end;

function getScopedDelimitedCommands(var L : TStringArray; var cursor : Integer; initStr : String = '') : TStringArrArray;
var
    M      : TStringArrArray;
    index  : LongInt = 0;
    jndex  : LongInt = 0;
    nestlv : LongInt;
    nesttx : String;
    quoted : Boolean = False;
begin
    nestlv := 1;
    //SetLength(M, 0);
    SetLength(M, 1);
    //writeln('delimi', #9, cursor, #9, jndex, #9, index, #9, initstr);
    if (initStr <> '') then
    begin
        checkWordString(initstr, quoted, nesttx, nestlv);
        //SetLength(M, 1);
        SetLength(M[jndex], 1);
        M[jndex][0] := initstr;
        Inc(index);
    end;
    Inc(cursor);
    while (nestlv > 0) and (cursor < Length(L)) do begin
        while L[cursor] = '' do Inc(cursor);
        checkWordString(L[cursor], quoted, nesttx, nestlv);
        if (nestlv = 1) and (L[cursor][1] in [':', ';']) and (not quoted) then
        begin
            Inc(jndex, 1);
            index := 0;
            Inc(cursor);
            SetLength(M, jndex+1);
            SetLength(M[jndex], index+1);
            //writeln('delimn', #9, cursor, #9, jndex, #9, index, #9, L[cursor]);
            checkWordString(L[cursor], quoted, nesttx, nestlv);
            M[jndex][index] := L[cursor];
            Inc(index);
            Inc(cursor);
        end else begin
            SetLength(M[jndex], index+1);
            M[jndex][index] := L[cursor];
            Inc(index);
            Inc(cursor);
        end;
    end;
    //writeln(nesttx);
    for index := 0 to jndex do
    begin
        nestlv := Length(M[index])-1;
        if (RightStr(M[index][nestlv], 1)[1] in ['}', ')', ']']) 
            then M[index][nestlv] := LeftStr(M[index][nestlv], Length(M[index][nestlv])-1); 
    end;

    //writeln('delimited list');
    //for index := 0 to Length(M)-1 do
    //begin
    //    write(index, ': ');
    //    for jndex := 0 to Length(M[index])-1 do
    //        write(M[index][jndex], #9);
    //    writeln();
    //end;
    Dec(cursor);
    Result := M;
end;

// repair to detect arrays
function getScopedCommand(var cursor : Integer; initStr : String = '') : TStringArray;
var
    M      : TStringArray;
    quoted : Boolean = False;
begin
    SetLength(M, 1);
    if (initStr <> '') then
    begin
        //checkWordString(initstr, quoted, nesttx, nestlv);
        SetLength(M, 1);
        M[0] := initstr;
    end;
    //if (RightStr(M[0], 1)[1] in ['}', ')', ']']) then M[0] := LeftStr(M[0], Length(M[0])-1); 
    Result := M;
end;

function buildCmdEval(input : String) : PSCommand;
begin
    Result.Name     := _EVAL;
    Result.StrParam := input;
end;

function buildCmdJump(input : LongInt) : PSCommand;
begin
    Result.Name     := _JUMP;
    Result.IntParam := input;
end;

function buildCmdPush(ent : Entity) : PSCommand;
begin
    Result.Name          := _PUSH;
    Result.EntityRelated := ent;
end;

function buildCmdPop() : PSCommand;
begin
    Result.Name := _POP;
end;

function buildCmdClone() : PSCommand;
begin
    Result.Name := _CLONE;
end;

function buildCmdQShift() : PSCommand;
begin
    Result.Name := _QSHIFT;
end;

function buildCmdPrint() : PSCommand;
begin
    Result.Name     := _PRINT;
    Result.Name2    := _NONE;
end;

function buildCmdPrintLn() : PSCommand;
begin
    Result.Name     := _PRINT;
    Result.Name2    := _LN;
end;

function buildCmdScan() : PSCommand;
begin
    Result.Name := _SCAN;
end;

function buildCmdSize() : PSCommand;
begin
    Result.Name := _SIZE;
end;

function buildCreateArray(input : LongInt) : PSCommand;
begin
    Result.Name     := _CREATE;
    Result.Name2    := _ARRAY;
    Result.IntParam := input;
end;

function buildCreateFunction(input : LongInt) : PSCommand;
begin
    Result.Name     := _CREATE;
    Result.Name2    := _FUNC;
    Result.IntParam := input;
end;

function buildCreateExpression(input : LongInt) : PSCommand;
begin
    Result.Name     := _CREATE;
    Result.Name2    := _EXPR;
    Result.IntParam := input;
end;

function buildDoTimes(input : LongInt) : PSCommand;
begin
    Result.Name     := _DO;
    Result.Name2    := _TIMES;
    Result.IntParam := input;
end;

function buildDoIf(input : LongInt) : PSCommand;
begin
    Result.Name     := _DO;
    Result.Name2    := _IF;
    Result.IntParam := input;
end;

function buildDoWhile(input1, input2 : LongInt) : PSCommand;
begin
    Result.Name      := _DO;
    Result.Name2     := _WHILE;
    Result.IntParam  := input1;
    Result.IntParam2 := input2;
end;

function buildDoDoWhile(input1, input2 : LongInt) : PSCommand;
begin
    Result.Name      := _DO;
    Result.Name2     := _DWHILE;
    Result.IntParam  := input1;
    Result.IntParam2 := input2;
end;

function buildDoDoUntil(input1, input2 : LongInt) : PSCommand;
begin
    Result.Name      := _DO;
    Result.Name2     := _DUNTIL;
    Result.IntParam  := input1;
    Result.IntParam2 := input2;
end;

function buildDoFor1(input1, input2, input3, input4 : LongInt) : PSCommand;
begin
    Result.Name      := _DO;
    Result.Name2     := _FOR;
    Result.IntParam  := input1;
    Result.IntParam2 := input2;
    Result.IntParam3 := input3;
    Result.IntParam4 := input4;
end;

function buildDoFor2(input1, input2, input3 : String; input4, input5 : LongInt) : PSCommand;
begin
    Result.Name      := _DO;
    Result.Name2     := _FOR2;
    Result.StrParam  := input1;
    Result.StrParam2 := input2;
    Result.StrParam3 := input3;
    Result.IntParam  := input4;
    Result.IntParam2 := input5;
end;

function buildDoFunction(input : LongInt) : PSCommand;
begin
    Result.Name     := _DO;
    Result.Name2    := _FUNC;
    Result.IntParam := input;
end;

function buildCallFunction(input : String = '<Function>') : PSCommand;
begin
    Result.Name     := _CALL;
    Result.Name2    := _FUNC;
    Result.StrParam := input;
end;

function buildSetGlobalVar(input : String) : PSCommand;
begin
    Result.Name     := _SET;
    Result.Name2    := _GVAR;
    Result.StrParam := input;
end;

function buildSetLocalVar(input : String) : PSCommand;
begin
    Result.Name     := _SET;
    Result.Name2    := _LVAR;
    Result.StrParam := input;
end;

function buildGetGlobalVar(input : String) : PSCommand;
begin
    Result.Name     := _GET;
    Result.Name2    := _GVAR;
    Result.StrParam := input;
end;

function buildGetLocalVar(input : String) : PSCommand;
begin
    Result.Name     := _GET;
    Result.Name2    := _LVAR;
    Result.StrParam := input;
end;

function buildCalcAdd() : PSCommand;
begin
    Result.Name  := _CALC;
    Result.Name2 := _ADD;
end;

function buildCalcSub() : PSCommand;
begin
    Result.Name  := _CALC;
    Result.Name2 := _SUB;
end;

function buildCalcInc() : PSCommand;
begin
    Result.Name  := _CALC;
    Result.Name2 := _INC;
end;

function buildCalcDec() : PSCommand;
begin
    Result.Name  := _CALC;
    Result.Name2 := _DEC;
end;

function buildCalcMul() : PSCommand;
begin
    Result.Name  := _CALC;
    Result.Name2 := _MUL;
end;

function buildCalcDiv() : PSCommand;
begin
    Result.Name  := _CALC;
    Result.Name2 := _DIV;
end;

function buildTestEq() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _EQ;
end;

function buildTestNeq() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _NEQ;
end;

function buildTestGt() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _GT;
end;

function buildTestLt() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _LT;
end;

function buildTestGe() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _GE;
end;

function buildTestLe() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _LE;
end;

function buildTestAnd() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _AND;
end;

function buildTestOr() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _OR;
end;

function buildTestXor() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _XOR;
end;

function buildTestNot() : PSCommand;
begin
    Result.Name  := _TEST;
    Result.Name2 := _NOT;
end;

procedure PSEnvironment.makeListOfCommands(L : TStringArray; var db : PSCommandDB; Args : TStringArray = Default(TStringArray));
var
	index  : LongInt;
    x, y   : LongInt;
    z, z1  : LongInt;
    z2, z3 : LongInt;
    s1, s2 : String;
    StrEax : String;
    Num    : Extended;
    Code   : Longint;
    M              : TStringArrArray;
    N              : TStringArray = NIL;
    mode           : PSReaderMode = M_NORM;
    ConditionBuilt : Boolean = False;
begin
    x := Length(db.Commands);
    y := 0;
    SetLength(db.Commands, Length(db.Commands)+1);
    if Length(Args) > 0 then
    begin
        //writeln(Length(Args));
        if (Length(Args[0]) > 0) and (Args[0][1] = '(') then Args[0] := RightStr(Args[0], Length(Args[0])-1); 
        for index := Length(Args)-1 downto 0 do
        begin
            if (Args[index] = '') then continue;
            if (Args[index][1] = '$') then Args[index] := RightStr(Args[index], Length(Args[index])-1);
            if isValidForVariables(Args[index]) then
            begin
                SetLength(db.Commands[x], y+1);
                db.Commands[x][y] := buildSetLocalVar(Args[index]);  
            end else begin
                raiserror('EVariable:CSetInvalid: Invalid variable string at "'+Args[index]+'"');
            end; 
            y := y+1;
        end;
    end;
    index := 0;
    //writeln('length ', #9, x, #9, Length(L));
	while (index < Length(L)) do
	begin
        //writeln('read ', #9, x, #9, L[index]);
        if (L[index] = '') then
		begin
			Inc(index);
			continue;
		end;

        SetLength(db.Commands[x], y+1);
        case L[index] of
            '->' : begin
                index := index + 1;
                while (L[index] = '') do index := index + 1; 
                if (L[index][1] = '$') then L[index] := RightStr(L[index], Length(L[index])-1);
                if isValidForVariables(L[index]) then
                begin
                    if LeftStr(L[index], 7) = 'global.' then 
                    begin
                        db.Commands[x][y] := buildSetGlobalVar(L[index]);
                    end else begin
                        db.Commands[x][y] := buildSetLocalVar(L[index]);
                    end;     
                end else begin
                    raiserror('EVariable:CSetInvalid: Invalid variable string at "'+L[index]+'"');
                end;
            end;
            '+'   : db.Commands[x][y] := buildCalcAdd();
            '-'   : db.Commands[x][y] := buildCalcSub();
            '++'  : db.Commands[x][y] := buildCalcInc();
            '--'  : db.Commands[x][y] := buildCalcDec();
            '*'   : db.Commands[x][y] := buildCalcMul();
            '/'   : db.Commands[x][y] := buildCalcDiv();
            '='   : db.Commands[x][y] := buildTestEq();
		    '!='  : db.Commands[x][y] := buildTestNeq();
		    '>'   : db.Commands[x][y] := buildTestGt();
		    '<'   : db.Commands[x][y] := buildTestLt();
		    '<='  : db.Commands[x][y] := buildTestLe();
		    '>='  : db.Commands[x][y] := buildTestGe();
		    'and' : db.Commands[x][y] := buildTestAnd();
		    'or'  : db.Commands[x][y] := buildTestOr();
		    'xor' : db.Commands[x][y] := buildTestXor();
		    'not' : db.Commands[x][y] := buildTestNot();
            'inc' : db.Commands[x][y] := buildCalcInc();
            'dec' : db.Commands[x][y] := buildCalcDec();
            'rem'     : db.Commands[x][y] := buildCmdPop();
            'clone'   : db.Commands[x][y] := buildCmdClone();
            'qshift'  : db.Commands[x][y] := buildCmdQShift();
            'print'   : db.Commands[x][y] := buildCmdPrint();
            'println' : db.Commands[x][y] := buildCmdPrintLn();
            'scan'    : db.Commands[x][y] := buildCmdScan();
            'size'    : db.Commands[x][y] := buildCmdSize();
            'all'     : db.Commands[x][y] := buildCmdSize();
            'call'    : db.Commands[x][y] := buildCallFunction();
            else begin
                if ((LeftStr(L[index], 1) = '"') and (RightStr(L[index], 1) <> '"'))
                    or ((LeftStr(L[index], 1) = '"') and (RightStr(L[index], 2) = '\"'))
                    or (L[index] = '"') then begin
                    StrEax := getQuotedString(L, index);
                    StrEax := StrEax.Substring(1, StrEax.Length - 2);
                    if Settings.stringmode = MCLIKE then StrEax := string_fromC(StrEax);
                    db.Commands[x][y] := buildCmdPush(buildString(StrEax));
                end else if ((LeftStr(L[index], 1) = '"') and (RightStr(L[index], 1) = '"')) then begin
                    StrEax := L[index];
                    StrEax := StrEax.Substring(1, StrEax.Length - 2);
                    if Settings.stringmode = MCLIKE then StrEax := string_fromC(StrEax);
                    db.Commands[x][y] := buildCmdPush(buildString(StrEax));
                end else if LeftStr(L[index], 1) = '{' then begin
                    z := Length(db.Commands);
                    makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    db.Commands[x][y] := buildCmdJump(z);
                end else if LeftStr(L[index], 1) = '[' then begin
                    z := Length(db.Commands);
                    makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    db.Commands[x][y] := buildCreateArray(z);
                end else if LeftStr(L[index], 1) = '$' then begin
                    L[index] := RightStr(L[index], Length(L[index])-1);
                    if (L[index] <> '') then
                    begin
                        if isValidForVariables(L[index]) then
                        begin
                            if LeftStr(L[index], 7) = 'global.' then 
                            begin
                                db.Commands[x][y] := buildGetGlobalVar(L[index]);
                            end else begin
                                db.Commands[x][y] := buildGetLocalVar(L[index]);
                            end;     
                        end else begin
                            raiserror('EVariable:CGetInvalid: Invalid variable string at $'+L[index]+'');
                        end;
                    end else begin
                        db.Commands[x][y] := buildCmdPush(raiseGetUnnamedVariable());
                    end;
                end else if LeftStr(L[index], 1) = '>' then begin
                    L[index] := RightStr(L[index], Length(L[index])-1);
                    if (L[index] <> '') then
                    begin
                        if isValidForVariables(L[index]) then
                        begin
                            if LeftStr(L[index], 7) = 'global.' then 
                            begin
                                db.Commands[x][y] := buildSetGlobalVar(L[index]);
                            end else begin
                                db.Commands[x][y] := buildSetLocalVar(L[index]);
                            end;     
                        end else begin
                            raiserror('EVariable:CSetInvalid: Invalid variable string at >'+L[index]+'');
                        end;
                    end else begin
                        db.Commands[x][y] := buildCmdPush(raiseGetUnnamedVariable());
                    end;
                end else if LeftStr(L[index], 2) = '->' then begin
                    L[index] := RightStr(L[index], Length(L[index])-2);
                    if (L[index] <> '') then
                    begin
                        if (L[index][1] = '$') then L[index] := RightStr(L[index], Length(L[index])-1);
                        if isValidForVariables(L[index]) then
                        begin
                            if LeftStr(L[index], 7) = 'global.' then 
                            begin
                                db.Commands[x][y] := buildSetGlobalVar(L[index]);
                            end else begin
                                db.Commands[x][y] := buildSetLocalVar(L[index]);
                            end;     
                        end else begin
                            raiserror('EVariable:CSetInvalid: Invalid variable string at >'+L[index]+'');
                        end;
                    end else begin
                        db.Commands[x][y] := buildCmdPush(raiseGetUnnamedVariable());
                    end;
                end else if LeftStr(L[index], 2) = '@@' then begin
                    L[index] := RightStr(L[index], Length(L[index])-2);
                    if (L[index] <> '') then
                    begin
                        if isValidForVariables(L[index]) then
                        begin
                            if LeftStr(L[index], 7) = 'global.' then 
                            begin
                                db.Commands[x][y] := buildGetGlobalVar(L[index]);
                            end else begin
                                db.Commands[x][y] := buildGetLocalVar(L[index]);
                            end;  
                            y := y+1;
                            SetLength(db.Commands[x], y+1);   
                            db.Commands[x][y] := buildCallFunction(L[index]);
                        end else begin
                            raiserror('EVariable:CGetInvalid: Invalid variable string at $'+L[index]+'');
                        end;
                    end else begin
                        db.Commands[x][y] := buildCmdPush(raiseGetUnnamedVariable());
                    end;
                end else if checkWord(L[index], 'times', 5) then begin
                    L[index] := RightStr(L[index], Length(L[index])-5);
                    while (L[index] = '') do index := index + 1; 
                    z := Length(db.Commands);
                    if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                    end;
                    //makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    db.Commands[x][y] := buildDoTimes(z);
                end else if checkWord(L[index], 'while', 5) then begin
                    L[index] := RightStr(L[index], Length(L[index])-5);
                    while (L[index] = '') do index := index + 1; 
                    z := Length(db.Commands);
                    if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                    end;
                    index := index + 1;
                    while (L[index] = '') do index := index + 1; 
                    z1 := Length(db.Commands);
                    if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                    end;
                    db.Commands[x][y] := buildDoWhile(z, z1);
                end else if checkWord(L[index], 'do', 2) then begin
                    L[index] := RightStr(L[index], Length(L[index])-2);
                    while (L[index] = '') do index := index + 1; 
                    z1 := Length(db.Commands);
                    if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                    end;
                    index := index + 1;
                    while (L[index] = '') do index := index + 1; 
                    if checkWord(L[index], 'while', 5) then 
                    begin
                        L[index] := RightStr(L[index], Length(L[index])-5);
                        while (L[index] = '') do index := index + 1; 
                        z := Length(db.Commands);
                        if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                        begin
                            makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                        end else begin
                            makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                        end;
                        db.Commands[x][y] := buildDoDoWhile(z, z1);
                    end else if checkWord(L[index], 'until', 5) then
                    begin
                        L[index] := RightStr(L[index], Length(L[index])-5);
                        while (L[index] = '') do index := index + 1; 
                        z := Length(db.Commands);
                        if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                        begin
                            makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                        end else begin
                            makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                        end;
                        db.Commands[x][y] := buildDoDoUntil(z, z1);
                    end else begin
                        raiserror('ESyntax:CDoLoop: Invalid syntax of a DO-WHILE/DO-UNTIL loop');
                    end;
                end else if checkWord(L[index], 'for', 3) then begin
                    L[index] := RightStr(L[index], Length(L[index])-3);
                    while (L[index] = '') do index := index + 1; 
                    if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                    begin
                        //M := getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1));
                        M := getScopedDelimitedCommands(L, index, RightStr(L[index], Length(L[index])-1));
                    end else begin
                        raiserror('ESyntax:CFor: Invalid syntax of a FOR loop');
                        //M := getScopedCommand(index, L[index]);
                    end;
                    case Length(M) of
                        3 : begin
                            z1 := Length(db.Commands);
                            makeListOfCommands(M[0], db);
                            z2 := Length(db.Commands);
                            makeListOfCommands(M[1], db);
                            z3 := Length(db.Commands);
                            makeListOfCommands(M[2], db);
                            index := index + 1;
                            while (L[index] = '') do index := index + 1; 
                            z := Length(db.Commands);
                            if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                            begin
                                makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                            end else begin
                                makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                            end;
                            db.Commands[x][y] := buildDoFor1(z1, z2, z3, z);
                        end;
                        2 : begin
                            // to fix
                            z2 := Length(M[0])-1;
                            if (z2 <= 2) then
                            begin
                                //writeln(z2);
                                if (z2 < 0) then raiserror('ESyntax:CForEach: No parameters in the FOR-EACH loop'); 
                                //writeln('ok');
                                if (z2 >= 0) then s2 := M[0][z2]       else s2 := ''; 
                                //writeln('ok');
                                if (z2 >= 1) then s1 := M[0][z2-1]     else s1 := ''; 
                                //writeln('ok');
                                if (z2  = 2) then StrEax := M[0][z2-2] else StrEax := ''; 
                                //writeln('ok');
                                if (StrEax <> '') and (StrEax <> 'const') then raiserror('ESyntax:CForEach: Invalid amount of parameters in the FOR-EACH loop'); 
                                if (Length(s1) > 0) and (s1[1] = '$') then s1 := RightStr(s1, Length(s1)-1);
                                if (s2[1] = '$') then s2 := RightStr(s2, Length(s2)-1);
                                if (Length(s1) > 0) and (not isValidForVariables(s1)) then
                                begin
                                    raiserror('EVariable:CSetInvalid: Invalid name of variable at "'+s1+'"');
                                end;
                                if (not isValidForVariables(s2)) then
                                begin
                                    raiserror('EVariable:CSetInvalid: Invalid name of variable at "'+s2+'"');
                                end;
                            end else begin
                                raiserror('ESyntax:CForEach: Invalid amount of parameters in the FOR-EACH loop');
                            end;
                            z1 := Length(db.Commands);
                            makeListOfCommands(M[1], db);
                            index := index + 1;
                            while (L[index] = '') do index := index + 1; 
                            z := Length(db.Commands);
                            if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                            begin
                                makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                            end else begin
                                //makeListOfCommands(getScopedCommand(index, L[index]), db);
                                makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                            end;
                            db.Commands[x][y] := buildDoFor2(StrEax, s1, s2, z1, z);
                        end;
                        else begin
                            raiserror('ESyntax:CFor: Invalid syntax of a FOR loop');
                        end;
                    end;
                    for z := 0 to Length(M)-1 do SetLength(M[z], 0);
                    SetLength(M, 0);
                end else if checkWord(L[index], 'if', 2) then begin
                    L[index] := RightStr(L[index], Length(L[index])-2);
                    while (L[index] = '') do index := index + 1; 
                    ConditionBuilt := False;
                    z := Length(db.Conditionals);
                    SetLength(db.Conditionals, z+1);
                    z2 := Length(db.Commands);
                    if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                    end;
                    index := index + 1;
                    while (L[index] = '') do index := index + 1; 
                    z3 := Length(db.Commands);
                    if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                    end;
                    db.Conditionals[z].MainItem.Condition := z2;
                    db.Conditionals[z].MainItem.Instruction := z3;
                    db.Conditionals[z].ElseItem := -1;
                    z1 := Length(db.Conditionals[z].ElifItems);
                    //repeat
                    while (not ConditionBuilt) and (index < Length(L)) do
                    begin
                        index := index + 1;
                        while (L[index] = '') do index := index + 1; 
                        if checkWord(L[index], 'elif', 4) then
                        begin
                            L[index] := RightStr(L[index], Length(L[index])-4);
                            while (L[index] = '') do index := index + 1; 
                            z2 := Length(db.Commands);
                            if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                            begin
                                makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                            end else begin
                                makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                            end;
                            index := index + 1;
                            while (L[index] = '') do index := index + 1; 
                            z3 := Length(db.Commands);
                            if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                            begin
                                makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                            end else begin
                                makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                            end;
                            SetLength(db.Conditionals[z].ElifItems, z1+1);
                            db.Conditionals[z].ElifItems[z1].Condition := z2;
                            db.Conditionals[z].ElifItems[z1].Instruction := z3;
                            z1 := z1 + 1;
                        end else if checkWord(L[index], 'else', 4) then
                        begin
                            L[index] := RightStr(L[index], Length(L[index])-4);
                            while (L[index] = '') do index := index + 1; 
                            z3 := Length(db.Commands);
                            if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                            begin
                                makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), db);
                            end else begin
                                makeListOfCommands(getScopedCommands(L, index, L[index], 0), db);
                            end;
                            db.Conditionals[z].ElseItem := z3;
                            ConditionBuilt := True;
                        end else begin
                            Dec(Index);
                            ConditionBuilt := True;
                        end;
                    end;
                    //until ConditionBuilt;
                    db.Commands[x][y] := buildDoIf(z);
                end else if checkWord(L[index], 'function', 8) then begin
                    L[index] := RightStr(L[index], Length(L[index])-8);
                    while (L[index] = '') do index := index + 1; 
                    if (LeftStr(L[index], 1) = '(') then
                    begin
                        //z1 := Length(db.Commands);
                        if (LeftStr(L[index], 1) = '(') and (RightStr(L[index], 1) <> ')') then
                        begin
                            N := getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1));
                        end else begin
                            //N := getScopedCommand(index, L[index]);
                            //if (LeftStr(L[index], 1) = '(') then RightStr(L[index], Length(L[index])-1);
                            //writeln(L[index]);
                            N := getScopedCommands(L, index, L[index], 0);
                        end;
                        index := index + 1;
                    end else begin
                        SetLength(N, 0);
                    end;
                    z := Length(Scripts);
                    SetLength(Scripts, z+1);
                    if (LeftStr(L[index], 1) = '{') and (RightStr(L[index], 1) <> '}') then
                    begin
                        makeListOfCommands(getScopedCommands(L, index, RightStr(L[index], Length(L[index])-1)), Scripts[z], N);
                    end else begin
                        makeListOfCommands(getScopedCommands(L, index, L[index], 0), Scripts[z], N);
                    end;
                    db.Commands[x][y] := buildCreateFunction(z);
                    SetLength(N, 0);
                end else begin
                    Val(L[index], Num, Code);
                    if Code <> 0 then
                    begin
                        db.Commands[x][y] := buildCmdEval(L[index]);
                    end else begin
                        db.Commands[x][y] := buildCmdPush(buildNumber(Num));
                    end;
                end;
            end;
        end;
        y := y+1;
        Inc(index);
    end;
end;


function PSEnvironment.buildCommands(input : String) : PSCommandDB;
var
    db : PSCommandDB;
    L  : TStringArray;
begin
    L := input.Split([' ', #9, #13, #10]);
    makeListOfCommands(L, db);
    Result := db;
end;

// to remove
//procedure PSEnvironment.buildCommands(input : String; var db : PSCommandDB);
//var
//    L : TStringArray;
//begin
//    L := input.Split([' ', #9, #13, #10]);
//    makeListOfCommands(L, db);
//end;

// -----------------------------------------------------------------------------------------
// code execution
// basic operations

procedure PSEnvironment.checkExceptions(var db : PSCommandDB);
begin
    if (Settings.StrictType) and (stack_searchException(Stack[Settings.StackPointer])) then
    begin
        //SetLength(db, 0);
        SetLength(Scripts, 0);
		raiserror(stack_pop(Stack[Settings.StackPointer]).Str);
	end;
end;

procedure doCalcAdd(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
    stack_push(env.Stack[env.Settings.StackPointer], EntEax + EntEbx);
end;

procedure doCalcSub(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
    if (not env.Settings.InfMode) or (EntEax.EntityType <> TNUM) or (EntEbx.EntityType <> TNUM) then
    begin
        stack_push(env.Stack[env.Settings.StackPointer], EntEax - EntEbx);    
    end else begin
        if ((EntEax.Num = Infinity) or (EntEbx.Num = -Infinity)) and ((EntEbx.Num = Infinity) or (EntEax.Num = -Infinity)) then
        begin
            stack_push(env.Stack[env.Settings.StackPointer], buildNumber(NaN));
        end else begin
            stack_push(env.Stack[env.Settings.StackPointer], EntEax - EntEbx);
        end;
    end;
end;

procedure doCalcInc(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    end;
    stack_push(env.Stack[env.Settings.StackPointer], EntEax + buildNumber(1));
end;

procedure doCalcDec(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    end;
    stack_push(env.Stack[env.Settings.StackPointer], EntEax - buildNumber(1));
end;

procedure doCalcMul(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
    stack_push(env.Stack[env.Settings.StackPointer], EntEax * EntEbx);
end;

procedure doCalcDiv(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
    if (not env.Settings.InfMode) then
    begin
        if isZero(EntEbx)
            then stack_push(env.Stack[env.Settings.StackPointer], raiseDivisionZero('/'))
            else stack_push(env.Stack[env.Settings.StackPointer], EntEax / EntEbx);
    end else begin
        if isNumber(EntEbx) then
        begin
            if isZero(EntEbx) then
            begin
                stack_push(env.Stack[env.Settings.StackPointer], EntEax * buildNumber(Infinity));
            end else if isZero(EntEbx) and isZero(EntEax) then
            begin
                stack_push(env.Stack[env.Settings.StackPointer], buildNumber(NaN));
            end else if isAnyInfinity(EntEbx) and isAnyInfinity(EntEax) then
            begin
                stack_push(env.Stack[env.Settings.StackPointer], buildNumber(NaN));
            end else if isPosInfinity(EntEbx) then
            begin
                stack_push(env.Stack[env.Settings.StackPointer], buildNumber(0));
            end else if isNegInfinity(EntEbx) then
            begin
                stack_push(env.Stack[env.Settings.StackPointer], buildNumber(0));
            end else begin
                stack_push(env.Stack[env.Settings.StackPointer], EntEax / EntEbx);
            end;
        end else begin
            stack_push(env.Stack[env.Settings.StackPointer], EntEax / EntEbx);
        end;
    end;
end;

procedure doTestEq(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := (EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num);
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEax));
end;

procedure doTestNeq(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := not ((EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num));
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEax));
end;

procedure doTestGt(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := EntEax.Num > EntEbx.Num;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEax));
end;

procedure doTestLt(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := EntEax.Num < EntEbx.Num;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEax));
end;

procedure doTestLe(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := EntEax.Num <= EntEbx.Num;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEax));
end;

procedure doTestGe(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := EntEax.Num >= EntEbx.Num;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEax));
end;

procedure doTestAnd(var env : PSEnvironment);
var
    EntEax, EntEbx         : Entity;
	LogEax, LogEbx, LogEcx : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	if EntEax.Num = 0 then LogEax := true else LogEax := false;
	if EntEbx.Num = 0 then LogEbx := true else LogEbx := false;
	LogEcx := LogEax and LogEbx;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEcx));
end;

procedure doTestOr(var env : PSEnvironment);
var
    EntEax, EntEbx         : Entity;
	LogEax, LogEbx, LogEcx : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	if EntEax.Num = 0 then LogEax := true else LogEax := false;
	if EntEbx.Num = 0 then LogEbx := true else LogEbx := false;
	LogEcx := LogEax or LogEbx;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEcx));
end;

procedure doTestXor(var env : PSEnvironment);
var
    EntEax, EntEbx         : Entity;
	LogEax, LogEbx, LogEcx : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	if EntEax.Num = 0 then LogEax := true else LogEax := false;
	if EntEbx.Num = 0 then LogEbx := true else LogEbx := false;
	LogEcx := LogEax xor LogEbx;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    	stack_push(env.Stack[env.Settings.StackPointer], EntEbx);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEcx));
end;

procedure doTestNot(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax, LogEcx : Boolean;
begin
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	if EntEax.Num = 0 then LogEax := true else LogEax := false;
	LogEcx := not LogEax;
	if not (env.Settings.Autoclear) then begin
    	stack_push(env.Stack[env.Settings.StackPointer], EntEax);
    end;
	stack_push(env.Stack[env.Settings.StackPointer], buildBoolean(LogEcx));
end;

// structures

procedure PSEnvironment.wrapArray(var db : PSCommandDB; at : LongInt);
var
    stk    : LongInt;
    ArrEcx : Entity;
begin
    stk := Settings.StackPointer;
    stack_push(Stack[Settings.StackPointer], buildNewEmptyArray(Stack, Settings));
    ArrEcx := stack_pop(Stack[Settings.StackPointer]);
    Settings.StackPointer := trunc(ArrEcx.Num);
    executeSet(db, at);
    //writeln(stack_size(Stack[Settings.StackPointer]));
    Settings.StackPointer := stk;
    stack_push(Stack[Settings.StackPointer], ArrEcx);
end;

procedure PSEnvironment.doTimes(var db : PSCommandDB; at : LongInt; n : LongInt);
var
    i : LongInt;
begin
    i := 1;
    while (i <= n) do
    begin
        executeSet(db, at);
        i := i+1;
    end;
end;

procedure PSEnvironment.doWhile(var db : PSCommandDB; cond : LongInt; inst : LongInt);
begin
    executeSet(db, cond);
    while True do
    begin
        if (trunc(stack_pop(Stack[Settings.StackPointer]).Num) <> 0) then break;
        executeSet(db, inst);
        executeSet(db, cond);
    end;
end;

procedure PSEnvironment.doDoWhile(var db : PSCommandDB; cond : LongInt; inst : LongInt);
begin
    while True do
    begin
        executeSet(db, inst);
        executeSet(db, cond);
        if (trunc(stack_pop(Stack[Settings.StackPointer]).Num) <> 0) then break;
    end;
end;

procedure PSEnvironment.doUntil(var db : PSCommandDB; cond : LongInt; inst : LongInt);
begin
    executeSet(db, cond);
    while True do
    begin
        if (trunc(stack_pop(Stack[Settings.StackPointer]).Num) = 0) then break;
        executeSet(db, inst);
        executeSet(db, cond);
    end;
end;

procedure PSEnvironment.doDoUntil(var db : PSCommandDB; cond : LongInt; inst : LongInt);
begin
    while True do
    begin
        executeSet(db, inst);
        executeSet(db, cond);
        if (trunc(stack_pop(Stack[Settings.StackPointer]).Num) = 0) then break;
    end;
end;

procedure PSEnvironment.doFor1(var db : PSCommandDB; init : LongInt; cond : LongInt; incr : LongInt; inst : LongInt);
begin
    executeSet(db, init);
    executeSet(db, cond);
    while True do
    begin
        if (trunc(stack_pop(Stack[Settings.StackPointer]).Num) <> 0) then break;
        executeSet(db, inst);
        executeSet(db, incr);
        executeSet(db, cond);
    end;
end;

procedure PSEnvironment.doFor2(var db : PSCommandDB; init : String; indx : String; item : String; arry : LongInt; inst : LongInt);
var
    ArrEax   : Entity;
    location : LongInt;
    alter    : Boolean = True;
    index    : LongInt = 0;
    usesvar  : Boolean = False;
begin
    executeSet(db, arry);
    // add own exception
    if (Settings.StrictType) and (assertEntityLocated(Stack[Settings.StackPointer], stack_get(Stack[Settings.StackPointer]), TVEC, '<for-each loop>')) then Exit; 
    ArrEax := stack_pop(Stack[Settings.StackPointer]);
    if (Length(db.Commands[arry]) = 1) and (db.Commands[arry][0].Name = _GET) and (db.Commands[arry][0].Name2 in [_LVAR, _GVAR]) then usesvar := True;
    location := trunc(ArrEax.Num);
    Variables.addLayer();
    if init = 'const' then alter := False;
    if indx = '' then
    begin
        for index := 0 to Length(Stack[location].Values)-1 do
        begin
            Variables.setLocalVariable(item, Stack[location].Values[index]);
            executeSet(db, inst);
            if alter then Stack[location].Values[index] := Variables.getLocalVariable(item);
        end;
    end else begin
        for index := 0 to Length(Stack[location].Values)-1 do
        begin
            Variables.setLocalVariable(indx, buildNumber(index));
            Variables.setLocalVariable(item, Stack[location].Values[index]);
            executeSet(db, inst);
            if alter then Stack[location].Values[index] := Variables.getLocalVariable(item);
        end;
    end;
    Variables.removeLayer();
    if not usesvar then stack_clear(Stack[location]);
end;

procedure PSEnvironment.doIf(var db : PSCommandDB; at : LongInt);
var
    flag : Boolean = True;
    i    : LongInt;
begin
    executeSet(db, db.Conditionals[at].MainItem.Condition);
    if stack_pop(Stack[Settings.StackPointer]).Num = 0 then 
    begin
        executeSet(db, db.Conditionals[at].MainItem.Instruction);
    end else begin
        if (Length(db.Conditionals[at].ElifItems) > 0) then
        begin
            for i := 0 to Length(db.Conditionals[at].ElifItems)-1 do
            begin
                executeSet(db, db.Conditionals[at].ElifItems[i].Condition);
                if stack_pop(Stack[Settings.StackPointer]).Num = 0 then 
                begin
                    flag := False;
                    executeSet(db, db.Conditionals[at].ElifItems[i].Instruction);
                    break;
                end;
            end;
        end;
        if flag and (db.Conditionals[at].ElseItem <> -1) then
        begin
            executeSet(db, db.Conditionals[at].ElseItem);
        end;
    end;
end;

//procedure PSEnvironment.doFunction(var db : PSCommandDB; at : LongInt);
//begin
//    Variables.addLayer();
//    executeSet(db, at);
//    Variables.removeLayer();
//end;

procedure PSEnvironment.doFunction(at : LongInt);
begin
    Variables.addLayer();
    executeCommands(Scripts[at]);
    Variables.removeLayer();
end;

// complicated evaluations

procedure PSEnvironment.variablePutOrRun(var db : PSCommandDB; guess : String);
var
    EntEax : Entity;
begin
    EntEax := Variables.getVariable(guess);
    if (EntEax.EntityType = TFUN) then
    begin
        doFunction(trunc(EntEax.Num));
    end else begin
        stack_push(Stack[Settings.StackPointer], EntEax);
    end;
end;

procedure PSEnvironment.evaluate(var db : PSCommandDB; input : String);
var
    Im     : Extended;
    Code   : Longint;
    StrEcx : String;
    Found  : Boolean = True;
begin
    Steps := 1;

    //checkExceptionsOld(Stack, Settings);
    if (LeftStr(input, 1) = '"') and (RightStr(input, 1) = '"') then
	begin
        StrEcx := input.Substring(1, input.Length - 2);
        if Settings.stringmode = MCLIKE then StrEcx := string_fromC(StrEcx);
        stack_push(Stack[Settings.StackPointer], buildString(StrEcx));
	end else begin
        //if not (Settings.CaseSensitive) then input := LowerCase(input);
    	Val(input, Im, Code);
        if Code <> 0 then
        begin
            if (Variables.isVarAssigned(input)) then
            begin
                variablePutOrRun(input, db)
            end else if (input[1] in ['$', '>', '-', '~', '@', '?']) then
            begin
                if not lib_directives(input, Self, db) then
                if not lib_logics(input, Self, db) then
                if not lib_variables(input, Self, db) then
                if not lib_ultravanilla(input, Self, db) then
                    Found := False;
            end else if (getPackage(input) <> '') then begin
                if not searchThroughNamespacesExplicit(input, Self, db) then 
                    Found := False;
            end else begin
    	        if not lib_constants(input, Self, db) then
    	        if not lib_logics(input, Self, db) then
                if not lib_ultravanilla(input, Self, db) then
                if not lib_exceptions(input, Self, db) then
                if not searchThroughNamespacesImplicit(input, Self, db) then 
                if not lib_files(input, Self, db) then
                if not lib_variables2(input, Self, db) then
                    Found := False;
            end;
        end else begin
            if (input <> '.')
                then stack_push(Stack[Settings.StackPointer], buildNumber(Im))
                else stack_push(Stack[Settings.StackPointer], raiseExceptionUnknownCommand(input));
        end;

        if not Found then
        begin
            if (Settings.StrictType) and (stack_searchException(Stack[Settings.StackPointer])) then
    		begin
				raiserror(stack_pop(Stack[Settings.StackPointer]).Str);
			end else begin
                stack_push(Stack[Settings.StackPointer], raiseExceptionUnknownCommand(input));
            end;
        end;
    end;
    //checkExceptionsOld(Stack, Settings);
end;


procedure PSEnvironment.executeSet(var db : PSCommandDB; at : LongInt = 0);
var
    i      : LongInt;
    EntEax : Entity;
begin
    //writeln('commands to do: ', Length(db.Commands[at]));
    for i := 0 to Length(db.Commands[at])-1 do
    begin
        //write('doing ', #9, db.Commands[at][i].Name);
        case db.Commands[at][i].Name of
            _EVAL : begin
                evaluate(db, db.Commands[at][i].StrParam);
            end;
            _PUSH : begin
                stack_push(Stack[Settings.StackPointer], db.Commands[at][i].EntityRelated);
            end;
            _POP : begin
                stack_justpop(Stack[Settings.StackPointer]);
            end;
            _JUMP : begin
                executeSet(db, db.Commands[at][i].IntParam);
            end;
            _CREATE : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _ARRAY : begin
                        wrapArray(db, db.Commands[at][i].IntParam);
                    end;
                    _FUNC : begin
                        stack_push(Stack[Settings.StackPointer], buildFunction(db.Commands[at][i].IntParam));
                    end;
                end;
            end;
            _DO : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _TIMES : begin
                        if (Settings.StrictType) and (assertNaturalLocated(Stack[Settings.StackPointer], stack_get(Stack[Settings.StackPointer]), 'times')) then Exit; 
                        doTimes(db, db.Commands[at][i].IntParam, trunc(stack_pop(Stack[Settings.StackPointer]).Num));
                    end;
                    _IF : begin
                        doIf(db, db.Commands[at][i].IntParam);
                    end;
                    _WHILE : begin
                        doWhile(db, db.Commands[at][i].IntParam, db.Commands[at][i].IntParam2);
                    end;
                    _DWHILE : begin
                        doDoWhile(db, db.Commands[at][i].IntParam, db.Commands[at][i].IntParam2);
                    end;
                    _DUNTIL : begin
                        doDoUntil(db, db.Commands[at][i].IntParam, db.Commands[at][i].IntParam2);
                    end;
                    _FOR : begin
                        doFor1(db, db.Commands[at][i].IntParam, 
                                   db.Commands[at][i].IntParam2,
                                   db.Commands[at][i].IntParam3,
                                   db.Commands[at][i].IntParam4
                        );
                    end;
                    _FOR2 : begin
                        doFor2(db, db.Commands[at][i].StrParam, 
                                   db.Commands[at][i].StrParam2,
                                   db.Commands[at][i].StrParam3,
                                   db.Commands[at][i].IntParam,
                                   db.Commands[at][i].IntParam2
                        );
                    end;
                    _FUNC : begin
                        doFunction(db.Commands[at][i].IntParam);
                    end;
                end;
            end;
            _CALL : begin
                case db.Commands[at][i].Name2 of
                    _FUNC : begin
                        // new exception
                        if (Settings.StrictType) and (assertEntityLocated(Stack[Settings.StackPointer], stack_get(Stack[Settings.StackPointer]), TFUN, '')) then Exit; 
                        EntEax := stack_pop(Stack[Settings.StackPointer]);
                        doFunction(trunc(EntEax.Num));
                    end;
                end;
            end;
            _SET : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _LVAR : begin
                        Variables.setLocalVariable(db.Commands[at][i].StrParam, stack_pop(Stack[Settings.StackPointer]));
                    end;
                    _GVAR : begin
                        Variables.setGlobalVariable(db.Commands[at][i].StrParam, stack_pop(Stack[Settings.StackPointer]));
                    end;
                end;
            end;
            _GET : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _LVAR : begin
                        stack_push(Stack[Settings.StackPointer], Variables.getVariable(db.Commands[at][i].StrParam));
                    end;
                    _GVAR : begin
                        stack_push(Stack[Settings.StackPointer], Variables.getGlobalVariable(db.Commands[at][i].StrParam));
                    end;
                end;
            end;
            _CALC : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _ADD : doCalcAdd(Self);
                    _SUB : doCalcSub(Self);
                    _INC : doCalcInc(Self);
                    _DEC : doCalcDec(Self);
                    _MUL : doCalcMul(Self);
                    _DIV : doCalcDiv(Self);
                end;
            end;
            _TEST : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _EQ  : doTestEq(Self);
                    _NEQ : doTestNeq(Self);
                    _GT  : doTestGt(Self);
                    _LT  : doTestLt(Self);
                    _GE  : doTestGe(Self);
                    _LE  : doTestLe(Self);
                    _NOT : doTestNot(Self);
                    _AND : doTestAnd(Self);
                    _OR  : doTestOr(Self);
                    _XOR : doTestXor(Self);
                end;
            end;
            _SCAN : begin
                stack_push(Stack[Settings.StackPointer], scan_value());
            end;
            _CLONE : begin
                stack_push(Stack[Settings.StackPointer], stack_get(Stack[Settings.StackPointer]));
            end;
            _QSHIFT : begin
                stack_push(Stack[Settings.StackPointer], stack_firstpop(Stack[Settings.StackPointer]));
            end;
            _PRINT : begin
                //write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _NONE : begin
                        EntEax := stack_get(Stack[Settings.StackPointer]);
                        if (Settings.Autoclear) then stack_pop(Stack[Settings.StackPointer]);
                        case EntEax.EntityType of
                            TNUM : writeOnConsole(FormatFloat(Settings.Mask, EntEax.Num));
                            TVEC : writeOnConsole(stack_showArrayPS(Stack[trunc(EntEax.Num)], Stack, Settings.Mask));
                            else   writeOnConsole(EntEax.Str);
                        end;
                    end;
                    _LN : begin
                        EntEax := stack_get(Stack[Settings.StackPointer]);
                        if (Settings.Autoclear) then stack_pop(Stack[Settings.StackPointer]);
                        case EntEax.EntityType of
                            TNUM : writelnOnConsole(FormatFloat(Settings.Mask, EntEax.Num));
                            TVEC : writelnOnConsole(stack_showArrayPS(Stack[trunc(EntEax.Num)], Stack, Settings.Mask));
                            else   writelnOnConsole(EntEax.Str);
                        end;
                    end;
                end;
            end;
            _SIZE : begin
          	    stack_push(Stack[Settings.StackPointer], buildNumber(stack_size(Stack[Settings.StackPointer])));
            end;
        end;
        //writeln();
        checkExceptions(db);
    end;
end;

procedure PSEnvironment.executeCommands(cmds : PSCommandDB);
var i, at : LongInt;
begin
    if (Length(cmds.Commands) > 0) then 
    begin
        //for at := 0 to Length(cmds)-1 do 
        //begin
        //    for i := 0 to Length(cmds[at])-1 do write(cmds[at][i].Command, #9);
        //    writeln();
        //end;
        executeSet(cmds);
    end;
end;

// to be rebuilt as script = function


procedure PSEnvironment.executePSCode(input : String);
var
//   cmds : PSCommandDB;
    index : LongInt;
begin
    //buildCommands(input, cmds);
    //executeCommands(cmds);
    //SetLength(cmds, 0);
    index := Length(Scripts);
    SetLength(Scripts, Length(Scripts)+1);
    Scripts[index] := buildCommands(input);
    executeCommands(Scripts[index]);
    //SetLength(Scripts, Length(Scripts)-1);
end;

end.
