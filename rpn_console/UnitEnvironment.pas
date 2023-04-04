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
    _NONE,
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
    _SHL,
    _SHR,
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
    _LN
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
        //procedure makeListOfCommands(L : TStringArray; var db : PSCommandDB; Args : TStringArray = Default(TStringArray));
        //procedure buildCommands(input : String; var db : PSCommandDB; Args : TStringArray = Default(TStringArray));
        //function makeListOfCommands(L : TStringArray; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
        //function buildCommands(input : String) : PSCommandDB;
        //function makeListOfCommands(L : TStringArray; var db : PSCommandDB; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
        //function buildCommands(input : String; var db : PSCommandDB; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
        //function buildCommands(input : String; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
        function makeListOfCommands(L : TStringArray; db : PSCommandDB; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
        function buildCommands(input : String; db : PSCommandDB; Args : TStringArray = Default(TStringArray)) : PSCommandDB;
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
    //RPNAbout,
    DateUtils;

var
	Steps : Integer;

{$I FunctionsImpl.fph}

{$I EnviDef1.fph}

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
        if (not togglequote) then
        begin
            if (input[i] in ['[', '{', '(', ']', '}', ')', ';', ':']) then
            begin
                if (input[i-1] <> ' ') 
                    then pom := pom + ' '+ input[i]
                    else pom := pom + input[i];
                if (input[i+1] <> ' ') 
                    then pom := pom + ' ';
            end else begin
                pom := pom + input[i];
            end;
        end else begin
            pom := pom + input[i];
        end;
        i := i + 1;
    end;
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


{$I CommandBuilder.fph}


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
    stack_push(env.Stack[env.Settings.StackPointer], EntEax + EntEbx);
end;

procedure doCalcSub(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
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
    stack_push(env.Stack[env.Settings.StackPointer], EntEax + buildNumber(1));
end;

procedure doCalcDec(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    stack_push(env.Stack[env.Settings.StackPointer], EntEax - buildNumber(1));
end;

procedure doCalcMul(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    stack_push(env.Stack[env.Settings.StackPointer], EntEax * EntEbx);
end;

procedure doCalcDiv(var env : PSEnvironment);
const CmdLabel = '/';
var
    EntEax, EntEbx : Entity;
begin
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if (not env.Settings.InfMode) then
    begin
        if isZero(EntEbx)
            then stack_push(env.Stack[env.Settings.StackPointer], raiseDivisionZero(CmdLabel))
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

procedure doCalcIntegerDiv(var env : PSEnvironment);
const CmdLabel = 'div';
var
    EntEax, EntEbx : Entity;
begin
    if (env.Settings.StrictType) and (assertEntityLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), TNUM, CmdLabel)) then Exit;
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    if (env.Settings.StrictType) and (assertEntityLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), TNUM, CmdLabel)) then Exit;
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if isZero(EntEbx)
        then stack_push(env.Stack[env.Settings.StackPointer], raiseDivisionZero(CmdLabel))
        else stack_push(env.Stack[env.Settings.StackPointer], buildNumber(fdiv(EntEax.Num, EntEbx.Num)));
end;

procedure doCalcMod(var env : PSEnvironment);
const CmdLabel = 'mod';
var
    EntEax, EntEbx : Entity;
begin
    if (env.Settings.StrictType) and (assertEntityLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), TNUM, CmdLabel)) then Exit;
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    if (env.Settings.StrictType) and (assertEntityLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), TNUM, CmdLabel)) then Exit;
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if isZero(EntEbx)
        then stack_push(env.Stack[env.Settings.StackPointer], raiseDivisionZero(CmdLabel))
        else stack_push(env.Stack[env.Settings.StackPointer], buildNumber(fmod(EntEax.Num, EntEbx.Num)));
end;

// TODO
// add support for real numbers (if possible)
procedure doCalcIntegerDiv2(var env : PSEnvironment);
const CmdLabel = 'cdiv';
var
    EntEax, EntEbx : Entity;
begin
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if isZero(EntEbx)
        then stack_push(env.Stack[env.Settings.StackPointer], raiseDivisionZero(CmdLabel))
        else stack_push(env.Stack[env.Settings.StackPointer], buildNumber(ffloor(EntEax.Num / EntEbx.Num)));
end;

// TODO
// add support for real numbers (if possible)
procedure doCalcMod2(var env : PSEnvironment);
const CmdLabel = 'cmod';
var
    EntEax, EntEbx : Entity;
    x, y, z        : LongInt;
begin
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
    if isZero(EntEbx)
        then stack_push(env.Stack[env.Settings.StackPointer], raiseDivisionZero(CmdLabel))
        else begin
            x := trunc(EntEax.Num);
            y := trunc(EntEbx.Num);
            if (x > 0) and (y < 0) then begin
            	z := ((x mod y) + y + y) mod y;
            end else if (x < 0) and (y > 0) then begin
            	z := ((x mod y) + y) mod y;
            end else begin
            	z := x mod y;
            end;
            stack_push(env.Stack[env.Settings.StackPointer], buildNumber(z));
        end;
end;

// TODO
// move everything to 64bit
procedure doCalcShl(var env : PSEnvironment);
const CmdLabel = 'shl';
var x, y : {$IFDEF cpu64} Int64 {$ELSE} LongInt {$ENDIF};
begin
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    y := trunc(stack_pop(env.Stack[env.Settings.StackPointer]).Num);
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    x := trunc(stack_pop(env.Stack[env.Settings.StackPointer]).Num);
    stack_push(env.Stack[env.Settings.StackPointer], buildNumber(x shl y));
end;

// TODO
// move everything to 64bit
procedure doCalcShr(var env : PSEnvironment);
const CmdLabel = 'shr';
var x, y : {$IFDEF cpu64} Int64 {$ELSE} LongInt {$ENDIF};
begin
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    y := trunc(stack_pop(env.Stack[env.Settings.StackPointer]).Num);
    if (env.Settings.StrictType) and (assertIntegerLocated(env.Stack[env.Settings.StackPointer], stack_get(env.Stack[env.Settings.StackPointer]), CmdLabel)) then Exit;
    x := trunc(stack_pop(env.Stack[env.Settings.StackPointer]).Num);
    stack_push(env.Stack[env.Settings.StackPointer], buildNumber(x shr y));
end;

procedure doTestEq(var env : PSEnvironment);
var
    EntEax, EntEbx : Entity;
	LogEax         : Boolean;
begin
	EntEbx := stack_pop(env.Stack[env.Settings.StackPointer]);
    EntEax := stack_pop(env.Stack[env.Settings.StackPointer]);
	LogEax := (EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num);
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
    if (Length(db.Commands[arry]) = 1) and (
        ((db.Commands[arry][0].Name = _GET) and (db.Commands[arry][0].Name2 in [_LVAR, _GVAR]))
        or (db.Commands[arry][0].Name = _EVAL)
        ) then usesvar := True;
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
    // improve garbage collection
    //if not usesvar then stack_clear(Stack[location]);
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
    //{debug-exec} writeln('commands to do: ', Length(db.Commands[at]));
    for i := 0 to Length(db.Commands[at])-1 do
    begin
        //{debug-exec} write('doing ', #9, db.Commands[at][i].Name);
        case db.Commands[at][i].Name of
            _EVAL : begin
                //{debug-exec} write(#9, db.Commands[at][i].StrParam);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _ADD   : doCalcAdd(Self);
                    _SUB   : doCalcSub(Self);
                    _INC   : doCalcInc(Self);
                    _DEC   : doCalcDec(Self);
                    _MUL   : doCalcMul(Self);
                    _DIV   : doCalcDiv(Self);
                    _MOD   : doCalcMod(Self);
                    _IDIV  : doCalcIntegerDiv(Self);
                    _SHL   : doCalcShl(Self);
                    _SHR   : doCalcShr(Self);
                    _CMOD  : doCalcMod2(Self);
                    _CIDIV : doCalcIntegerDiv2(Self);
                end;
            end;
            _TEST : begin
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
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
                //{debug-exec} write(#9, db.Commands[at][i].Name2);
                case db.Commands[at][i].Name2 of
                    _NONE : begin
                        EntEax := stack_pop(Stack[Settings.StackPointer]);
                        case EntEax.EntityType of
                            TNUM : writeOnConsole(FormatFloat(Settings.Mask, EntEax.Num));
                            TVEC : writeOnConsole(stack_showArrayPS(Stack[trunc(EntEax.Num)], Stack, Settings.Mask));
                            else   writeOnConsole(EntEax.Str);
                        end;
                    end;
                    _LN : begin
                        EntEax := stack_pop(Stack[Settings.StackPointer]);
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
        checkExceptions(db);
    end;
end;

procedure PSEnvironment.executeCommands(cmds : PSCommandDB);
var i, at : LongInt;
begin
    //{debug-exec} writeln('length ', Length(cmds.Commands));
    if (Length(cmds.Commands) > 0) then 
    begin
        //{debug-exec} for at := 0 to Length(cmds.Commands)-1 do 
        //{debug-exec} begin
        //{debug-exec}     writeln('set ', at);
        //{debug-exec}     for i := 0 to Length(cmds.Commands[at])-1 do 
        //{debug-exec}     begin
        //{debug-exec}         write(cmds.Commands[at][i].Name, #9);
        //{debug-exec}         if (cmds.Commands[at][i].Name2 <> _NONE) then write(cmds.Commands[at][i].Name2, #9);
        //{debug-exec}         writeln();
        //{debug-exec}     end;
        //{debug-exec}     writeln();
        //{debug-exec} end;
        executeSet(cmds);
    end;
end;

// to be rebuilt as script = function

// --------------------------------------------------
// run code

procedure PSEnvironment.executePSCode(input : String);
var
//   cmds : PSCommandDB;
    index : LongInt;
begin
    index := Length(Scripts);
    SetLength(Scripts, index+1);
    //{debug-adjust} writeln('adjusted code:', #13#10, input); 
    //{debug-adjust} writeln('Done.'); 
    //{debug-build} writeln('2 - COMMANDS CONSTRUCTION'); 
    {build} Scripts[index] := buildCommands(input, Scripts[index]);
    //{debug-build} writeln('Done.'); 
    //{debug-exec} writeln('3 - COMMANDS EXECUTION'); 
    {exec} executeCommands(Scripts[index]);
    // xd
end;

procedure PSEnvironment.runFromString(input : String);
begin
    //{debug-adjust} writeln('1 - CODE ADJUSTMENT'); 
    //{debug-adjust} writeln('raw code:', #13#10, input); 
    // adjust code
    if (Length(input) > 0) then 
    begin
        input := cutShebang(input);
        input := cutCommentMultiline(input);
        input := cutCommentEndline(input);
        //maybe: spacing from both sides for chars
        input := correctParentheses(input);
    end;
    case checkParentheses(input) of 
        -1 : raiserror('ESyntax:CQuotes: Wrong amount of quotation marks. Quotes are not closed.');
        0  : raiserror('ESyntax:CLevels: Wrong amount of braces and/or parentheses.');
        1  : begin
            executePSCode(input);
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

end.
