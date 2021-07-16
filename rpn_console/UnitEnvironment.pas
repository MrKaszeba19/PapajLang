unit UnitEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, 
  UnitEntity, UnitStack, UnitFunctions, UnitVariables;

const
    MNORM = 0;
    MIF = 1;
    MFUN = 2;
    MWHILE = 3;
    MDOWHILE = 4;
    MDOUNTIL = 9;
    MDO = 8;
    MFOR = 10;
    //MFOR1 = 5;
    //MFOR2 = 6;
    MELIF = 7;

type PSEnvironment = record
    Stack     : StackDB;
    Settings  : TSettings;
    Variables : VariableDB;
    AutoReset : Boolean;
end;

function read_source(filename : String; var env : PSEnvironment) : StackDB;
procedure runFromString(guess : String; var pocz : StackDB; var Steps : Integer; sets : TSettings; vardb : VariableDB);

//procedure doFunction(Body : String; var pocz : StackDB; sets : TSettings; vardb : VariableDB; Args : String = '');
procedure doFunction(f : Entity; var pocz : StackDB; sets : TSettings; vardb : VariableDB);
procedure doDoUntil(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);

function commentcut(input : String) : String;
function cutCommentMultiline(input : String) : String;
function cutCommentEndline(input : String) : String;
function cutShebang(input : String) : String;
function checkParentheses(input : String) : Boolean;
procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
function parseScoped(input : string; pocz : StackDB; sets : TSettings; vardb : VariableDB) : StackDB;
function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;

function buildNewEnvironment(LoadAll : Boolean = False) : PSEnvironment;
procedure disposeEnvironment(var env : PSEnvironment);

implementation

uses Unit5, DateUtils, StringUtils;

var
	Steps : Integer;

// HELPFUL THINGS

function checkLevel(input : String) : Integer;
begin
         if (input = '{')         then checkLevel := 1
    //else if (input = 'else{')     then checkLevel := 1
    else if (input = 'fun{')      then checkLevel := 1
    else if (input = 'function{') then checkLevel := 1
    else if (input = '[')         then checkLevel := 1
    else if (input = '(')         then checkLevel := 1
    //else if (input = 'if(')       then checkLevel := 1
	else if (input = '}')         then checkLevel := -1
    else if (input = ']')         then checkLevel := -1
    else if (input = ')')         then checkLevel := -1
    else checkLevel := 0;
end;

// FUNCTION

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

procedure doFunction(f : Entity; var pocz : StackDB; sets : TSettings; vardb : VariableDB);
begin
    vardb.addLayer();
    if (f.Str2 <> '') then wrapArgs(f.Str2, pocz, sets, vardb);
    pocz := parseOpen(f.Str, pocz, sets, vardb);
    vardb.removeLayer();
end;

// LOOPS

procedure doWhile(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
begin
    //while True do
    //begin
    //    pocz := parseOpen(StrCond, pocz, sets, vardb);
    //    if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) or (sets.KeepWorking = 0) then break;
    //    pocz := parseOpen(StrInst, pocz, sets, vardb);
    //end;
    
    pocz := parseOpen(StrCond, pocz, sets, vardb);
    while True do
    begin
        if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) or (sets.KeepWorking = 0) then break;
        pocz := parseOpen(StrInst + LineBreak + StrCond, pocz, sets, vardb);
    end;
end;

procedure doDoWhile(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
begin
    while True do
    begin
        //pocz := parseOpen(StrInst, pocz, sets, vardb);
        //pocz := parseOpen(StrCond, pocz, sets, vardb);
        //if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then break;
        pocz := parseOpen(StrInst + LineBreak + StrCond, pocz, sets, vardb);
        if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then break;
    end;
end;

procedure doDoUntil(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
begin
    while True do
    begin
        //pocz := parseOpen(StrInst, pocz, sets, vardb);
        //pocz := parseOpen(StrCond, pocz, sets, vardb);
        //if (trunc(stack_pop(pocz[sets.StackPointer]).Num) = 0) then break;
        pocz := parseOpen(StrInst + LineBreak + StrCond, pocz, sets, vardb);
        if (trunc(stack_pop(pocz[sets.StackPointer]).Num) = 0) then break;
    end;
end;

procedure doFor(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
var
	L               : TStringArray;
    index, location : LongInt;
    addr            : VariableAddress;
    input, tname    : String;
    cnt             : LongInt;
    env             : PSEnvironment;
    is_set, alter   : Boolean;
begin
    if OccurrencesOfChar(StrCond, ';') = 2 then
    begin
        L := StrCond.Split(';');
        //pocz := parseOpen(L[0], pocz, sets, vardb);
        //while True do
        //begin
        //    pocz := parseOpen(L[1], pocz, sets, vardb);
        //    if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then break;
        //    pocz := parseOpen(StrInst, pocz, sets, vardb);
        //    pocz := parseOpen(L[2], pocz, sets, vardb);
        //end;
        pocz := parseOpen(L[0] + LineBreak + L[1], pocz, sets, vardb);
        while True do
        begin
            if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then break;
            pocz := parseOpen(StrInst + LineBreak + L[2] + LineBreak + L[1], pocz, sets, vardb);
        end;
    end else if OccurrencesOfChar(StrCond, ':') = 1 then
    begin
        L := StrCond.Split(':');
        L[0] := trim(L[0]);
        L[1] := trim(L[1]);
        alter := True;
        if (LeftStr(L[0], 6) = 'const ') then
        begin
            alter := False;
            L[0] := RightStr(L[0], Length(L[0])-6);
        end;
        if (L[0][1] = '$') then L[0] := RightStr(L[0], Length(L[0])-1);
        if (L[1][1] = '$') then L[1] := RightStr(L[1], Length(L[1])-1);
        // check if RHS is either variable or not
        if isValidForVariables(L[0]) then
        begin
            is_set := False;
            if (LeftStr(L[1], 1) = '[') and (RightStr(L[1], 1) = ']') then
            begin
                input := L[1].Substring(1, L[1].Length - 2);
                env := buildNewEnvironment();
                env.Stack := parseOpen(input, env.Stack, sets, vardb);
                cnt := stack_size(env.Stack[env.Settings.StackPointer]);
                disposeEnvironment(env);
                tname := 'T_'+IntToStr(DateTimeToUnix(Now));
                pocz := parseOpen(input+' '+IntToStr(cnt)+' toArray >'+tname, pocz, sets, vardb);
                L[1] := tname;
                is_set := True;
            end;
            addr := vardb.locateVariable(L[1]);
            if (addr.Layer = -1) then 
                stack_push(pocz[sets.StackPointer], raiseExceptionUnknownArray(pocz[sets.StackPointer], L[1]))
            else begin
                if assertEntityLocated(pocz[sets.StackPointer], vardb.getLocatedVariable(L[1], addr), TVEC, L[1]) then Exit; 
                location := trunc(vardb.getLocatedVariable(L[1], addr).Num);
                for index := 0 to Length(pocz[location].Values)-1 do
                begin
                    vardb.setLocalVariable(L[0], pocz[location].Values[index]);
                    pocz := parseOpen(StrInst, pocz, sets, vardb);
                    if alter then pocz[location].Values[index] := vardb.getLocalVariable(L[0]);
                end;
            end;
            if is_set then 
            begin
                pocz := parseOpen('~'+tname, pocz, sets, vardb);
            end;
        end else begin
            raiserror('EVariable:CSetInvalid: Invalid variable string at "'+L[0]+'"');
        end;
        if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    	begin
			raiserror(stack_pop(pocz[sets.StackPointer]).Str);
        end;
    end else begin
        stack_push(pocz[sets.StackPointer], raiseSyntaxErrorExpression(StrCond));
    end;
end;

procedure runFromString(guess : String; var pocz : StackDB; var Steps : Integer; sets : TSettings; vardb : VariableDB);
var
    EntEax : Entity;
begin
    EntEax := vardb.getVariable(guess);
    if (EntEax.EntityType = TFUN) then
    begin
        doFunction(EntEax, pocz, sets, vardb);
    end else begin
        stack_push(pocz[sets.StackPointer], EntEax);
    end;
end;


// EVALUATION

function wrapArrayFromString(input : String; var pocz : StackDB; sets : TSettings; vardb : VariableDB) : Entity;
var
    env        : PSEnvironment;
    cnt, index : LongInt;
    ArrEcx     : Entity;
begin
    env := buildNewEnvironment();
    env.Stack := parseOpen(input, env.Stack, sets, vardb);
    cnt := stack_size(env.Stack[env.Settings.StackPointer]);
    stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, cnt));
    ArrEcx := stack_pop(pocz[sets.StackPointer]);
    for index := 0 to cnt-1 do
    begin
        pocz[trunc(ArrEcx.Num)].Values[index] := env.Stack[env.Settings.StackPointer].Values[index];
    end; 
    disposeEnvironment(env);
    wrapArrayFromString := ArrEcx;
end;

function read_source(filename : String; var env : PSEnvironment) : StackDB;
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
  //writeln(fun);
    fun := cutCommentMultiline(fun);
    fun := cutCommentEndline(fun);
  read_source := parseScoped(trim(fun), env.Stack, env.Settings, env.Variables); 
end;

procedure checkExceptions(var pocz : StackDB; var sets : TSettings);
begin
    if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    begin
		raiserror(stack_pop(pocz[sets.StackPointer]).Str);
	end;
end;

function getPackage(input : String) : String;
var
    position : LongInt; 
begin
    position := Pos('.', input);
    if (position >= 2)
        then Result := LeftStr(input, position-1)
        else Result := '';
end;

function searchThroughNamespacesExplicit(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
begin
    Result := False;
    case getPackage(i) of
        'Array'   : if (sets.Packages.UseArray)   then Result := lib_arrays(i, pocz, Steps, sets, vardb);
        'Console' : if (sets.Packages.UseConsole) then Result := lib_datetime(i, pocz, Steps, sets, vardb);
        'Date'    : if (sets.Packages.UseDate)    then Result := lib_consolemanipulators(i, pocz, Steps, sets, vardb);
        'Math'    : if (sets.Packages.UseMath)    then Result := lib_math(i, pocz, Steps, sets, vardb);
        'String'  : if (sets.Packages.UseString)  then Result := lib_strings(i, pocz, Steps, sets, vardb);
        //else begin
        //    Result := vardb.isVarAssigned(i);
        //    if Result then runFromString(i, pocz, Steps, sets, vardb);
        //end;
    end;
    
end;

function searchThroughNamespacesImplicit(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
begin
    Result := True;
    if (not sets.Packages.UseMath) or (not lib_math(concat('Math.',i), pocz, Steps, sets, vardb)) then
    if (not sets.Packages.UseString) or (not lib_strings(concat('String.',i), pocz, Steps, sets, vardb)) then
    if (not sets.Packages.UseArray) or (not lib_arrays(concat('Array.',i), pocz, Steps, sets, vardb)) then
    if (not sets.Packages.UseConsole) or (not lib_consolemanipulators(concat('Console.',i), pocz, Steps, sets, vardb)) then
    if (not sets.Packages.UseDate) or (not lib_datetime(concat('Date.',i), pocz, Steps, sets, vardb)) then
        Result := False;
end;

procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
var
    Im     : Extended;
    Code   : Longint;
    StrEcx : String;
    Found  : Boolean = True;
begin
    Steps := 1;

    checkExceptions(pocz, sets);
    if (LeftStr(i, 1) = '"') and (RightStr(i, 1) = '"') then
	begin
        StrEcx := i.Substring(1, i.Length - 2);
        if sets.stringmode = MCLIKE then StrEcx := string_toC(StrEcx);
        stack_push(pocz[sets.StackPointer], buildString(StrEcx));
	end else begin
        if not (sets.CaseSensitive) then i := LowerCase(i);
    	Val(i, Im, Code);
        if Code <> 0 then
        begin
            if (vardb.isVarAssigned(i)) then
            begin
                runFromString(i, pocz, Steps, sets, vardb)
            end else if (i[1] in ['$', '>', '-', '~', '@', '?']) then
            begin
                if not lib_directives(i, pocz, Steps, sets, vardb) then
                if not lib_logics(i, pocz, Steps, sets, vardb) then
                if not lib_variables(i, pocz, Steps, sets, vardb) then
                if not lib_ultravanilla(i, pocz, Steps, sets, vardb) then
                    Found := False;
            end else if (getPackage(i) <> '') then begin
                if not searchThroughNamespacesExplicit(i, pocz, Steps, sets, vardb) then 
                    Found := False;
            end else begin
    	        if not lib_constants(i, pocz, Steps, sets, vardb) then
    	        if not lib_logics(i, pocz, Steps, sets, vardb) then
                if not lib_ultravanilla(i, pocz, Steps, sets, vardb) then
                if not lib_exceptions(i, pocz, Steps, sets, vardb) then
                if not searchThroughNamespacesImplicit(i, pocz, Steps, sets, vardb) then 
                if not lib_files(i, pocz, Steps, sets, vardb) then
                if not lib_variables2(i, pocz, Steps, sets, vardb) then
                    Found := False;
            end;
        end else begin
            stack_push(pocz[sets.StackPointer], buildNumber(Im));
        end;

        if not Found then
        begin
            if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    		begin
				raiserror(stack_pop(pocz[sets.StackPointer]).Str);
			end else begin
                stack_push(pocz[sets.StackPointer], raiseExceptionUnknownCommand(pocz[sets.StackPointer], i));
            end;
        end;
    end;
    checkExceptions(pocz, sets);
end;

function commentcut(input : String) : String;
var 
	pom         : String;
	togglequote : Boolean;
	i           : Integer;
begin
	pom := '';
	togglequote := false;
	for i := 0 to Length(input) do begin
		if (input[i] = '"') then togglequote := not (togglequote);
		if (not ((input[i] = '/') and (input[i+1] = '/'))) or (togglequote) then begin
			pom := concat(pom, input[i]);
		end else begin
			if not (togglequote) then break;
		end;
	end;
	commentcut := pom;
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
    cutCommentMultiline := trim(pom);
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
    cutCommentEndline := trim(pom);
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
    cutShebang := trim(pom);
end;

function checkParentheses(input : String) : Boolean;
var
    isValid    : Boolean;
    v1, v2, v3 : LongInt;
    i          : LongInt;
    comment    : Boolean;
begin
    isValid := True;
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
                        isValid := False;
                        Break;
                    end;
                end; 
                '(' : begin
                    v2 := v2 + 1;
                end; 
                ')' : begin
                    v2 := v2 - 1;
                    if (v2 < 0) then begin
                        isValid := False;
                        Break;
                    end;
                end; 
                '[' : begin
                    v3 := v3 + 1;
                end; 
                ']' : begin
                    v3 := v3 - 1;
                    if (v3 < 0) then begin
                        isValid := False;
                        Break;
                    end;
                end; 
            end;
        end;
        i := i + 1;
    end;
    if (v1 <> 0) or (v2 <> 0) or (v3 <> 0) then isValid := False;
    checkParentheses := isValid;
end;

function getScopedString(var L : TStringArray; var cursor : Integer; initStr : String = '') : String;
var
    nestlv : LongInt;
    nesttx : String;
begin
    nestlv := 1;
	nesttx := '';
    Inc(cursor);
	while (nestlv > 0) and (cursor < Length(L)) do begin
        nestlv := nestlv + checkLevel(L[cursor]);
		if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
		Inc(cursor);
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
        Inc(cursor);
	until (RightStr(L[cursor-1], 1) = '"') or (cursor-1 >= Length(L));
    Dec(cursor);
    if (RightStr(nesttx, 1) = '"')
        then Result := nesttx
        else raiserror('ESyntax:CQuotes: Wrong amount of quotation marks. Quotes are not closed.');
end;


function parseScoped(input : string; pocz : StackDB; sets : TSettings; vardb : VariableDB) : StackDB;
begin
	parseScoped := parseOpen(input, pocz, sets, vardb);
end;

function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;
var
    L                : TStringArray;
	index            : LongInt;
	step             : Integer;
    permit           : Boolean = True;
	cond             : ShortInt = -1;
    mode             : ShortInt = MNORM;
    OldCond          : ShortInt = 0;
    ExecStr          : String = '';
    BracesStr        : String = '';
    BracketsStr      : String = '';
    ParenthStr       : String = '';
    InstructionBuilt : Boolean = False;

begin
    L := input.Split([' ', #9, #13, #10]);

  	Steps := 1;
  	//cond := -1;
  	////permit := True;
    //mode := MNORM;

    //ExecStr := '';
    //BracesStr := '';
    //ParenthStr := '';
    //BracketsStr := '';
    //InstructionBuilt := False;

  	index := 0;
	while (input <> '') and (input <> #10) and (index < Length(L)) and (sets.KeepWorking > 0) do
	begin
		if (sets.KeepWorking = 1) or (L[index] = '') then
		begin
			Inc(index);
			sets.KeepWorking := 2;
			continue;
		end;

        case L[index] of
            'if' : mode := MIF;
            'elif' : mode := MELIF;
            'else' : begin
			    if (OldCond = 0) then permit := False
			    else permit := True;
            end;
            'function' : mode := MFUN;
            'fun' : mode := MFUN;
            'do' : mode := MDO;
            'while' : if mode = MDO then mode := MDOWHILE else mode := MWHILE;
            'until' : mode := MDOUNTIL;
            'for' : mode := MFOR;
            '?' : begin
                cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            end;
            'if:' : begin
                if cond = 0 then permit := True
			    else permit := False;
            end;
            'else:' :  begin
			    if (cond = 0) then permit := False
			    else permit := True;
            end;
            'unless:' :  begin
			    if (cond = 0) then permit := False
			    else permit := True;
            end; 
            '->' : begin
                if (L[index+1][1] = '$') then L[index+1] := RightStr(L[index+1], Length(L[index+1])-1);
                if isValidForVariables(L[index+1]) then
                begin
                    if LeftStr(L[index+1], 7) = 'global.' 
                        then vardb.setGlobalVariable(L[index+1], stack_pop(pocz[sets.StackPointer]))
                        else
                            //vardb.setVariable(StrEax, EntEax);
                            vardb.setLocalVariable(L[index+1], stack_pop(pocz[sets.StackPointer]));        
                end else begin
                    raiserror('EVariable:CSetInvalid: Invalid variable string at "'+L[index+1]+'"');
                end;
                index := index + 1;
            end;
            '\"' : begin
                if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) 
                    then raiserror(stack_pop(pocz[sets.StackPointer]).Str)
		            else stack_push(pocz[sets.StackPointer], buildString('"'));
		    end
            else begin
                if ((LeftStr(L[index], 1) = '"') and (RightStr(L[index], 1) <> '"')) or (L[index] = '"') then begin
                    ExecStr := getQuotedString(L, index);
                    permit := True;
                    InstructionBuilt := True;
			    end else if L[index] = '{' then
                begin
                    BracesStr := getScopedString(L, index);
                    if (mode <> MDO) then InstructionBuilt := True;
                end else if L[index] = '[' then begin
                    BracketsStr := getScopedString(L, index);
                    InstructionBuilt := True;
                end else if (L[index] = 'fun{') or (L[index] = 'function{') then
                begin
                    mode := MFUN;
                    BracesStr := getScopedString(L, index);
                    InstructionBuilt := True;
                end else if L[index] = '(' then begin
                    ParenthStr := getScopedString(L, index);
                    if mode in [MNORM, MIF, MELIF, MDOWHILE, MDOUNTIL] then InstructionBuilt := True;
                end else begin
                    ExecStr := L[index];
                    InstructionBuilt := True;
                end;
            end;
        end;

        if InstructionBuilt then
        begin
            case mode of
                MNORM : begin
                    if ExecStr <> '' then
                    begin
                        if (permit) then
                        begin
	    			        if Steps = -1 then begin
	    			    	    repeat
	    			    		    evaluate(ExecStr, pocz, Steps, sets, vardb);
	    			    	    until EOF;
	    			    	    stack_pop(pocz[sets.StackPointer]);
	    			        end else if Steps > 0 then
                                for step := 1 to Steps do 
                                    evaluate(ExecStr, pocz, Steps, sets, vardb);
                            OldCond := 0;
                        end;
	    		        permit := True; 
                        ExecStr := '';
                    end else if BracesStr <> '' then
                    begin
                        if (permit) then
                        begin
	    	            	if Steps = -1 then begin
	    	            		repeat
	    	            			pocz := parseScoped(BracesStr, pocz, sets, vardb); 
	    	            		until EOF;
	    	            		stack_pop(pocz[sets.StackPointer]);
	    	            	end else if Steps > 0 then
                            begin
                                for step := 1 to Steps do 
                                    pocz := parseScoped(BracesStr, pocz, sets, vardb);
                            end else if Steps = 0 then Steps := 1;;
                            OldCond := 0;
                        //end else begin
                        //    mode := MNORM;
                        end;
                        permit := True;
                        BracesStr := '';
                    end else if ParenthStr <> '' then
                    begin
                        if (permit) then
                            if Steps = -1 then begin
                                repeat
	    	            			stack_push(pocz[sets.StackPointer], buildExpression(ParenthStr));
	    	            		until EOF;
                            end else if Steps > 0 then
                                for step := 1 to Steps do 
                                    stack_push(pocz[sets.StackPointer], buildExpression(ParenthStr));
                        permit := True;
                        ParenthStr := '';
                    end else if BracketsStr <> '' then
                    begin
                        if (permit) then
	    	            	if Steps = -1 then begin
	    	            		repeat
	    	            			stack_push(pocz[sets.StackPointer], wrapArrayFromString(trimLeft(BracketsStr), pocz, sets, vardb));
	    	            		until EOF;
	    	            		stack_pop(pocz[sets.StackPointer]);
	    	            	end else if Steps > 0 then
                                for step := 1 to Steps do 
                                    stack_push(pocz[sets.StackPointer], wrapArrayFromString(trimLeft(BracketsStr), pocz, sets, vardb));
	    	            permit := True;
                        BracketsStr := '';
                    end;
                end;
                MIF : begin
                    if (ParenthStr <> '') then
                    begin
                        OldCond := 1;
                        pocz := parseScoped(ParenthStr, pocz, sets, vardb);
	    	            cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                        if cond = 0 then permit := True
		                else permit := False;
                        mode := MNORM;
                        ParenthStr := '';
                    end;                       
                end;
                MELIF : begin
                    if (ParenthStr <> '') then
                    begin
                        if (OldCond = 1) then
                        begin
                            pocz := parseScoped(ParenthStr, pocz, sets, vardb);
	    	                cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                            if (cond = 0) then permit := True
		                    else permit := False;
                            mode := MNORM;
                        end else begin
                            permit := False;
                        end;
                        ParenthStr := '';
                    end;
                end;
                MFUN : begin
                    if BracesStr <> '' then
                    begin
                        if (permit) then
                        begin
                            if Steps = -1 then begin
                                repeat
                                    stack_push(pocz[sets.StackPointer], buildFunction(BracesStr, ParenthStr)); 
                                until EOF;
                                stack_pop(pocz[sets.StackPointer]);
                            end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], buildFunction(BracesStr, ParenthStr));

                        end;
                        mode := MNORM;
                        permit := True;
                        BracesStr := '';
                        ParenthStr := '';
                    end;
                end;
                //MDO : ;
                MDOWHILE : begin
                    if ParenthStr <> '' then 
                    begin
                        if BracesStr <> '' then ExecStr := BracesStr;
                        if (permit) then 
                        begin
                            doDoWhile(ParenthStr, ExecStr, pocz, sets, vardb);
                        end;
                        mode := MNORM;
                        BracesStr := '';
                        ParenthStr := '';
                        ExecStr := '';
                        permit := True;
                    end;
                end;
                MDOUNTIL : begin
                    if ParenthStr <> '' then 
                    begin
                        if BracesStr <> '' then ExecStr := BracesStr;
                        if (permit) then 
                        begin
                            doDoUntil(ParenthStr, ExecStr, pocz, sets, vardb);
                        end;
                        mode := MNORM;
                        BracesStr := '';
                        ParenthStr := '';
                        ExecStr := '';
                        permit := True;
                    end;
                end;
                MWHILE : begin
                    if ParenthStr <> '' then
                    begin
                        if BracesStr <> '' then ExecStr := BracesStr;
                        if (permit) then 
                        begin
                            doWhile(ParenthStr, ExecStr, pocz, sets, vardb);
                            OldCond := 0;
                        end;
                        mode := MNORM;
                        BracesStr := '';
                        ParenthStr := '';
                        ExecStr := '';
                        permit := True;
                    end;
                end;
                MFOR : begin
                    if BracesStr <> '' then ExecStr := BracesStr;
                    if (permit) then 
                    begin
                        doFor(ParenthStr, ExecStr, pocz, sets, vardb);
                        OldCond := 0;
                    end;
                    mode := MNORM;
                    ExecStr := '';
                    BracesStr := '';
                    ParenthStr := '';
                    permit := True;
                end;
            end;
            InstructionBuilt := False;
        end;
        Inc(index);
  	end;
	sets.KeepWorking := 2;
	//z := '';
	//L.Free;
  	Result := pocz;
end;

function buildNewEnvironment(LoadAll : Boolean = False) : PSEnvironment;
var
	env : PSEnvironment;
begin
	SetLength(env.Stack, 1);
	env.Stack[0] := stack_null();
	env.Settings := default_settings(LoadAll);
    env.Variables.Create;
    env.AutoReset := False;
    buildNewEnvironment := env;
end;

procedure disposeEnvironment(var env : PSEnvironment);
var
	i : LongInt;
begin
    env.Variables.Destroy;
	for i := 0 to Length(env.Stack)-1 do stack_clear(env.Stack[i]);
	SetLength(env.Stack, 0);
end;

end.

