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

procedure doFunction(Str : String; var pocz : StackDB; sets : TSettings; vardb : VariableDB);
procedure doDoUntil(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);

function commentcut(input : String) : String;
function cutCommentMultiline(input : String) : String;
function cutCommentEndline(input : String) : String;
function cutShebang(input : String) : String;
function checkParentheses(input : String) : Boolean;
procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
function parseScoped(input : string; pocz : StackDB; sets : TSettings; vardb : VariableDB) : StackDB;
function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;

function buildNewEnvironment() : PSEnvironment;
procedure disposeEnvironment(var env : PSEnvironment);

implementation

uses Unit5, DateUtils;

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

// LOOPS

procedure doFunction(Str : String; var pocz : StackDB; sets : TSettings; vardb : VariableDB);
var
    i : ShortInt;
begin
    vardb.addLayer();
    pocz := parseOpen(Str, pocz, sets, vardb);
    vardb.removeLayer();
end;

procedure doWhile(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
begin
    while True do
    begin
        pocz := parseOpen(StrCond, pocz, sets, vardb);
        if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) or (sets.KeepWorking = 0) then break;
        pocz := parseOpen(StrInst, pocz, sets, vardb);
    end;
end;

procedure doDoWhile(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
begin
    while True do
    begin
        pocz := parseOpen(StrInst, pocz, sets, vardb);
        pocz := parseOpen(StrCond, pocz, sets, vardb);
        if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then break;
    end;
end;

procedure doDoUntil(StrCond : String; StrInst : String; var pocz : StackDB; sets : TSettings; var vardb : VariableDB);
begin
    while True do
    begin
        pocz := parseOpen(StrInst, pocz, sets, vardb);
        pocz := parseOpen(StrCond, pocz, sets, vardb);
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
    is_set          : Boolean;
begin
    if OccurrencesOfChar(StrCond, ';') = 2 then
    begin
        L := StrCond.Split(';');
        pocz := parseOpen(L[0], pocz, sets, vardb);
        while True do
        begin
            pocz := parseOpen(L[1], pocz, sets, vardb);
            if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then break;
            pocz := parseOpen(StrInst, pocz, sets, vardb);
            pocz := parseOpen(L[2], pocz, sets, vardb);
        end;
    end else if OccurrencesOfChar(StrCond, ':') = 1 then
    begin
        L := StrCond.Split(':');
        L[0] := trim(L[0]);
        L[1] := trim(L[1]);
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
                    pocz[location].Values[index] := vardb.getLocalVariable(L[0]);
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
        doFunction(EntEax.Str, pocz, sets, vardb);
    end else begin
        stack_push(pocz[sets.StackPointer], EntEax);
    end;
end;


// EVALUATION

function wrapArrayFromString(input : String; var pocz : StackDB; sets : TSettings; vardb : VariableDB) : Entity;
var
    env : PSEnvironment;
    cnt : LongInt;
begin
    env := buildNewEnvironment();
    env.Stack := parseOpen(input, env.Stack, sets, vardb);
    cnt := stack_size(env.Stack[env.Settings.StackPointer]);
    disposeEnvironment(env);
    pocz := parseOpen(input+' '+IntToStr(cnt)+' toArray', pocz, sets, vardb);
    wrapArrayFromString := stack_pop(pocz[sets.StackPointer]);
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

procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
var
    Im     : Extended;
    Code   : Longint;
    StrEcx : String;
begin
    Steps := 1;

    //checkSIGINT();

    StrEcx := i.Substring(1, i.Length - 2);
    if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    begin
		raiserror(stack_pop(pocz[sets.StackPointer]).Str);
	end;
	
	if (LeftStr(i, 1) = '"') and (RightStr(i, 1) = '"') then
	begin
		if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    	begin
			raiserror(stack_pop(pocz[sets.StackPointer]).Str);
		end else stack_push(pocz[sets.StackPointer], buildString(StrEcx));
	end else begin
    	if not (sets.CaseSensitive) then i := LowerCase(i);
    	Val (i,Im,Code);
    	If Code<>0 then
    	begin
			if (not sets.Packages.UseMath) or ((sets.Packages.UseMath) and (not lib_math(concat('Math.',i), pocz, Steps, sets, vardb))) then
			if (not sets.Packages.UseString) or ((sets.Packages.UseString) and (not lib_strings(concat('String.',i), pocz, Steps, sets, vardb))) then
            if (not sets.Packages.UseArray) or ((sets.Packages.UseArray) and (not lib_arrays(concat('Array.',i), pocz, Steps, sets, vardb))) then
            if (not sets.Packages.UseConsole) or ((sets.Packages.UseConsole) and (not lib_consolemanipulators(concat('Console.',i), pocz, Steps, sets, vardb))) then

    	    if not lib_directives(i, pocz, Steps, sets, vardb) then
    	    if not lib_constants(i, pocz, Steps, sets, vardb) then
    	    if not lib_logics(i, pocz, Steps, sets, vardb) then
    	    if not lib_variables(i, pocz, Steps, sets, vardb) then
    	    if not lib_ultravanilla(i, pocz, Steps, sets, vardb) then
			if not lib_math(i, pocz, Steps, sets, vardb) then
			if not lib_strings(i, pocz, Steps, sets, vardb) then
    	    if not lib_consolemanipulators(i, pocz, Steps, sets, vardb) then
			if not lib_arrays(i, pocz, Steps, sets, vardb) then
            if not lib_files(i, pocz, Steps, sets, vardb) then		
    	    if not lib_exceptions(i, pocz, Steps, sets, vardb) then
    	    if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    		begin
				raiserror(stack_pop(pocz[sets.StackPointer]).Str);
			end else begin
                if (vardb.isVarAssigned(i)) 
                    then runFromString(i, pocz, Steps, sets, vardb)
                    else stack_push(pocz[sets.StackPointer], raiseExceptionUnknownCommand(pocz[sets.StackPointer], i));
            end;
    	end else begin
    	    stack_push(pocz[sets.StackPointer], buildNumber(Im));
    	end;

		if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    	begin
			raiserror(stack_pop(pocz[sets.StackPointer]).Str);
		end;

	end;
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
    i := 0;
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


function parseScoped(input : string; pocz : StackDB; sets : TSettings; vardb : VariableDB) : StackDB;
begin
	parseScoped := parseOpen(input, pocz, sets, vardb);
end;

function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;
var
	//L      : TStrings;
	L      : TStringArray;
	i      : String;
	index  : LongInt;
	z      : String;
	step   : Integer;
	cursor : LongInt;
	nestlv : ShortInt;
	nesttx : String;
    permit : Boolean;
	cond   : ShortInt;
    mode   : ShortInt;
    StrCond, StrInst : String;
    OldCond : ShortInt;
begin
	//L := TStringlist.Create;
	//L.Delimiter := ' ';
	//L.QuoteChar := '"';
	//L.StrictDelimiter := false;
	//L.DelimitedText := input;

	//L := input.Split([' ', #9, #13, #10], '"');
    L := input.Split([' ', #9, #13, #10]);

  	Steps := 1;
  	cond := -1;
  	permit := True;
    mode := MNORM;
  	index := 0;
  	//while (index < L.Count) and (sets.KeepWorking > 0) do
	while (input <> '') and (input <> #10) and (index < Length(L)) and (sets.KeepWorking > 0) do
	begin
		//writeln(L.Text);
		if (sets.KeepWorking = 1) or (L[index] = '') then
		begin
			Inc(index);
			sets.KeepWorking := 2;
			continue;
		end;

		if L[index] = '?' then begin
			cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
        end else if (L[index] = 'if') then begin
			mode := MIF;
		end else if (L[index] = 'if:') then begin
			if cond = 0 then permit := True
			else permit := False;
		end else if  (L[index] = 'else:') or (L[index] = 'unless:') then begin
			if (cond = 0) then permit := False
			else permit := True;
        end else if (L[index] = 'else') then begin
			if (OldCond = 0) then permit := False
			else permit := True;
        end else if (L[index] = 'function') or (L[index] = 'fun') then begin
			mode := MFUN;
        end else if (L[index] = 'elif') then begin
			mode := MELIF;
        end else if (L[index] = 'do') then begin
            mode := MDO;
        end else if (L[index] = 'while') then begin
            if mode = MDO then mode := MDOWHILE else mode := MWHILE;
        end else if (L[index] = 'until') then begin
            mode := MDOUNTIL;
        end else if (L[index] = 'for') then begin
            mode := MFOR;
        end else if (L[index] = '->') then begin
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
		end else begin
			//if L[index] = 'break' then break
			//else if L[index] = 'continue' then begin 
			//	Inc(index);
			//	continue;
			//end else
            if (L[index] = '\"') then
            begin
                if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    	            begin
			        raiserror(stack_pop(pocz[sets.StackPointer]).Str);
		        end else stack_push(pocz[sets.StackPointer], buildString('"'));
            end else if ((LeftStr(L[index], 1) = '"') and (RightStr(L[index], 1) <> '"')) or (L[index] = '"')
            then begin
                nesttx := L[index];
	    		cursor := index + 1;
				repeat
					nesttx := nesttx + ' ' + L[cursor];
                    Inc(cursor);
	    		until (RightStr(L[cursor-1], 1) = '"') or (cursor-1 >= Length(L));
                if (RightStr(nesttx, 1) = '"') then
	            begin
                    nesttx := nesttx.Substring(1, nesttx.Length - 2);
		            if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    	            begin
			            raiserror(stack_pop(pocz[sets.StackPointer]).Str);
		            end else stack_push(pocz[sets.StackPointer], buildString(nesttx));
                end else begin
                    raiserror('ESyntax:CQuotes: Wrong amount of quotation marks. Quotes are not closed.');
                end;
                permit := True;
	    		index := cursor - 1;
			end else if L[index] = '{' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
				while (nestlv > 0) and (cursor < Length(L)) do begin
                    nestlv := nestlv + checkLevel(L[cursor]);
					if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
	    			Inc(cursor);
	    		end;
                if mode = MFUN then begin
                    if (permit) then
                        if Steps = -1 then begin
                            repeat
                                stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx))); 
                            until EOF;
                            stack_pop(pocz[sets.StackPointer]);
                        end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx)));
                    mode := MNORM;
                end else if mode = MWHILE then begin
                    StrInst := trimLeft(nesttx);
                    doWhile(StrCond, StrInst, pocz, sets, vardb);
                    mode := MNORM;
                end else if mode = MFOR then begin
                    StrInst := trimLeft(nesttx);
                    doFor(StrCond, StrInst, pocz, sets, vardb);
                    mode := MNORM;
                end else if mode = MDO then begin
                    StrInst := trimLeft(nesttx);
                end else begin
	    		    if (permit) then
                    begin
	    		    	if Steps = -1 then begin
	    		    		repeat
	    		    			pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb); 
	    		    		until EOF;
	    		    		stack_pop(pocz[sets.StackPointer]);
	    		    	end else for step := 1 to Steps do pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
                        OldCond := 0;
                    end;
                end;
                index := cursor - 1;
            end else if L[index] = '[' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
				while (nestlv > 0) and (cursor < Length(L)) do begin
                    nestlv := nestlv + checkLevel(L[cursor]);
					if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
	    			Inc(cursor);
	    		end;
				//writeln(nesttx);
	    		if (permit) then
	    			if Steps = -1 then begin
	    				repeat
	    					pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb); 
	    				until EOF;
	    				stack_pop(pocz[sets.StackPointer]);
	    			end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], wrapArrayFromString(trimLeft(nesttx), pocz, sets, vardb));
	    		permit := True;
	    		index := cursor - 1;
            //end else if L[index] = 'else{' then begin
	    	//	nestlv := 1;
	    	//	nesttx := '';
	    	//	cursor := index + 1;
			//	while (nestlv > 0) and (cursor < Length(L)) do begin
            //        nestlv := nestlv + checkLevel(L[cursor]);
			//		if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
	    	//		Inc(cursor);
	    	//	end;
            //    if (cond = 0) or (OldCond <> 0) then permit := False else permit := True;
	    	//	if (permit) then
            //    begin
	    	//	    if Steps = -1 then begin
	    	//	    	repeat
	    	//	    		pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb); 
	    	//	    	until EOF;
	    	//	    	stack_pop(pocz[sets.StackPointer]);
	    	//	    end else for step := 1 to Steps do pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
            //    end;
            //    mode := MNORM;
            //    permit := True;
	    	//	index := cursor - 1;
            end else if (L[index] = 'fun{') or (L[index] = 'function{') then begin
                nestlv := 1;
                nesttx := '';
                cursor := index + 1;
                while (nestlv > 0) and (cursor < Length(L)) do begin
                    nestlv := nestlv + checkLevel(L[cursor]);
					if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
                    Inc(cursor);
                end;
                //writeln(trimLeft(nesttx));
                if (permit) then
                    if Steps = -1 then begin
                        repeat
                            stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx))); 
                        until EOF;
                        stack_pop(pocz[sets.StackPointer]);
                    end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx)));
                mode := MNORM;
                permit := True;
                index := cursor - 1;
            //end else if (L[index] = 'if(') then begin
            //    mode := MIF;
            //    OldCond := 1;
            //    nestlv := 1;
            //    nesttx := '';
            //    cursor := index + 1;
            //    while (nestlv > 0) and (cursor < Length(L)) do begin
            //        nestlv := nestlv + checkLevel(L[cursor]);
			//		if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
            //        Inc(cursor);
            //    end;
            //    pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
	    	//	cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            //    if cond = 0 then permit := True
			//    else permit := False;
            //    mode := MNORM;
            //    index := cursor - 1;
            end else if L[index] = '(' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
				while (nestlv > 0) and (cursor < Length(L)) do begin
                    nestlv := nestlv + checkLevel(L[cursor]);
					if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
	    			Inc(cursor);
	    		end;
                if mode = MIF then begin
                    OldCond := 1;
                    pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
	    		    cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                    if cond = 0 then permit := True
			        else permit := False;
                    mode := MNORM;
                end else if mode = MELIF then begin
                    pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
	    		    cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                    if (cond = 0) then permit := permit
			        else permit := False;
                    mode := MNORM;
                end else if mode = MWHILE then begin
                    StrCond := trimLeft(nesttx);
                    permit := True;
                end else if mode = MFOR then begin
                    StrCond := trimLeft(nesttx);
                    permit := True;
                end else if mode = MDOWHILE then begin
                    StrCond := trimLeft(nesttx);
                    doDoWhile(StrCond, StrInst, pocz, sets, vardb);
                    mode := MNORM;
                end else if mode = MDOUNTIL then begin
                    StrCond := trimLeft(nesttx);
                    doDoUntil(StrCond, StrInst, pocz, sets, vardb);
                    mode := MNORM;
                end else if mode = MWHILE then begin
                    mode := MNORM;
                end else begin
                    //stack_push(pocz[sets.StackPointer], raiseSyntaxErrorExpression(nesttx));
                    stack_push(pocz[sets.StackPointer], buildExpression(trimLeft(nesttx)));
                    //if (permit) then
                    //    if Steps = -1 then begin
                    //        repeat
                    //            stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx))); 
                    //        until EOF;
                    //        stack_pop(pocz[sets.StackPointer]);
                    //    end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx)));
                    permit := True;
                end;
	    		index := cursor - 1;
            end else begin
                if mode = MIF then begin
                    OldCond := 1;
                    evaluate(L[index], pocz, Steps, sets, vardb);
	    		    cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                    if cond = 0 then permit := True
			        else permit := False;
                    mode := MNORM;
                end else if mode = MELIF then begin
                    evaluate(L[index], pocz, Steps, sets, vardb);
	    		    cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                    if (cond = 0) then permit := permit
			        else permit := False;
                    mode := MNORM;
                //end else if mode = MDO then begin
                //    StrInst := trimLeft(L[index]);
                //end else if mode = MDOWHILE then begin
                //    StrCond := trimLeft(L[index]);
                //    while True do
                //    begin
                //        pocz := parseScoped(StrInst, pocz, sets, vardb);
                //        pocz := parseScoped(StrCond, pocz, sets, vardb);
                //        cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                //        if (cond <> 0) then break;
                //    end;
                //    mode := MNORM;
                end else begin
                    if (permit) then
                    begin
	    			    if Steps = -1 then begin
	    				    repeat
	    					    evaluate(L[index], pocz, Steps, sets, vardb);
	    				    until EOF;
	    				    stack_pop(pocz[sets.StackPointer]);
	    			    end else for step := 1 to Steps do evaluate(L[index], pocz, Steps, sets, vardb);
                        OldCond := 0;
                    end;
	    		    permit := True; 
                end;
	    	end;
	    end;
    	Inc(index);
  	end;
	sets.KeepWorking := 2;
	//z := '';
	//L.Free;
  	parseOpen := pocz;
end;

function buildNewEnvironment() : PSEnvironment;
var
	env : PSEnvironment;
begin
	SetLength(env.Stack, 1);
	env.Stack[0] := stack_null();
	env.Settings := default_settings();
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

