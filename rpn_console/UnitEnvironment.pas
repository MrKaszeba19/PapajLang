unit UnitEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, 
  UnitEntity, UnitStack, UnitFunctions;

type PSEnvironment = record
    Stack     : StackDB;
    Settings  : TSettings;
    Variables : VariableDB;
    AutoReset : Boolean;
end;

function commentcut(input : String) : String;
procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
function parseScoped(input : string; pocz : StackDB; var sets : TSettings; vardb : VariableDB) : StackDB;
function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;

function buildNewEnvironment() : PSEnvironment;
procedure disposeEnvironment(var env : PSEnvironment);

implementation

uses Unit5, crt;

var
	Steps : Integer;


// EVALUATION

procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
var
    Im     : Extended;
    Code   : Longint;
    StrEcx : String;
begin
    Steps := 1;

    StrEcx := i;
    if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    begin
		raiserror(stack_pop(pocz[sets.StackPointer]).Str);
	end;

    if not (sets.CaseSensitive) then i := LowerCase(i);
    Val (i,Im,Code);
    If Code<>0 then
    begin
        if not lib_directives(i, pocz, Steps, sets, vardb) then
        if not lib_constants(i, pocz, Steps, sets, vardb) then
        if not lib_logics(i, pocz, Steps, sets, vardb) then
        if not lib_variables(i, pocz, Steps, sets, vardb) then
        if not lib_ultravanilla(i, pocz, Steps, sets, vardb) then
        if not lib_consolemanipulators(i, pocz, Steps, sets, vardb) then
        if not lib_exceptions(i, pocz, Steps, sets, vardb) then
        if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    	begin
			raiserror(stack_pop(pocz[sets.StackPointer]).Str);
		end else stack_push(pocz[sets.StackPointer], buildString(StrEcx));
    end else begin
        stack_push(pocz[sets.StackPointer], buildNumber(Im));
    end;

    if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    begin
		raiserror(stack_pop(pocz[sets.StackPointer]).Str);
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

function parseScoped(input : string; pocz : StackDB; var sets : TSettings; vardb : VariableDB) : StackDB;
begin
	parseScoped := parseOpen(input, pocz, sets, vardb);
end;

function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;
var
	L      : TStrings;
	i      : String;
	index  : LongInt;
	z      : String;
	step   : Integer;
	cursor : LongInt;
	nestlv : ShortInt;
	nesttx : String;
	cond   : ShortInt;
	permit : Boolean;
begin
	L := TStringlist.Create;
	L.Delimiter := ' ';
	L.QuoteChar := '"';
	L.StrictDelimiter := false;
	L.DelimitedText := input;

  	Steps := 1;
  	cond := -1;
  	permit := True;
  	index := 0;
  	while index < L.Count do
	begin
		if L[index] = '?' then begin
			cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
		end else if (L[index] = 'if') then begin
			if cond = 0 then permit := True
			else permit := False;
		end else if (L[index] = 'else') or (L[index] = 'unless') then begin
			if cond = 0 then permit := False
			else permit := True;
		end else begin
			if L[index] = '{' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
	    		while (nestlv > 0) and (cursor < L.Count) do begin
	    			if (L[cursor] = '{') then Inc(nestlv);
                    if (L[cursor] = 'fun{') then Inc(nestlv);
	    			if (L[cursor] = '}') then Dec(nestlv);
	    			if (nestlv > 0) and (L[cursor] <> DelSpace(L[cursor])) then nesttx := nesttx + ' ' + ANSIQuotedStr(L[cursor], '"')
	    			else if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
	    			Inc(cursor);
	    		end;
	    		if (permit) then
	    			if Steps = -1 then begin
	    				repeat
	    					pocz := parseScoped(nesttx, pocz, sets, vardb); 
	    				until EOF;
	    				stack_pop(pocz[sets.StackPointer]);
	    			end else for step := 1 to Steps do pocz := parseScoped(nesttx, pocz, sets, vardb);
	    		permit := True;
	    		index := cursor - 1;
            end else if L[index] = 'fun{' then begin
                nestlv := 1;
                nesttx := '';
                cursor := index + 1;
                while (nestlv > 0) and (cursor < L.Count) do begin
                    if (L[cursor] = '{') then Inc(nestlv);
                    if (L[cursor] = 'fun{') then Inc(nestlv);
                    if (L[cursor] = '}') then Dec(nestlv);
                    if (nestlv > 0) and (L[cursor] <> DelSpace(L[cursor])) then nesttx := nesttx + ' ' + ANSIQuotedStr(L[cursor], '"')
                    else if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
                    Inc(cursor);
                end;
                if (permit) then
                    if Steps = -1 then begin
                        repeat
                            stack_push(pocz[sets.StackPointer], buildFunction(nesttx)); 
                        until EOF;
                        stack_pop(pocz[sets.StackPointer]);
                    end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], buildFunction(nesttx));
                permit := True;
                index := cursor - 1;
            end else begin
	    		if (permit) then
	    			if Steps = -1 then begin
	    				repeat
	    					evaluate(L[index], pocz, Steps, sets, vardb);
	    				until EOF;
	    				stack_pop(pocz[sets.StackPointer]);
	    			end else for step := 1 to Steps do evaluate(L[index], pocz, Steps, sets, vardb);
	    		permit := True; 
	    	end;
	    end;
    	Inc(index);
  	end;
	//z := '';
	L.Free;
  	parseOpen := pocz;
end;

function buildNewEnvironment() : PSEnvironment;
var
	env : PSEnvironment;
begin
	SetLength(env.Stack, 1);
	env.Stack[0] := stack_null();
	env.Settings := default_settings();
    env.Variables := createVariables();
    env.AutoReset := False;
    buildNewEnvironment := env;
end;

procedure disposeEnvironment(var env : PSEnvironment);
var
	i : LongInt;
begin
	for i := 0 to Length(env.Stack)-1 do stack_clear(env.Stack[i]);
	SetLength(env.Stack, 0);
end;

end.

