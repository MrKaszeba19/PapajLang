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

function read_source(filename : String; var env : PSEnvironment) : StackDB;

function commentcut(input : String) : String;
function cutCommentMultiline(input : String) : String;
function cutCommentEndline(input : String) : String;
function cutShebang(input : String) : String;
procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
function parseScoped(input : string; pocz : StackDB; var sets : TSettings; vardb : VariableDB) : StackDB;
function parseOpen(input : string; pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;

function buildNewEnvironment() : PSEnvironment;
procedure disposeEnvironment(var env : PSEnvironment);

implementation

uses Unit5;

var
	Steps : Integer;


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
    fun := fun + ' ' + S;
  end;
  closefile(fp);
  writeln(fun);
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
    	    if not lib_exceptions(i, pocz, Steps, sets, vardb) then
    	    if (sets.StrictType) and (stack_searchException(pocz[sets.StackPointer])) then
    		begin
				raiserror(stack_pop(pocz[sets.StackPointer]).Str);
			end else stack_push(pocz[sets.StackPointer], raiseExceptionUnknownCommand(pocz[sets.StackPointer], i));
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
        if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
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
        if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
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
        if (not commentmode) and (input[i] = '"') then togglequote := not (togglequote);
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

function parseScoped(input : string; pocz : StackDB; var sets : TSettings; vardb : VariableDB) : StackDB;
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
	cond   : ShortInt;
	permit : Boolean;
    ifMode : Boolean;
begin
	//L := TStringlist.Create;
	//L.Delimiter := ' ';
	//L.QuoteChar := '"';
	//L.StrictDelimiter := false;
	//L.DelimitedText := input;

	L := input.Split([' ', #9, #13, #10], '"');

  	Steps := 1;
  	cond := -1;
  	permit := True;
    ifMode := False;
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
			ifMode := True;
		end else if (L[index] = 'if:') then begin
			if cond = 0 then permit := True
			else permit := False;
		end else if (L[index] = 'else') or (L[index] = 'else:') or (L[index] = 'unless:') then begin
			if cond = 0 then permit := False
			else permit := True;
		end else begin
			//if L[index] = 'break' then break
			//else if L[index] = 'continue' then begin 
			//	Inc(index);
			//	continue;
			//end else 
			if L[index] = '{' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
	    		//while (nestlv > 0) and (cursor < L.Count) do begin
				while (nestlv > 0) and (cursor < Length(L)) do begin
	    			if (L[cursor] = '{') then Inc(nestlv);
                    if (L[cursor] = 'fun{') then Inc(nestlv);
                    if (L[cursor] = '[') then Inc(nestlv);
                    if (L[cursor] = '(') then Inc(nestlv);
	    			if (L[cursor] = '}') then Dec(nestlv);//;
                    if (L[cursor] = ']') then Dec(nestlv);
                    if (L[cursor] = ')') then Dec(nestlv);
	    			//if (nestlv > 0) and (L[cursor] <> DelSpace(L[cursor])) then nesttx := nesttx + ' ' + ANSIQuotedStr(L[cursor], '"')
	    			//else 
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
	    			end else for step := 1 to Steps do pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
	    		permit := True;
	    		index := cursor - 1;
            end else if L[index] = '[' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
	    		//while (nestlv > 0) and (cursor < L.Count) do begin
				while (nestlv > 0) and (cursor < Length(L)) do begin
	    			if (L[cursor] = '{') then Inc(nestlv);
                    if (L[cursor] = 'fun{') then Inc(nestlv);
                    if (L[cursor] = '[') then Inc(nestlv);
                    if (L[cursor] = '(') then Inc(nestlv);
	    			if (L[cursor] = '}') then Dec(nestlv);//;
                    if (L[cursor] = ']') then Dec(nestlv);
                    if (L[cursor] = ')') then Dec(nestlv);
	    			//if (nestlv > 0) and (L[cursor] <> DelSpace(L[cursor])) then nesttx := nesttx + ' ' + ANSIQuotedStr(L[cursor], '"')
	    			//else 
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
            end else if L[index] = 'fun{' then begin
                nestlv := 1;
                nesttx := '';
                cursor := index + 1;
				//while (nestlv > 0) and (cursor < L.Count) do begin
                while (nestlv > 0) and (cursor < Length(L)) do begin
                    if (L[cursor] = '{') then Inc(nestlv);
                    if (L[cursor] = 'fun{') then Inc(nestlv);
                    if (L[cursor] = '(') then Inc(nestlv);
	    			if (L[cursor] = '}') then Dec(nestlv);//;
                    if (L[cursor] = ']') then Dec(nestlv);
                    if (L[cursor] = ')') then Dec(nestlv);
                    //if (nestlv > 0) and (L[cursor] <> DelSpace(L[cursor])) then nesttx := nesttx + ' ' + ANSIQuotedStr(L[cursor], '"')
                    //else 
					if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
                    Inc(cursor);
                end;
				//writeln(nesttx);
                if (permit) then
                    if Steps = -1 then begin
                        repeat
                            stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx))); 
                        until EOF;
                        stack_pop(pocz[sets.StackPointer]);
                    end else for step := 1 to Steps do stack_push(pocz[sets.StackPointer], buildFunction(trimLeft(nesttx)));
                permit := True;
                index := cursor - 1;
            end else if L[index] = '(' then begin
	    		nestlv := 1;
	    		nesttx := '';
	    		cursor := index + 1;
	    		//while (nestlv > 0) and (cursor < L.Count) do begin
				while (nestlv > 0) and (cursor < Length(L)) do begin
	    			if (L[cursor] = '{') then Inc(nestlv);
                    if (L[cursor] = 'fun{') then Inc(nestlv);
                    if (L[cursor] = '[') then Inc(nestlv);
                    if (L[cursor] = '(') then Inc(nestlv);
	    			if (L[cursor] = '}') then Dec(nestlv);//;
                    if (L[cursor] = ']') then Dec(nestlv);
                    if (L[cursor] = ')') then Dec(nestlv);
	    			//if (nestlv > 0) and (L[cursor] <> DelSpace(L[cursor])) then nesttx := nesttx + ' ' + ANSIQuotedStr(L[cursor], '"')
	    			//else 
					if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
	    			Inc(cursor);
	    		end;
				//writeln(nesttx);
                if ifMode then begin
                    pocz := parseScoped(trimLeft(nesttx), pocz, sets, vardb);
	    		    cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                    if cond = 0 then permit := True
			        else permit := False;
                    ifMode := False;
                end;
	    		index := cursor - 1;
            end else begin
	    		if (permit) then
	    			if Steps = -1 then begin
	    				repeat
	    					evaluate(L[index], pocz, Steps, sets, vardb);
	    				until EOF;
	    				stack_pop(pocz[sets.StackPointer]);
	    			end else for step := 1 to Steps do evaluate(L[index], pocz, Steps, sets, vardb);
                if ifMode then begin
	    		    cond := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                    if cond = 0 then permit := True
			        else permit := False;
                    ifMode := False;
                end else begin
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

