unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Process, UnitStack, UnitEntity, UnitFunctions, UnitEnvironment;

function commentcut(input : String) : String;
procedure evaluate(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB);
function read_sourcefile(filename : String; var pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : String;
function parseRPN(input : string; var pocz : StackDB; var sets : TSettings; vardb : VariableDB) : String;
function PS_parseString(input : string) : String;
procedure PS_runREPL();

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
    // stack_push(pocz, buildString(StrEcx));
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
        stack_push(pocz, buildString(StrEcx));
    end else begin
        stack_push(pocz, buildNumber(Im));
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

function read_sourcefile(filename : String; var pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : String;
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
    if (S <> '') then S := commentcut(S);
    fun := fun + ' ' + S;
  end;
  closefile(fp);
  S := parseRPN(fun, pocz, sets, vardb);
  read_sourcefile := S;
end;

function parseRPN(input : string; var pocz : StackDB; var sets : TSettings; vardb : VariableDB) : String;
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
			cond := trunc(stack_pop(pocz).Num);
		end else if (L[index] = 'if') then begin
			if cond = 0 then permit := True
			else permit := False;
		end else if L[index] = 'else' then begin
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
	    				  parseRPN(nesttx, pocz, sets, vardb); 
	    				until EOF;
	    				stack_pop(pocz);
	    			end else for step := 1 to Steps do parseRPN(nesttx, pocz, sets, vardb);
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
                            stack_push(pocz, buildFunction(nesttx)); 
                        until EOF;
                        stack_pop(pocz);
                    end else for step := 1 to Steps do stack_push(pocz, buildFunction(nesttx));
                permit := True;
                index := cursor - 1;
            end else begin
	    		if (permit) then
	    			if Steps = -1 then begin
	    				repeat
	    					evaluate(L[index], pocz, Steps, sets, vardb);
	    				until EOF;
	    				stack_pop(pocz);
	    			end else for step := 1 to Steps do evaluate(L[index], pocz, Steps, sets, vardb);
	    		permit := True; 
	    	end;
	    end;
    	Inc(index);
  	end;
  z := '';
  L.Free;

  parseRPN := z;
end;

function PS_parseString(input : string) : String;
var
    res : String;
    env : PSEnvironment;
begin
    env := buildNewEnvironment();
    res := parseRPN(input, env.Stack, env.Settings, env.Variables);
    res := stack_show(env.Stack, env.Settings.Mask);
    if (env.Settings.Prevent) then res := '';
    PS_parseString := res;
end;

// ========= REPL

procedure repl_showhelp();
begin
    writeln('REPL for PapajScript');
    writeln();
    writeln('End each line with \ to make multiline commands.');
    writeln('Type \reset to reset the REPL.');
    writeln('Type \export:FILE to export your history to a file (relative or absolute path)');
    writeln('Type \history to display what you did.');
    writeln('Type \hclear to clear all history');
    writeln('Type \hclear:N to clear the N-th command of history (N >= 1)');
    writeln('Type \!! to repeat last command');
    writeln('Type \!N to execute the N-th command of history (N >= 1)');
    writeln('Type \help to display this help again.');
    writeln('Type \q or \quit to exit the REPL.');
    writeln();
end;

procedure PS_runREPL();
var
    env     : PSEnvironment;
    command : String;
    input   : String;
    res     : String;
    fun     : LongInt;
    history : array of String;
    i       : Integer;
    Im      : Extended;
    Code    : Longint;
    alright : Boolean;
    fp      : Text;
    fname   : String;
begin
    env := buildNewEnvironment();
    SetLength(history, 0);
    repl_showhelp();
    repeat
        alright := true;
        input := '';
        TextColor(14);
        write('=> ');
        TextColor(7);
        readln(command);
        if (RightStr(command, 1) = '\') then 
        begin
            input := LeftStr(command, Length(command)-1) + ' ';
            repeat
                TextColor(6);
                write('-> ');
                TextColor(7);
                readln(command);
                if (RightStr(command, 1) = '\') then
                begin
                    input := input + LeftStr(command, Length(command)-1) + ' ';
                end else begin
                    input := input + command;
                end;
            until (RightStr(command, 1) <> '\');
        end else begin
            input := command;
        end;

        if input = '\!!' then 
        begin
            if Length(history) = 0 then
            begin
                input := '';
            end else begin
                input := history[Length(history)-1];
            end;
        end else if LeftStr(input, 2) = '\!' then
        begin
            Val(RightStr(input, Length(input)-2), Im, Code);
            input := '';
            if Code = 0 then
                if (Trunc(Im) >= 1) and (Trunc(Im) <= Length(history)) then
                    input := history[Trunc(Im)-1]; 
        end else if LeftStr(input, 8) = '\hclear:' then
        begin
            Val(RightStr(input, Length(input)-8), Im, Code);
            if Code = 0 then
                if (Trunc(Im) >= 1) and (Trunc(Im) <= Length(history)) then
                begin
                    fname := history[Trunc(Im)-1];
                    for i := Trunc(Im)-1 to Length(history)-2 do
                        history[i] := history[i+1];
                    SetLength(history, Length(history)-1);
                    TextColor(10);
                    writeln('Entry #',RightStr(input, Length(input)-8),' of ',QuotedStr(fname),' has been removed successfully.');
                    TextColor(7);
                end;
            input := '';
        end else if (LeftStr(input, 8) = '\export:') or (input = '\export') then
        begin
            if (input <> '\export') then fname := RightStr(input, Length(input)-8)
            else fname := '';
            input := '';
            if (fname = '') then begin
                TextColor(15);
                writeln('REPL Warning: No valid filepath specified - exporting to a "export.rpn" file.');
                TextColor(7);
                fname := 'export.rpn';
            end;
            try
                assignfile(fp, fname);
                rewrite(fp);
                for res in history do writeln(fp, res);
                closefile(fp);
                TextColor(10);
                writeln('Exported to "', fname,'" successfully.');
                TextColor(7);
            except
                On E : Exception do
                begin
                    TextColor(12);
                    writeln(StdErr, 'REPL Error: A problem occured when exporting to a file.');
                    TextColor(7);
                    writeln();
                end;
            end;

        end;

        case input of 
            '\q' : begin
                writeln('Bye.');
            end;
            '\quit' : begin
                writeln('Goodbye. :)');
            end;
            '\hclear' : begin
                SetLength(history, 0);
                TextColor(10);
                writeln('Your history is empty now.');
                writeln();
                TextColor(7);
            end;
            '\comamrobic' : begin
                randomize;
                fun := Random(10);
                TextColor(14);
                case fun of
                    0 : writeln('Tak jak pan Jezus powiedzial.');
                    1 : writeln('Badzmy lagodni.'); 
                    2 : writeln('Nie lekajcie sie.'); 
                    3 : writeln('Podazajcie z entuzjazmem.');
                    4 : writeln('Tolerancja jest mozliwa.');
                    5 : writeln('Nie wiem.'); 
                    6 : writeln('Trwajcie mocno w Chrystusie.');
                    7 : writeln('Niech zstapi Duch Twoj.'); 
                    8 : writeln('Jestem z Wami');
                    9 : writeln('Wyplywajcie na glebie.');
                end;
                TextColor(7);
            end;
            '\rzulta' : begin
                TextColor(14);
                writeln('  .----\----.  ');
                writeln(' ------------- ');
                writeln(' (  .-.  ___  )');
                writeln('((  (o)  (o)  |');
                writeln('`|     _\,    |');
                writeln('  \     _.   / ');
                writeln('   ---_____-   ');
                writeln();
                TextColor(15);
                writeln('Jan Papaj 2 interpretowal male skrypty. xD');
                writeln();
                TextColor(7);
            end;
            '\reset' : begin
                env := buildNewEnvironment();
                SetLength(history, 0);
                TextColor(10);
                writeln('All settings, history and data have been reset.');
                TextColor(7);
                writeln();
            end;
            '\history' : begin
                for i := 0 to Length(history)-1 do
                begin
                    writeln(PadLeft(IntToStr(i+1), 4) + ' : ' + history[i]);
                end;
                writeln();
            end;
            '\help' : begin
                repl_showhelp();
            end
            else begin
                if (input <> '') then
                begin
                    SetLength(history, Length(history)+1);
                    history[Length(history)-1] := input;
                end;
                try
                    res := parseRPN(input, env.Stack, env.Settings, env.Variables);
                    res := stack_show(env.Stack, env.Settings.Mask);
                    if not (env.Settings.Prevent) then 
                    begin
                        TextColor(3);
                        writeln(res);
                        TextColor(7);
                    end;
                except
                    On E : Exception do
                    begin
                        TextColor(12);
                        writeln(StdErr, E.ToString);
                        TextColor(7);
                        writeln();
                    end;
                end;
            end;
        end;
    until not ((input <> '\q') and (input <> '\quit'));
end;

end.
