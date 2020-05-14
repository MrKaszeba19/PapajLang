unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, Process, SysUtils;

function PS_parseString(input : string) : String;
procedure PS_runREPL();

implementation

uses StrUtils, crt, UnitStack, UnitEnvironment;

function PS_parseString(input : string) : String;
var
    res : String;
    env : PSEnvironment;
begin
    if (input <> '') then input := cutCommentMultiline(input);
    env := buildNewEnvironment();
    env.Stack := parseOpen(input, env.Stack, env.Settings, env.Variables);
    res := stack_show(env.Stack[0], env.Settings.Mask);
    disposeEnvironment(env);
    if (env.Settings.Prevent) then res := '';
    PS_parseString := res;
end;

// ========= REPL

procedure repl_showshorthelp();
begin
    writeln('Type \reset to reset the REPL.');
    writeln('Type \help to display help.');
    writeln('Type \q or \quit to exit the REPL.');
    writeln();
end;

procedure repl_showhelp();
begin
    writeln('End each line with \ to make multiline commands.');
    writeln('Type \autoreset:true (or \autoreset) to reset the environment every command.');
    writeln('Type \autoreset:false to prevent from doing the thing above. (set by default)');
    writeln('Type \export:FILE to export your history to a file (relative or absolute path)');
    writeln('Type \history to display what you did.');
    writeln('Type \hclear to clear all history');
    writeln('Type \hclear:N to clear the N-th command of history (N >= 1)');
    writeln('Type \!! to repeat last command');
    writeln('Type \!N to execute the N-th command of history (N >= 1)');
    repl_showshorthelp();
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
    fp      : Text;
    fname   : String;
begin
    env := buildNewEnvironment();
    SetLength(history, 0);
    writeln('REPL for PapajScript');
    writeln();
    repl_showshorthelp();
    repeat
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
            '\autoreset:false' : begin
                env.AutoReset := False;
                TextColor(10);
                writeln('Autoreset is off.');
                writeln();
                TextColor(7);
            end;
            '\autoreset:true' : begin
                env.AutoReset := True;
                TextColor(10);
                writeln('Autoreset is on.');
                writeln();
                TextColor(7);
            end;
            '\autoreset' : begin
                env.AutoReset := True;
                TextColor(10);
                writeln('Autoreset is on.');
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
                disposeEnvironment(env);
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
                    if (env.AutoReset) then env.Stack := parseOpen('clear', env.Stack, env.Settings, env.Variables);
                    env.Stack := parseOpen(input, env.Stack, env.Settings, env.Variables);
                    res := stack_show(env.Stack[env.Settings.StackPointer], env.Settings.Mask);
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
    disposeEnvironment(env);
end;

end.
