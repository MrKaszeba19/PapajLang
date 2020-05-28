unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, Process, SysUtils;

type REPLTheme = record
    ColorPrompt1 : ShortInt;
    ColorPrompt2 : ShortInt;
    ColorStack   : ShortInt;
    ColorGood    : ShortInt;
    ColorBad     : ShortInt;
    ColorWarn    : ShortInt;
    ColorRzulta1 : ShortInt;
    ColorRzulta2 : ShortInt;
    ColorReset   : ShortInt;
end;

function PS_parseString(input : string) : String;
procedure PS_runREPL();

implementation

uses StrUtils, UnitStack,
{$IFDEF MSWINDOWS}
		crt,
{$ELSE}
        ConsoleUtils,
{$ENDIF}
 UnitEnvironment;

function PS_parseString(input : string) : String;
var
    res : String;
    env : PSEnvironment;
begin
    if (Length(input) > 0) then 
    begin
        //input := cutShebang(input); 
        input := cutCommentMultiline(input);
        input := cutCommentEndline(input);
    end;
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
    writeln('Type \theme to check available themes for REPL');
    writeln('Type \!! to repeat last command');
    writeln('Type \!N to execute the N-th command of history (N >= 1)');
    repl_showshorthelp();
end;

{$IFDEF MSWINDOWS}
procedure noTheme(var th : REPLTheme);
begin
    th.ColorPrompt1 := 7;
    th.ColorPrompt2 := 7;
    th.ColorStack   := 7;
    th.ColorGood    := 7;
    th.ColorBad     := 7;
    th.ColorWarn    := 7;
    th.ColorRzulta1 := 7;
    th.ColorRzulta2 := 7;
    th.ColorReset   := 7;
end;

procedure normalTheme(var th : REPLTheme);
begin
    th.ColorPrompt1 := 14;
    th.ColorPrompt2 :=  6;
    th.ColorStack   :=  3;
    th.ColorGood    := 10;
    th.ColorBad     := 12;
    th.ColorWarn    := 15;
    th.ColorRzulta1 := 14;
    th.ColorRzulta2 := 15;
    th.ColorReset   := 7;
end;

procedure blueTheme(var th : REPLTheme);
begin
    th.ColorPrompt1 := 11;
    th.ColorPrompt2 :=  3;
    th.ColorStack   :=  4;
    th.ColorGood    := 10;
    th.ColorBad     :=  1;
    th.ColorWarn    := 15;
    th.ColorRzulta1 := 14;
    th.ColorRzulta2 := 15;
    th.ColorReset   := 7;
end;
{$ELSE}
procedure noTheme(var th : REPLTheme);
begin
    th.ColorPrompt1 := -1;
    th.ColorPrompt2 := -1;
    th.ColorStack   := -1;
    th.ColorGood    := -1;
    th.ColorBad     := -1;
    th.ColorWarn    := -1;
    th.ColorRzulta1 := -1;
    th.ColorRzulta2 := -1;
    th.ColorReset   := -1;
end;

procedure normalTheme(var th : REPLTheme);
begin
    th.ColorPrompt1 := 11;
    th.ColorPrompt2 :=  3;
    th.ColorStack   :=  6;
    th.ColorGood    := 10;
    th.ColorBad     :=  9;
    th.ColorWarn    := 15;
    th.ColorRzulta1 := 11;
    th.ColorRzulta2 := 15;
    th.ColorReset   := 7;
end;

procedure blueTheme(var th : REPLTheme);
begin
    th.ColorPrompt1 := 14;
    th.ColorPrompt2 :=  6;
    th.ColorStack   :=  3;
    th.ColorGood    := 10;
    th.ColorBad     := 12;
    th.ColorWarn    := 15;
    th.ColorRzulta1 := 11;
    th.ColorRzulta2 := 15;
    th.ColorReset   := -1;
end;
{$ENDIF}

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
    th      : REPLTheme;
begin
    normalTheme(th);
    env := buildNewEnvironment();
    SetLength(history, 0);
    writeln('REPL for PapajScript');
    writeln();
    repl_showshorthelp();
    repeat
        input := '';
        TextColor(th.ColorPrompt1);
        write('=> ');
        TextColor(th.ColorReset);
        readln(command);
        if (RightStr(command, 1) = '\') then 
        begin
            input := LeftStr(command, Length(command)-1) + ' ';
            repeat
                TextColor(th.ColorPrompt2);
                write('-> ');
                TextColor(th.ColorReset);
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
                    TextColor(th.ColorGood);
                    writeln('Entry #',RightStr(input, Length(input)-8),' of ',QuotedStr(fname),' has been removed successfully.');
                    TextColor(th.ColorReset);
                end;
            input := '';
        end else if (LeftStr(input, 8) = '\import:') or (input = '\import') then
        begin
            try
                if (env.AutoReset) then env.Stack := parseOpen('clear', env.Stack, env.Settings, env.Variables);
                if (input <> '\import') then fname := RightStr(input, Length(input)-8)
                else fname := '';
                if (fname = '') then begin
                    TextColor(th.ColorWarn);
                    writeln('REPL Warning: No valid filepath specified - importing from a "export.ppsc" file.');
                    TextColor(th.ColorReset);
                    fname := 'export.ppsc';
                end;
                env.Stack := read_source(fname, env);
            except
                On E : Exception do
                begin
                    TextColor(th.ColorBad);
                    writeln('REPL Error:   A problem occured when importing a file.');
                    writeln(E.ToString);
                    TextColor(th.ColorReset);
                    writeln();
                end;
            end;
            input := '';
        end else if (LeftStr(input, 8) = '\export:') or (input = '\export') then
        begin
            if (input <> '\export') then fname := RightStr(input, Length(input)-8)
            else fname := '';
            input := '';
            if (fname = '') then begin
                TextColor(th.ColorWarn);
                writeln('REPL Warning: No valid filepath specified - exporting to a "export.ppsc" file.');
                TextColor(th.ColorReset);
                fname := 'export.ppsc';
            end;
            try
                assignfile(fp, fname);
                rewrite(fp);
                for res in history do writeln(fp, res);
                closefile(fp);
                TextColor(th.ColorGood);
                writeln('Exported to "', fname,'" successfully.');
                TextColor(th.ColorReset);
            except
                On E : Exception do
                begin
                    TextColor(th.ColorBad);
                    writeln(StdErr, 'REPL Error:   A problem occured when exporting to a file.');
                    TextColor(th.ColorReset);
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
                TextColor(th.ColorGood);
                writeln('Your history is empty now.');
                writeln();
                TextColor(th.ColorReset);
            end;
            '\autoreset:false' : begin
                env.AutoReset := False;
                TextColor(th.ColorGood);
                writeln('Autoreset is off.');
                writeln();
                TextColor(th.ColorReset);
            end;
            '\autoreset:true' : begin
                env.AutoReset := True;
                TextColor(th.ColorGood);
                writeln('Autoreset is on.');
                writeln();
                TextColor(th.ColorReset);
            end;
            '\autoreset' : begin
                env.AutoReset := True;
                TextColor(th.ColorGood);
                writeln('Autoreset is on.');
                writeln();
                TextColor(th.ColorReset);
            end;
            '\comamrobic' : begin
                randomize;
                fun := Random(10);
                TextColor(th.ColorRzulta1);
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
                TextColor(th.ColorReset);
            end;
            '\rzulta' : begin
                TextColor(th.ColorRzulta1);
                writeln('  .----\----.  ');
                writeln(' ------------- ');
                writeln(' (  .-.  ___  )');
                writeln('((  (o)  (o)  |');
                writeln('`|     _\,    |');
                writeln('  \     _.   / ');
                writeln('   ---_____-   ');
                writeln();
                TextColor(th.ColorRzulta2);
                writeln('Jan Papaj 2 interpretowal male skrypty. xD');
                writeln();
                TextColor(th.ColorReset);
            end;
            '\reset' : begin
                disposeEnvironment(env);
                env := buildNewEnvironment();
                SetLength(history, 0);
                TextColor(th.ColorGood);
                writeln('All settings, history and data have been reset.');
                TextColor(th.ColorReset);
                writeln();
            end;
            '\theme' : begin
                writeln('Available themes: ');
                writeln('   \theme:false   : No theme');
                writeln('   \theme:default : Classical REPL (default)');
                writeln('   \theme:blue    : Blue');
            end;
            
            '\theme:false' : begin
                noTheme(th);
            end;
            '\theme:default' : begin
                normalTheme(th);
            end;
            '\theme:blue' : begin
                blueTheme(th);
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
                    if (Length(input) > 0) then 
                    begin
                        input := cutCommentMultiline(input);
                        input := cutCommentEndline(input);
                    end;
                    env.Stack := parseOpen(input, env.Stack, env.Settings, env.Variables);
                    res := stack_show(env.Stack[env.Settings.StackPointer], env.Settings.Mask);
                    if not (env.Settings.Prevent) then 
                    begin
                        TextColor(th.ColorStack);
                        writeln(res);
                        TextColor(th.ColorReset);
                    end;
                except
                    On E : Exception do
                    begin
                        TextColor(th.ColorBad);
                        writeln(StdErr, E.ToString);
                        TextColor(th.ColorReset);
                        writeln();
                    end;
                end;
            end;
        end;
    until not ((input <> '\q') and (input <> '\quit'));
    disposeEnvironment(env);
end;

end.
