unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure clrscr();
procedure clrln();
procedure ClrEol;
procedure ClrLine;
procedure CursorOn;
procedure CursorOff;
procedure MoveUp(const Lines: word = 1);
procedure MoveDown(const Lines: word = 1);
procedure MoveRight(const Cols: word = 1);
procedure MoveLeft(const Cols: word = 1);
procedure MoveLineUp(const Lines: word = 1);
procedure MoveLineDown(const Lines: word = 1);
procedure GotoLine(const Line: word = 1);
procedure GotoColumn(const Col: word = 1);
procedure GotoXY(x, y : Integer);
procedure TextBackground(x : ShortInt);
procedure TextColor(x : ShortInt);
procedure TextBackgroundANSI(x : ShortInt);
procedure TextColorANSI(x : ShortInt);
procedure TextBackgroundRGB(r,g,b : Byte);
procedure TextColorRGB(r,g,b : Byte);
procedure TextReset();
procedure TextBold();
procedure TextItalic();
procedure TextUnderline();
procedure TextBlink();
procedure TextFastBlink();
procedure TextInverse();
procedure TextBoldOff();
procedure TextItalicOff();
procedure TextUnderlineOff();
procedure TextBlinkOff();
procedure TextFastBlinkOff();
procedure TextInverseOff();


implementation

procedure TextBackground(x : ShortInt);
begin
    case x of 
        -1 : write(#27+'[49m'); // color reset
        0  : write(#27+'[40m'); // black
        1  : write(#27+'[44m'); // dark red
        2  : write(#27+'[42m'); // dark green
        3  : write(#27+'[46m'); // dark yellow
        4  : write(#27+'[41m'); // dark blue
        5  : write(#27+'[45m'); // dark magenta
        6  : write(#27+'[43m'); // dark cyan
        7  : write(#27+'[47m'); // bright gray
        8  : write(#27+'[40m'); // dark gray
        9  : write(#27+'[44m'); // bright red
        10 : write(#27+'[42m'); // bright green
        11 : write(#27+'[46m'); // bright yellow
        12 : write(#27+'[41m'); // bright blue
        13 : write(#27+'[45m'); // bright magenta
        14 : write(#27+'[43m'); // bright cyan
        15 : write(#27+'[47m'); // white
        else ;
    end;
end;

procedure TextColor(x : ShortInt);
begin
    case x of 
        -1 : write(#27+'[0;39m');   // color reset
        0  : write(#27+'[0;30m'); // black
        1  : write(#27+'[0;34m'); // dark blue
        2  : write(#27+'[0;32m'); // dark green
        3  : write(#27+'[0;36m'); // dark cyan
        4  : write(#27+'[0;31m'); // dark red
        5  : write(#27+'[0;35m'); // dark magenta
        6  : write(#27+'[0;33m'); // dark yellow
        7  : write(#27+'[0;37m'); // bright gray
        8  : write(#27+'[1;30m'); // dark gray
        9  : write(#27+'[1;34m'); // bright red
        10 : write(#27+'[1;32m'); // bright green
        11 : write(#27+'[1;36m'); // bright yellow
        12 : write(#27+'[1;31m'); // bright blue
        13 : write(#27+'[1;35m'); // bright magenta
        14 : write(#27+'[1;33m'); // bright cyan
        15 : write(#27+'[1;37m'); // white
        else ;
    end;
end;

procedure TextBackgroundANSI(x : ShortInt);
begin
    case x of 
        -1 : write(#27+'[49m'); // color reset
        0  : write(#27+'[40m'); // black
        1  : write(#27+'[41m'); // dark red
        2  : write(#27+'[42m'); // dark green
        3  : write(#27+'[43m'); // dark yellow
        4  : write(#27+'[44m'); // dark blue
        5  : write(#27+'[45m'); // dark magenta
        6  : write(#27+'[46m'); // dark cyan
        7  : write(#27+'[47m'); // gray
        8  : write(#27+'[40m'); // dark gray
        9  : write(#27+'[41m'); // red
        10 : write(#27+'[42m'); // green
        11 : write(#27+'[43m'); // yellow
        12 : write(#27+'[44m'); // blue
        13 : write(#27+'[45m'); // magenta
        14 : write(#27+'[46m'); // cyan
        15 : write(#27+'[47m'); // white
        16 : write(#27+'[100m'); // bright gray
        17 : write(#27+'[101m'); // bright red
        18 : write(#27+'[102m'); // bright green
        19 : write(#27+'[103m'); // bright yellow
        20 : write(#27+'[104m'); // bright blue
        21 : write(#27+'[105m'); // bright magenta
        22 : write(#27+'[106m'); // bright cyan
        23 : write(#27+'[107m'); // white
        else ;
    end;
end;

procedure TextColorANSI(x : ShortInt);
begin
    case x of 
        -1 : write(#27+'[0;39m'); // color reset
        0  : write(#27+'[0;30m'); // black
        1  : write(#27+'[0;31m'); // dark red
        2  : write(#27+'[0;32m'); // dark green
        3  : write(#27+'[0;33m'); // dark yellow
        4  : write(#27+'[0;34m'); // dark blue
        5  : write(#27+'[0;35m'); // dark magenta
        6  : write(#27+'[0;36m'); // dark cyan
        7  : write(#27+'[0;37m'); // gray
        8  : write(#27+'[1;30m'); // dark gray
        9  : write(#27+'[1;31m'); // red
        10 : write(#27+'[1;32m'); // green
        11 : write(#27+'[1;33m'); // yellow
        12 : write(#27+'[1;34m'); // blue
        13 : write(#27+'[1;35m'); // magenta
        14 : write(#27+'[1;36m'); // cyan
        15 : write(#27+'[1;37m'); // white
        16 : write(#27+'[90m'); // bright gray
        17 : write(#27+'[91m'); // bright red
        18 : write(#27+'[92m'); // bright green
        19 : write(#27+'[93m'); // bright yellow
        20 : write(#27+'[94m'); // bright blue
        21 : write(#27+'[95m'); // bright magenta
        22 : write(#27+'[96m'); // bright cyan
        23 : write(#27+'[97m'); // white
        else ;
    end;
end;

procedure TextBackgroundRGB(r,g,b : Byte);
begin
    write(#27+'[48;2;'+IntToStr(r)+';'+IntToStr(g)+';'+IntToStr(b)+'m');
end;

procedure TextColorRGB(r,g,b : Byte);
begin
    write(#27+'[38;2;'+IntToStr(r)+';'+IntToStr(g)+';'+IntToStr(b)+'m');
end;

procedure GotoXY(x, y : Integer);
begin
    write(#27+'['+IntToStr(y)+';'+IntToStr(x)+'f');
end;

procedure clrscr();
begin
    write(#27+'[1;1H'+#27+'[2J');
end;

procedure clrln();
begin
    write(#27+'2dK');
end;

procedure ClrEol;
begin
    write(#27'[0K');
end;

procedure ClrLine;
begin
    write(#27'[2K');
end;

procedure CursorOn;
begin
    write(#27'[?25h');
end;

procedure CursorOff;
begin
    write(#27'[?25l');
end;

procedure MoveUp(const Lines: word = 1);
begin
    write(#27'[',lines,'A');
end;

procedure MoveDown(const Lines: word = 1);
begin
    write(#27'[',lines,'B');
end;

procedure MoveRight(const Cols: word = 1);
begin
    write(#27'[',cols,'C');
end;

procedure MoveLeft(const Cols: word = 1);
begin
    write(#27'[',cols,'D');
end;

procedure MoveLineDown(const Lines: word = 1);
begin
    write(#27'[',lines,'E');
end;

procedure MoveLineUp(const Lines: word = 1);
begin
    write(#27'[',lines,'F');
end;

procedure GotoColumn(const Col: word = 1);
begin
    write(#27'[',col,'G');
end;

procedure GotoLine(const Line: word = 1);
begin
    write(#27'[',line,'d');
end;

procedure TextReset();
begin
    write(#27+'[0m');
end;
procedure TextBold();
begin
    write(#27+'[1m');
end;
procedure TextItalic();
begin
    write(#27+'[3m');
end;
procedure TextUnderline();
begin
    write(#27+'[4m');
end;
procedure TextBlink();
begin
    write(#27+'[5m');
end;
procedure TextFastBlink();
begin
    write(#27+'[6m');
end;
procedure TextInverse();
begin
    write(#27+'[7m');
end;

procedure TextBoldOff();
begin
    write(#27+'[21m');
end;
procedure TextItalicOff();
begin
    write(#27+'[23m');
end;
procedure TextUnderlineOff();
begin
    write(#27+'[24m');
end;
procedure TextBlinkOff();
begin
    write(#27+'[25m');
end;
procedure TextFastBlinkOff();
begin
    write(#27+'[26m');
end;
procedure TextInverseOff();
begin
    write(#27+'[27m');
end;

end.

