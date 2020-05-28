unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure TextColor(x : ShortInt);

implementation

procedure TextColor(x : ShortInt);
begin
    case x of 
        -1 : write(#27+'[0m');    // color reset
        0  : write(#27+'[0;30m'); // black
        1  : write(#27+'[0;31m'); // dark red
        2  : write(#27+'[0;32m'); // dark green
        3  : write(#27+'[0;33m'); // dark yellow
        4  : write(#27+'[0;34m'); // dark blue
        5  : write(#27+'[0;35m'); // dark magenta
        6  : write(#27+'[0;36m'); // dark cyan
        7  : write(#27+'[0;37m'); // dark gray
        8  : write(#27+'[1;30m'); // bright gray
        9  : write(#27+'[1;31m'); // bright red
        10 : write(#27+'[1;32m'); // bright green
        11 : write(#27+'[1;33m'); // bright yellow
        12 : write(#27+'[1;34m'); // bright blue
        13 : write(#27+'[1;35m'); // bright magenta
        14 : write(#27+'[1;36m'); // bright cyan
        15 : write(#27+'[1;37m'); // white
        else ;
    end;
end;

end.

