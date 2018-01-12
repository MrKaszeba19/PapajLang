unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
    is_gui = true;

var
   arax : String;

function scan_value() : extended;

implementation
uses Unit4;

function scan_value() : extended;
begin
     arax := '';
     Form2.ShowModal;
     scan_value := StrToFloat(arax);
end;

end.
