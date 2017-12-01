unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
var
   arax : Extended;

function scan_value() : Extended;

implementation
uses Unit4;

function scan_value() : Extended;
var
   x : Extended;
begin
     arax := 0;
     Form2.ShowModal;
     scan_value := arax;
end;

end.

