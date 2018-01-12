unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
var
   arax : String;

function scan_value() : String;

implementation
uses Unit4;

function scan_value() : String;
begin
     arax := '';
     Form2.ShowModal;
     scan_value := arax;
end;

end.
