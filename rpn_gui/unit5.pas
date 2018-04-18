unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unit2;

const
    is_gui = true;

var
   arax : String;

function scan_value() : Entity;
function scan_number() : Entity;
function scan_string() : Entity;

implementation
uses Unit4;

function scan_value() : Entity;
begin
     arax := '';
     Form2.ShowModal;
     scan_value := buildNumber(StrToFloat(arax));
end;

function scan_number() : Entity;
var
	x : Extended;
begin
    arax := '';
    Form2.ShowModal;
    scan_number := buildNumber(StrToFloat(arax));
end;

function scan_string() : Entity;
var
	x : String;
begin
    arax := '';
    Form2.ShowModal;
    scan_string := buildString(arax);
end;

end.
