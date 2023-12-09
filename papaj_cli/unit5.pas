unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  RPNAbout, ComplexNumbers, Unit2, UnitEntity;

function scan_value() : Entity;
function scan_number() : Entity;
function scan_string() : Entity;
procedure writeOnConsole(x : String);
procedure writelnOnConsole(x : String);

implementation

//Uses Unit2;

function scan_value() : Entity;
var
	x : String;
	dummyNumber : ComplexType; 
 	posError : ShortInt; 
begin
    readln(x);
 	val(x, dummyNumber, posError); 
 	if (posError = 0) then begin
 		scan_value := buildNumber(dummyNumber);
 	end else begin
 		scan_value := buildString(x);
 	end;
end;

function scan_number() : Entity;
var
	x : Extended;
begin
    read(x);
 	scan_number := buildNumber(x);
end;

function scan_string() : Entity;
var
	x : String;
begin
    read(x);
 	scan_string := buildString(x);
end;

procedure writeOnConsole(x : String);
begin
    write(x);
end;

procedure writelnOnConsole(x : String);
begin
    writeln(x);
end;


end.
