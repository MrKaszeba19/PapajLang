unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Unit2, Unit7;

const
  is_gui = false;

function scan_value() : Entity;
function scan_number() : Entity;
function scan_string() : Entity;

implementation

//Uses Unit2;

function scan_value() : Entity;
var
	x : String;
	dummyNumber:Extended; 
 	posError:integer; 
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


end.
