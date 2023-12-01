unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, 
  ComplexNumbers, RPNAbout,
  Unit2, UnitEntity;

var
   arax : String;

function scan_value() : Entity;
function scan_number() : Entity;
function scan_string() : Entity;
procedure writeOnConsole(x : String);
procedure writelnOnConsole(x : String);

implementation
uses Unit4, Unit1;

function scan_value() : Entity;
var
	dummyNumber : ComplexType; 
 	posError : ShortInt; 
begin
    arax := '';
    Form2.ShowModal;
    val(arax, dummyNumber, posError); 
 	if (posError = 0) then begin
 		Result := buildNumber(dummyNumber);
 	end else begin
 		Result := buildString(arax);
 	end;
end;

function scan_number() : Entity;
var
	dummyNumber : ComplexType; 
 	posError : ShortInt; 
begin
    arax := '';
    Form2.ShowModal;
    val(arax, dummyNumber, posError); 
 	if (posError = 0) then begin
 		Result := buildNumber(dummyNumber);
 	end else begin
 		Result := buildString(arax);
 	end;
    Form1.Memo1.Text := Form1.Memo1.Text + #13#10;
end;

function scan_string() : Entity;
var
	x : String;
begin
    arax := '';
    Form2.ShowModal;
    scan_string := buildString(arax);
    Form1.Memo1.Text := Form1.Memo1.Text + #13#10;
end;

procedure writeOnConsole(x : String);
begin
     Form1.Memo1.Text := Form1.Memo1.Text + x;
end;

procedure writelnOnConsole(x : String);
begin
     Form1.Memo1.Text := Form1.Memo1.Text + x + #13#10;
end;

end.
