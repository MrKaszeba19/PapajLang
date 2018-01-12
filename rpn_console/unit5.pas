unit Unit5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function scan_value() : Extended;

implementation

function scan_value() : Extended;
var
   x : Extended;
begin
     read(x);
     scan_value := x;
end;

end.
