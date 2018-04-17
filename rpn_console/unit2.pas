unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
        PI = 3.1415926535897932384626433832795;
        EU = 2.7182818284590452353602874713526;
        FI = 1.6180339887498948482045868343656;

type PStos = ^TStos;
     TStos    = record
     Val   : Extended;
     Next  : PStos;
end;

type TSettings = record
    Prevent   : Boolean;
    Autoclear : Boolean;
    Mask      : String;
end;

function pow(x,y:Extended):Extended;
function pow2(x,y:Extended):Extended;
function fact(x:Extended):Extended;
function newton_int(n, k: Extended) : Extended;
function newton_real(n, k: Extended) : Extended;
function fib(n: Extended) : Extended;

procedure stack_add(var pocz:PStos; number:Extended);
function stack_clone(poc : PStos) : PStos;
function stack_reverse(poc : PStos) : PStos;
procedure stack_remove(var pocz:PStos);
procedure stack_clear(var pocz:PStos);

function default_settings() : TSettings;

procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; var sets : TSettings);
function calc_parseRPN(input : string; var sets : TSettings) : String;

implementation
uses Unit5;

var
        Steps   : Integer;

function pow(x,y:Extended):Extended;
var
        s : Extended;
        i : integer;
begin
        s := 1;
        for i := 1 to trunc(abs(y)) do s := s * x;
        if (y < 0) then s := 1 / s;
        pow := s;
end;

function pow2(x,y:Extended):Extended;
var
        s : Extended;
begin
        s := exp(y*ln(x));
        pow2 := s;
end;

function fact(x:Extended):Extended;
var
        s : Extended;
        i : integer;
begin
     s := 1;
     for i := 1 to trunc(abs(x)) do s := s * i;
     fact := s;
end;

function newton_int(n, k : Extended) : Extended;
begin
     //newton (n, 0) = 1;
     //newton (n, n) = 1;
     //newton (n, k) = newton (n-1, k-1) + newton (n-1, k);
     if (k = 0.0) or (k = n) then newton_int := 1.0
     else newton_int := newton_int(n-1, k-1) + newton_int(n-1, k);
end;

function newton_real(n, k : Extended) : Extended;
var s, j : Extended;
begin
  s := 1;
  if (n < 0) then
    newton_real := pow(-1.0, k) * newton_real(k-n-1, k)
  else begin
    j := 1.0;
    while (j <= k) do
    begin
      s := s * (n-k+j)/j;
      j := j + 1;
    end;
    newton_real := s;
  end;
end;

function gcd(a, b : Extended) : Extended;
begin
	while (a <> b) do
	begin
        if (a > b) then a := a - b
        else b := b - a;
    end;
	gcd := a;
end;

function lcm(a, b : Extended) : Extended;
begin
	lcm := (a*b)/gcd(a, b);
end;

function fib(n: Extended) : Extended;
begin
     if n = 0.0 then fib := 0.0
     else if n = 1.0 then fib := 1.0
     else fib := fib(n-1.0) + fib(n-2.0);
end;

// STACK OPERATIONS

procedure stack_add(var pocz:PStos; number:Extended);
var
    Nowy: PStos;
  begin
    New(Nowy);
    Nowy^.Val := number;
    Nowy^.Next := Pocz;
    Pocz := Nowy;
  end;

procedure stack_remove(var pocz:PStos);
var
    Pom: PStos;
begin
    Pom := Pocz^.Next;
    Dispose(Pocz);
    Pocz := Pom;
end;

procedure stack_clear(var pocz:PStos);
begin
  while (pocz <> nil) do stack_remove(pocz);
end;

function stack_clone(poc : PStos) : PStos;
begin
    stack_clone := poc;
end;

function stack_reverse(poc : PStos) : PStos;
var
  pom : PStos;
begin
    while (poc <> nil) do begin
      stack_add(pom, poc^.Val);
      poc := poc^.Next;
    end;
    stack_reverse := pom;
end;

function default_settings() : TSettings;
var pom : TSettings;
begin
  pom.Prevent := false;
  pom.Autoclear := true;
  pom.Mask := '0.################';
  default_settings := pom;
end;



// EVALUATION

procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; var sets : TSettings);
var
    x, y, z        : Extended;
    a              : Extended;
    Im             : Extended;
    Code, index    : Integer;
    Sizer          : PStos;
    HelpTable      : array of Extended;
begin
    Steps := 1;
    SetLength(HelpTable, 0);

    Val (i,Im,Code);
    If Code<>0 then
        begin
             case i of
             // binary
             '+' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 z := x+y;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             '-' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 z := x-y;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             '*' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 z := x*y;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             '/' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 z := x/y;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             '^' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 if (y = trunc(y)) then begin
                    z := pow(x,y);
                 end else begin
                     z := pow2(x,y);
                 end;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             'pow' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 if (y = trunc(y)) then begin
                    z := pow(x,y);
                 end else begin
                     z := pow2(x,y);
                 end;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             'log' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 z := ln(x)/ln(y);
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             'root' : begin
                    y := pocz^.Val;
                    stack_remove(pocz);
                    x := pocz^.Val;
                    stack_remove(pocz);
                    z := pow2(x,1/y);
                    if not (sets.Autoclear) then begin
                        stack_add(pocz, x);
                        stack_add(pocz, y);
                    end;
                    stack_add(pocz, z);
             end;
             'mod' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   x := pocz^.Val;
                   stack_remove(pocz);
                   z := trunc(x) mod trunc(y);
                   if not (sets.Autoclear) then begin
                       stack_add(pocz, x);
                       stack_add(pocz, y);
                   end;
                   stack_add(pocz, z);
             end;
             'div' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   x := pocz^.Val;
                   stack_remove(pocz);
                   z := trunc(x) div trunc(y);
                   if not (sets.Autoclear) then begin
                       stack_add(pocz, x);
                       stack_add(pocz, y);
                   end;
                   stack_add(pocz, z);
             end;
             'cdiv' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   x := pocz^.Val;
                   stack_remove(pocz);
                   if trunc(x/y) < 0 then begin
                      z := trunc(x/y)-1;
                   end else begin
                      z := trunc(x/y);
                   end;
                   if not (sets.Autoclear) then begin
                       stack_add(pocz, x);
                       stack_add(pocz, y);
                   end;
                   stack_add(pocz, z);
             end;
             'cmod' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   x := pocz^.Val;
                   stack_remove(pocz);
                   if (x > 0) and (y < 0) then begin
                      z := ((trunc(x) mod trunc(y))+3)*(-1);
                   end else if (x < 0) and (y > 0) then begin
                      z := (trunc(x) mod trunc(y))+3;
                   end else begin
                      z := trunc(x) mod trunc(y);
                   end;
                   if not (sets.Autoclear) then begin
                       stack_add(pocz, x);
                       stack_add(pocz, y);
                   end;
                   stack_add(pocz, z);
             end;
             'choose' : begin
                 y := pocz^.Val;
                 stack_remove(pocz);
                 x := pocz^.Val;
                 stack_remove(pocz);
                 if (x = trunc(x)) then begin
                     z := newton_int(x,y);
                 end else begin
                     z := newton_real(x,y);
                 end;
                 if not (sets.Autoclear) then begin
                     stack_add(pocz, x);
                     stack_add(pocz, y);
                 end;
                 stack_add(pocz, z);
             end;
             'gcd' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   x := pocz^.Val;
                   stack_remove(pocz);
                   z := gcd(x, y);
                   if not (sets.Autoclear) then begin
                       stack_add(pocz, x);
                       stack_add(pocz, y);
                   end;
                   stack_add(pocz, z);
             end;
             'lcm' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   x := pocz^.Val;
                   stack_remove(pocz);
                   z := lcm(x, y);
                   if not (sets.Autoclear) then begin
                       stack_add(pocz, x);
                       stack_add(pocz, y);
                   end;
                   stack_add(pocz, z);
             end;


             // constants
             'PI' : begin
                   stack_add(pocz, PI);
             end;
             'EU' : begin
                   stack_add(pocz, EU);
             end;
             'FI' : begin
                   stack_add(pocz, FI);
             end;

             // unary
             'exp' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := exp(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'abs' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := abs(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'sqrt' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := sqrt(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'sin' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := sin(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'cos' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := cos(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'csc' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := 1/sin(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'sec' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := 1/cos(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'tan' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := sin(y)/cos(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'cot' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := cos(y)/sin(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             '!' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := fact(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'fact' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := fact(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'ln' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := ln(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'trunc' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := trunc(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'round' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := round(y);
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'fib' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := fib(trunc(y));
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'inc' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := y + 1;
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             'dec' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := y - 1;
                   if not (sets.Autoclear) then stack_add(pocz, y);
                   stack_add(pocz, z);
             end;
             '++' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := y + 1;
                   stack_add(pocz, z);
             end;
             '--' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   z := y - 1;
                   stack_add(pocz, z);
             end;

             // Directives
             '#silent' : begin
                sets.Prevent := true;
             end;
             '#autoclear=true' : begin
                sets.Autoclear := true;
             end;
             '#autoclear=false' : begin
                sets.Autoclear := false;
             end;
             '#real' : begin
                sets.Mask := '0.################';
             end;
             '#decimal' : begin
                sets.Mask := '#,###.################';
             end;
             '#milli' : begin
                sets.Mask := '0.000';
             end;
             '#float' : begin
                sets.Mask := '0.000000';
             end;
             '#double' : begin
                sets.Mask := '0.000000000000000';
             end;
             '#money' : begin
                sets.Mask := '0.00';
             end;
             '#amoney' : begin
                sets.Mask := '#,###.00';
             end;
             '#int' : begin
                sets.Mask := '0';
             end;
             '#scientific' : begin
                sets.Mask := '0.################E+00';
             end;
             '#scientific1' : begin
                sets.Mask := '0.000000000000000E+0000';
             end;


             // single operands
             'scan' : begin
                   z := scan_value();
                   stack_add(pocz, z);
             end;
             'times' : begin
                   y := pocz^.Val;
                   stack_remove(pocz);
                   if (y >= 0) then Steps := trunc(y);
             end;

             'clone' : begin
                   y := pocz^.Val;
                   stack_add(pocz, y);
             end;
             'print' : begin
                   y := pocz^.Val;
                   //write(y);
                   write(FormatFloat(sets.Mask, y));
             end;
             'println' : begin
                   y := pocz^.Val;
                   //writeln(y);
                   writeln(FormatFloat(sets.Mask, y));
             end;
             'rprint' : begin
                   y := pocz^.Val;
                   //write(y);
                   write(FormatFloat(sets.Mask, y));
                   stack_remove(pocz);
             end;
             'rprintln' : begin
                   y := pocz^.Val;
                   //writeln(y);
                   writeln(FormatFloat(sets.Mask, y));
                   stack_remove(pocz);
             end;
             'rem' : begin
                   stack_remove(pocz);
             end;
             'clear' : begin
                   stack_clear(pocz);
             end;
             'keep' : begin
                  y := pocz^.Val;
                  stack_remove(pocz);
                  if (y >= 1) then begin
                      SetLength(HelpTable, trunc(y));
                      for index := 0 to trunc(y)-1 do begin
                        HelpTable[index] := pocz^.Val;
                        stack_remove(pocz);
                      end;
                      stack_clear(pocz);
                      for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpTable[index]);
                      SetLength(HelpTable, 0);
                  end else if (y = 0) then begin
                      stack_clear(pocz);
                  end;
             end;
             'copy' : begin
                  y := pocz^.Val;
                  stack_remove(pocz);
                  if (y >= 1) then begin
                      SetLength(HelpTable, trunc(y));
                      for index := 0 to trunc(y)-1 do begin
                        HelpTable[index] := pocz^.Val;
                        stack_remove(pocz);
                      end;
                      for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpTable[index]);
                      for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpTable[index]);
                      SetLength(HelpTable, 0);
                  end else if (y = 0) then begin
                      stack_clear(pocz);
                  end;
             end;
             'mcopy' : begin
                  y := pocz^.Val;
                  stack_remove(pocz);
                  if (y >= 1) then begin
                      SetLength(HelpTable, trunc(y));
                      for index := 0 to trunc(y)-1 do begin
                        HelpTable[index] := pocz^.Val;
                        stack_remove(pocz);
                      end;
                      for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpTable[index]);
                      for index := 0 to trunc(y)-1 do stack_add(pocz, HelpTable[index]);
                      SetLength(HelpTable, 0);
                  end else if (y = 0) then begin
                      stack_clear(pocz);
                  end;
             end;
             


             // stack operands
             'sum' : begin
                   z := 0.0;
                   while (pocz <> nil) do
                   begin
                        y := pocz^.Val;
                        stack_remove(pocz);
                        z := z + y;
                   end;
                   stack_add(pocz, z);
             end;
             'product' : begin
                   z := 1.0;
                   while (pocz <> nil) do
                   begin
                        y := pocz^.Val;
                        stack_remove(pocz);
                        z := z * y;
                   end;
                   stack_add(pocz, z);
             end;
             'count' : begin
                   z := 0.0;
                   while (pocz <> nil) do
                   begin
                       z := z + 1;
                       stack_remove(pocz);
                   end;
                   stack_add(pocz, z);
             end;
             'size' : begin
                  Sizer := stack_reverse(pocz);
                  z := 0.0;
                  while (Sizer <> nil) do
                  begin
                       z := z + 1;
                       stack_remove(Sizer);
                  end;
                  stack_add(pocz, z);
             end;
             'avg' : begin
                   z := 0.0;
                   a := 0.0;
                   while (pocz <> nil) do
                   begin
                        y := pocz^.Val;
                        stack_remove(pocz);
                        z := z + y;
                        a := a + 1.0;
                   end;
                   stack_add(pocz, z/a);
             end;
             'min' : begin
                   a := pocz^.Val;
                   while (pocz <> nil) do
                   begin
                   		if a > pocz^.Val then a := pocz^.Val;
                    	stack_remove(pocz);
                   end;
                   stack_add(pocz, a);
             end;
             'max' : begin
                   a := pocz^.Val;
                   while (pocz <> nil) do
                   begin
                   		if a < pocz^.Val then a := pocz^.Val;
                    	stack_remove(pocz);
                   end;
                   stack_add(pocz, a);
             end;

             'rev' : begin
                   pocz := stack_reverse(pocz);
             end;

             'repl' : begin
                   pocz := stack_clone(pocz);
             end;
             
             // stack creators
             'seq' : begin
             	z := pocz^.Val;
                stack_remove(pocz);
             	y := pocz^.Val;
                stack_remove(pocz);
                x := pocz^.Val;
                stack_remove(pocz);
                if (x <= z) then
                begin
                	while (x <= z) do 
                	begin
                		stack_add(pocz, x);
                		x := x + y;
                	end;
                end else begin
                	while (x >= z) do 
                	begin
                		stack_add(pocz, x);
                		x := x - y;
                	end;
                end;
             end;

             'seql' : begin
             	z := pocz^.Val;
                if (sets.Autoclear) then stack_remove(pocz);
             	y := pocz^.Val;
                if (sets.Autoclear) then stack_remove(pocz);
                x := pocz^.Val;
                if (sets.Autoclear) then stack_remove(pocz);
                a := 1.0;
          		while (a <= z) do 
                begin
                	stack_add(pocz, x);
                	x := x + y;
                	a := a + 1.0;
                end;
             end;

             'gseq' : begin
             	z := pocz^.Val;
                stack_remove(pocz);
             	y := pocz^.Val;
                stack_remove(pocz);
                x := pocz^.Val;
                stack_remove(pocz);
                if (x <= z) then
                begin
                	while (x <= z) do 
                	begin
                		stack_add(pocz, x);
                		x := x * y;
                	end;
                end else begin
                	while (x >= z) do 
                	begin
                		stack_add(pocz, x);
                		x := x / y;
                	end;
                end;
             end;

             'gseql' : begin
             	z := pocz^.Val;
                stack_remove(pocz);
             	y := pocz^.Val;
                stack_remove(pocz);
                x := pocz^.Val;
                stack_remove(pocz);
                a := 1.0;
          		while (a <= z) do 
                begin
                	stack_add(pocz, x);
                	x := x * y;
                	a := a + 1.0;
                end;
             end;

             else begin
                 case LeftStr(i, 1) of
                      'X' : begin
                          if (RightStr(i, Length(i)-1) = '*') and (not (Unit5.is_gui)) then Steps := -1
                          else Steps := StrToInt(RightStr(i, Length(i)-1));
                      end;
                 end;
             end;

             end;
             end else begin
        begin
            stack_add(pocz, Im);
        end;
    end;

end;

function calc_parseRPN(input : string; var sets : TSettings) : String;
var
        L              : TStrings;
        i              : String;
        pocz           : PStos;
        z              : String;
        step           : Integer;
begin
        // delimites string
        //https://forum.lazarus.freepascal.org/index.php?topic=33644.0
        L := TStringlist.Create;
        L.Delimiter := ' ';
        L.QuoteChar := '"';
        L.StrictDelimiter := false;  // set this to false and the second 'test me' will be separate items! Try it.
        L.DelimitedText := input;

        pocz := nil;
        Steps := 1;
        for i in L do
        begin
             if Steps = -1 then
             begin
                repeat
                  evaluate(i, pocz, Steps, sets);
                until EOF;
                stack_remove(pocz);
             end
             else for step := 1 to Steps do evaluate(i, pocz, Steps, sets);
        end;
        z := '';
        while pocz <> nil do begin
          z := FormatFloat(sets.Mask, pocz^.Val) + ' ' + z;
          stack_remove(pocz);
        end;
        L.Free;

        calc_parseRPN := z;
end;


end.
