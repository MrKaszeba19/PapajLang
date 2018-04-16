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
     Liczba   : extended;
     Nastepny : PStos;
end;

function pow(x,y:extended):extended;
function pow2(x,y:extended):extended;
function fact(x:extended):extended;
function newton_int(n, k: extended) : extended;
function newton_real(n, k: extended) : extended;
function fib(n: extended) : extended;

procedure stack_add(var pocz:PStos; number:extended);
function stack_clone(poc : PStos) : PStos;
function stack_reverse(poc : PStos) : PStos;
procedure stack_remove(var pocz:PStos);
procedure stack_clear(var pocz:PStos);

procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; mask : String);
function calc_parseRPN(input : string; flag : String; var prev : Integer) : String;

implementation
uses Unit5;

var
        Steps   : Integer;
        Prevent : Integer;

function pow(x,y:extended):extended;
var
        s : extended;
        i : integer;
begin
        s := 1;
        for i := 1 to trunc(abs(y)) do s := s * x;
        if (y < 0) then s := 1 / s;
        pow := s;
end;

function pow2(x,y:extended):extended;
var
        s : extended;
begin
        s := exp(y*ln(x));
        pow2 := s;
end;

function fact(x:extended):extended;
var
        s : extended;
        i : integer;
begin
     s := 1;
     for i := 1 to trunc(abs(x)) do s := s * i;
     fact := s;
end;

function newton_int(n, k : extended) : extended;
begin
     //newton (n, 0) = 1;
     //newton (n, n) = 1;
     //newton (n, k) = newton (n-1, k-1) + newton (n-1, k);
     if (k = 0.0) or (k = n) then newton_int := 1.0
     else newton_int := newton_int(n-1, k-1) + newton_int(n-1, k);
end;

function newton_real(n, k : extended) : extended;
var s, j : extended;
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

function gcd(a, b : extended) : extended;
begin
	while (a <> b) do
	begin
        if (a > b) then a := a - b
        else b := b - a;
    end;
	gcd := a;
end;

function lcm(a, b : extended) : extended;
begin
	lcm := (a*b)/gcd(a, b);
end;

function fib(n: extended) : extended;
begin
     if n = 0.0 then fib := 0.0
     else if n = 1.0 then fib := 1.0
     else fib := fib(n-1.0) + fib(n-2.0);
end;

// STACK OPERATIONS

procedure stack_add(var pocz:PStos; number:extended);
var
    Nowy: PStos;
  begin
    New(Nowy);
    Nowy^.Liczba := number;
    Nowy^.Nastepny := Pocz;
    Pocz := Nowy;
  end;

procedure stack_remove(var pocz:PStos);
var
    Pom: PStos;
begin
    Pom := Pocz^.Nastepny;
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
      stack_add(pom, poc^.Liczba);
      poc := poc^.Nastepny;
    end;
    stack_reverse := pom;
end;



// EVALUATION

procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; mask : String);
var
    x, y, z        : extended;
    a              : extended;
    Im             : Extended;
    Code           : Integer;
    Sizer          : PStos;
begin
    Steps := 1;
    Val (i,Im,Code);
    If Code<>0 then
        begin
             case i of
             // binary
             '+' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 z := x+y;
                 stack_add(pocz, z);
             end;
             '-' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 z := x-y;
                 stack_add(pocz, z);
             end;
             '*' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 z := x*y;
                 stack_add(pocz, z);
             end;
             '/' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 z := x/y;
                 stack_add(pocz, z);
             end;
             '^' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 if (y = trunc(y)) then begin
                    z := pow(x,y);
                 end else begin
                     z := pow2(x,y);
                 end;
                 stack_add(pocz, z);
             end;
             'pow' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 if (y = trunc(y)) then begin
                    z := pow(x,y);
                 end else begin
                     z := pow2(x,y);
                 end;
                 stack_add(pocz, z);
             end;
             'log' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 z := ln(x)/ln(y);
                 stack_add(pocz, z);
             end;
             'root' : begin
                    y := pocz^.Liczba;
                    stack_remove(pocz);
                    x := pocz^.Liczba;
                    stack_remove(pocz);
                    z := pow2(x,1/y);
                    stack_add(pocz, z);
             end;
             'mod' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   x := pocz^.Liczba;
                   stack_remove(pocz);
                   z := trunc(x) mod trunc(y);
                   stack_add(pocz, z);
             end;
             'div' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   x := pocz^.Liczba;
                   stack_remove(pocz);
                   z := trunc(x) div trunc(y);
                   stack_add(pocz, z);
             end;
             'cdiv' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   x := pocz^.Liczba;
                   stack_remove(pocz);
                   if trunc(x/y) < 0 then begin
                      z := trunc(x/y)-1;
                   end else begin
                      z := trunc(x/y);
                   end;
                   stack_add(pocz, z);
             end;
             'cmod' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   x := pocz^.Liczba;
                   stack_remove(pocz);
                   if (x > 0) and (y < 0) then begin
                      z := ((trunc(x) mod trunc(y))+3)*(-1);
                   end else if (x < 0) and (y > 0) then begin
                      z := (trunc(x) mod trunc(y))+3;
                   end else begin
                      z := trunc(x) mod trunc(y);
                   end;
                   stack_add(pocz, z);
             end;
             'choose' : begin
                 y := pocz^.Liczba;
                 stack_remove(pocz);
                 x := pocz^.Liczba;
                 stack_remove(pocz);
                 if (x = trunc(x)) then begin
                     z := newton_int(x,y);
                 end else begin
                     z := newton_real(x,y);
                 end;
                 stack_add(pocz, z);
             end;
             'gcd' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   x := pocz^.Liczba;
                   stack_remove(pocz);
                   z := gcd(x, y);
                   stack_add(pocz, z);
             end;
             'lcm' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   x := pocz^.Liczba;
                   stack_remove(pocz);
                   z := lcm(x, y);
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
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := exp(y);
                   stack_add(pocz, z);
             end;
             'abs' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := abs(y);
                   stack_add(pocz, z);
             end;
             'sqrt' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := sqrt(y);
                   stack_add(pocz, z);
             end;
             'sin' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := sin(y);
                   stack_add(pocz, z);
             end;
             'cos' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := cos(y);
                   stack_add(pocz, z);
             end;
             'csc' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := 1/sin(y);
                   stack_add(pocz, z);
             end;
             'sec' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := 1/cos(y);
                   stack_add(pocz, z);
             end;
             'tan' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := sin(y)/cos(y);
                   stack_add(pocz, z);
             end;
             'cot' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := cos(y)/sin(y);
                   stack_add(pocz, z);
             end;
             '!' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := fact(y);
                   stack_add(pocz, z);
             end;
             'fact' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := fact(y);
                   stack_add(pocz, z);
             end;
             'ln' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := ln(y);
                   stack_add(pocz, z);
             end;
             'trunc' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := trunc(y);
                   stack_add(pocz, z);
             end;
             'round' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := round(y);
                   stack_add(pocz, z);
             end;
             'fib' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := fib(trunc(y));
                   stack_add(pocz, z);
             end;
             'inc' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := y + 1;
                   stack_add(pocz, z);
             end;
             'dec' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := y - 1;
                   stack_add(pocz, z);
             end;
             '++' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := y + 1;
                   stack_add(pocz, z);
             end;
             '--' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   z := y - 1;
                   stack_add(pocz, z);
             end;

             // single operands
             'scan' : begin
                   z := scan_value();
                   stack_add(pocz, z);
             end;
             'times' : begin
                   y := pocz^.Liczba;
                   stack_remove(pocz);
                   if (y >= 1) then Steps := trunc(y);
             end;
             '#silent' : begin
                Prevent := 1;
             end;

             'clone' : begin
                   y := pocz^.Liczba;
                   stack_add(pocz, y);
             end;
             'print' : begin
                   y := pocz^.Liczba;
                   //write(y);
                   write(FormatFloat(mask, y));
             end;
             'println' : begin
                   y := pocz^.Liczba;
                   //writeln(y);
                   writeln(FormatFloat(mask, y));
             end;
             'rprint' : begin
                   y := pocz^.Liczba;
                   //write(y);
                   write(FormatFloat(mask, y));
                   stack_remove(pocz);
             end;
             'rprintln' : begin
                   y := pocz^.Liczba;
                   //writeln(y);
                   writeln(FormatFloat(mask, y));
                   stack_remove(pocz);
             end;
             'rem' : begin
                   stack_remove(pocz);
             end;
             'clear' : begin
                   stack_clear(pocz);
             end;


             // stack operands
             'sum' : begin
                   z := 0.0;
                   while (pocz <> nil) do
                   begin
                        y := pocz^.Liczba;
                        stack_remove(pocz);
                        z := z + y;
                   end;
                   stack_add(pocz, z);
             end;
             'product' : begin
                   z := 1.0;
                   while (pocz <> nil) do
                   begin
                        y := pocz^.Liczba;
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
                        y := pocz^.Liczba;
                        stack_remove(pocz);
                        z := z + y;
                        a := a + 1.0;
                   end;
                   stack_add(pocz, z/a);
             end;
             'min' : begin
                   a := pocz^.Liczba;
                   while (pocz <> nil) do
                   begin
                   		if a > pocz^.Liczba then a := pocz^.Liczba;
                    	stack_remove(pocz);
                   end;
                   stack_add(pocz, a);
             end;
             'max' : begin
                   a := pocz^.Liczba;
                   while (pocz <> nil) do
                   begin
                   		if a < pocz^.Liczba then a := pocz^.Liczba;
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
             	z := pocz^.Liczba;
                stack_remove(pocz);
             	y := pocz^.Liczba;
                stack_remove(pocz);
                x := pocz^.Liczba;
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
             	z := pocz^.Liczba;
                stack_remove(pocz);
             	y := pocz^.Liczba;
                stack_remove(pocz);
                x := pocz^.Liczba;
                stack_remove(pocz);
                a := 1.0;
          		while (a <= z) do 
                begin
                	stack_add(pocz, x);
                	x := x + y;
                	a := a + 1.0;
                end;
             end;

             'gseq' : begin
             	z := pocz^.Liczba;
                stack_remove(pocz);
             	y := pocz^.Liczba;
                stack_remove(pocz);
                x := pocz^.Liczba;
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
             	z := pocz^.Liczba;
                stack_remove(pocz);
             	y := pocz^.Liczba;
                stack_remove(pocz);
                x := pocz^.Liczba;
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

function calc_parseRPN(input : string; flag : String; var prev : Integer) : String;
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
                  evaluate(i, pocz, Steps, flag);
                until EOF;
                stack_remove(pocz);
             end
             else for step := 1 to Steps do evaluate(i, pocz, Steps, flag);
        end;
        z := '';
        while pocz <> nil do begin
          z := FormatFloat(flag, pocz^.Liczba) + ' ' + z;
          stack_remove(pocz);
        end;
        L.Free;

        prev := Prevent;

        calc_parseRPN := z;
end;


end.
