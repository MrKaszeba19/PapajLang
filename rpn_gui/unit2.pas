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
procedure calc_stack_add(var pocz:PStos; number:extended);
function calc_stack_show(pocz:PStos) : extended;
procedure calc_stack_remove(var pocz:PStos);
procedure evaluate(i : String; var pocz : PStos);
function calc_parseRPN(input : string) : extended;

implementation
uses Unit5;

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
     if (k = 0) or (k = n) then newton_int := 1
     else newton_int := newton_int(n-1, k-1) + newton_int(n-1, k);
end;

procedure calc_stack_add(var pocz:PStos; number:extended);
var
    Nowy: PStos;
  begin
    New(Nowy);
    Nowy^.Liczba := number;
    Nowy^.Nastepny := Pocz;
    Pocz := Nowy;
  end;


function calc_stack_show(pocz:PStos) : extended;
begin
        calc_stack_show := pocz^.Liczba;
end;

procedure calc_stack_remove(var pocz:PStos);
var
    Pom: PStos;
  begin
      Pom := Pocz^.Nastepny;
      Dispose(Pocz);
      Pocz := Pom;
  end;

procedure evaluate(i : String; var pocz : PStos);
var
    x, y, z        : extended;
    Im             : Extended;
    Code           : Integer;
begin
    Val (i,Im,Code);
    If Code<>0 then
        begin
             case i of
             // binary
             '+' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 z := x+y;
                 calc_stack_add(pocz, z);
             end;
             '-' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 z := x-y;
                 calc_stack_add(pocz, z);
             end;
             '*' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 z := x*y;
                 calc_stack_add(pocz, z);
             end;
             '/' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 z := x/y;
                 calc_stack_add(pocz, z);
             end;
             '^' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 if (y = trunc(y)) then begin
                    z := pow(x,y);
                 end else begin
                     z := pow2(x,y);
                 end;
                 calc_stack_add(pocz, z);
             end;
             'pow' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 if (y = trunc(y)) then begin
                    z := pow(x,y);
                 end else begin
                     z := pow2(x,y);
                 end;
                 calc_stack_add(pocz, z);
             end;
             'log' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 z := ln(x)/ln(y);
                 calc_stack_add(pocz, z);
             end;
             'root' : begin
                    y := pocz^.Liczba;
                    calc_stack_remove(pocz);
                    x := pocz^.Liczba;
                    calc_stack_remove(pocz);
                    z := pow2(x,1/y);
                    calc_stack_add(pocz, z);
             end;
             'mod' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   x := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := trunc(x) mod trunc(y);
                   calc_stack_add(pocz, z);
             end;
             'div' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   x := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   if trunc(x/y) < 0 then begin
                      z := trunc(x/y)-1;
                   end else begin
                      z := trunc(x/y);
                   end;
                   calc_stack_add(pocz, z);
             end;
             'newton' : begin
                 y := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 x := pocz^.Liczba;
                 calc_stack_remove(pocz);
                 if (x = trunc(x)) then begin
                    z := newton_int(x,y);
                 end else begin
                     // insert newton for real x
                     z := pow2(y,x);
                 end;
                 calc_stack_add(pocz, z);
             end;
             // constants
             'PI' : begin
                   calc_stack_add(pocz, PI);
             end;
             'EU' : begin
                   calc_stack_add(pocz, EU);
             end;
             'FI' : begin
                   calc_stack_add(pocz, FI);
             end;

             // unary
             'exp' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := exp(y);
                   calc_stack_add(pocz, z);
             end;
             'abs' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := abs(y);
                   calc_stack_add(pocz, z);
             end;
             'sqrt' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := sqrt(y);
                   calc_stack_add(pocz, z);
             end;
             'sin' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := sin(y);
                   calc_stack_add(pocz, z);
             end;
             'cos' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := cos(y);
                   calc_stack_add(pocz, z);
             end;
             'csc' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := 1/sin(y);
                   calc_stack_add(pocz, z);
             end;
             'sec' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := 1/cos(y);
                   calc_stack_add(pocz, z);
             end;
             'tan' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := sin(y)/cos(y);
                   calc_stack_add(pocz, z);
             end;
             'cot' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := cos(y)/sin(y);
                   calc_stack_add(pocz, z);
             end;
             '!' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := fact(y);
                   calc_stack_add(pocz, z);
             end;
             'fact' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := fact(y);
                   calc_stack_add(pocz, z);
             end;
             'ln' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := ln(y);
                   calc_stack_add(pocz, z);
             end;
             'trunc' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := trunc(y);
                   calc_stack_add(pocz, z);
             end;
             'round' : begin
                   y := pocz^.Liczba;
                   calc_stack_remove(pocz);
                   z := round(y);
                   calc_stack_add(pocz, z);
             end;
             // single operands
             '>' : begin
                   z := scan_value();
                   calc_stack_add(pocz, z);
             end;

             end;
             end else begin
        begin
            calc_stack_add(pocz, Im);
        end;
    end;

end;

function calc_parseRPN(input : string) : extended;
var
        L              : TStrings;
        i              : String;
        pocz           : PStos;
        z              : Extended;
begin
        // delimites string
        //https://forum.lazarus.freepascal.org/index.php?topic=33644.0
        L := TStringlist.Create;
        L.Delimiter := ' ';
        L.QuoteChar := '"';
        L.StrictDelimiter := false;  // set this to false and the second 'test me' will be separate items! Try it.
        L.DelimitedText := input;

        pocz := nil;
        for i in L do
        begin
             evaluate(i, pocz);
        end;
        z := pocz^.Liczba;

        while pocz <> nil do calc_stack_remove(pocz);
        L.Free;

        calc_parseRPN := z;
end;


end.
