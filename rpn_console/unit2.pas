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
    SortType  : ShortInt;
end;
// 0 - bubblesort
// 1 - quicksort
// 2 - mergesort
// 3 - bogosort

function pow(x,y:Extended):Extended;
function pow2(x,y:Extended):Extended;
function fact(x:Extended):Extended;
function newton_int(n, k: Extended) : Extended;
function newton_real(n, k: Extended) : Extended;
function fib(n: Extended) : Extended;

procedure qs_engine(var AI: array of Extended; ALo, AHi: Integer);
procedure ms_merge(var a : array of Extended; l,r,x,y:Integer);
procedure ms_engine(var a : array of Extended; l,r:Integer);
function bs_isSorted(var data : array of Extended) : Boolean;
procedure bs_shuffle(var data : array of Extended);
procedure bubblesort(var tab : array of Extended);
procedure quicksort(var tab : array of Extended);
procedure mergesort(var tab : array of Extended);
procedure bogosort(var tab : array of Extended);

procedure stack_add(var pocz:PStos; number:Extended);
function stack_clone(poc : PStos) : PStos;
function stack_reverse(poc : PStos) : PStos;
procedure stack_remove(var pocz:PStos);
procedure stack_clear(var pocz:PStos);
function stack_size(poc : PStos) : Longint;
function stack_show(poc : PStos; mask : String) : String;

function default_settings() : TSettings;

function commentcut(input : String) : String;
procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; var sets : TSettings);
function read_sourcefile(filename : String; var pocz : PStos; var sets : TSettings) : String;
function parseRPN(input : string; var pocz : PStos; var sets : TSettings) : String;
function calc_parseRPN(input : string; var sets : TSettings) : String;

implementation
uses Unit5;

var
        Steps   : Integer;

// MATHEMATICAL FUNCTIONS

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

// SORTS

procedure bubblesort(var tab : array of Extended);
var
  i, j : Longint;
  pom  : Extended;
begin
  for j := Length(tab)-1 downto 1 do
    for i := 0 to j-1 do 
      if (tab[i] > tab[i+1]) then begin
        pom := tab[i];
        tab[i] := tab[i+1];
        tab[i+1] := pom;
      end;
end;

procedure qs_engine(var AI: array of Extended; ALo, AHi: Integer);
var
  Lo, Hi   : Integer;
  Pivot, T : Extended;
begin
  Lo := ALo;
  Hi := AHi;
  Pivot := AI[(Lo + Hi) div 2];
  repeat
    while AI[Lo] < Pivot do
      Inc(Lo) ;
    while AI[Hi] > Pivot do
      Dec(Hi) ;
    if Lo <= Hi then
    begin
      T := AI[Lo];
      AI[Lo] := AI[Hi];
      AI[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > ALo then
    qs_engine(AI, ALo, Hi);
  if Lo < AHi then
    qs_engine(AI, Lo, AHi);
end;

procedure quicksort(var tab : array of Extended);
begin
  qs_engine(tab, 0, Length(tab)-1);
end;

procedure ms_merge(var a : array of Extended; l,r,x,y:Integer);
var 
  i,j,k,s : Integer;
  c       : array of Extended;
begin
  i:=l;
  j:=y;
  k:=0;
  SetLength(c,r-l+1+y-x+1);
  while (l<=r) and (x<=y) do
  begin
    if a[l]<a[x] then
    begin
      c[k]:=a[l];
      inc(l);
    end else begin
      c[k]:=a[x];
      inc(x);
    end;
    inc(k);
  end;

  if l<=r then
    for s:=l to r do
    begin
      c[k]:=a[s];
      inc(k);
    end 
  else
    for s:=x to y do
    begin
      c[k]:=a[s];
      inc(k);
    end;

  k:=0;
  for s:=i to j do
  begin
    a[s]:=c[k];
    inc(k);
  end;
end;

procedure ms_engine(var a : array of Extended; l,r:Integer);
var 
  m : Integer;
begin
  if l=r then Exit;
  m:=(l+r) div 2;
  ms_engine(a, l, m);
  ms_engine(a, m+1, r);
  ms_merge(a, l, m, m+1, r);
end;

procedure mergesort(var tab : array of Extended);
begin
  ms_engine(tab, 0, Length(tab)-1);
end;

function bs_isSorted(var data : array of Extended) : Boolean;
var
  Count : Longint;
  res   : Boolean;
begin
  res := true;
  for count := Length(data)-1 downto 1 do
    if (data[count] < data[count - 1]) then res := false;
  bs_isSorted := res;
end;

procedure bs_shuffle(var data : array of Extended);
var
  Count, rnd, i : Longint;
  pom           : Extended;
begin
  Count := Length(data);
  for i := 0 to Count-1 do
  begin   
    rnd := random(count);
    pom := data[i];
    data[i] := data[rnd];
    data[rnd] := pom;
  end;
end;

procedure bogosort(var tab : array of Extended);
begin
  randomize();
  while not (bs_isSorted(tab)) do bs_shuffle(tab);
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

function stack_size(poc : PStos) : Longint;
var
  x : Longint;
begin
    x := 0;
    while (poc <> nil) do begin
      x := x + 1;
      poc := poc^.Next;
    end;
    stack_size := x;
end;

function stack_show(poc : PStos; mask : String) : String;
var
  z : String;
begin
    z := '';
    while (poc <> nil) do begin
      z := FormatFloat(mask, poc^.Val) + ' ' + z;
      poc := poc^.Next;
    end;
    stack_show := z;
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
  pom.SortType := 1;
  default_settings := pom;
end;



// EVALUATION

procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; var sets : TSettings);
var
    x, y, z        : Extended;
    a              : Extended;
    Im             : Extended;
    Code, index    : Longint;
    Size           : Longint;
    Sizer          : PStos;
    HelpTable      : array of Extended;
    StrEax, StrEbx : String;
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
          '#sorttype=bsort' : begin
             sets.sorttype := 0;
          end;
          '#sorttype=qsort' : begin
             sets.sorttype := 1;
          end;
          '#sorttype=msort' : begin
             sets.sorttype := 2;
          end;
          '#sorttype=rsort' : begin
             sets.sorttype := 3;
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
          'status' : begin
                write(stack_show(pocz, sets.Mask));
          end;
          'statusln' : begin
                writeln(stack_show(pocz, sets.Mask));
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
          'sort' : begin
             size := stack_size(pocz);
             SetLength(HelpTable, size);
             for index := 0 to size-1 do begin
               HelpTable[index] := pocz^.Val;
               stack_remove(pocz);
             end;
             if (sets.sorttype = 0) then bubblesort(HelpTable);
             if (sets.sorttype = 1) then quicksort(HelpTable);
             if (sets.sorttype = 2) then mergesort(HelpTable);
             if (sets.sorttype = 3) then bogosort(HelpTable);
             for index := 0 to size-1 do stack_add(pocz, HelpTable[index]);         
             SetLength(HelpTable, 0);
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
              else begin
                case LeftStr(i, 9) of
                  '@source="' : begin
                    if (RightStr(i, 1) = '"') then begin
                      StrEax := RightStr(i, Length(i)-9);
                      StrEax := LeftStr(StrEax, Length(StrEax)-1);
                      StrEbx := read_sourcefile(StrEax, pocz, sets);
                    end else begin
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end else begin
        stack_add(pocz, Im);
      end;
end;

function commentcut(input : String) : String;
var 
  pom : String;
  i   : Integer;
begin
  pom := '';
  for i := 0 to Length(input) do begin
    if not ((input[i] = '/') and (input[i+1] = '/')) then begin
      pom := concat(pom, input[i]);
    end else begin
      break;
    end;
  end;
  commentcut := pom;
end;

function read_sourcefile(filename : String; var pocz : PStos; var sets : TSettings) : String;
var
  fun, S : String;
  fp     : Text;
begin
  fun := '';
  assignfile(fp, filename);
    reset(fp);
    while not eof(fp) do
    begin
      readln(fp, S);
      if (S <> '') then S := commentcut(S);
      fun := fun + ' ' + S;
    end;
    closefile(fp);
    S := parseRPN(fun, pocz, sets);
  read_sourcefile := S;
end;

function parseRPN(input : string; var pocz : PStos; var sets : TSettings) : String;
var
        L    : TStrings;
        i    : String;
        z    : String;
        step : Integer;
begin
        // delimites string
        //https://forum.lazarus.freepascal.org/index.php?topic=33644.0
        L := TStringlist.Create;
        L.Delimiter := ' ';
        L.QuoteChar := '"';
        L.StrictDelimiter := false;  // set this to false and the second 'test me' will be separate items! Try it.
        L.DelimitedText := input;

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
        L.Free;

        parseRPN := z;
end;

function calc_parseRPN(input : string; var sets : TSettings) : String;
var
  stack  : PStos;
  res    : String;
begin
  stack := nil;
  res := parseRPN(input, stack, sets);
  while stack <> nil do begin
    res := FormatFloat(sets.Mask, stack^.Val) + ' ' + res;
    stack_remove(stack);
  end;
  calc_parseRPN := res;
end;


end.
