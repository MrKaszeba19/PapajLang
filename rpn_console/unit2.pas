unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils;

const
	PI = 3.1415926535897932384626433832795;
	EU = 2.7182818284590452353602874713526;
	FI = 1.6180339887498948482045868343656;
	TNIL = 0;
	TNUM = 1;
	TSTR = 2;
	TVEN = 3;
	TVES = 4;
	TVEC = 5;

type Entity = record
	EntityType : Integer;
	Num        : Extended;	// plans to make them arrays
	Str        : String;
end;
// 0 - unknown/null
// 1 - number
// 2 - string
// 3 - vector<number>
// 4 - vector<string>
// 5 - vector<any>


type PStos = ^TStos;
	TStos = record
	Val   : Entity;
	Next  : PStos;
end;

type TSettings = record
    Prevent       : Boolean;
    Autoclear     : Boolean;
    Mask          : String;
    SortType      : ShortInt;
    StrictType    : Boolean;
    CaseSensitive : Boolean;
end;
// sorts
// 0 - bubblesort
// 1 - quicksort
// 2 - mergesort
// 3 - bogosort

//function pow(x,y:Extended):Extended;
//function pow2(x,y:Extended):Extended;
//function fact(x:Extended):Extended;
//function newton_int(n, k: Extended) : Extended;
//function newton_real(n, k: Extended) : Extended;
//function fib(n: Extended) : Extended;

//procedure qs_engine(var AI: array of Extended; ALo, AHi: Integer);
//procedure ms_merge(var a : array of Extended; l,r,x,y:Integer);
//procedure ms_engine(var a : array of Extended; l,r:Integer);
//function bs_isSorted(var data : array of Extended) : Boolean;
//procedure bs_shuffle(var data : array of Extended);
//procedure bubblesort(var tab : array of Extended);
procedure quicksort(var tab : array of Extended);
procedure mergesort(var tab : array of Extended);
procedure bogosort(var tab : array of Extended);

procedure assertEntity(val : Entity; const wtype : Integer);
procedure assertEntityLocated(val : Entity; const wtype : Integer; operand : String);
function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
function buildNumber(val : Extended) : Entity;
function buildString(val : String) : Entity;

procedure stack_add(var pocz:PStos; node : Entity);
function stack_clone(poc : PStos) : PStos;
function stack_reverse(poc : PStos) : PStos;
procedure stack_remove(var pocz:PStos);
procedure stack_clear(var pocz:PStos);
function stack_get(pocz:PStos) : Entity;
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

// TABLES AGGREGATE

procedure table_reverse(var tab : array of Extended);
var 
	pom : array of Extended;
	i   : Integer;
begin
	SetLength(pom, Length(tab));
	for i := 0 to Length(tab)-1 do pom[i] := tab[Length(tab)-1-i];
	for i := 0 to Length(tab)-1 do tab[i] := pom[i];
	SetLength(pom, 0);
end;

//procedure ReverseInPlaceWithPointers(var A: array of double);
//var
//    Tmp: double;
//    iMin, imax: pDouble;
//begin
//    iMin := @A[0];
//    imax := @A[High(A)];
//    while iMin < iMax do begin
//      Tmp := iMax^;
//      iMax^ := iMin^;
//      iMin^ := Tmp;
//      Inc(iMin);
//      Dec(iMax);
//    end;
//end; 

function table_sum(tab : array of Extended) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i];
  table_sum := s;
end;

function table_product(tab : array of Extended) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 1.0;
  for i := 0 to Length(tab)-1 do
  	s := s * tab[i];
  table_product := s;
end;

function table_avg(tab : array of Extended) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i];
  table_avg := s/Length(tab);
end;

function table_min(tab : array of Extended) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := tab[0];
  for i := 1 to Length(tab)-1 do
  	if (tab[i] < s) then s := tab[i];
  table_min := s;
end;

function table_max(tab : array of Extended) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := tab[0];
  for i := 1 to Length(tab)-1 do
  	if (tab[i] > s) then begin
  		s := tab[i];
  		//writeln(s);
  	end; 
  table_max := s;
end;

function table_median(tab : array of Extended) : Extended;
begin
	quicksort(tab);
	if (Length(tab) mod 2 = 0) then table_median := 0.5*(tab[Length(tab) div 2 - 1] + tab[Length(tab) div 2])
	else table_median := tab[Length(tab) div 2];
end;

function table_abs(tab : array of Extended) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i]*tab[i];
  table_abs := sqrt(s);
end;

function table_variance(tab : array of Extended) : Extended;
var
	i    : Integer;
	s    : Extended;
	mean : Extended;
begin
	mean := table_avg(tab);
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + sqr(tab[i]-mean);
	table_variance := s/Length(tab);
end;

function table_stddev(tab : array of Extended) : Extended;
begin
	table_stddev := sqrt(table_variance(tab));
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

// ENTITY OPERATIONS

procedure raiserror(Const msg : string);  
begin  
  raise exception.create(Msg) at  
  get_caller_addr(get_frame),  
  get_caller_frame(get_frame);  
end;  

function getEntityTypeName(const x : Integer) : String;
begin
  case x of
    TNIL : getEntityTypeName := 'nil';
    TNUM : getEntityTypeName := 'number';
    TSTR : getEntityTypeName := 'string';
    TVEN : getEntityTypeName := 'vector<number>';
    TVES : getEntityTypeName := 'vector<string>';
    TVEC : getEntityTypeName := 'vector<any>';
    else getEntityTypeName := 'unknown';
  end;
end;

function getEntitySpec(x : Entity) : String;
begin
  case x.EntityType of
    TNIL : getEntitySpec := '<nil>';
    TNUM : getEntitySpec := FormatFloat('0.###############', x.Num) + ' : <number>';
    TSTR : getEntitySpec := '"' + x.Str + '" : <string>';
    TVEN : getEntitySpec := '<vector<number>>';
    TVES : getEntitySpec := '<vector<string>>';
    TVEC : getEntitySpec := '<vector<any>>';
    else getEntitySpec := '<unknown>';
  end;
end;

procedure assertEntity(val : Entity; const wtype : Integer);
begin
  if (val.EntityType <> wtype) then 
    raiserror('Type mismatch: <'+getEntityTypeName(wtype)+'> expected, got <'+getEntitySpec(val)+'>');
end;

procedure assertEntityLocated(val : Entity; const wtype : Integer; operand : String);
begin
  if (val.EntityType <> wtype) then 
    raiserror('Type mismatch at "'+operand+'": <'+getEntityTypeName(wtype)+'> expected, got ['+getEntitySpec(val)+']');
end;

function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
var
  pom : Entity;
begin
  pom.EntityType := TNUM;
  pom.Num := val;
  pom.Str := FormatFloat(sets.Mask, val);
  buildNumberFormattted := pom;
end;

function buildNumber(val : Extended) : Entity;
var
  pom : Entity;
begin
  pom.EntityType := TNUM;
  pom.Num := val;
  pom.Str := '' + FormatFloat('0.###############' ,val);
  buildNumber := pom;
end;

function buildString(val : String) : Entity;
var
  pom : Entity;
begin
  pom.EntityType := TSTR;
  pom.Str := val;
  pom.Num := Length(val);
  buildString := pom;
end;

// STACK OPERATIONS

procedure stack_add(var pocz:PStos; node : Entity);
var
    Nowy: PStos;
  begin
    New(Nowy);
    Nowy^.Val := node;
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

function stack_get(pocz:PStos) : Entity;
begin
        stack_get := pocz^.Val;
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
      if (poc^.Val.EntityType = TNUM) then z := FormatFloat(mask, poc^.Val.Num) + ' ' + z;
      if (poc^.Val.EntityType = TSTR) then z := '"' + poc^.Val.Str + '" ' + z;
      poc := poc^.Next;
    end;
    stack_show := z;
end;


function stack_reverse(poc : PStos) : PStos;
var
  pom : PStos;
begin
    pom := nil;
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
  pom.StrictType := true;
  pom.CaseSensitive := true;
  default_settings := pom;
end;



// EVALUATION

procedure evaluate(i : String; var pocz : PStos; var Steps : Integer; var sets : TSettings);
var
    x, y, z, a, Im         : Extended;
    Code, index    	       : Longint;
    Size           	       : Longint;
    Sizer          	       : PStos;
    HelpETable     	       : array of Entity;
    HelpNTable     	       : array of Extended;
    HelpSTable     	       : array of String;
    HelpTStrings           : TStrings;
    StrEax, StrEbx, StrEcx : String;
    EntEax, EntEbx         : Entity;
    ExtEax, ExtEbx         : Extended; 
    IntEax, IntEbx         : integer; 
begin
    Steps := 1;
    SetLength(HelpETable, 0);
    SetLength(HelpNTable, 0);
    SetLength(HelpSTable, 0);

    StrEcx := i;
    if not (sets.CaseSensitive) then i := LowerCase(i);
    Val (i,Im,Code);
    If Code<>0 then
      begin
        case i of
          // binary
          '+' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := x+y;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          '-' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := x-y;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          '*' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := x*y;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          '/' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := x/y;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          '^' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (y = trunc(y)) then begin
               z := pow(x,y);
            end else begin
                z := pow2(x,y);
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'pow' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (y = trunc(y)) then begin
               z := pow(x,y);
            end else begin
                z := pow2(x,y);
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'log' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := ln(x)/ln(y);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'root' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := pow2(x,1/y);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'mod' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := trunc(x) mod trunc(y);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'div' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := trunc(x) div trunc(y);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'cdiv' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if trunc(x/y) < 0 then begin
               z := trunc(x/y)-1;
            end else begin
               z := trunc(x/y);
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'cmod' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (x > 0) and (y < 0) then begin
               z := ((trunc(x) mod trunc(y))+3)*(-1);
            end else if (x < 0) and (y > 0) then begin
               z := (trunc(x) mod trunc(y))+3;
            end else begin
               z := trunc(x) mod trunc(y);
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'choose' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (x = trunc(x)) then begin
                z := newton_int(x,y);
            end else begin
                z := newton_real(x,y);
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'gcd' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := gcd(x, y);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;
          'lcm' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            z := lcm(x, y);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
            end;
            stack_add(pocz, buildNumber(z));
          end;


          // constants
          'PI' : begin
            stack_add(pocz, buildNumber(PI));
          end;
          'EU' : begin
            stack_add(pocz, buildNumber(EU));
          end;
          'FI' : begin
            stack_add(pocz, buildNumber(FI));
          end;

          // unary
          'exp' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := exp(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'abs' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := abs(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'sqrt' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := sqrt(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'sin' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := sin(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'cos' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := cos(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'csc' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := 1/sin(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'sec' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := 1/cos(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'tan' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := sin(y)/cos(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'cot' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := cos(y)/sin(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          '!' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := fact(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'fact' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := fact(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'ln' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := ln(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'trunc' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := trunc(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'round' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := round(y);
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'fib' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := fib(trunc(y));
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'inc' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := y + 1;
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          'dec' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := y - 1;
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
          '++' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := y + 1;
            stack_add(pocz, buildNumber(z));
          end;
          '--' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := y - 1;
            stack_add(pocz, buildNumber(z));
          end;

          // String operations
          'concat' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEax := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := concat(StrEax, StrEbx);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEax));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'crush' : begin
          	SetLength(HelpSTable, 0);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            IntEbx := 1;
            while (IntEbx <= Length(StrEbx)) do begin
            	SetLength(HelpSTable, IntEbx+1);
            	HelpSTable[IntEbx] := Copy(StrEbx, IntEbx, 1);
            	IntEbx := IntEbx + 1; 
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(y));
              stack_add(pocz, buildString(StrEbx));
            end;
            for index := 1 to Length(HelpSTable)-1 do stack_add(pocz, buildString(HelpSTable[index])); 
            SetLength(HelpSTable, 0);
          end;
          'crushby' : begin
          	SetLength(HelpSTable, 0);
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            IntEax := 1;
            IntEbx := 1;
            while (IntEax <= Length(StrEbx)) do begin
            	SetLength(HelpSTable, IntEbx+1);
            	HelpSTable[IntEbx] := Copy(StrEbx, IntEax, trunc(y));
            	IntEax := IntEax + trunc(y);
            	IntEbx := IntEbx + 1; 
            end;
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(y));
              stack_add(pocz, buildString(StrEbx));
            end;
            for index := 1 to Length(HelpSTable)-1 do stack_add(pocz, buildString(HelpSTable[index])); 
            SetLength(HelpSTable, 0);
          end;
          'leftstr' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            StrEax := LeftStr(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(y));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEax));
          end;
          'rightstr' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            StrEax := RightStr(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(y));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEax));
          end;
          'trim' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := Trim(StrEbx);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'ltrim' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := TrimLeft(StrEbx);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'rtrim' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := TrimRight(StrEbx);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'despace' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := DelChars(StrEbx, ' ');
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'onespace' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := DelSpace1(StrEbx);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'dechar' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEcx := pocz^.Val.Str;
            stack_remove(pocz); 
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := DelChars(StrEbx, StrEcx[1]);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'bind' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEax := pocz^.Val.Str;
            stack_remove(pocz);
            
            StrEcx := StrEax + ' ' + StrEbx;

            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEax));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEcx));
          end;
          'split' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            if (sets.Autoclear) then stack_remove(pocz); 
            
            HelpTStrings := TStringlist.Create;
            HelpTStrings.Delimiter := ' ';
            HelpTStrings.QuoteChar := '"';
            HelpTStrings.StrictDelimiter := false;
            HelpTStrings.DelimitedText := StrEbx;

            for StrEax in HelpTStrings do stack_add(pocz, buildString(StrEax)); 
            HelpTStrings.Free;
          end;
          'splitby' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEcx := pocz^.Val.Str;
            stack_remove(pocz); 
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            if (sets.Autoclear) then stack_remove(pocz); 
            
            HelpTStrings := TStringlist.Create;
            HelpTStrings.Delimiter := StrEcx[1];
            HelpTStrings.QuoteChar := '"';
            HelpTStrings.StrictDelimiter := false;
            HelpTStrings.DelimitedText := StrEbx;

            for StrEax in HelpTStrings do stack_add(pocz, buildString(StrEax)); 
            HelpTStrings.Free;
          end;
          'substr' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            StrEax := Copy(StrEbx, trunc(x), trunc(y));
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEax)); 
          end;
          'strbetween' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            StrEax := Copy(StrEbx, trunc(x), trunc(y)-trunc(x)+1);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildNumber(x));
              stack_add(pocz, buildNumber(y));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildString(StrEax)); 
          end;
          'pos' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEax := pocz^.Val.Str;
            stack_remove(pocz); 
            ExtEax := Pos(StrEbx, StrEax);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEax));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildNumber(ExtEax)); 
          end;
          'npos' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i); 
            y := pocz^.Val.Num;
            stack_remove(pocz); 
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEax := pocz^.Val.Str;
            stack_remove(pocz); 
            ExtEax := NPos(StrEbx, StrEax, trunc(y));
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEax));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildNumber(ExtEax)); 
          end;
          'occur' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz); 
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEax := pocz^.Val.Str;
            stack_remove(pocz);
            IntEax := 0;
            repeat
              IntEbx := NPos(StrEbx, StrEax, IntEax+1);
              if (IntEbx <> 0) then Inc(IntEax);
            until (IntEbx = 0);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEax));
              stack_add(pocz, buildString(StrEbx));
            end;
            stack_add(pocz, buildNumber(IntEax)); 
          end;
          'strparse' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i); 
            StrEbx := pocz^.Val.Str;
            stack_remove(pocz);
            StrEcx := parseRPN(StrEbx, pocz, sets);
            if not (sets.Autoclear) then begin
              stack_add(pocz, buildString(StrEbx));
            end;
            //stack_add(pocz, buildString(StrEcx));
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
          '#stricttype=true' : begin
             sets.StrictType := true;
          end;
          '#stricttype=false' : begin
             sets.StrictType := false;
          end;
          '#casesensitive=true' : begin
             sets.CaseSensitive := true;
          end;
          '#casesensitive=false' : begin
             sets.CaseSensitive := false;
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
            EntEax := scan_value();
            stack_add(pocz, EntEax);
          end;
          'scannum' : begin
            EntEax := scan_number();
            if (sets.StrictType) then assertEntityLocated(EntEax, TNUM, i);
            stack_add(pocz, EntEax);
          end;
          'scanstr' : begin
            EntEax := scan_string();
            if (sets.StrictType) then assertEntityLocated(EntEax, TSTR, i);
            stack_add(pocz, EntEax);
          end;
          'times' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (y >= 0) then Steps := trunc(y);
          end;
          'clone' : begin
                EntEax := pocz^.Val;
                stack_add(pocz, EntEax);
          end;
          'type' : begin
          	EntEax := pocz^.Val;
          	if (sets.Autoclear) then stack_remove(pocz);
          	stack_add(pocz, buildString(getEntityTypeName(EntEax.EntityType)));
          end;
          'tostring' : begin
          	EntEax := pocz^.Val;
            if (sets.Autoclear) then stack_remove(pocz);
            stack_add(pocz, buildString(EntEax.Str));
          end;
          'length' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i);
            StrEax := pocz^.Val.Str;
            if (sets.Autoclear) then stack_remove(pocz);
            stack_add(pocz, buildNumber(Length(StrEax)));
          end;
          'len' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i);
            StrEax := pocz^.Val.Str;
            //if (sets.Autoclear) then stack_remove(pocz);
            stack_add(pocz, buildNumber(Length(StrEax)));
          end;
          'val' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TSTR, i);
            StrEax := pocz^.Val.Str;
            if (sets.Autoclear) then stack_remove(pocz);
            val(StrEax, ExtEax, IntEax); 
            if (IntEax = 0) then begin
              stack_add(pocz, buildNumber(ExtEax));
            end else begin
              stack_add(pocz, buildString(StrEax));
            end;
          end;
          'print' : begin
            EntEax := pocz^.Val;
            if (sets.Autoclear) then stack_remove(pocz);
            if (EntEax.EntityType = TNUM) then write(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then write(EntEax.Str);
          end;
          'println' : begin
            EntEax := pocz^.Val;
            if (sets.Autoclear) then stack_remove(pocz);
            if (EntEax.EntityType = TNUM) then writeln(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writeln(EntEax.Str);
          end;
          'rprint' : begin
            EntEax := pocz^.Val;
            stack_remove(pocz);
            if (EntEax.EntityType = TNUM) then write(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then write(EntEax.Str);
          end;
          'rprintln' : begin
            EntEax := pocz^.Val;
            stack_remove(pocz);
            if (EntEax.EntityType = TNUM) then writeln(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writeln(EntEax.Str);
          end;
          'newln' : begin
            writeln();
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
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpETable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          		  HelpETable[index] := pocz^.Val;
          		  stack_remove(pocz);
          		end;
          		stack_clear(pocz);
          		for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpETable[index]);
          		SetLength(HelpETable, 0);
          	end else if (y = 0) then begin
          	  stack_clear(pocz);
          	end;
          end;
          'copy' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (y >= 1) then begin
              SetLength(HelpETable, trunc(y));
              for index := 0 to trunc(y)-1 do begin
                HelpETable[index] := pocz^.Val;
                stack_remove(pocz);
              end;
              for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpETable[index]);
              for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpETable[index]);
              SetLength(HelpETable, 0);
            end else if (y = 0) then begin
              stack_clear(pocz);
            end;
          end;
          'mcopy' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpETable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          		  HelpETable[index] := pocz^.Val;
          		  stack_remove(pocz);
          		end;
          		for index := trunc(y)-1 downto 0 do stack_add(pocz, HelpETable[index]);
          		for index := 0 to trunc(y)-1 do stack_add(pocz, HelpETable[index]);
          		SetLength(HelpETable, 0);
          	end else if (y = 0) then begin
          	    stack_clear(pocz);
          	end;
          end;
          'sort' : begin
          	size := stack_size(pocz);
            SetLength(HelpNTable, size);
            for index := 0 to size-1 do begin
            	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
              HelpNTable[index] := pocz^.Val.Num;
              stack_remove(pocz);
            end;
            if (sets.sorttype = 0) then bubblesort(HelpNTable);
            if (sets.sorttype = 1) then quicksort(HelpNTable);
            if (sets.sorttype = 2) then mergesort(HelpNTable);
            if (sets.sorttype = 3) then bogosort(HelpNTable);
            for index := 0 to size-1 do stack_add(pocz, buildNumber(HelpNTable[index]));         
            SetLength(HelpNTable, 0);
          end;
          'rand' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            z := random(trunc(y));
            if not (sets.Autoclear) then stack_add(pocz, buildNumber(y));
            stack_add(pocz, buildNumber(z));
          end;
             


          // stack operands
          'sum' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		z := table_sum(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'product' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_product(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(1.0));
          	end;
          end;
          'count' : begin
          	z := 0.0;
          	while (pocz <> nil) do
          	begin
          		z := z + 1;
          		stack_remove(pocz);
          	end;
          	stack_add(pocz, buildNumber(z));
          end;
          'size' : begin
          	z := stack_size(pocz);
          	stack_add(pocz, buildNumber(z));
          end;
          'all' : begin
          	z := stack_size(pocz);
          	stack_add(pocz, buildNumber(z));
          end;
          'avg' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_avg(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'min' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_min(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'max' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_max(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'median' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_median(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'variance' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_variance(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'stddev' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
          	stack_remove(pocz);
          	if (y >= 1) then begin
          		SetLength(HelpNTable, trunc(y));
          		for index := 0 to trunc(y)-1 do begin
          			if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          		  HelpNTable[index] := pocz^.Val.Num;
          		  stack_remove(pocz);
          		end;
          		table_reverse(HelpNTable);
          		z := table_stddev(HelpNTable);
          		if not (sets.Autoclear) then begin 
          			IntEax := trunc(y)-1;
          			repeat
          				stack_add(pocz, buildNumber(HelpNTable[IntEax]));
          				Dec(IntEax);
          			until IntEax = 0;
          		end;
          		SetLength(HelpNTable, 0);
          		stack_add(pocz, buildNumber(z));
          	end else if (y = 0) then begin
          	  stack_add(pocz, buildNumber(0.0));
          	end;
          end;
          'rev' : begin
          	pocz := stack_reverse(pocz);
          end;
             
          // stack creators
          'seq' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	z := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
          	y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (x <= z) then
            begin
            	while (x <= z) do 
            	begin
            		stack_add(pocz, buildNumber(x));
            		x := x + y;
            	end;
            end else begin
            	while (x >= z) do 
            	begin
            		stack_add(pocz, buildNumber(x));
            		x := x - y;
            	end;
            end;
          end;

          'seql' : begin
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            z := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            a := 1.0;
          	while (a <= z) do 
            begin
              stack_add(pocz, buildNumber(x));
              x := x + y;
              a := a + 1.0;
            end;
          end;

          'gseq' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            z := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            if (x <= z) then
            begin
              while (x <= z) do 
              begin
                stack_add(pocz, buildNumber(x));
                x := x * y;
              end;
            end else begin
              while (x >= z) do 
              begin
                stack_add(pocz, buildNumber(x));
                x := x / y;
              end;
            end;
          end;

          'gseql' : begin
          	if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            z := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            y := pocz^.Val.Num;
            stack_remove(pocz);
            if (sets.StrictType) then assertEntityLocated(pocz^.Val, TNUM, i);
            x := pocz^.Val.Num;
            stack_remove(pocz);
            a := 1.0;
          	while (a <= z) do 
            begin
              stack_add(pocz, buildNumber(x));
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
                  else begin
                    stack_add(pocz, buildString(StrEcx));
                  end;
                end;
              end;
            end;
          end;
        end;
      end else begin
        stack_add(pocz, buildNumber(Im));
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
        L      : TStrings;
        i      : String;
        index  : LongInt;
        z      : String;
        step   : Integer;
        cursor : LongInt;
        nestlv : ShortInt;
        nesttx : String;
begin
  L := TStringlist.Create;
  L.Delimiter := ' ';
  L.QuoteChar := '"';
  L.StrictDelimiter := false;
  L.DelimitedText := input;

  Steps := 1;

  index := 0;
  while index < L.Count do
  begin
    if L[index] = '{' then begin
      nestlv := 1;
      nesttx := '';
      cursor := index + 1;
      while (nestlv > 0) and (cursor < L.Count) do begin
        if (L[cursor] = '{') then Inc(nestlv);
        if (L[cursor] = '}') then Dec(nestlv);
        if (nestlv > 0) then nesttx := nesttx + ' ' + L[cursor];
        Inc(cursor);
      end;
      if Steps = -1 then begin
        repeat
          parseRPN(nesttx, pocz, sets); 
        until EOF;
        stack_remove(pocz);
      end else for step := 1 to Steps do parseRPN(nesttx, pocz, sets); 
      index := cursor - 1;
    end else begin
      if Steps = -1 then begin
        repeat
          evaluate(L[index], pocz, Steps, sets);
        until EOF;
        stack_remove(pocz);
      end else for step := 1 to Steps do evaluate(L[index], pocz, Steps, sets); 
    end;


    Inc(index);
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
  //while stack <> nil do begin
  //  res := FormatFloat(sets.Mask, stack^.Val.Num) + ' ' + res;
  //  stack_remove(stack);
  //end;
  res := stack_show(stack, sets.Mask);
  calc_parseRPN := res;
end;


end.
