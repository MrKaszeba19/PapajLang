unit UnitStack;
// Unit with Stacks

{$mode objfpc}{$H+}

interface

uses UnitEntity, Classes, SysUtils;

type StackDB = record
  Values : TEntities;
end;

function stack_null() : StackDB;
procedure stack_push(var pocz:StackDB; node : Entity);
function stack_pop(var pocz:StackDB) : Entity;
function stack_firstpop(var poc : StackDB) : Entity;
procedure stack_justpop(var pocz:StackDB);
procedure stack_clear(var pocz:StackDB);
function stack_get(pocz : StackDB) : Entity;
function stack_getback(pocz : StackDB; index : LongInt) : Entity;
function stack_size(poc : StackDB) : Longint;
function stack_show(poc : StackDB; mask : String) : String;
function stack_showBeautiful(poc : StackDB; mask : String) : String;
function stack_showFull(poc : StackDB) : String;
function stack_reverse(poc : StackDB) : StackDB;

function stack_searchException(poc : StackDB) : Boolean;

function stack_popback(var poc : StackDB; index : LongInt) : Entity; 
function stack_getCollection(poc : StackDB; index : LongInt) : TEntities;
function stack_popCollection(var poc : StackDB; index : LongInt) : TEntities;
procedure stack_justpopCollection(var poc : StackDB; index : LongInt);
procedure stack_pushCollection(var poc : StackDB; nodes : TEntities);
procedure stack_reverseCollection(var poc : StackDB; index : LongInt);

function assertEntity(var stack : StackDB; val : Entity; const wtype : Integer) : Boolean;
function assertEntityLocated(var stack : StackDB; val : Entity; const wtype : Integer; operand : String) : Boolean;
function assertNotNegativeLocated(var stack : StackDB; val : Entity; operand : String) : Boolean;
function assertIntegerLocated(var stack : StackDB; val : Entity; operand : String) : Boolean;
function assertNaturalLocated(var stack : StackDB; val : Entity; operand : String) : Boolean;

implementation

uses StrUtils;

// helpful ones

function find_maxStrlen(tab : TEntities) : LongInt;
var
    i : Integer;
    s : LongInt;
begin
    s := 0;
    for i := 0 to Length(tab)-1 do
    if (Length(tab[i].Str) > s) then begin
        s := Length(tab[i].Str);
    end; 
    find_maxStrlen := s;
end;

// STACK OPERATIONS

function stack_null() : StackDB;
var
    pom : StackDB;
begin
    SetLength(pom.Values, 0);
    stack_null := pom;
end;

procedure stack_push(var pocz:StackDB; node : Entity);
var
    len : LongInt;
begin
    len := Length(pocz.Values);
    SetLength(pocz.Values, len+1);
    pocz.Values[len] := node;
end;

function stack_pop(var pocz:StackDB) : Entity;
var
    len : LongInt;
    pom : Entity;
begin
    len := Length(pocz.Values);
    pom := pocz.Values[len-1];
    SetLength(pocz.Values, len-1);
    stack_pop := pom;
end;

procedure stack_justpop(var pocz:StackDB);
var
    len : LongInt;
    pom : Entity;
begin
    len := Length(pocz.Values);
    SetLength(pocz.Values, len-1);
end;

procedure stack_clear(var pocz:StackDB);
begin
    while Length(pocz.Values) > 0 do stack_justpop(pocz);
end;

function stack_get(pocz : StackDB) : Entity;
begin
    stack_get := pocz.Values[Length(pocz.Values)-1];
end;

function stack_getback(pocz : StackDB; index : LongInt) : Entity;
begin
    stack_getback := pocz.Values[Length(pocz.Values)-index];
end;

function stack_size(poc : StackDB) : Longint;
begin
    stack_size := Length(poc.Values);
end;

function stack_show(poc : StackDB; mask : String) : String;
var
  z : String;
  i : Entity;
begin
    z := '';
    for i in poc.Values do
    begin
        if (i.EntityType = TNUM) then z := z + FormatFloat(mask, i.Num) + ' ';
        if (i.EntityType = TSTR) then z := z + '"' + i.Str + '" ';
        if (i.EntityType = TNIL) then z := z + i.Str + ' ';
        if (i.EntityType = TBOO) then z := z + i.Str + ' ';
        if (i.EntityType = TOBJ) then z := z + '<object> ';
        if (i.EntityType = TFUN) then z := z + '<function> '; 
        if (i.EntityType = TEXC) then z := z + '<exception> '; 
    end;
    z := LeftStr(z, Length(z)-1);
    stack_show := z;
end;

function stack_showBeautiful(poc : StackDB; mask : String) : String;
var
  z   : String;
  i   : Entity;
  col : LongInt;
begin
    z := '';
    col := find_maxStrlen(poc.Values)+1;
    for i in poc.Values do
    begin
        if (i.EntityType = TNUM) then z := z + PadLeft(FormatFloat(mask, i.Num), col) + ' ';
        if (i.EntityType = TSTR) then z := z + '"' + PadLeft(i.Str, col) + '" ';
        if (i.EntityType = TNIL) then z := z + PadLeft(i.Str, col) + ' ';
        if (i.EntityType = TBOO) then z := z + PadLeft(i.Str, col) + ' ';
    end;
    z := LeftStr(z, Length(z)-1);
    stack_showBeautiful := z;
end;

function identTabs(x : Integer) : String;
var
    i : Integer;
    s : String;
begin
    s := '';
    for i := 1 to x do s := s + #9; 
    identTabs := s;
end;

function stack_showFull(poc : StackDB) : String;
var
  z : String;
  i : LongInt;
begin
    z := 'stack{ ' + #13#10;
    for i := 0 to Length(poc.Values)-2 do
    begin
        z := z + identTabs(1) + getEntitySpec(poc.Values[i]) + ', ' + #13#10;
    end;
    z := z + identTabs(1) + getEntitySpec(poc.Values[Length(poc.Values)-1]) + #13#10 + '} ';
    z := LeftStr(z, Length(z)-1);
    stack_showFull := z;
end;

function stack_reverse(poc : StackDB) : StackDB;
var
  pom : StackDB;
  i   : LongInt;
begin
    pom := stack_null();
    for i := Length(poc.Values)-1 downto 0 do
    begin
        stack_push(pom, poc.Values[i]);
    end;
    stack_reverse := pom;   
end;

function stack_searchException(poc : StackDB) : Boolean;
begin
    if (Length(poc.Values) > 0) and (poc.Values[Length(poc.Values)-1].EntityType = TEXC) then 
    begin 
        stack_searchException := True;
    end else begin
        stack_searchException := False;
    end;
end;



function stack_popback(var poc : StackDB; index : LongInt) : Entity; 
var
	pom : Entity;
	i   : LongInt;
begin
	pom := poc.Values[Length(poc.Values)-index];
	for i := Length(poc.Values)-index to Length(poc.Values)-2 do poc.Values[i] := poc.Values[i+1];
	SetLength(poc.Values, Length(poc.Values)-1);
	stack_popback := pom;
end;

function stack_getCollection(poc : StackDB; index : LongInt) : TEntities;
var
	pom : TEntities;
	i   : LongInt;
begin
	SetLength(pom, index);
	for i := 0 to index-1 do pom[i] := poc.Values[Length(poc.Values)-index+i];
	stack_getCollection := pom;
end;

function stack_popCollection(var poc : StackDB; index : LongInt) : TEntities;
var
	pom : TEntities;
	i   : LongInt;
begin
	SetLength(pom, index);
	for i := 0 to index-1 do pom[i] := poc.Values[Length(poc.Values)-index+i];
	SetLength(poc.Values, Length(poc.Values)-index);
	stack_popCollection := pom;
end;

procedure stack_justpopCollection(var poc : StackDB; index : LongInt);
begin
	SetLength(poc.Values, Length(poc.Values)-index);
end;

procedure stack_pushCollection(var poc : StackDB; nodes : TEntities);
var
    len : LongInt;
    i   : LongInt;
begin
	len := Length(poc.Values);
    SetLength(poc.Values, len+Length(nodes));
    for i := 0 to Length(nodes)-1 do poc.Values[len+i] := nodes[i];
end;

procedure stack_reverseCollection(var poc : StackDB; index : LongInt);
var
	i : LongInt;
begin
	for i := 0 to (index div 2)-1 do swapEntities(poc.Values[Length(poc.Values)-index+i], poc.Values[Length(poc.Values)-i-1]);
end;

function stack_firstpop(var poc:StackDB) : Entity;
var
    pom : Entity;
    i   : LongInt;
begin
    pom := poc.Values[0];
    for i := 0 to Length(poc.Values)-2 do poc.Values[i] := poc.Values[i+1];
    SetLength(poc.Values, Length(poc.Values)-1);
    stack_firstpop := pom;
end;

// ====== assertions

function assertEntity(var stack : StackDB; val : Entity; const wtype : Integer) : Boolean;
begin
    if (val.EntityType <> wtype) then
    begin 
        stack_push(stack, buildException('Type mismatch: <'+getEntityTypeName(wtype)+'> expected, got <'+getEntitySpec(val)+'>'));
        assertEntity := true;
    end else assertEntity := false;
end;

function assertEntityLocated(var stack : StackDB; val : Entity; const wtype : Integer; operand : String) : Boolean;
begin
    if (val.EntityType <> wtype) then 
    begin
        stack_push(stack, buildException('Type mismatch at "'+operand+'": <'+getEntityTypeName(wtype)+'> expected, got '+getEntitySpec(val)+'.'));
        assertEntityLocated := true;
    end else assertEntityLocated := false;
end;

function assertNotNegativeLocated(var stack : StackDB; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, buildException('Type mismatch at "'+operand+'": <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+'.'));
        assertNotNegativeLocated := true;    
    end else if (val.Num < 0) then
    begin 
        stack_push(stack, buildException('Exception when taking a numeric value at "'+operand+'": an positive real number or zero expected'));
        assertNotNegativeLocated := true;
    end else assertNotNegativeLocated := false;
end;

function assertIntegerLocated(var stack : StackDB; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, buildException('Type mismatch at "'+operand+'": <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+'.'));
        assertIntegerLocated := true;
    end else if (val.Num <> trunc(val.Num)) then 
    begin
        stack_push(stack, buildException('Exception when taking a numeric value at "'+operand+'": integer expected, got a real number'));
        assertIntegerLocated := true;
    end else assertIntegerLocated := false;
end;

function assertNaturalLocated(var stack : StackDB; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, buildException('Type mismatch at "'+operand+'": <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+'.'));  
        assertNaturalLocated := true;
    end else if (val.Num < 0) or (val.Num <> trunc(val.Num)) then 
    begin
        stack_push(stack, buildException('Exception when taking a numeric value at "'+operand+'": an positive integer or zero expected'));
        assertNaturalLocated := true;
    end else assertNaturalLocated := false;
end;

end.
