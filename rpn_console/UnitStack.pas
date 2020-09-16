unit UnitStack;
// Unit with Stacks

{$mode objfpc}{$H+}

interface

uses UnitEntity, Classes, SysUtils;

type TStack = record
  Values : TEntities;
end;

type StackDB = array of TStack;

function stack_null() : TStack;
procedure stack_push(var pocz:TStack; node : Entity);
function stack_pop(var pocz:TStack) : Entity;
function stack_firstpop(var poc : TStack) : Entity;
procedure stack_justpop(var pocz:TStack);
procedure stack_clear(var pocz:TStack);
function stack_get(pocz : TStack) : Entity;
function stack_getback(pocz : TStack; index : LongInt) : Entity;
function stack_size(poc : TStack) : Longint;
function stack_show(poc : TStack; mask : String) : String;
function stack_showBeautiful(poc : TStack; mask : String) : String;
function stack_showFull(poc : TStack) : String;
function stack_showArray(poc : TStack; mask : String) : String;
function stack_showArrayPS(poc : TStack; db : StackDB; mask : String) : String;
function stack_showArrayFull(poc : TStack; db : StackDB; mask : String) : String;
function stack_reverse(poc : TStack) : TStack;

function stack_searchException(poc : TStack) : Boolean;

function stack_popback(var poc : TStack; index : LongInt) : Entity;
function stack_getCollection(poc : TStack; index : LongInt) : TEntities;
function stack_popCollection(var poc : TStack; index : LongInt) : TEntities;
procedure stack_justpopCollection(var poc : TStack; index : LongInt);
procedure stack_pushCollection(var poc : TStack; nodes : TEntities);
procedure stack_reverseCollection(var poc : TStack; index : LongInt);

procedure stack_pushFront(var pocz : TStack; node : Entity; index : LongInt);
function stack_popFront(var pocz : TStack; index : LongInt) : Entity;
procedure stack_replaceFront(var pocz : TStack; node : Entity; index : LongInt);
function stack_getFront(pocz : TStack; index : LongInt) : Entity;

function assertEntity(var stack : TStack; val : Entity; const wtype : Integer) : Boolean;
function assertEntityLocated(var stack : TStack; val : Entity; const wtype : Integer; operand : String) : Boolean;
function assertEitherLocated(var stack : TStack; val : Entity; const wtype1 : Integer; const wtype2 : Integer; operand : String) : Boolean;
function assertNotNegativeLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertIntegerLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertNonZeroLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertPositiveNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertCharLocated(var stack : TStack; val : Entity; operand : String) : Boolean;

function raiseExceptionUnknownCommand(var stack : TStack; operand : String) : Entity;
function raiseExceptionUnknownArray(var stack : TStack; operand : String) : Entity;
function raiseSyntaxErrorExpression(operand : String) : Entity;

function buildNewArray(var db : StackDB; sets : TSettings; count : LongInt) : Entity;
function buildNewArray(var db : StackDB; sets : TSettings; pom : TEntities) : Entity;

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

function stack_null() : TStack;
var
    pom : TStack;
begin
    SetLength(pom.Values, 0);
    stack_null := pom;
end;

procedure stack_push(var pocz:TStack; node : Entity);
var
    len : LongInt;
begin
    len := Length(pocz.Values);
    SetLength(pocz.Values, len+1);
    pocz.Values[len] := node;
end;

function stack_pop(var pocz:TStack) : Entity;
var
    len : LongInt;
    pom : Entity;
begin
    len := Length(pocz.Values);
    pom := pocz.Values[len-1];
    SetLength(pocz.Values, len-1);
    stack_pop := pom;
end;

procedure stack_justpop(var pocz:TStack);
var
    len : LongInt;
begin
    len := Length(pocz.Values);
    SetLength(pocz.Values, len-1);
end;

procedure stack_clear(var pocz:TStack);
begin
    while Length(pocz.Values) > 0 do stack_justpop(pocz);
end;

function stack_get(pocz : TStack) : Entity;
begin
    stack_get := pocz.Values[Length(pocz.Values)-1];
end;

function stack_getback(pocz : TStack; index : LongInt) : Entity;
begin
    stack_getback := pocz.Values[Length(pocz.Values)-index];
end;

function stack_size(poc : TStack) : Longint;
begin
    stack_size := Length(poc.Values);
end;

function stack_show(poc : TStack; mask : String) : String;
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
        if (i.EntityType = TVEC) then z := z + '<Array> ';
        if (i.EntityType = TOBJ) then z := z + '<Object> ';
        if (i.EntityType = TFUN) then z := z + '<Function> '; 
        if (i.EntityType = TEXC) then z := z + '<Exception> '; 
    end;
    z := LeftStr(z, Length(z)-1);
    stack_show := z;
end;

function stack_showBeautiful(poc : TStack; mask : String) : String;
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

function printEntityValueFull(x : Entity; db : StackDB; mask : String) : String;
var
  z : String;
begin
    z := '';
    if (x.EntityType = TNUM) then z := FormatFloat(mask, x.Num);
    if (x.EntityType = TSTR) then z := '"' + x.Str + '"';
    if (x.EntityType = TNIL) then z := x.Str;
    if (x.EntityType = TBOO) then z := x.Str;
    if (x.EntityType = TVEC) then z := stack_showArrayPS(db[trunc(x.Num)], db, mask);
    if (x.EntityType = TOBJ) then z := '<Object>';
    if (x.EntityType = TFUN) then z := '<Function>'; 
    if (x.EntityType = TEXC) then z := '<Exception>'; 
    printEntityValueFull := z;
end;

function stack_showArray(poc : TStack; mask : String) : String;
var
  z : String;
  i : LongInt;
begin
    z := '[';
    if (Length(poc.Values) > 0) then begin
        for i := 0 to Length(poc.Values)-2 do
        begin
            z := z + printEntityValue(poc.Values[i], mask) + ', ';
        end;
        z := z + printEntityValue(poc.Values[Length(poc.Values)-1], mask) + '] ';
        z := LeftStr(z, Length(z)-1);
        stack_showArray := z;
    end else begin
        stack_showArray := '[]';
    end;
end;

function stack_showArrayPS(poc : TStack; db : StackDB; mask : String) : String;
var
  z : String;
  i : LongInt;
begin
    z := '[ ';
    if (Length(poc.Values) > 0) then begin
        for i := 0 to Length(poc.Values)-1 do
        begin
            z := z + printEntityValueFull(poc.Values[i], db, mask) + ' ';
        end;
        z := z + '] ';
        z := LeftStr(z, Length(z)-1);
        stack_showArrayPS := z;
    end else begin
        stack_showArrayPS := '[]';
    end;
end;


function stack_showArrayFull(poc : TStack; db : StackDB; mask : String) : String;
var
  z : String;
  i : LongInt;
begin
    z := '[';
    if (Length(poc.Values) > 0) then begin
        for i := 0 to Length(poc.Values)-2 do
        begin
            z := z + printEntityValueFull(poc.Values[i], db, mask) + ', ';
        end;
        z := z + printEntityValueFull(poc.Values[Length(poc.Values)-1], db, mask) + '] ';
        z := LeftStr(z, Length(z)-1);
        stack_showArrayFull := z;
    end else begin
        stack_showArrayFull := '[]';
    end;
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

function stack_showFull(poc : TStack) : String;
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

function stack_reverse(poc : TStack) : TStack;
var
  pom : TStack;
  i   : LongInt;
begin
    pom := stack_null();
    for i := Length(poc.Values)-1 downto 0 do
    begin
        stack_push(pom, poc.Values[i]);
    end;
    stack_reverse := pom;   
end;

function stack_searchException(poc : TStack) : Boolean;
begin
    if (Length(poc.Values) > 0) and (poc.Values[Length(poc.Values)-1].EntityType = TEXC) and (poc.Values[Length(poc.Values)-1].Num = 1) then
    begin 
        stack_searchException := True;
    end else begin
        stack_searchException := False;
    end;
end;



function stack_popback(var poc : TStack; index : LongInt) : Entity; 
var
	pom : Entity;
	i   : LongInt;
begin
	pom := poc.Values[Length(poc.Values)-index];
	for i := Length(poc.Values)-index to Length(poc.Values)-2 do poc.Values[i] := poc.Values[i+1];
	SetLength(poc.Values, Length(poc.Values)-1);
	stack_popback := pom;
end;

procedure stack_pushFront(var pocz : TStack; node : Entity; index : LongInt);
var
    len, i : LongInt;
begin
    len := Length(pocz.Values);
    SetLength(pocz.Values, len+1);
    for i := len-1 downto index do pocz.Values[i+1] := pocz.Values[i];
    pocz.Values[index] := node;
end;

function stack_popFront(var pocz : TStack; index : LongInt) : Entity;
var
	pom : Entity;
	i   : LongInt;
begin
	pom := pocz.Values[index];
	for i := index to Length(pocz.Values)-2 do pocz.Values[i] := pocz.Values[i+1];
	SetLength(pocz.Values, Length(pocz.Values)-1);
	stack_popFront := pom;
end;

procedure stack_replaceFront(var pocz : TStack; node : Entity; index : LongInt);
begin
    pocz.Values[index] := node;
end;

function stack_getFront(pocz : TStack; index : LongInt) : Entity;
begin
    stack_getFront := pocz.Values[index];
end;

function stack_getCollection(poc : TStack; index : LongInt) : TEntities;
var
	pom : TEntities;
	i   : LongInt;
begin
	SetLength(pom, index);
	for i := 0 to index-1 do pom[i] := poc.Values[Length(poc.Values)-index+i];
	stack_getCollection := pom;
end;

function stack_popCollection(var poc : TStack; index : LongInt) : TEntities;
var
	pom : TEntities;
	i   : LongInt;
begin
	SetLength(pom, index);
	for i := 0 to index-1 do pom[i] := poc.Values[Length(poc.Values)-index+i];
	SetLength(poc.Values, Length(poc.Values)-index);
	stack_popCollection := pom;
end;

procedure stack_justpopCollection(var poc : TStack; index : LongInt);
begin
	SetLength(poc.Values, Length(poc.Values)-index);
end;

procedure stack_pushCollection(var poc : TStack; nodes : TEntities);
var
    len : LongInt;
    i   : LongInt;
begin
	len := Length(poc.Values);
    SetLength(poc.Values, len+Length(nodes));
    for i := 0 to Length(nodes)-1 do poc.Values[len+i] := nodes[i];
end;

procedure stack_reverseCollection(var poc : TStack; index : LongInt);
var
	i : LongInt;
begin
	for i := 0 to (index div 2)-1 do swapEntities(poc.Values[Length(poc.Values)-index+i], poc.Values[Length(poc.Values)-i-1]);
end;

function stack_firstpop(var poc:TStack) : Entity;
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

function assertEntity(var stack : TStack; val : Entity; const wtype : Integer) : Boolean;
begin
    if (val.EntityType <> wtype) then
    begin 
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(wtype)+': <'+getEntityTypeName(wtype)+'> expected, got <'+getEntitySpec(val)+'>'));
        assertEntity := true;
    end else assertEntity := false;
end;

function assertEntityLocated(var stack : TStack; val : Entity; const wtype : Integer; operand : String) : Boolean;
begin
    if (val.EntityType <> wtype) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(wtype)+': <'+getEntityTypeName(wtype)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        assertEntityLocated := true;
    end else assertEntityLocated := false;
end;

function assertEitherLocated(var stack : TStack; val : Entity; const wtype1 : Integer; const wtype2 : Integer; operand : String) : Boolean;
begin
    if (val.EntityType <> wtype1) and (val.EntityType <> wtype2) then 
    begin
        stack_push(stack, raiseException('EType:CEntityType: <'+getEntityTypeName(wtype1)+'> or <'+getEntityTypeName(wtype2)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        assertEitherLocated := true;
    end else assertEitherLocated := false;
end;

function assertNotNegativeLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        assertNotNegativeLocated := true;    
    end else if (val.Num < 0) then
    begin 
        stack_push(stack, raiseException('EConstraint:CNonNegative: a positive real number or zero expected at "'+operand+'".'));
        assertNotNegativeLocated := true;
    end else assertNotNegativeLocated := false;
end;

function assertIntegerLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        assertIntegerLocated := true;
    end else if (val.Num <> trunc(val.Num)) then 
    begin
        stack_push(stack, raiseException('EConstraint:CInteger: an integer expected, got a real number at "'+operand+'".'));
        assertIntegerLocated := true;
    end else assertIntegerLocated := false;
end;

function assertNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));  
        assertNaturalLocated := true;
    end else if (val.Num < 0) or (val.Num <> trunc(val.Num)) then 
    begin
        stack_push(stack, raiseException('EConstraint:CNonNegativeInteger: a positive integer or zero expected at "'+operand+'".'));
        assertNaturalLocated := true;
    end else assertNaturalLocated := false;
end;

function assertPositiveNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));  
        assertPositiveNaturalLocated := true;
    end else if (val.Num <= 0) or (val.Num <> trunc(val.Num)) then 
    begin
        stack_push(stack, raiseException('EConstraint:CPositiveInteger: a positive integer expected at "'+operand+'".'));
        assertPositiveNaturalLocated := true;
    end else assertPositiveNaturalLocated := false;
end;

function assertNonZeroLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));  
        assertNonZeroLocated := true;
    end else if (val.Num = 0) then 
    begin
        stack_push(stack, raiseException('EConstraint:CNonZero: a non-zero number expected at "'+operand+'".'));
        assertNonZeroLocated := true;
    end else assertNonZeroLocated := false;
end;

function assertCharLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TSTR) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TSTR)+': <'+getEntityTypeName(TSTR)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));  
        assertCharLocated := true;
    end else if (Length(val.Str) > 1) then 
    begin
        stack_push(stack, raiseException('EConstraint:CChar: a single char expected at "'+operand+'".'));
        assertCharLocated := true;
    end else assertCharLocated := false;
end;

function raiseExceptionUnknownCommand(var stack : TStack; operand : String) : Entity;
begin
    raiseExceptionUnknownCommand := raiseException('EInput:CUnknown: Unknown expression at "'+operand+'".');
end;

function raiseExceptionUnknownArray(var stack : TStack; operand : String) : Entity;
begin
    Result := raiseException('EInput:CNonArray: Array expression expected at "'+operand+'".');
end;

function raiseSyntaxErrorExpression(operand : String) : Entity;
begin
    raiseSyntaxErrorExpression := raiseException('ESyntax:CExpression: Syntax Error at expression "('+operand+' )".');
end;

// exceed boundaries
// not null
// isEmpty

// ============= Arrays

function buildNewArray(var db : StackDB; sets : TSettings; count : LongInt) : Entity;
var
    pom     : TEntities;
    memsize : LongInt;
    ent     : Entity;
begin
    pom := stack_popCollection(db[sets.StackPointer], count);
    memsize := Length(db);
    SetLength(db, memsize+1);
    stack_pushCollection(db[memsize], pom);

	ent.EntityType := TVEC;
	ent.Str := IntToStr(count);
	ent.Num := memsize;
	buildNewArray := ent;
end;

function buildNewArray(var db : StackDB; sets : TSettings; pom : TEntities) : Entity;
var
    memsize : LongInt;
    ent     : Entity;
begin
    memsize := Length(db);
    SetLength(db, memsize+1);
    stack_pushCollection(db[memsize], pom);

	ent.EntityType := TVEC;
	ent.Str := IntToStr(Length(pom));
	ent.Num := memsize;
	buildNewArray := ent;
end;


//function arrayTransfer(var db : StackDB; sets : TSettings; i, j : LongInt) : Entity;
//var
//    memsize : LongInt;
//    index   : LongInt;
//begin
//    memsize := stack_size(db[i]);
//    SetLength(db[j], i);
//    for index := 0 to i-1 do
//    begin
//        db[j] := db[i];
//    end;
//end;

end.
