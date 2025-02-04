unit UnitStack;
// Unit with Stacks

{$mode objfpc}{$H+}

interface

uses UnitEntity, ComplexNumbers, 
     Classes, SysUtils;

type TStack = record
    RefCount : LongInt;
    VarAddr  : LongInt;
    Values   : TEntities;
end;

type StackDB = array of TStack;

//type EntMemory = object
//    private
//        Layers : array of VariableLayer;
//    public
//        constructor Create;
//        destructor Destroy;      
//end;

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
function stack_showPolyPS(poc : TStack; db : StackDB; mask : String) : String;
function stack_reverse(poc : TStack) : TStack;

function stack_searchException(poc : TStack) : Boolean;

function stack_popback(var poc : TStack; index : LongInt) : Entity;
function stack_getCollection(poc : TStack; index : LongInt) : TEntities;
function stack_popCollection(var poc : TStack; index : LongInt) : TEntities;
procedure stack_justpopCollection(var poc : TStack; index : LongInt);
procedure stack_pushCollection(var poc : TStack; nodes : TEntities);
procedure stack_reverseCollection(var poc : TStack; index : LongInt);

procedure stack_pushFront(var pocz : TStack; node : Entity; index : LongInt = 0);
function stack_popFront(var pocz : TStack; index : LongInt = 0) : Entity;
procedure stack_justpopFront(var pocz : TStack; index : LongInt);
procedure stack_replaceFront(var pocz : TStack; node : Entity; index : LongInt);
function stack_getFront(pocz : TStack; index : LongInt = 0) : Entity;

function assertEntity(var stack : TStack; val : Entity; const wtype : TEntityType) : Boolean;
function assertEntityLocated(var stack : TStack; val : Entity; const wtype : TEntityType; operand : String) : Boolean;
function assertEitherLocated(var stack : TStack; val : Entity; const wtype1 : TEntityType; const wtype2 : TEntityType; operand : String) : Boolean;
function assertNotNegativeLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertNotNegativeRealLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertNotRealNegativeLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertIntegerLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertRealLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertComplexLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertNonZeroLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertPositiveNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
function assertCharLocated(var stack : TStack; val : Entity; operand : String) : Boolean;

function buildNewArray(var db : StackDB; sets : TSettings; count : LongInt) : Entity;
function buildNewArray(var db : StackDB; sets : TSettings; pom : TEntities) : Entity;
function buildNewEmptyArray(var db : StackDB; sets : TSettings; count : LongInt = 0) : Entity;

implementation

uses StrUtils, MathUtils;

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
        case i.EntityType of
            TNUM : z := z + toStringFormat(i.Num, mask) + ' ';
            TSTR : z := z + '"' + i.Str + '" ';
            TNIL : z := z + i.Str + ' ';
            TBOO : z := z + i.Str + ' ';
            TDAT : z := z + 'Date{' + i.Str + '} ';
            TVEC : z := z + '<Array> ';
            TOBJ : z := z + '<Object> ';
            TFUN : z := z + '<Function> '; 
            TEXC : z := z + '<Exception> '; 
            TEXP : z := z + '<LogicalExpression> '; 
            TPLY : z := z + '<Polynomial> '; 
            else z := z + '<Unknown> ';
        end; 
        //if (i.EntityType = TNUM) then z := z + toStringFormat(i.Num, mask) + ' ';
        //if (i.EntityType = TSTR) then z := z + '"' + i.Str + '" ';
        //if (i.EntityType = TNIL) then z := z + i.Str + ' ';
        //if (i.EntityType = TBOO) then z := z + i.Str + ' ';
        //if (i.EntityType = TDAT) then z := z + '"' + i.Str + '" ';
        //if (i.EntityType = TVEC) then z := z + '<Array> ';
        //if (i.EntityType = TOBJ) then z := z + '<Object> ';
        //if (i.EntityType = TFUN) then z := z + '<Function> '; 
        //if (i.EntityType = TEXC) then z := z + '<Exception> '; 
        //if (i.EntityType = TEXP) then z := z + '<LogicalExpression> '; 
    end;
    z := LeftStr(z, Length(z)-1);
    Result := z;
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
        if (i.EntityType = TNUM) then z := z + PadLeft(toStringFormat(i.Num, mask), col) + ' ';
        if (i.EntityType = TSTR) then z := z + '"' + PadLeft(i.Str, col) + '" ';
        if (i.EntityType = TNIL) then z := z + PadLeft(i.Str, col) + ' ';
        if (i.EntityType = TBOO) then z := z + PadLeft(i.Str, col) + ' ';
        if (i.EntityType = TDAT) then z := z + 'Date{' + PadLeft(i.Str, col) + '} ';
    end;
    z := LeftStr(z, Length(z)-1);
    stack_showBeautiful := z;
end;

function printEntityValueFull(x : Entity; db : StackDB; mask : String) : String;
var
  z : String;
begin
    z := '';
    case x.EntityType of 
        TNUM : z := toStringFormat(x.Num, mask);
        TSTR : z := '"' + x.Str + '"';
        TDAT : z := 'Date{' + x.Str + '}';
        TNIL : z := x.Str;
        TBOO : z := x.Str;
        TVEC : z := stack_showArrayPS(db[trunc(x.Num.Re)], db, mask);
        TOBJ : z := '<Object>';
        TFUN : z := '<Function>'; 
        TEXC : z := '<Exception>'; 
        TEXP : z := '<LogicalExpression>'; 
        TPLY : z := stack_showArrayPS(db[trunc(x.Num.Re)], db, mask); // todo: change to polynomial output
        else z := '<Unknown>';
    end;
    Result := z;
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
        Result := z;
    end else begin
        Result := '[]';
    end;
end;

{*
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
        Result := z;
    end else begin
        Result := '[]';
    end;
end;

function stack_showPolyPS(poc : TStack; db : StackDB; mask : String) : String;
var
  z : String;
  i : LongInt;
begin
    z := 'poly{ ';
    if (Length(poc.Values) > 0) then begin
        for i := 0 to Length(poc.Values)-1 do
        begin
            z := z + printEntityValueFull(poc.Values[i], db, mask) + ' ';
        end;
        z := z + '} ';
        z := LeftStr(z, Length(z)-1);
        Result := z;
    end else begin
        Result := 'poly{}';
    end;
end;
*}

function stack_showArrayPS(poc : TStack; db : StackDB; mask : String) : String;
var
  z : String;
  i : LongInt;
begin
    z := '[';
    if (Length(poc.Values) > 0) then begin
        for i := 0 to Length(poc.Values)-2 do
        begin
            z := z + printEntityValueFull(poc.Values[i], db, mask) + ' ';
        end;
        z := z + printEntityValueFull(poc.Values[Length(poc.Values)-1], db, mask) + '] ';
        z := LeftStr(z, Length(z)-1);
        Result := z;
    end else begin
        Result := '[]';
    end;
end;

function stack_showPolyPS(poc : TStack; db : StackDB; mask : String) : String;
var
  z : String;
  i : LongInt;
begin
    z := 'Poly{';
    if (Length(poc.Values) > 0) then begin
        for i := 0 to Length(poc.Values)-2 do
        begin
            z := z + printEntityValueFull(poc.Values[i], db, mask) + ' ';
        end;
        z := z + printEntityValueFull(poc.Values[Length(poc.Values)-1], db, mask) + '} ';
        z := LeftStr(z, Length(z)-1);
        Result := z;
    end else begin
        Result := 'Poly{}';
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
    if (Length(poc.Values) = 0) then
    begin
        z := 'stack{}';
    end else begin
        z := 'stack{ ' + #13#10;
        for i := 0 to Length(poc.Values)-2 do
        begin
            z := z + identTabs(1) + getEntitySpec(poc.Values[i]) + ', ' + #13#10;
        end;
        z := z + identTabs(1) + getEntitySpec(poc.Values[Length(poc.Values)-1]) + #13#10 + '} ';
        z := LeftStr(z, Length(z)-1);
    end;
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

procedure stack_pushFront(var pocz : TStack; node : Entity; index : LongInt = 0);
var
    len, i : LongInt;
begin
    len := Length(pocz.Values);
    SetLength(pocz.Values, len+1);
    for i := len-1 downto index do pocz.Values[i+1] := pocz.Values[i];
    pocz.Values[index] := node;
end;

function stack_popFront(var pocz : TStack; index : LongInt = 0) : Entity;
var
	pom : Entity;
	i   : LongInt;
begin
	pom := pocz.Values[index];
	for i := index to Length(pocz.Values)-2 do pocz.Values[i] := pocz.Values[i+1];
	SetLength(pocz.Values, Length(pocz.Values)-1);
	stack_popFront := pom;
end;

procedure stack_justpopFront(var pocz : TStack; index : LongInt);
var
	i   : LongInt;
begin
	for i := index to Length(pocz.Values)-2 do pocz.Values[i] := pocz.Values[i+1];
	SetLength(pocz.Values, Length(pocz.Values)-1);
end;

procedure stack_replaceFront(var pocz : TStack; node : Entity; index : LongInt);
begin
    pocz.Values[index] := node;
end;

function stack_getFront(pocz : TStack; index : LongInt = 0) : Entity;
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

function assertEntity(var stack : TStack; val : Entity; const wtype : TEntityType) : Boolean;
begin
    if (val.EntityType <> wtype) then
    begin 
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(wtype)+': <'+getEntityTypeName(wtype)+'> expected, got <'+getEntitySpec(val)+'>'));
        assertEntity := true;
    end else assertEntity := false;
end;

function assertEntityLocated(var stack : TStack; val : Entity; const wtype : TEntityType; operand : String) : Boolean;
begin
    if (val.EntityType <> wtype) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(wtype)+': <'+getEntityTypeName(wtype)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        assertEntityLocated := true;
    end else assertEntityLocated := false;
end;

function assertEitherLocated(var stack : TStack; val : Entity; const wtype1 : TEntityType; const wtype2 : TEntityType; operand : String) : Boolean;
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
    end else if not (val.Num.Re >= 0) then
    begin 
        stack_push(stack, raiseException('EConstraint:CNonNegative: a positive real number or zero expected at "'+operand+'".'));
        assertNotNegativeLocated := true;
    end else assertNotNegativeLocated := false;
end;

function assertNotNegativeRealLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        Result := true;    
    end else if not ((isReal(val.Num)) and (val.Num.Re >= 0)) then
    begin 
        stack_push(stack, raiseException('EConstraint:CNonNegative: a positive real number or zero expected at "'+operand+'".'));
        Result := true;
    end else Result := false;
end;

// check for everything except for negative reals
function assertNotRealNegativeLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        Result := true;    
    end else if ((isReal(val.Num)) and (val.Num.Re < 0)) then
    begin 
        stack_push(stack, raiseException('EConstraint:CNonNegative: number not being negative real expected at "'+operand+'".'));
        Result := true;
    end else Result := false;
end;

function assertIntegerLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        assertIntegerLocated := true;
    end else if (not isReal(val.Num)) or (val.Num.Re <> ftrunc(val.Num.Re)) then 
    begin
        stack_push(stack, raiseException('EConstraint:CInteger: an integer expected at "'+operand+'".'));
        assertIntegerLocated := true;
    end else assertIntegerLocated := false;
end;

function assertRealLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        Result := true;
    end else if (not isReal(val.Num)) then 
    begin
        stack_push(stack, raiseException('EConstraint:CInteger: an real number expected at "'+operand+'".'));
        Result := true;
    end else Result := false;
end;

function assertComplexLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));
        Result := true;
    end else if (not isComplex(val.Num)) then 
    begin
        stack_push(stack, raiseException('EConstraint:CInteger: an integer expected at "'+operand+'".'));
        Result := true;
    end else Result := false;
end;

function assertNaturalLocated(var stack : TStack; val : Entity; operand : String) : Boolean;
begin
    if (val.EntityType <> TNUM) then 
    begin
        stack_push(stack, raiseException('EType:C'+getEntityTypeName(TNUM)+': <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+' at "'+operand+'".'));  
        assertNaturalLocated := true;
    end else if (val.Num.Re < 0) or (not isReal(val.Num)) or (val.Num.Re <> ftrunc(val.Num.Re)) then 
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
    end else if (val.Num.Re <= 0) or (not isReal(val.Num)) or (val.Num.Re <> ftrunc(val.Num.Re))  then 
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
    db[memsize].RefCount := 1;
    db[memsize].VarAddr := -1;

	ent.EntityType := TVEC;
	ent.Str := IntToStr(count);
	ent.Num2 := memsize;
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
    db[memsize].RefCount := 1;
    db[memsize].VarAddr := -1;

	ent.EntityType := TVEC;
	ent.Str := IntToStr(Length(pom));
	ent.Num2 := memsize;
	buildNewArray := ent;
end;

function buildNewEmptyArray(var db : StackDB; sets : TSettings; count : LongInt = 0) : Entity;
var
    i       : LongInt;
    memsize : LongInt;
    ent     : Entity;
begin
    memsize := Length(db);
    SetLength(db, memsize+1);
    for i := 0 to count-1 do
        stack_push(db[memsize], buildNull());
    db[memsize].RefCount := 1;
    db[memsize].VarAddr := -1;

	ent.EntityType := TVEC;
	ent.Str := IntToStr(count);
	ent.Num2 := memsize;
	Result := ent;
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
