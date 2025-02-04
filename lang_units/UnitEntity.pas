unit UnitEntity;
// Unit with Settings, Entities and Variables

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, 
    ComplexNumbers,
    DateUtils, DTUtils;


const
    MCLIKE = 1;
    MPASCL = 0;
	SHELL_BASH = '/bin/bash';
	SHELL_ZSH  = '/bin/zsh';
	SHELL_SH   = '/bin/sh';
	SHELL_CMD  = 'C:\Windows\System32\cmd.exe';
	SHELL_PWSH = 'C:\Windows\System32\WindowsPowershell\v1.0\powershell.exe';
	PI = 3.1415926535897932384626433832795;
	EU = 2.7182818284590452353602874713526;
	FI = 1.6180339887498948482045868343656;

type TEntityType = (
    TNIL,   // null
	TNUM,   // number
	TSTR,   // string
	TEXP,   // logicalexpression
    TFIL,   // file
	TVEC,   // array
	TBOO,   // boolean
	TOBJ,   // object
	TFUN,   // function
	TEXC,   // exception
    TDAT,   // datetime
    TPLY,   // polynomial
    TMAT,   // matrix
    TDTF,   // dataframe
    TTYP,   // entity type
    TDAY,   // date
    TTIM   // time
);

type TPackages = record
	UseAnything   : Boolean;
	UseMath       : Boolean;
	UseString     : Boolean;
    UseArray      : Boolean;
    UseConsole    : Boolean;
    UseDate       : Boolean;
    UseNumber     : Boolean;
    UsePolynomial : Boolean;
end;

type TSettings = record
    Prevent       : Boolean;
    Autoclear     : Boolean;
    Mask          : String;
    SortType      : ShortInt;
    StrictType    : Boolean;
    CaseSensitive : Boolean;
    Shell         : String;
	StackPointer  : LongInt;
	KeepWorking   : ShortInt;
    InfMode       : Boolean;
    StringStart   : ShortInt;
    StringMode    : ShortInt;
	Packages      : TPackages;
end;
// sorts
// 0 - bubblesort
// 1 - quicksort
// 2 - mergesort
// 3 - bogosort

// KeepWorking: 2 - do, 1 - continue, 0 - break

type Entity = record
	EntityType : TEntityType;
    //EntityType : Integer;
	Num        : ComplexType;
    Num2       : LongInt;
	Str        : String;
    Str2       : String;
end;

//type Entity = ^TStos;
//TStos = record
//    EntityType : Integer;
//	Num        : Extended;	// plans to make them arrays
//	Str        : String;
//    EArray     : array of Entity;
//end;

type TEntities = array of Entity;

function verifyPackages(var L : TPackages) : Boolean;
function default_settings(LoadAll : Boolean = False) : TSettings;
procedure raiserror(Const msg : string);  
//procedure checkSIGINT();

function getEntityTypeName(const x : TEntityType) : String;
function getEntitySpec(x : Entity) : String;
function printEntityValue(x : Entity; mask : String) : String;

procedure swapEntities(var e1 : Entity; var e2 : Entity);

function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
function buildNumber(val : ComplexType) : Entity;
function buildString(val : String) : Entity;
function buildBoolean(val : Boolean) : Entity;
//function buildFunction(val : String; args : String = '') : Entity;
function buildFunction(val : LongInt) : Entity;
function buildExpression(val : String) : Entity;
function buildException(val : String) : Entity;
function raiseException(val : String) : Entity;
function buildDateTime(val : TDateTime) : Entity;
function buildDateTime(val : Extended) : Entity;
//function buildDate(val : TDateTime) : Entity;
//function buildTime(val : TDateTime) : Entity;
function buildNull() : Entity;

function raiseExceptionUnknownCommand(operand : String) : Entity;
function raiseExceptionUnknownArray(operand : String) : Entity;
function raiseSyntaxErrorExpression(operand : String) : Entity;
function raiseNumRangeConstraint(operand : String; x, y : Extended) : Entity;
function raiseStringMaxLength(operand : String; str : String; MaxLength : LongInt) : Entity;
function raiseStringSameLength(operand : String) : Entity;
function raiseImpossibleArithmetics(ent1, ent2 : Entity; operand : String) : Entity;
function raiseDivisionZero(operand : String) : Entity;
function raiseInvalidTypecast(twrong, tgood : TEntityType) : Entity;
function raiseNonNumericTypecast(operand : String) : Entity;
function raiseNonNumericArrayLike(operand : String) : Entity;
function raiseSetInvalidVariable(operand : String; str : String) : Entity;
function raiseSetUnnamedVariable() : Entity;
function raiseGetUnnamedVariable() : Entity;

function isZero(e : Entity) : Boolean;
function isNumber(e : Entity) : Boolean;
function isAnyInfinity(e : Entity) : Boolean;
function isPosInfinity(e : Entity) : Boolean;
function isNegInfinity(e : Entity) : Boolean;
function isNull(e : Entity) : Boolean;
function isEmptyString(e : Entity) : Boolean;
function isString(e : Entity) : Boolean;

operator - (a : String; b : String) s : String; 
operator - (a : String; b : LongInt) s : String; 
operator * (a : String; b : LongInt) s : String;
operator / (a : String; b : String) s : String; 
operator / (a : String; b : LongInt) s : String; 

operator = (a : Entity; b : Entity) : Boolean;
operator + (a : Entity; b : Entity) res : Entity;
operator - (a : Entity; b : Entity) res : Entity;
operator * (a : Entity; b : Entity) res : Entity;
operator / (a : Entity; b : Entity) res : Entity;

implementation

{$IFDEF MSWINDOWS}
uses crt,
{$ELSE}
uses UnixCrt,
{$ENDIF}
    StrUtils, Math;


function verifyPackages(var L : TPackages) : Boolean;
begin
	Result := L.UseMath or L.UseString or L.UseArray or L.UseConsole or L.UseDate or L.UseNumber or L.UsePolynomial;
end;

function default_packages(LoadAll : Boolean = False) : TPackages;
var pom : TPackages;
begin
	pom.UseMath := LoadAll;
	pom.UseString := LoadAll;
	pom.UseAnything := LoadAll;
    pom.UseArray := LoadAll;
    pom.UseConsole := LoadAll;
    pom.UseDate := LoadAll;
    pom.UseNumber := LoadAll;
    pom.UsePolynomial := LoadAll;
	Result := pom;
end;


function default_settings(LoadAll : Boolean = False) : TSettings;
var pom : TSettings;
begin
    pom.Prevent := false;
    pom.Autoclear := true;
    pom.Mask := '0.################';
    pom.SortType := 1;
    pom.StrictType := true;
    pom.CaseSensitive := true;
    pom.StackPointer := 0;
    pom.KeepWorking := 2;
    pom.Packages := default_packages(LoadAll);
    pom.InfMode := false;
    pom.StringStart := 0;
    pom.StringMode := MCLIKE;
    {$IFDEF MSWINDOWS}
    pom.Shell := SHELL_CMD;
    {$ENDIF}
    {$IFDEF UNIX}
        {$IFDEF LINUX}
        pom.Shell := SHELL_BASH;
        {$ELSE}
        pom.Shell := SHELL_SH;
        {$ENDIF}
    {$ENDIF}
    Result := pom;
end;

procedure raiserror(Const msg : string);  
begin  
  raise exception.create(Msg) at  
  get_caller_addr(get_frame),  
  get_caller_frame(get_frame);  
end; 

function getEntityTypeName(const x : TEntityType) : String;
begin
    case x of
        TNIL : Result := 'Null';
        TNUM : Result := 'Number';
        TSTR : Result := 'String';
        TVEC : Result := 'Array';
        TBOO : Result := 'Boolean';
        TOBJ : Result := 'Object';
        TFUN : Result := 'Function';
        TEXC : Result := 'Exception';
        TEXP : Result := 'LogicalExpression';
        TFIL : Result := 'File';
        TDAT : Result := 'DateTime';
        TPLY : Result := 'Polynomial';
        else Result := 'Unknown';
    end;
end;

function getEntitySpec(x : Entity) : String;
begin
    case x.EntityType of
        TNIL : Result := '<Null>';
        TNUM : Result := toStringFormat(x.Num) + ' : <Number>';
        TSTR : Result := '"' + x.Str + '" : <String>';
        TVEC : Result := '<Array>';
        TBOO : Result := x.Str + ' : <Boolean>';
        TOBJ : Result := '<Object>';
        TFUN : Result := '<Function>';
        TEXP : Result := '<LogicalExpression>';
        TFIL : Result := '<File>';
        TDAT : Result := '"' + x.Str + '" : <DateTime>';
        TPLY : Result := '<Polynomial>';
        else Result := '<Unknown>';
    end;
end;

function printEntityValue(x : Entity; mask : String) : String;
var
  z : String;
begin
    z := '';
    case x.EntityType of
        TNUM : z := toStringFormat(x.Num, mask);
        TSTR : z := '"' + x.Str + '"';
        TNIL : z := x.Str;
        TBOO : z := x.Str;
        TVEC : z := '<Array>';
        TOBJ : z := '<Object>';
        TFUN : z := '<Function>';
        TEXC : z := '<Exception>'; 
        TFIL : z := '<File>';
        TEXP : z := '<LogicalExpression>';
        TDAT : z := x.Str;
        TPLY : z := '<Polynomial>';
        else z := '<Unknown>'; 
    end;
    Result := z;
end;

procedure swapEntities(var e1 : Entity; var e2 : Entity);
var
	pom : Entity;
begin
	pom := e1;
	e1 := e2;
	e2 := pom;
end;

// =============================================================================
// build entities

function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
var
  pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TNUM;
	pom.Num := val;
    pom.Num2 := 0;
	pom.Str := FormatFloat(sets.Mask, val);
	//pom.EArray := nil;
	buildNumberFormattted := pom;
end;

function buildNumber(val : ComplexType) : Entity;
var
  pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TNUM;
	pom.Num := val;
    pom.Num2 := 0;
	pom.Str := '' + toStringFormat(val);
	//pom.EArray := nil;
	buildNumber := pom;
end;

function buildString(val : String) : Entity;
var
  pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TSTR;
	pom.Str := val;
	pom.Num := Length(val);
    pom.Num2 := 0;
	//pom.EArray := nil;
	buildString := pom;
end;

function buildBoolean(val : Boolean) : Entity;
var
  pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TBOO;
	if (val) then
	begin
		pom.Str := 'TRUE';
		pom.Num := 0;
        pom.Num2 := 0;
	end else begin
		pom.Str := 'FALSE';
		pom.Num := 1;
        pom.Num2 := 0;
	end;
	//pom.EArray := nil;
	buildBoolean := pom;
end;

function buildFunction(val : String; args : String = '') : Entity;
var
	pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TFUN;
	pom.Str := val;
    pom.Str2 := args;
	pom.Num := Length(val);
    pom.Num2 := 0;
	//pom.EArray := nil;
	buildFunction := pom;
end;

function buildFunction(val : LongInt) : Entity;
var
	pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TFUN;
	pom.Str := '';
	pom.Num := 0;
    pom.Num2 := val;
	//pom.EArray := nil;
	buildFunction := pom;
end;

function buildExpression(val : String) : Entity;
var
	pom : Entity;
begin
	pom.EntityType := TEXP;
	pom.Str := val;
	pom.Num := Length(val);
    pom.Num2 := 0;
	buildExpression := pom;
end;

function buildException(val : String) : Entity;
var
	pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TEXC;
	pom.Str := val;
	pom.Num := 0;
    pom.Num2 := 0;
	//pom.EArray := nil;
	buildException := pom;
end;

function raiseException(val : String) : Entity;
var
	pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TEXC;
	pom.Str := val;
	pom.Num := 1;
    pom.Num2 := 0;
	//pom.EArray := nil;
	raiseException := pom;
end;

function buildNull() : Entity;
var
  pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TNIL;
	pom.Str := 'NULL';
	pom.Num := 0;
    pom.Num2 := 0;
	//pom.EArray := nil;
	buildNull := pom;
end;

function buildDateTime(val : TDateTime) : Entity;
var
	pom : Entity;
    tmp : TTimeStamp;
begin
    tmp := DateTimeToTimestamp(val);
	pom.EntityType := TDAT;
	pom.Str := FormatYMD(val);
	pom.Num := tmp.Date;
    pom.Num2 := tmp.Time;
	Result := pom;
end;

function buildDateTime(val : Extended) : Entity;
var
	pom : Entity;
    tmp : TTimeStamp;
begin
    tmp := MSecsToTimestamp(trunc(val*1000));
	pom.EntityType := TDAT;
	pom.Str := FormatYMD(TimestampToDateTime(tmp));
	pom.Num := tmp.Date;
    pom.Num2 := tmp.Time;
	Result := pom;
end;

//function buildDate(val : TDateTime) : Entity;
//var
//	pom : Entity;
//begin
//	pom.EntityType := TDAY;
//	pom.Str := FormatYMD(val);
//	pom.Num := DateTimeToUnix(DateOf(val));
//    pom.Num2 := 0;
//	Result := pom;
//end;
//
//function buildTime(val : TDateTime) : Entity;
//var
//	pom : Entity;
//begin
//	pom.EntityType := TTIM;
//	pom.Str := FormatYMD(val);
//	pom.Num := DateTimeToUnix(val);
//    pom.Num2 := 0;
//	Result := pom;
//end;

// =============================================================================
// exceptions

function raiseExceptionUnknownCommand(operand : String) : Entity;
begin
    Result := raiseException('EInput:CUnknown: Unknown expression at "'+operand+'".');
end;

function raiseExceptionUnknownArray(operand : String) : Entity;
begin
    Result := raiseException('EInput:CNonArray: Array expression expected at "'+operand+'".');
end;

function raiseSyntaxErrorExpression(operand : String) : Entity;
begin
    Result := raiseException('ESyntax:CExpression: Syntax error at expression "('+operand+' )".');
end;

function raiseImpossibleArithmetics(ent1, ent2 : Entity; operand : String) : Entity;
begin
    Result := raiseException('EInput:CArithmetics: Unknown arithmetics of ('+getEntityTypeName(ent1.EntityType)+operand+getEntityTypeName(ent2.EntityType)+').');
end;

function raiseStringMaxLength(operand : String; str : String; MaxLength : LongInt) : Entity;
begin
    Result := raiseException('EInput:CMaxStrLen: String length constraint violated (max length '+IntToStr(MaxLength)+' of "'+str+'") at "'+operand+'".');
end;

function raiseStringSameLength(operand : String) : Entity;
begin
    Result := raiseException('EInput:CSameStrLen: Strings must have the same length at "'+operand+'".');
end;

function raiseDivisionZero(operand : String) : Entity;
begin
    Result := raiseException('EInput:CDivZero: Divison by zero at "'+operand+'".');
end;

function raiseNumRangeConstraint(operand : String; x, y : Extended) : Entity;
begin
    Result := raiseException('EInput:CNumRange: Number must be within a range of ['+FormatFloat('0.###############', x)+', '+FormatFloat('0.###############', y)+'] at "'+operand+'".');
end;

function raiseNonNumericTypecast(operand : String) : Entity;
begin
    Result := raiseException('EType:CNonNumeric: Got a non-numeric entity at "'+operand+'".');
end;

function raiseNonNumericArrayLike(operand : String) : Entity;
begin
    Result := raiseException('EType:CNonNumericArray: Got a non-numeric array or polynomial at "'+operand+'".');
end;

function raiseInvalidTypecast(twrong, tgood : TEntityType) : Entity;
begin
    Result := raiseException('EInput:CCast: Invalid typecast: '+getEntityTypeName(twrong)+' -> '+getEntityTypeName(tgood)+'.');
end;

function raiseSetInvalidVariable(operand : String; str : String) : Entity;
begin
    Result := raiseException('EVariable:CSetInvalid: Invalid variable string of "'+str+'" at "'+operand+'"');
end;

function raiseSetUnnamedVariable() : Entity;
begin
    Result := raiseException('EVariable:CSetUnnamed: Attempt of setting an unnamed variable.');
end;

function raiseGetUnnamedVariable() : Entity;
begin
    Result := raiseException('EVariable:CSetUnnamed: Attempt of getting an unnamed variable.');
end;


// exceed boundaries
// not null
// isEmpty

// =============================================================================
// functions for checking entities

function isNumber(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TNUM);
end;

function isZero(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TNUM) and (e.Num = 0);
end;

function isAnyInfinity(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TNUM) and (abs(e.Num) = Infinity);
end;

function isPosInfinity(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TNUM) and (e.Num = Infinity);
end;

function isNegInfinity(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TNUM) and (e.Num = -Infinity);
end;

function isNull(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TNIL);
end;

function isEmptyString(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TSTR) and (e.Str = '');
end;

function isString(e : Entity) : Boolean;
begin
    Result := (e.EntityType = TSTR);
end;

// =============================================================================
// arithmetics on entities

operator - (a : String; b : String) s : String;  
var
    index : LongInt;
begin  
    index := RPos(b, a);
    Delete(a, index, Length(b));
    s := a;
end;

operator - (a : String; b : LongInt) s : String;  
begin  
    s := LeftStr(a, Length(a)-Trunc(b));
end;

operator * (a : String; b : LongInt) s : String;  
var
    index : LongInt;
begin  
    s := '';
    for index := 1 to b do s := s + a;
end;

operator / (a : String; b : String) s : String;  
begin  
    while (a - b <> a) do a := a - b;
    s := a;
end;

operator / (a : String; b : LongInt) s : String;  
begin  
    s := LeftStr(a, Trunc(Length(a)/Trunc(b)));
end;

operator = (a : Entity; b : Entity) : Boolean;
begin
    Result := (a.Str = b.Str) and (a.Num = b.Num);
end;

operator + (a : Entity; b : Entity) res : Entity;
begin
    if (a.EntityType = TNUM) and (b.EntityType = TNUM) then
    begin
        res := buildNumber(a.Num + b.Num);
    end else if (a.EntityType = TSTR) and (b.EntityType = TSTR) then 
    begin
        res := buildString(a.Str + b.Str);
    end else begin
        res := raiseImpossibleArithmetics(a, b, '+');
    end;
end;

operator - (a : Entity; b : Entity) res : Entity;
begin
    if (a.EntityType = TNUM) and (b.EntityType = TNUM) then
    begin
        res := buildNumber(a.Num - b.Num);
    end else if (a.EntityType = TSTR) and (b.EntityType = TSTR) then
    begin
        res := buildString(a.Str - b.Str);
    end else if (a.EntityType = TSTR) and (b.EntityType = TNUM) then
    begin
        res := buildString(a.Str - trunc(b.Num.Re));
    end else begin
        res := raiseImpossibleArithmetics(a, b, '-');
    end;
end;

operator * (a : Entity; b : Entity) res : Entity;
begin
    if (a.EntityType = TNUM) and (b.EntityType = TNUM) then
    begin
        res := buildNumber(a.Num * b.Num);
    end else if (a.EntityType = TSTR) and (b.EntityType = TNUM) then 
    begin
        res := buildString(a.Str * trunc(b.Num.Re));
    end else if (a.EntityType = TNUM) and (b.EntityType = TSTR) then 
    begin
        res := buildString(b.Str * trunc(a.Num.Re));
    end else begin
        res := raiseImpossibleArithmetics(a, b, '*');
    end;
end;

operator / (a : Entity; b : Entity) res : Entity;
begin
    if (a.EntityType = TNUM) and (b.EntityType = TNUM) then
    begin
        res := buildNumber(a.Num / b.Num);
    end else if (a.EntityType = TSTR) and (b.EntityType = TSTR) then
    begin
        res := buildString(a.Str / b.Str);
    end else if (a.EntityType = TSTR) and (b.EntityType = TNUM) then
    begin
        res := buildString(a.Str / trunc(b.Num.Re));
    end else begin
        res := raiseImpossibleArithmetics(a, b, '/');
    end;
end;

end.

