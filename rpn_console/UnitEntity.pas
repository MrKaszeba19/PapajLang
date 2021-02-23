unit UnitEntity;
// Unit with Settings, Entities and Variables

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, DTUtils;

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
    TDAY,   // date
    TTIM,   // time
);

type TPackages = record
	UseAnything : Boolean;
	UseMath     : Boolean;
	UseString   : Boolean;
    UseArray    : Boolean;
    UseConsole  : Boolean;
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
	Num        : Extended;
    Num2       : Extended;
	Str        : String;
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
function default_settings() : TSettings;
procedure raiserror(Const msg : string);  
//procedure checkSIGINT();

function getEntityTypeName(const x : TEntityType) : String;
function getEntitySpec(x : Entity) : String;
function printEntityValue(x : Entity; mask : String) : String;

procedure swapEntities(var e1 : Entity; var e2 : Entity);

function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
function buildNumber(val : Extended) : Entity;
function buildString(val : String) : Entity;
function buildBoolean(val : Boolean) : Entity;
function buildFunction(val : String) : Entity;
function buildExpression(val : String) : Entity;
function buildException(val : String) : Entity;
function raiseException(val : String) : Entity;
function buildDateTime(val : TDateTime) : Entity;
function buildNull() : Entity;

implementation

{$IFDEF MSWINDOWS}
uses crt;
{$ELSE}
uses ConsoleUtils;
{$ENDIF}


function verifyPackages(var L : TPackages) : Boolean;
begin
	verifyPackages := L.UseMath or L.UseString or L.UseArray or L.UseConsole;
end;

function default_packages() : TPackages;
var pom : TPackages;
begin
	pom.UseMath := false;
	pom.UseString := false;
	pom.UseAnything := false;
    pom.UseArray := false;
    pom.UseConsole := false;
	default_packages := pom;
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
  pom.StackPointer := 0;
  pom.KeepWorking := 2;
  pom.Packages := default_packages();
  pom.InfMode := false;
  pom.StringStart := 0;
  pom.StringMode := MCLIKE;
  {$IFDEF MSWINDOWS}
  pom.Shell := SHELL_CMD;
  {$ELSE}
  pom.Shell := SHELL_BASH;
  {$ENDIF}
  default_settings := pom;
end;

procedure raiserror(Const msg : string);  
begin  
  raise exception.create(Msg) at  
  get_caller_addr(get_frame),  
  get_caller_frame(get_frame);  
end; 


{*
procedure checkSIGINT();
begin
    if KeyPressed then         
        if ReadKey = ^C then
        begin
            writeln('Halted!');
            Halt(1);
        end;
end;
*}

function getEntityTypeName(const x : TEntityType) : String;
begin
    case x of
        TNIL : getEntityTypeName := 'Null';
        TNUM : getEntityTypeName := 'Number';
        TSTR : getEntityTypeName := 'String';
        TVEC : getEntityTypeName := 'Array';
        TBOO : getEntityTypeName := 'Boolean';
        TOBJ : getEntityTypeName := 'Object';
        TFUN : getEntityTypeName := 'Function';
        TEXC : getEntityTypeName := 'Exception';
        TEXP : getEntityTypeName := 'LogicalExpression';
        TFIL : getEntityTypeName := 'File';
        TDAT : getEntityTypeName := 'DateTime';
        else getEntityTypeName := 'Unknown';
    end;
end;

function getEntitySpec(x : Entity) : String;
begin
    case x.EntityType of
        TNIL : getEntitySpec := '<Null>';
        TNUM : getEntitySpec := FormatFloat('0.###############', x.Num) + ' : <Number>';
        TSTR : getEntitySpec := '"' + x.Str + '" : <String>';
        TVEC : getEntitySpec := '<Array>';
        TBOO : getEntitySpec := x.Str + ' : <Boolean>';
        TOBJ : getEntitySpec := '<Object>';
        TFUN : getEntitySpec := '<Function>';
        TEXP : getEntitySpec := '<LogicalExpression>';
        TFIL : getEntitySpec := '<File>';
        TDAT : getEntitySpec := '"' + x.Str + '" : <DateTime>';
        else getEntitySpec := '<Unknown>';
    end;
end;

function printEntityValue(x : Entity; mask : String) : String;
var
  z : String;
begin
    z := '';
         if (x.EntityType = TNUM) then z := FormatFloat(mask, x.Num)
    else if (x.EntityType = TSTR) then z := '"' + x.Str + '"'
    else if (x.EntityType = TNIL) then z := x.Str
    else if (x.EntityType = TBOO) then z := x.Str
    else if (x.EntityType = TVEC) then z := '<Array>'
    else if (x.EntityType = TOBJ) then z := '<Object>'
    else if (x.EntityType = TFUN) then z := '<Function>'
    else if (x.EntityType = TEXC) then z := '<Exception>' 
    else if (x.EntityType = TFIL) then z := '<File>' 
    else if (x.EntityType = TEXP) then z := '<LogicalExpression>'
    else if (x.EntityType = TDAT) then z := x.Str
    else z := '<Unknown>'; 
    printEntityValue := z;
end;

procedure swapEntities(var e1 : Entity; var e2 : Entity);
var
	pom : Entity;
begin
	pom := e1;
	e1 := e2;
	e2 := pom;
end;


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

function buildNumber(val : Extended) : Entity;
var
  pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TNUM;
	pom.Num := val;
    pom.Num2 := 0;
	pom.Str := '' + FormatFloat('0.###############' ,val);
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

function buildFunction(val : String) : Entity;
var
	pom : Entity;
begin
	//pom := New(Entity);
	pom.EntityType := TFUN;
	pom.Str := val;
	pom.Num := Length(val);
    pom.Num2 := 0;
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
begin
	pom.EntityType := TDAT;
	pom.Str := FormatYMD(val);
	pom.Num := DateTimeToUnix(val);
    pom.Num2 := 0;
	Result := pom;
end;

end.

