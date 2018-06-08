unit Unit7;
// Unit with Settings, Entities and Variables

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
	TNIL = 0;
	TNUM = 1;
	TSTR = 2;
	TVEN = 3;
	TVES = 4;
	TVEC = 5;
	TBOO = 6;
	SHELL_BASH = '/bin/bash';
	SHELL_ZSH  = '/bin/zsh';
	SHELL_SH   = '/bin/sh';
	SHELL_CMD  = 'C:\Windows\System32\cmd.exe';
	SHELL_PWSH = 'C:\Windows\System32\WindowsPowershell\v1.0\powershell.exe';

type TSettings = record
    Prevent            : Boolean;
    Autoclear          : Boolean;
    Mask               : String;
    SortType           : ShortInt;
    StrictType         : Boolean;
    CaseSensitive      : Boolean;
    Shell              : String;
end;
// sorts
// 0 - bubblesort
// 1 - quicksort
// 2 - mergesort
// 3 - bogosort

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
// 6 - boolean

type TEntities = array of Entity;

type Variable = record 
	VarName   : String;
	StoredVar : Entity;
end;

type VariableDB = record
	Content : array of Variable;
end;

function default_settings() : TSettings;

function getEntityTypeName(const x : Integer) : String;
function getEntitySpec(x : Entity) : String;
procedure assertEntity(val : Entity; const wtype : Integer);
procedure assertEntityLocated(val : Entity; const wtype : Integer; operand : String);
procedure assertNotNegativeLocated(val : Entity; operand : String);
procedure assertIntegerLocated(val : Entity; operand : String);
procedure assertNaturalLocated(val : Entity; operand : String);

function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
function buildNumber(val : Extended) : Entity;
function buildString(val : String) : Entity;
function buildBoolean(val : Boolean) : Entity;
function buildNull() : Entity;

function buildVariable(namevar : String; contentvar : Entity) : Variable;
function createVariables() : VariableDB;
function isVarAssigned(db : VariableDB; guess : String) : Boolean;
function getVariable(db : VariableDB; guess : String) : Entity;
procedure setVariable(var db : VariableDB; newname : String; newvalue : Entity);

implementation

function default_settings() : TSettings;
var pom : TSettings;
begin
  pom.Prevent := false;
  pom.Autoclear := true;
  pom.Mask := '0.################';
  pom.SortType := 1;
  pom.StrictType := true;
  pom.CaseSensitive := true;
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

function getEntityTypeName(const x : Integer) : String;
begin
  case x of
    TNIL : getEntityTypeName := 'null';
    TNUM : getEntityTypeName := 'number';
    TSTR : getEntityTypeName := 'string';
    TVEN : getEntityTypeName := 'vector<number>';
    TVES : getEntityTypeName := 'vector<string>';
    TVEC : getEntityTypeName := 'vector<any>';
    TBOO : getEntityTypeName := 'boolean';
    else getEntityTypeName := 'unknown';
  end;
end;

function getEntitySpec(x : Entity) : String;
begin
  case x.EntityType of
    TNIL : getEntitySpec := '<null>';
    TNUM : getEntitySpec := FormatFloat('0.###############', x.Num) + ' : <number>';
    TSTR : getEntitySpec := '"' + x.Str + '" : <string>';
    TVEN : getEntitySpec := '<vector<number>>';
    TVES : getEntitySpec := '<vector<string>>';
    TVEC : getEntitySpec := '<vector<any>>';
    TBOO : getEntitySpec := x.Str + ' : <boolean>';
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
    raiserror('Type mismatch at "'+operand+'": <'+getEntityTypeName(wtype)+'> expected, got '+getEntitySpec(val)+'.');
end;

procedure assertNotNegativeLocated(val : Entity; operand : String);
begin
  if (val.EntityType <> TNUM) then 
    raiserror('Type mismatch at "'+operand+'": <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+'.');
  if (val.Num < 0) then 
    raiserror('Exception when taking a numeric value at "'+operand+'": an positive real number or zero expected');
end;

procedure assertIntegerLocated(val : Entity; operand : String);
begin
  if (val.EntityType <> TNUM) then 
    raiserror('Type mismatch at "'+operand+'": <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+'.');
  if (val.Num <> trunc(val.Num)) then 
    raiserror('Exception when taking a numeric value at "'+operand+'": integer expected, got a real number');
end;

procedure assertNaturalLocated(val : Entity; operand : String);
begin
  if (val.EntityType <> TNUM) then 
    raiserror('Type mismatch at "'+operand+'": <'+getEntityTypeName(TNUM)+'> expected, got '+getEntitySpec(val)+'.');
  if (val.Num < 0) or (val.Num <> trunc(val.Num)) then 
    raiserror('Exception when taking a numeric value at "'+operand+'": an positive integer or zero expected');
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

function buildBoolean(val : Boolean) : Entity;
var
  pom : Entity;
begin
  pom.EntityType := TBOO;
  if (val) then
  begin
    pom.Str := 'true';
    pom.Num := 0;
  end else begin
    pom.Str := 'false';
    pom.Num := 1;
  end;
  buildBoolean := pom;
end;

function buildNull() : Entity;
var
  pom : Entity;
begin
  pom.EntityType := TNIL;
  pom.Str := 'null';
  pom.Num := 0;
  buildNull := pom;
end;

// VARIABLES OPERATIONS

function buildVariable(namevar : String; contentvar : Entity) : Variable;
var
	pom : Variable;
begin
	pom.VarName := namevar;
	pom.StoredVar := contentvar;
	buildVariable := pom;
end;

function createVariables() : VariableDB;
var 
	pom : VariableDB;
begin
	SetLength(pom.Content, 0);
	createVariables := pom;
end;

function isVarAssigned(db : VariableDB; guess : String) : Boolean;
var
	res : Boolean;
	tk  : Variable;
begin
	res := false;
	for tk in db.Content do 
		if (tk.VarName = guess) then
		begin
			res := true;
			break;
		end;
	isVarAssigned := res;
end;

function getVariable(db : VariableDB; guess : String) : Entity;
var
	i : Variable;
	pom : Entity;
begin
	pom := buildNull();
	for i in db.Content do
	begin
		if (i.VarName = guess) then
		begin
			pom := i.StoredVar;
			break;
		end;
	end;
	getVariable := pom;
end;

procedure setVariable(var db : VariableDB; newname : String; newvalue : Entity);
var
	i      : Integer;
	is_set : Boolean;
begin
	is_set := false;
	for i := 0 to Length(db.Content)-1 do 
	begin
		if (newname = db.Content[i].VarName) then
		begin
			db.Content[i].StoredVar := newvalue;
			is_set := true;
			break;
		end;
	end;
	if not (is_set) then 
	begin
		i := Length(db.Content);
		SetLength(db.Content, i+1);
		db.Content[i].VarName := newname;
		db.Content[i].StoredVar := newvalue;
	end;
end;


end.

