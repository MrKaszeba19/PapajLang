unit UnitEntity;
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
	TOBJ = 7;
	TFUN = 8;
	TEXC = 9;
	SHELL_BASH = '/bin/bash';
	SHELL_ZSH  = '/bin/zsh';
	SHELL_SH   = '/bin/sh';
	SHELL_CMD  = 'C:\Windows\System32\cmd.exe';
	SHELL_PWSH = 'C:\Windows\System32\WindowsPowershell\v1.0\powershell.exe';
	PI = 3.1415926535897932384626433832795;
	EU = 2.7182818284590452353602874713526;
	FI = 1.6180339887498948482045868343656;

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

type TEntities = array of Entity;

type Variable = record 
	VarName   : String;
	StoredVar : Entity;
end;

type VariableDB = record
	Content : array of Variable;
end;

function default_settings() : TSettings;
procedure raiserror(Const msg : string);  

function getEntityTypeName(const x : Integer) : String;
function getEntitySpec(x : Entity) : String;

procedure swapEntities(var e1 : Entity; var e2 : Entity);

function buildNumberFormattted(val : Extended; sets : TSettings) : Entity;
function buildNumber(val : Extended) : Entity;
function buildString(val : String) : Entity;
function buildBoolean(val : Boolean) : Entity;
function buildFunction(val : String) : Entity;
function buildException(val : String) : Entity;
function buildNull() : Entity;

function buildVariable(namevar : String; contentvar : Entity) : Variable;
function createVariables() : VariableDB;
function isVarAssigned(db : VariableDB; guess : String) : Boolean;
function getVariable(db : VariableDB; guess : String) : Entity;
procedure setVariable(var db : VariableDB; newname : String; newvalue : Entity);
procedure destroyVariable(var db : VariableDB; guess : String);
procedure destroyVariables(var db : VariableDB);

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
    TVEC : getEntityTypeName := 'vector';
    TBOO : getEntityTypeName := 'boolean';
    TOBJ : getEntityTypeName := 'object';
    TFUN : getEntityTypeName := 'function';
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
    TVEC : getEntitySpec := '<vector>';
    TBOO : getEntitySpec := x.Str + ' : <boolean>';
    TOBJ : getEntitySpec := '<object>';
    TFUN : getEntitySpec := '<function>';
    else getEntitySpec := '<unknown>';
  end;
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
    pom.Str := 'TRUE';
    pom.Num := 0;
  end else begin
    pom.Str := 'FALSE';
    pom.Num := 1;
  end;
  buildBoolean := pom;
end;

function buildFunction(val : String) : Entity;
var
	pom : Entity;
begin
	pom.EntityType := TFUN;
	pom.Str := val;
	pom.Num := Length(val);
	buildFunction := pom;
end;

function buildException(val : String) : Entity;
var
	pom : Entity;
begin
	pom.EntityType := TEXC;
	pom.Str := val;
	pom.Num := Length(val);
	buildException := pom;
end;

function buildNull() : Entity;
var
  pom : Entity;
begin
  pom.EntityType := TNIL;
  pom.Str := 'NULL';
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
	i      : LongInt;
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

procedure destroyVariable(var db : VariableDB; guess : String);
var
    i      : LongInt;
    addr   : LongInt;
    is_set : Boolean;
begin
    is_set := False;
    for i := 0 to Length(db.Content)-1 do  
    begin
        if (guess = db.Content[i].VarName) then
        begin
            addr := i;
            is_set := true;
            break;
        end;
    end;
    if (is_set) then 
    begin
        for i := addr to Length(db.Content)-2 do
            db.Content[i] := db.Content[i+1];
        SetLength(db.Content, Length(db.Content)-1);
    end;
end;

procedure destroyVariables(var db : VariableDB);
begin
    SetLength(db.Content, 0);
end;


end.

