unit UnitVariables;

{$mode objfpc}{$H+}

interface

uses 
    Classes, SysUtils, 
    UnitEntity;

type Variable = record 
	VarName   : String;
	StoredVar : Entity;
end;

type VariableDB = record
	Content : array of Variable;
end;

function buildVariable(namevar : String; contentvar : Entity) : Variable;
function createVariables() : VariableDB;
function isVarAssigned(db : VariableDB; guess : String) : Boolean;
function getVariable(db : VariableDB; guess : String) : Entity;
procedure setVariable(var db : VariableDB; newname : String; newvalue : Entity);
procedure destroyVariable(var db : VariableDB; guess : String);
procedure destroyVariables(var db : VariableDB);

implementation

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

