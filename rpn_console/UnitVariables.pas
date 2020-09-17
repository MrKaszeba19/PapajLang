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

type VariableLayer = record
	Content : array of Variable;
end;

type VariableDB = object
    private
        Layers : array of VariableLayer;
    public
        constructor Create;
        destructor Destroy;
        function getGlobalLayer() : VariableLayer;
        function getLocalLayer() : VariableLayer;
        procedure addLayer();
        procedure removeLayer();
        procedure setVariable(newname : String; newvalue : Entity);
        function getVariable(guess : String) : Entity;
        function isVarAssigned(guess : String) : Boolean;
        procedure removeVariable(guess : String);
        procedure clearAllVariables();
end; 

function buildVariable(namevar : String; contentvar : Entity) : Variable;



implementation

constructor VariableDB.Create;
begin
    SetLength(Layers, 1);
    SetLength(Layers[0].Content, 0);
end;

destructor VariableDB.Destroy;
var
    i : LongInt;
begin
    for i := Length(Layers)-1 downto 0 do SetLength(Layers[i].Content, 0);
    SetLength(Layers, 0);
end;

function VariableDB.getGlobalLayer() : VariableLayer;
begin
    Result := Layers[0];
end;
function VariableDB.getLocalLayer() : VariableLayer;
begin
    Result := Layers[Length(Layers)-1];
end;

procedure VariableDB.addLayer();
var
    len : LongInt;
begin
    len := Length(Layers);
    SetLength(Layers, len+1);
    SetLength(Layers[len].Content, 0);
end;

procedure VariableDB.removeLayer();
var
    len : LongInt;
begin
    len := Length(Layers);
    SetLength(Layers[len-1].Content, 0);
    SetLength(Layers, len-1);
end;

procedure VariableDB.setVariable(newname : String; newvalue : Entity);
var
	i      : LongInt;
    latest : LongInt;
	is_set : Boolean;
begin
	is_set := false;
    latest := Length(Layers)-1;
	for i := 0 to Length(Layers[latest].Content)-1 do 
	begin
		if (newname = Layers[latest].Content[i].VarName) then
		begin
			Layers[latest].Content[i].StoredVar := newvalue;
			is_set := true;
			break;
		end;
	end;
	if not (is_set) then 
	begin
		i := Length(Layers[latest].Content);
		SetLength(Layers[latest].Content, i+1);
		Layers[latest].Content[i].VarName := newname;
		Layers[latest].Content[i].StoredVar := newvalue;
	end;
end;

function VariableDB.getVariable(guess : String) : Entity;
var
	i      : Variable;
	pom    : Entity;
    latest : LongInt;
begin
	pom := buildNull();
    latest := Length(Layers)-1;
	for i in Layers[latest].Content do
	begin
		if (i.VarName = guess) then
		begin
			pom := i.StoredVar;
			break;
		end;
	end;
	Result := pom;
end;

function VariableDB.isVarAssigned(guess : String) : Boolean;
var
	res    : Boolean;
	tk     : Variable;
    latest : LongInt;
begin
    latest := Length(Layers)-1;
	res := false;
	for tk in Layers[latest].Content do 
		if (tk.VarName = guess) then
		begin
			res := true;
			break;
		end;
	Result := res;
end;

procedure VariableDB.removeVariable(guess : String);
var
    i      : LongInt;
    addr   : LongInt;
    is_set : Boolean;
    latest : LongInt;
begin
    is_set := False;
    latest := Length(Layers)-1;
    for i := 0 to Length(Layers[latest].Content)-1 do  
    begin
        if (guess = Layers[latest].Content[i].VarName) then
        begin
            addr := i;
            is_set := true;
            break;
        end;
    end;
    if (is_set) then 
    begin
        for i := addr to Length(Layers[latest].Content)-2 do
            Layers[latest].Content[i] := Layers[latest].Content[i+1];
        SetLength(Layers[latest].Content, Length(Layers[latest].Content)-1);
    end;
end;

procedure VariableDB.clearAllVariables();
var
    i : LongInt;
begin
    for i := Length(Layers)-1 downto 0 do SetLength(Layers[i].Content, 0);
    SetLength(Layers, 0);
end;

// ========================

function buildVariable(namevar : String; contentvar : Entity) : Variable;
var
	pom : Variable;
begin
	pom.VarName := namevar;
	pom.StoredVar := contentvar;
	buildVariable := pom;
end;

end.

