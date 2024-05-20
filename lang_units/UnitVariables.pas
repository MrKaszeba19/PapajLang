unit UnitVariables;

{$mode objfpc}{$H+}

interface

uses 
    Classes, SysUtils, 
    UnitEntity;

type Variable = record 
	VarID     : LongInt;
	VarName   : String;
	StoredVar : Entity;
end;

type VariableAddress = record
    Layer    : LongInt; 
    Location : LongInt;
end;

type VariableLayer = record
	Content : array of Variable;
end;

type VariableDB = object
    private
		NextID : LongInt;
        Layers : array of VariableLayer;
    public
        constructor Create;
        destructor Destroy;
        function getGlobalLayer() : VariableLayer;
        function getLocalLayer() : VariableLayer;
        procedure addLayer();
        procedure removeLayer();
        procedure setVariable(newname : String; newvalue : Entity);
        procedure setLocalVariable(newname : String; newvalue : Entity);
        procedure setGlobalVariable(newname : String; newvalue : Entity);
        function getVariable(guess : String) : Entity;
        function getLocalVariable(guess : String) : Entity;
        function getGlobalVariable(guess : String) : Entity;
        function getLocatedVariable(guess : String; addr : VariableAddress) : Entity;
        function isVarAssigned(guess : String) : Boolean;
        function locateVariable(guess : String) : VariableAddress;
        function isLocalVarAssigned(guess : String) : Boolean;
        function isGlobalVarAssigned(guess : String) : Boolean;
        procedure removeVariable(guess : String);
        procedure clearAllVariables();
end; 

function buildVariable(namevar : String; contentvar : Entity) : Variable;
function isValidForVariables(guess : String) : Boolean;


implementation

constructor VariableDB.Create;
begin
	NextID := 0;
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
	i, j   : LongInt;
	is_set : Boolean;
    latest : LongInt;
begin
	is_set := false;
    latest := Length(Layers)-1;
    for j := latest downto 0 do
    begin
	    for i := 0 to Length(Layers[j].Content)-1 do 
	    begin
	    	if (newname = Layers[j].Content[i].VarName) then
	    	begin
	    		Layers[j].Content[i].StoredVar := newvalue;
	    		is_set := true;
	    		break;
	    	end;
	    end;
        if is_set then break;
    end;
	if not (is_set) then 
	begin
		i := Length(Layers[latest].Content);
		SetLength(Layers[latest].Content, i+1);
		Layers[latest].Content[i].VarID := NextID;
		Layers[latest].Content[i].VarName := newname;
		Layers[latest].Content[i].StoredVar := newvalue;
		Inc(NextID);
	end;
end;

function VariableDB.getVariable(guess : String) : Entity;
var
	i      : LongInt;
    j      : Variable;
	pom    : Entity;
    is_set : Boolean;
begin
    is_set := false;
	pom := buildNull();
    for i := Length(Layers)-1 downto 0 do
    begin
	    for j in Layers[i].Content do
	    begin
	    	if (j.VarName = guess) then
	    	begin
	    		pom := j.StoredVar;
                is_set := True;
	    		break;
	    	end;
	    end;
        if is_set then break;
    end;
	Result := pom;
end;

procedure VariableDB.setGlobalVariable(newname : String; newvalue : Entity);
var
	i      : LongInt;
	is_set : Boolean;
begin
	is_set := false;
	for i := 0 to Length(Layers[0].Content)-1 do 
	begin
		if (newname = Layers[0].Content[i].VarName) then
		begin
			Layers[0].Content[i].StoredVar := newvalue;
			is_set := true;
			break;
		end;
	end;
	if not (is_set) then 
	begin
		i := Length(Layers[0].Content);
		SetLength(Layers[0].Content, i+1);
		Layers[0].Content[i].VarID := NextID;
		Layers[0].Content[i].VarName := newname;
		Layers[0].Content[i].StoredVar := newvalue;
		Inc(NextID);
	end;
end;

procedure VariableDB.setLocalVariable(newname : String; newvalue : Entity);
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
		Layers[latest].Content[i].VarID := NextID;
		Layers[latest].Content[i].VarName := newname;
		Layers[latest].Content[i].StoredVar := newvalue;
		Inc(NextID);
	end;
end;

function VariableDB.getLocalVariable(guess : String) : Entity;
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

function VariableDB.getGlobalVariable(guess : String) : Entity;
var
	i      : Variable;
	pom    : Entity;
begin
	pom := buildNull();
	for i in Layers[0].Content do
	begin
		if (i.VarName = guess) then
		begin
			pom := i.StoredVar;
			break;
		end;
	end;
	Result := pom;
end;

function VariableDB.getLocatedVariable(guess : String; addr : VariableAddress) : Entity;
var
	i      : Variable;
	pom    : Entity;
    latest : LongInt;
begin
    pom := buildNull();
    if Layers[addr.Layer].Content[addr.Location].VarName = guess then
	    pom := Layers[addr.Layer].Content[addr.Location].StoredVar;
	Result := pom;
end;

function VariableDB.isVarAssigned(guess : String) : Boolean;
var
	res  : Boolean;
	tk   : Variable;
    i    : LongInt;
begin
	res := false;
    for i := Length(Layers)-1 downto 0 do
    begin
	    for tk in Layers[i].Content do 
	    	if (tk.VarName = guess) then
	    	begin
	    		res := true;
	    		break;
	    	end;
        if res then break;
    end;
	Result := res;
end;

function VariableDB.locateVariable(guess : String) : VariableAddress;
var
	res  : VariableAddress;
    i, j : LongInt;
    flag : Boolean;
begin
    flag := False;
	res.Layer    := -1;
    res.Location := -1;
    for i := Length(Layers)-1 downto 0 do
    begin
	    for j := 0 to Length(Layers[i].Content)-1 do 
	    	if (Layers[i].Content[j].VarName = guess) then
	    	begin
	    		res.Layer    := i;
                res.Location := j;
                flag := True;
	    		break;
	    	end;
        if Flag then break;
    end;
	Result := res;
end;

function VariableDB.isLocalVarAssigned(guess : String) : Boolean;
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

function VariableDB.isGlobalVarAssigned(guess : String) : Boolean;
var
	res    : Boolean;
	tk     : Variable;
begin
	res := false;
	for tk in Layers[0].Content do 
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

// ASCII 
// 33     !
// 45     -
// 46     .
// 48-57  0-9
// 65-90  A-Z
// 95     _
// 97-122 a-z
function isValidForVariables(guess : String) : Boolean;
var
    i    : LongInt;
    flag : Boolean = False;
    chr  : LongInt;
begin
    if (Length(guess) > 0) then
    begin
        flag := True;
        chr := ord(guess[1]);
        if (chr < 45) 
        or (chr in [47..64])
        or (chr in [91..94])
        or (chr = 96)
        or (chr >= 123) 
        then begin
            flag := False;
        end;
        if flag then
        begin
            for i := 2 to Length(guess) do 
            begin
                chr := ord(guess[i]);
                if (chr < 45) 
                or (chr = 47) 
                or (chr in [58..64])
                or (chr in [91..94])
                or (chr = 96)
                or (chr >= 123) 
                then begin
                    flag := False;
                    break;
                end;
            end;
        end;
    end else begin
        flag := False;
    end;
    Result := flag;
end;

end.

