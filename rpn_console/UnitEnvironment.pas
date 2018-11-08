unit UnitEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UnitEntity, UnitStack;

type PSEnvironment = record
    Stack     : StackDB;
    Settings  : TSettings;
    Variables : VariableDB;
end;

function buildNewEnvironment() : PSEnvironment;

implementation

function buildNewEnvironment() : PSEnvironment;
var
	env : PSEnvironment;
begin
	env.Stack := stack_null();
	env.Settings := default_settings();
    env.Variables := createVariables();
    buildNewEnvironment := env;
end;

end.

