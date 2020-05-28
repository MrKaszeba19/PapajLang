unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, Process, SysUtils;

function PS_parseString(input : string) : String;

implementation

uses UnitStack, UnitEnvironment;

function PS_parseString(input : string) : String;
var
    res : String;
    env : PSEnvironment;
begin
    if (Length(input) > 0) then 
    begin
        //input := cutShebang(input); 
        input := cutCommentMultiline(input);
        input := cutCommentEndline(input);
    end;
    env := buildNewEnvironment();
    env.Stack := parseOpen(input, env.Stack, env.Settings, env.Variables);
    res := stack_show(env.Stack[0], env.Settings.Mask);
    disposeEnvironment(env);
    if (env.Settings.Prevent) then res := '';
    PS_parseString := res;
end;

end.
