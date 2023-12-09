unit Unit2;

{$mode objfpc}{$H+}

interface

uses
    Classes, Process, SysUtils, UnitEnvironment;


//function PS_parseString(input : string; LoadAll : Boolean = false) : String;
function PS_parseString(input : string; LoadAll : Boolean = false; startFrom : LongInt = -1) : String;

implementation

uses UnitStack;

procedure raiserror(Const msg : string);  
begin  
  raise exception.create(Msg) at  
  get_caller_addr(get_frame),  
  get_caller_frame(get_frame);  
end; 

//function PS_parseString(input : string; LoadAll : Boolean = false) : String;
function PS_parseString(input : string; LoadAll : Boolean = false; startFrom : LongInt = -1) : String;
var
    res    : String;
    env    : PSEnvironment;
    Params : TParams;
begin
    env.Create(LoadAll);
    env.assignParams(startFrom);
    env.runFromString(input);
    res := stack_show(env.Stack[0], env.Settings.Mask);
    SetLength(Params, 0);
    env.Destroy;
    if (env.Settings.Prevent) then res := '';
    Result := res;
end;

end.
