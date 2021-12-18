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
    if (Length(input) > 0) then 
    begin
        //input := cutShebang(input); 
        input := cutCommentMultiline(input);
        input := cutCommentEndline(input);
    end;
    case checkParentheses(input) of 
        -1 : raiserror('ESyntax:CQuotes: Wrong amount of quotation marks. Quotes are not closed.');
        0  : raiserror('ESyntax:CLevels: Wrong amount of braces and/or parentheses.');
        1  : begin
            env := buildNewEnvironment(LoadAll);
            Params := obtainParams(startFrom);
            assignParameters(env, Params);
            env.Stack := parseOpen(input, env.Stack, env.Settings, env.Variables);
            res := stack_show(env.Stack[0], env.Settings.Mask);
            SetLength(Params, 0);
            disposeEnvironment(env);
            if (env.Settings.Prevent) then res := '';
            Result := res;
        end;
    end; 
end;

end.
