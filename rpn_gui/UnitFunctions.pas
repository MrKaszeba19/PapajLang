unit UnitFunctions;

{$mode objfpc}{$H+}
	
//{$ZEROBASEDSTRINGS ON}

interface

uses
	Classes, SysUtils, StrUtils, Process,
	UnitStack, UnitEntity, UnitVariables;

const
	PI = 3.1415926535897932384626433832795;
	EU = 2.7182818284590452353602874713526;
	FI = 1.6180339887498948482045868343656;
    EM = 0.5772156649015328606065120900824;

function read_sourcefile(filename : String; var pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;

function lib_ultravanilla(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_math(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_strings(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_directives(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_constants(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_variables(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_variables2(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_logics(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_consolemanipulators(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_exceptions(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_arrays(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_files(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_datetime(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;

implementation

uses Unit5, MathUtils, Math, DTUtils, ArrayUtils, StringUtils, ConsoleUtils,
    {$IFDEF MSWINDOWS}
		ShellApi, crt,
    {$ELSE}
        UnixCrt,
 	{$ENDIF}
    UnitEnvironment, DateUtils;

//function lib_template(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
//var
//	Found : Boolean;
//begin
//	Found := true;
//	case i of
//	
//	end;
//	lib_template := Found;
//end;

function read_sourcefile(filename : String; var pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;
var
  fun, S : String;
  fp     : Text;
begin
  fun := '';
  assignfile(fp, filename);
  reset(fp);
  while not eof(fp) do
  begin
    readln(fp, S);
    if (S <> '') then S := trim(S);
    fun := fun + #10 + S;
  end;
  closefile(fp);
  pocz := parseScoped(fun, pocz, sets, vardb);
  read_sourcefile := pocz;
end;

// COMMANDS' EXECUTION


function lib_ultravanilla(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found          : Boolean;
	x, y, z, a     : Extended;
	Size           : Longint;
	index          : Longint;
	IntEax         : LongInt;
	StrEbx         : String;
	StrEcx         : String;
	EntEax, EntEbx : Entity;
	ExtEax         : Extended;
    LogEax         : Boolean;
	HelpETable     : array of Entity;
begin
	Found := true;
	case i of
    	// binary
    	'+' : begin
            EntEbx := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], EntEax + EntEbx);
    	end;
    	'-' : begin
            EntEbx := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
            if (not sets.InfMode) or (EntEax.EntityType <> TNUM) or (EntEbx.EntityType <> TNUM) then
            begin
                stack_push(pocz[sets.StackPointer], EntEax - EntEbx);    
            end else begin
                if ((EntEax.Num = Infinity) or (EntEbx.Num = -Infinity)) and ((EntEbx.Num = Infinity) or (EntEax.Num = -Infinity)) then
                begin
                    stack_push(pocz[sets.StackPointer], buildNumber(NaN));
                end else begin
                    stack_push(pocz[sets.StackPointer], EntEax - EntEbx);
                end;
            end;
        end;
        '*' : begin
            EntEbx := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], EntEax * EntEbx);
        end;
        '/' : begin
            EntEbx := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
            if (not sets.InfMode) then
            begin
                if isZero(EntEbx)
                    then stack_push(pocz[sets.StackPointer], raiseDivisionZero('/'))
                    else stack_push(pocz[sets.StackPointer], EntEax / EntEbx);
            end else begin
                if isNumber(EntEbx) then
                begin
                    if isZero(EntEbx) then
                    begin
                        stack_push(pocz[sets.StackPointer], EntEax * buildNumber(Infinity));
                    end else if isZero(EntEbx) and isZero(EntEax) then
                    begin
                        stack_push(pocz[sets.StackPointer], buildNumber(NaN));
                    end else if isAnyInfinity(EntEbx) and isAnyInfinity(EntEax) then
                    begin
                        stack_push(pocz[sets.StackPointer], buildNumber(NaN));
                    end else if isPosInfinity(EntEbx) then
                    begin
                        stack_push(pocz[sets.StackPointer], buildNumber(0));
                    end else if isNegInfinity(EntEbx) then
                    begin
                        stack_push(pocz[sets.StackPointer], buildNumber(0));
                    end else begin
                        stack_push(pocz[sets.StackPointer], EntEax / EntEbx);
                    end;
                end else begin
                    stack_push(pocz[sets.StackPointer], EntEax / EntEbx);
                end;
            end;
        end;
        '^' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //if (y = ftrunc(y)) then begin
            if (isInteger(y)) then begin
            	z := pow(x,y);
            end else begin
                z := pow2(x,y);
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'pow' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (isInteger(y)) then begin
            	z := pow(x,y);
            end else begin
            	z := pow2(x,y);
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'log' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(x)/ln(y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'exp' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := exp(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'ln' : begin
          	if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'root' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := pow2(x,1/y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'mod' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := fmod(x,y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'div' : begin
            //if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //z := trunc(x) div trunc(y);
            z := fdiv(x,y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'cdiv' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if x < 0 then begin
            	z := ffloor(x/y);
            end else begin
            	z := ffloor(x/y);
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'cmod' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (x > 0) and (y < 0) then begin
            	z := ((trunc(x) mod trunc(y)) + trunc(y+y)) mod trunc(y);
            end else if (x < 0) and (y > 0) then begin
            	z := ((trunc(x) mod trunc(y)) + trunc(y)) mod trunc(y);
            end else begin
            	z := trunc(x) mod trunc(y);
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'shr' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := trunc(x) shr trunc(y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'shl' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := trunc(x) shl trunc(y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        


        // constants
        'NULL' : begin
          stack_push(pocz[sets.StackPointer], buildNull());
        end;
        '[]' : begin
            stack_push(pocz[sets.StackPointer], buildNewArray(pocz, sets, 0));
        end;
        'INF' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(Infinity));
        end;
        '+INF' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(Infinity));
        end;
        '-INF' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(-Infinity));
        end;
        'NaN' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(NaN));
        end;

        // unary
		'trunc' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ftrunc(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'frac' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(ffrac(z)));
        end;
		'sqrt' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := sqrt(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'inc' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := y + 1;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'dec' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := y - 1;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        '++' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := y + 1;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        '--' : begin
        	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := y - 1;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;

        // String operations
        
        'call' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
            EntEax := stack_pop(pocz[sets.StackPointer]);
            //pocz := parseScoped(StrEbx, pocz, sets, vardb);
            doFunction(EntEax, pocz, sets, vardb);
        end;
        'callIf' : begin
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, TBOO, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TFUN) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                EntEax := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num = 0 then 
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(EntEax, pocz, sets, vardb);
            end else begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num = 0 then begin
                    if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                    EntEax := stack_pop(pocz[sets.StackPointer]);
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(EntEax, pocz, sets, vardb);
                end else begin
                    if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
            end;
        end;   
        'callUnless' : begin
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, TBOO, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TFUN) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                EntEax := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num <> 0 then 
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(EntEax, pocz, sets, vardb);
            end else begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num <> 0 then begin
                    if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                    EntEax := stack_pop(pocz[sets.StackPointer]);
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(EntEax, pocz, sets, vardb);
                end else begin
                    if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
            end;
        end;   
        'callTimes' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            for IntEax := 1 to trunc(y) do
                pocz := parseScoped(StrEbx, pocz, sets, vardb);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end; 
        //callLoop         

        'break' : begin
            sets.KeepWorking := 0;
        end;
        'continue' : begin
            sets.KeepWorking := 1;
        end;

        'rand' : begin
            z := random();
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'random' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_get(pocz[sets.StackPointer]).Num;
            stack_pop(pocz[sets.StackPointer]);
            z := random(trunc(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;

        // single operands
        'scan' : begin
            EntEax := scan_value();
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'scannum' : begin
            EntEax := scan_number();
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'scanstr' : begin
            EntEax := scan_string();
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'times' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_get(pocz[sets.StackPointer]).Num;
            stack_pop(pocz[sets.StackPointer]);
            if (y >= 0) then Steps := trunc(y);
        end;
        'tilleof' : begin
          	Steps := -1;
        end;
        'clone' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'type' : begin
          	EntEax := stack_get(pocz[sets.StackPointer]);
          	if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
          	stack_push(pocz[sets.StackPointer], buildString(getEntityTypeName(EntEax.EntityType)));
        end;
        'toString' : begin
          	EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TVEC) then
            begin
                stack_push(pocz[sets.StackPointer], buildString(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask))); 
            end else if (EntEax.EntityType = TNUM) then
            begin
                stack_push(pocz[sets.StackPointer], buildString(FormatFloat(sets.Mask, EntEax.Num)));
            end else begin
                stack_push(pocz[sets.StackPointer], buildString(EntEax.Str));
            end;
        end;
        'toNumber' : begin
            //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            val(EntEax.Str, ExtEax, IntEax); 
            if (IntEax = 0) then begin
             	stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
            end else begin
            	if (sets.StrictType) and (EntEax.EntityType <> TBOO) and (EntEax.EntityType <> TDAT) 
                    then stack_push(pocz[sets.StackPointer], buildException('EType:CNonNumeric: Got a non-numeric entity at "toNumber".'))
            	    else stack_push(pocz[sets.StackPointer], buildNumber(EntEax.Num));
            end;
        end;
        'toBoolean' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.Num = 0 then LogEax := true else LogEax := false;
            stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
        end;
        'toArray' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            // expection about boundaries
            if (IntEax > stack_size(pocz[sets.StackPointer])) then IntEax := stack_size(pocz[sets.StackPointer]); 
            stack_push(pocz[sets.StackPointer], buildNewArray(pocz, sets, IntEax));
        end;
        'makeChar' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_get(pocz[sets.StackPointer]).Num;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildString(Chr(trunc(y))));
        end;
        'getAscii' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEcx := stack_get(pocz[sets.StackPointer]).Str;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(Ord(StrEcx[1])));
        end;
        'isNumber' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TNUM 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isBoolean' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TBOO
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isString' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TSTR 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isArray' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TVEC 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isNull' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TNIL 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isException' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TEXC 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isLogicalExpression' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TEXP 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isFunction' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TFUN 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        'isDateTime' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if EntEax.EntityType = TDAT 
                then stack_push(pocz[sets.StackPointer], buildBoolean(True))
                else stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        //
        'length' : begin
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, TVEC, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                IntEax := trunc(stack_get(pocz[sets.StackPointer]).Num);
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(stack_size(pocz[IntEax])));
            end else begin
                StrEbx := stack_get(pocz[sets.StackPointer]).Str;
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEbx)));
            end;
        end;
        //'len' : begin
        //  	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
        //    StrEax := stack_get(pocz[sets.StackPointer]).Str;
        //    //if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
        //    stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEax)));
        //end;
        'print' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writeOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writeOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
            if (EntEax.EntityType = TDAT) then writeOnConsole(EntEax.Str);
        end;
        'println' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writelnOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writelnOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
            if (EntEax.EntityType = TDAT) then writelnOnConsole(EntEax.Str);
        end;
        'rprint' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writeOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writeOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
            if (EntEax.EntityType = TDAT) then writeOnConsole(EntEax.Str);
        end;
        'rprintln' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writelnOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writelnOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
            if (EntEax.EntityType = TDAT) then writelnOnConsole(EntEax.Str);
        end;
        'colprint' : begin
        	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then write(EntEax.Str : trunc(y));
            if (EntEax.EntityType = TNUM) then write(FormatFloat(sets.Mask, EntEax.Num) : trunc(y));
            if (EntEax.EntityType = TSTR) then write(EntEax.Str : trunc(y));
            if (EntEax.EntityType = TNIL) then write(EntEax.Str : trunc(y));
            if (EntEax.EntityType = TVEC) then write(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask) : trunc(y));
            if (EntEax.EntityType = TDAT) then write(EntEax.Str : trunc(y));
        end;
        'colprintln' : begin
        	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TNUM) then writeln(FormatFloat(sets.Mask, EntEax.Num) : trunc(y));
            if (EntEax.EntityType = TSTR) then writeln(EntEax.Str : trunc(y));
            if (EntEax.EntityType = TNIL) then writeln(EntEax.Str : trunc(y));
            if (EntEax.EntityType = TBOO) then writeln(EntEax.Str : trunc(y));
            if (EntEax.EntityType = TVEC) then writeln(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask) : trunc(y));
            if (EntEax.EntityType = TDAT) then writeln(EntEax.Str : trunc(y));
        end;
        'newln' : begin
            writelnOnConsole('');
        end;
        'status' : begin
            writeOnConsole(stack_show(pocz[sets.StackPointer], sets.Mask));
        end;
        'statusln' : begin
            writelnOnConsole(stack_show(pocz[sets.StackPointer], sets.Mask));
        end;
        'autocolstatus' : begin
            writeOnConsole(stack_showBeautiful(pocz[sets.StackPointer], sets.Mask));
        end;
        'autocolstatusln' : begin
            writelnOnConsole(stack_showBeautiful(pocz[sets.StackPointer], sets.Mask));
        end;
        'statusfull' : begin
            writelnOnConsole(stack_showFull(pocz[sets.StackPointer]));
        end;
        'getchar' : begin
            readln();
        end;
        'rem' : begin
        	stack_justpop(pocz[sets.StackPointer]);
        end;
        'frontrem' : begin
        	stack_justpopFront(pocz[sets.StackPointer], 0);
        end;
        'qshift' : begin
        	stack_push(pocz[sets.StackPointer], stack_firstpop(pocz[sets.StackPointer]));
        end;
        'clear' : begin
        	stack_clear(pocz[sets.StackPointer]);
        end;
        'keep' : begin
          	if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
          	y := stack_pop(pocz[sets.StackPointer]).Num;
          	SetLength(HelpETable, trunc(y));
          	for index := 0 to trunc(y)-1 do HelpETable[index] := stack_pop(pocz[sets.StackPointer]);
          	stack_clear(pocz[sets.StackPointer]);
          	for index := trunc(y)-1 downto 0 do stack_push(pocz[sets.StackPointer], HelpETable[index]);
          	SetLength(HelpETable, 0);
        end;
        'copy' : begin
          	if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_get(pocz[sets.StackPointer]).Num;
            stack_pop(pocz[sets.StackPointer]);
            SetLength(HelpETable, trunc(y));
            for index := 0 to trunc(y)-1 do HelpETable[index] := stack_pop(pocz[sets.StackPointer]);
            for index := trunc(y)-1 downto 0 do stack_push(pocz[sets.StackPointer], HelpETable[index]);
            for index := trunc(y)-1 downto 0 do stack_push(pocz[sets.StackPointer], HelpETable[index]);
            SetLength(HelpETable, 0);
        end;
        'mcopy' : begin
          	if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
          	y := stack_get(pocz[sets.StackPointer]).Num;
          	stack_pop(pocz[sets.StackPointer]);
          	SetLength(HelpETable, trunc(y));
          	for index := 0 to trunc(y)-1 do begin
          		HelpETable[index] := stack_get(pocz[sets.StackPointer]);
          		stack_pop(pocz[sets.StackPointer]);
          	end;
          	for index := trunc(y)-1 downto 0 do stack_push(pocz[sets.StackPointer], HelpETable[index]);
          	for index := 0 to trunc(y)-1 do stack_push(pocz[sets.StackPointer], HelpETable[index]);
          	SetLength(HelpETable, 0);
        end;
        'sort' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TNUM) then
            begin
                //if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                if (size >= 0) then
                begin
                    SetLength(HelpETable, size);
                    if (sets.Autoclear) then 
                        HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                    else 
                        HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                    if (sets.sorttype = 0) then bubblesort(HelpETable);
                    if (sets.sorttype = 1) then quicksort(HelpETable);
                    if (sets.sorttype = 2) then mergesort(HelpETable);
                    if (sets.sorttype = 3) then bogosort(HelpETable);
                    stack_pushCollection(pocz[sets.StackPointer], HelpETable);
                    SetLength(HelpETable, 0);
                end;
            end else Found := false;
        end;
        'numsort' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                if (sets.sorttype = 0) then bubblesort(HelpETable);
                if (sets.sorttype = 1) then quicksort(HelpETable);
                if (sets.sorttype = 2) then mergesort(HelpETable);
                if (sets.sorttype = 3) then bogosort(HelpETable);
                stack_pushCollection(pocz[sets.StackPointer], HelpETable);
                SetLength(HelpETable, 0);
            end;
        end;
		'strsort' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                strings_sort(HelpETable);
                stack_pushCollection(pocz[sets.StackPointer], HelpETable);
                SetLength(HelpETable, 0);
            end;
        end;
        'reverse' : begin
            pocz[sets.StackPointer] := stack_reverse(pocz[sets.StackPointer]);
        end;
        'rev' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then stack_reverseCollection(pocz[sets.StackPointer], size);
        end;
        'swap' : begin
            stack_reverseCollection(pocz[sets.StackPointer], 2);
        end;             


        // stack operands
        'sum' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
          	size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
          	if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_sum(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNumber(0.0));
            end;
        end;
        'product' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_product(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNumber(1.0));
            end;
        end;
        'count' : begin
          	z := 0.0;
          	while (stack_size(pocz[sets.StackPointer]) > 0) do
          	begin
          		z := z + 1;
          		stack_justpop(pocz[sets.StackPointer]);
          	end;
          	stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'size' : begin
          	z := stack_size(pocz[sets.StackPointer]);
          	stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'all' : begin
          	z := stack_size(pocz[sets.StackPointer]);
          	stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'avg' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_avg(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNumber(0.0));
            end;
        end;
        'mean' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_avg(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNumber(0.0));
            end;
        end;
        'min' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_min(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNull());
            end;
        end;
        'max' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_max(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNull());
            end;
        end;
        'median' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_median(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNull());
            end;
        end;
        'variance' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_variance(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNull());
            end;
        end;
        'stddev' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            size := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (size >= 0) then
            begin
                SetLength(HelpETable, size);
                if (sets.Autoclear) then 
                    HelpETable := stack_popCollection(pocz[sets.StackPointer], size)
                else 
                    HelpETable := stack_getCollection(pocz[sets.StackPointer], size);
                ExtEax := table_stddev(HelpETable);
                stack_push(pocz[sets.StackPointer], buildNumber(sqrt(ExtEax)));
                SetLength(HelpETable, 0);
            end else begin
                stack_push(pocz[sets.StackPointer], buildNull());
            end;
          end;
             
        // stack creators
        'seq' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
          	z := stack_get(pocz[sets.StackPointer]).Num;
            stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
          	y := stack_get(pocz[sets.StackPointer]).Num;
            stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_get(pocz[sets.StackPointer]).Num;
            stack_pop(pocz[sets.StackPointer]);
            if (x <= z) then
            begin
            	while (x <= z) do 
            	begin
                    //checkSIGINT();
            		stack_push(pocz[sets.StackPointer], buildNumber(x));
            		x := x + y;
            	end;
            end else begin
            	while (x >= z) do 
            	begin
                    //checkSIGINT();
            		stack_push(pocz[sets.StackPointer], buildNumber(x));
            		x := x - y;
            	end;
            end;
        end;
        'seql' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            a := 1.0;
          	while (a <= z) do 
            begin
                //checkSIGINT();
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	x := x + y;
            	a := a + 1.0;
            end;
        end;
        'gseq' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (x <= z) then
            begin
              while (x <= z) do 
              begin
                //checkSIGINT();
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                x := x * y;
              end;
            end else begin
              while (x >= z) do 
              begin
                //checkSIGINT();
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                x := x / y;
              end;
            end;
        end;
        'gseql' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            a := 1.0;
          	while (a <= z) do 
            begin
                //checkSIGINT();
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	x := x * y;
            	a := a + 1.0;
            end;
        end;

        else begin
            Found := false;
            {*
            case LeftStr(i, 1) of
            	'X' : begin
              		if (RightStr(i, Length(i)-1) = '*') and (not (Unit5.is_gui)) then Steps := -1
              		else if (RightStr(i, Length(i)-1) <> '') then Steps := StrToInt(RightStr(i, Length(i)-1))
                    else stack_push(pocz[sets.StackPointer], buildString('X'));
              	end;
              	else begin
                	Found := false;
              	end;
            end;
            *}
        end;
    end;
    lib_ultravanilla := Found;
end;

function lib_math(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found        : Boolean;
	x, y, z, w   : Extended;
    index, jndex : LongInt;
    ArrEax       : Entity;
begin
	Found := true;
	case i of

		// binary
		'Math.choose' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (x = trunc(x)) then begin
                z := newton_int(x,y);
            end else begin
                z := newton_real(x,y);
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
        	stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.gcd' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := gcd(x, y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.lcm' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := lcm(x, y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;

		// unary
        'Math.sqr' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := y*y;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.cub' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := y*y*y;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.inv' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1/y;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
		'Math.exp' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := exp(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
		'Math.log' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(x)/ln(y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
		'Math.lbin' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := log2(x);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.ldec' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := log10(x);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.lhex' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(x)/ln(16);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.cbrt' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if y = 0 then begin
                z := 0;
            end else if y < 0 then begin
                z := -exp(ln(-y)/3);
            end else begin
                z := exp(ln(y)/3);
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.abs' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := abs(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.sign' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (y = 0)
                then z := 0
                else z := y/abs(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.sgn' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (y = 0)
                then z := 0
                else z := y/abs(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.!' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := fact(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.factorial' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := fact(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.ln' : begin
          	if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.log2' : begin
          	if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := log2(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.log10' : begin
          	if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := log10(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.log16' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(x)/ln(16);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.floor' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ffloor(y);
			//if (y = trunc(y)) then z := trunc(y)
			//else if (y < 0) then z := trunc(y)-1 else z := trunc(y);
            //if (y = ftrunc(y)) 
            //    then z := ftrunc(y)
			//    else if (y < 0) 
            //        then z := ftrunc(y)-1 
            //        else z := ftrunc(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.ceiling' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := fceiling(y);
            //if (y = trunc(y)) then z := trunc(y)
			//else if (y < 0) then z := trunc(y) else z := trunc(y)+1;
            //if (y = ftrunc(y)) 
            //    then z := ftrunc(y)
			//    else if (y < 0) 
            //        then z := ftrunc(y) 
            //        else z := ftrunc(y)+1;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.trunc' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ftrunc(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.round' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := fround(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
		'Math.fibonacci' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := fib(trunc(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;

        // Trigonometrics

        'Math.sin' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := sin(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.cos' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := cos(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.csc' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1/sin(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.sec' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1/cos(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.tan' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := sin(y)/cos(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.cot' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := cos(y)/sin(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arcsin' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arcsin(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arccos' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arccos(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arctan' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arctan(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arccot' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := PI/2-arctan(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arcsec' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1.0 / y;
            z := arccos(z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arccsc' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1.0 / y;
            z := arcsin(z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.sinh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := sinh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.cosh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := cosh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.csch' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1/sinh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.sech' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 1/cosh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.tanh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := sinh(y)/cosh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.coth' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := cosh(y)/sinh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arsinh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arsinh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arcosh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arcosh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.artanh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := artanh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arcoth' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 0.5*ln((y+1)/(y-1));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arsech' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln((1 + sqrt(1 - y*y)) / y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arcsch' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := ln(1.0/y + sqrt(1.0/(y*y) + 1));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.toRadians' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := degtorad(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.toDegrees' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := radtodeg(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;


		// boolean functions for numbers
        'Math.isPrime' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(isPrime(y)));
        end;
		'Math.isEven' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            //stack_push(pocz[sets.StackPointer], buildBoolean(trunc(y) mod 2 = 0));
            stack_push(pocz[sets.StackPointer], buildBoolean(fmod(y, 2) = 0));
        end;
		'Math.isOdd' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            //stack_push(pocz[sets.StackPointer], buildBoolean(trunc(y) mod 2 = 1));
            stack_push(pocz[sets.StackPointer], buildBoolean(fmod(y, 2) = 1));
        end;
		'Math.isInteger' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(isInteger(y)));
        end;
        'Math.isNatural' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean((isInteger(y)) and (y >= 0)));
        end;

        // constants
		'Math.PI' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(PI));
        end;
        'Math.EU' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(EU));
        end;
        'Math.FI' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(FI));
        end;
		'Math.EM' : begin
          stack_push(pocz[sets.StackPointer], buildNumber(EM));
        end;

        // Statistics
		'Math.gamma' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := vgamma(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.gammaln' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := LogGamma(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
		'Math.distNormStd' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := dstdnorm(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.funcNormStd' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fnorm(x, 0, 1);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomNormStd' : begin
            z := randg(0, 1);
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genNormStd' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                w := randg(0, 1);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
        end;
		'Math.distNorm' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dnorm(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.funcNorm' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fnorm(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomNorm' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := randg(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genNorm' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                w := randg(y, z);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.randomBinom' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            w := rbinom(trunc(y), z);
            stack_push(pocz[sets.StackPointer], buildNumber(w));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genBinom' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                w := rbinom(trunc(y), z);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.funcBinom' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fbinom(trunc(y), trunc(x), z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distBinom' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dbinom(trunc(y), trunc(x), z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;

        'Math.funcGeom' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fgeom(trunc(x), y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distGeom' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dgeom(trunc(x), y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomGeom' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := rgeom(x);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.genGeom' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                w := rgeom(y);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;

        'Math.funcExp' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fexp(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distExp' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dexp(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomExp' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := rexp(x);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.genExp' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                w := rexp(y);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Math.funcPoisson' : begin
            if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fpoisson(trunc(x), trunc(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distPoisson' : begin
			if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dpoisson(trunc(x), trunc(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomPoisson' : begin
			if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := rpoisson(trunc(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.genPoisson' : begin
            if (sets.StrictType) and (assertPositiveNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                w := rpoisson(trunc(y));
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Math.funcGamma' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fgamma(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distGamma' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dgamma(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomGamma' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //z := rgamma1(x, y);
            z := rgengamma(0, 1/y, x);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genGamma' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                //w := rgengamma(1/z, y, 1);
                //w := rgamma1(y, z);
                w := rgengamma(0, 1/z, y);
                //w := rgengamma(z, y, 1);
                // do poprawki
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.funcChiSq' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fgamma(x, y/2, 0.5);
            //w := fchisq(x, trunc(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distChiSq' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dgamma(x, y/2, 0.5);
            //w := dchisq(x, trunc(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomChiSq' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //if ftrunc(x) = 1 
            //    then z := rgamma1(x/2, 0.5)
            //    else z := rgengamma(0, 2, x/2);
            z := rchisq(x);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genChiSq' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                //w := rgengamma(1/z, y, 1);
                //if ftrunc(y) = 1 
                //    then w := rgamma1(y/2, 0.5)
                //    else w := rgengamma(0, 2, y/2);
                w := rchisq(y);
                // do poprawki
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.funcErlang' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := ferlang(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distErlang' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := derlang(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomErlang' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //z := rgamma1(x, y);
            //z := rgengamma(0, 1/y, x);
            z := rerlang(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genErlang' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                //w := rgengamma(1/z, y, 1);
                //w := rgamma1(y, z);
                //w := rgengamma(0, 1/z, y);
                w := rerlang(y, z);
                //w := rgengamma(z, y, 1);
                // do poprawki
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;

        'Math.funcT' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //w := fgamma(x, y/2, 0.5);
            w := fstudentt(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distT' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            //w := dgamma(x, y/2, 0.5);
            w := dstudentt(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomT' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := rstudentt(x);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genT' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                w := rstudentt(y);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Math.funcBeta' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := fbeta(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distBeta' : begin
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dbeta(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomBeta' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := rbeta(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genBeta' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                //w := rgengamma(1/z, y, 1);
                //w := rgamma1(y, z);
                w := rbeta(y, z);
                //w := rgengamma(z, y, 1);
                // do poprawki
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.funcF' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := ffischerf(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.distF' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            w := dfischerf(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomF' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := rfischerf(x, y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genF' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            z := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                w := rfischerf(y, z);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.moment' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            index := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_moment(pocz[trunc(ArrEax.Num)].Values, index)));
        end;
        'Math.quantile' : begin
            if (sets.StrictType) and (assertNotNegativeLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 1) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile(pocz[trunc(ArrEax.Num)].Values, x)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 1));
            end;
        end;
        'Math.tertile' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 3) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile2(pocz[trunc(ArrEax.Num)].Values, x, 3)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 3));
            end;
        end;
        'Math.quartile' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 4) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile2(pocz[trunc(ArrEax.Num)].Values, x, 4)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 4));
            end;
        end;
        'Math.octile' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 8) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile2(pocz[trunc(ArrEax.Num)].Values, x, 8)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 8));
            end;
        end;
        'Math.decile' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 10) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile2(pocz[trunc(ArrEax.Num)].Values, x, 10)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 10));
            end;
        end;
        'Math.hexadecile' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 16) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile2(pocz[trunc(ArrEax.Num)].Values, x, 16)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 16));
            end;
        end;
        'Math.percentile' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (x >= 0) and (x <= 100) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(table_quantile2(pocz[trunc(ArrEax.Num)].Values, x, 100)));
            end else begin
                stack_push(pocz[sets.StackPointer], raiseNumRangeConstraint(i, 0, 100));
            end;
        end;


        // Number Theory
        'Math.genNaturalDivisors' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            index := 1;
            while (index*index <= x) do
            begin
                if (divides(ftrunc(x), index)) then 
                begin
                    stack_push(pocz[sets.StackPointer], buildNumber(index));
                    if (index*index <> x) then stack_push(pocz[sets.StackPointer], buildNumber(x/index));
                end;
                index := index + 1;
            end;
        end;
        'Math.genDivisors' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            index := 1;
            while (index*index <= x) do
            begin
                if (divides(ftrunc(x), index)) then 
                begin
                    stack_push(pocz[sets.StackPointer], buildNumber(index));
                    stack_push(pocz[sets.StackPointer], buildNumber(-index));
                    if (index*index <> x) then begin 
                        stack_push(pocz[sets.StackPointer], buildNumber(x/index));
                        stack_push(pocz[sets.StackPointer], buildNumber(-x/index));
                    end;
                end;
                index := index + 1;
            end;
        end;
        'Math.divides' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(divides(x, y)));
        end;
        'Math.countNaturalDivisors' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_tau(x)));
        end;
        'Math.sumNaturalDivisors' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_sigma(x)));
        end;
        'Math.fTau' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_tau(x)));
        end;
        'Math.fSigma' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_sigma(x)));
        end;
        'Math.fMobius' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_mobius(x)));
        end;
        'Math.fMu' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_mobius(x)));
        end;
        'Math.fEuler' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_euler(x)));
        end;
        'Math.fPhi' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_euler(x)));
        end;
        'Math.fPi' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_pi(x)));
        end;
        //'Math.factorize' : begin
        //    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
        //    x := stack_pop(pocz[sets.StackPointer]).Num;
        //    if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
        //    index := trunc(x);
        //    while (index > 1) do
        //    begin
        //        jndex := 2;
        //        while not (divides(index, jndex)) do jndex := jndex + 1;
        //        stack_push(pocz[sets.StackPointer], buildNumber(jndex));
        //        index := index div jndex;
        //    end;
        //end;
        'Math.primeDistribution' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            while (x > 1) do
            begin
                if (isPrime(x)) then
                begin
                    stack_push(pocz[sets.StackPointer], buildNumber(x));
                    //stack_push(pocz[sets.StackPointer], buildNumber(1));
                    x := 1;
                end else begin
                    y := 2;
                    while not (divides(x, y)) do y := y + 1;
                    stack_push(pocz[sets.StackPointer], buildNumber(y));
                    x := x / y;
                end;
            end;
        end;
        'Math.leastPrimeDivisor' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if (isPrime(x)) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
            end else begin
                y := 2;
                while not (divides(x, y)) do y := y + 1;
                stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
        end;
        'Math.factorize' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if (isPrime(x)) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                stack_push(pocz[sets.StackPointer], buildNumber(1));
            end else begin
                y := 2;
                while not (divides(x, y)) do y := y + 1;
                stack_push(pocz[sets.StackPointer], buildNumber(y));
                stack_push(pocz[sets.StackPointer], buildNumber(x/y));
            end;
        end;
        'Math.randomIntRange' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                stack_push(pocz[sets.StackPointer], buildNumber(y));
            end; 
            if (x < y) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(randomIntRange(x, y)));
            end else if (x = y) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
            end else begin
                stack_push(pocz[sets.StackPointer], buildNumber(randomIntRange(y, x)));
            end;
        end;
        'Math.randomRealRange' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                stack_push(pocz[sets.StackPointer], buildNumber(y));
            end; 
            if (x < y) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(randomRealRange(x, y)));
            end else if (x = y) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(NaN));
            end else begin
                stack_push(pocz[sets.StackPointer], buildNumber(randomRealRange(y, x)));
            end;
        end;
        'Math.euclidean' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                stack_push(pocz[sets.StackPointer], buildNumber(y));
            end; 
            w := gcdExtended(trunc(x), trunc(y), index, jndex);
            stack_push(pocz[sets.StackPointer], buildNumber(index));
            stack_push(pocz[sets.StackPointer], buildNumber(jndex));
        end;
        'Math.bezoutCoefs' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then begin
                stack_push(pocz[sets.StackPointer], buildNumber(x));
                stack_push(pocz[sets.StackPointer], buildNumber(y));
            end; 
            w := gcdExtended(trunc(x), trunc(y), index, jndex);
            stack_push(pocz[sets.StackPointer], buildNumber(index));
            stack_push(pocz[sets.StackPointer], buildNumber(jndex));
        end;
        'Math.erf' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            w := ferf(x);
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.erfc' : begin
			if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            w := ferfc(x);
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.fomega' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_omega(x)));
        end;
        'Math.fOmega' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_omega2(x)));
        end;
        'Math.countDistinctFactors' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_omega(x)));
        end;
        'Math.countAllFactors' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_omega2(x)));
        end;
        'Math.fLambda' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_liouville(x)));
        end;
        'Math.fLiouville' : begin
			if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            stack_push(pocz[sets.StackPointer], buildNumber(num_liouville(x)));
        end;
        'Math.isSquareFree' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(num_isSquareFree(y)));
        end;
		else begin
            Found := false;
        end;
	end;
	lib_math := Found;
end;

function lib_strings(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found          : Boolean;
	x, y           : Extended;
	index          : Longint;
	IntEax, IntEbx : LongInt;
	StrEax, StrEbx : String;
	StrEcx, StrEdx : String;
	ExtEax         : Extended;
    EntEax, EntEbx : Entity;
	HelpTStrings   : TStrings;
    HelpSTable     : array of String;
begin
	Found := true;
	case i of
		'String.concat' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(concat(StrEax, StrEbx)));
        end;
        'String.join' : begin
            if  (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) 
            and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TSTR) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEax));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(concat(StrEax, StrEbx)));
            end else Found := false;
        end;
        'String.crush' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                SetLength(HelpSTable, 0);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                IntEbx := 1;
                while (IntEbx <= Length(StrEbx)) do begin
                	SetLength(HelpSTable, IntEbx+1);
                	HelpSTable[IntEbx] := Copy(StrEbx, IntEbx, 1);
                	IntEbx := IntEbx + 1; 
                end;
                if not (sets.Autoclear) then begin
                  //stack_push(pocz[sets.StackPointer], buildNumber(y));
                  stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                for index := 1 to Length(HelpSTable)-1 do stack_push(pocz[sets.StackPointer], buildString(HelpSTable[index])); 
                SetLength(HelpSTable, 0);
            end else Found := false;
        end;
        'String.crushBy' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
                y := stack_pop(pocz[sets.StackPointer]).Num;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                IntEax := 1;
                IntEbx := 1;
                SetLength(HelpSTable, 0);
                while (IntEax <= Length(StrEbx)) do begin
                	SetLength(HelpSTable, IntEbx+1);
                	HelpSTable[IntEbx] := Copy(StrEbx, IntEax, trunc(y));
                	IntEax := IntEax + trunc(y);
                	IntEbx := IntEbx + 1; 
                end;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                for index := 1 to Length(HelpSTable)-1 do stack_push(pocz[sets.StackPointer], buildString(HelpSTable[index])); 
                SetLength(HelpSTable, 0);
            end else Found := false;
        end;
        'String.left' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
                y := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(LeftStr(StrEbx, trunc(y))));
            end else Found := false;
        end;
        'String.right' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(RightStr(StrEbx, trunc(y))));
            end else Found := false;
        end;
        'String.trim' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(Trim(StrEbx)));
            end else Found := False;
        end;
        'String.trimLeft' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(TrimLeft(StrEbx)));
            end else Found := False;
        end;
        'String.trimRight' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(TrimRight(StrEbx)));
            end else Found := False;
        end;
        'String.trimChars' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(TrimChars(StrEbx, StrEax[1])));
        end;
        'String.trimCharsLeft' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(TrimCharsLeft(StrEbx, StrEax[1])));
        end;
        'String.trimCharsRight' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(TrimCharsRight(StrEbx, StrEax[1])));
        end;
        'String.padLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
        	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(PadLeft(StrEbx, trunc(y))));
            end else Found := False;
        end;
        'String.padRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(PadRight(StrEbx, trunc(y))));
            end else Found := False;
        end;
        'String.pad' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(PadCenter(StrEbx, trunc(y))));
            end else Found := False;
        end;
        'String.padCharsLeft' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            //StrEcx := AddChar(StrEax[1], StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            //stack_push(pocz[sets.StackPointer], buildString(StrEcx));
            stack_push(pocz[sets.StackPointer], buildString(AddChar(StrEax[1], StrEbx, trunc(y))));
        end;
        'String.padCharsRight' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            //StrEcx := AddCharR(StrEax[1], StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            //stack_push(pocz[sets.StackPointer], buildString(StrEcx));
            stack_push(pocz[sets.StackPointer], buildString(AddCharR(StrEax[1], StrEbx, trunc(y))));
        end;
        'String.padChars' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            stack_push(pocz[sets.StackPointer], buildString(PadCharsCenter(StrEbx, trunc(y), StrEax[1])));
        end;
        'String.despace' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(DelChars(StrEbx, ' ')));
        end;
        'String.onespace' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(DelSpace1(StrEbx)));
        end;
        'String.dechar' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEcx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.bind' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //StrEcx := StrEax + ' ' + StrEbx;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax + ' ' + StrEbx));
        end;
        'String.bindBy' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEcx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //StrEdx := StrEax + StrEcx + StrEbx;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax + StrEcx + StrEbx));
        end;
        'String.splitBySpace' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_get(pocz[sets.StackPointer]).Str;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]); 
            HelpTStrings := TStringlist.Create;
            HelpTStrings.Delimiter := ' ';
            HelpTStrings.QuoteChar := '"';
            HelpTStrings.StrictDelimiter := false;
            HelpTStrings.DelimitedText := StrEbx;
            for StrEax in HelpTStrings do stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
            HelpTStrings.Free;
        end;
        'String.splitBy' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEcx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_get(pocz[sets.StackPointer]).Str;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);   
            HelpTStrings := TStringlist.Create;
            HelpTStrings.Delimiter := StrEcx[1];
            HelpTStrings.QuoteChar := '"';
            HelpTStrings.StrictDelimiter := false;
            HelpTStrings.DelimitedText := StrEbx;
            for StrEax in HelpTStrings do stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
            HelpTStrings.Free;
        end;
        'String.substring' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            //StrEax := Copy(StrEbx, trunc(x), trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(Copy(StrEbx, trunc(x), trunc(y)))); 
        end;
        'String.between' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                x := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                //StrEax := Copy(StrEbx, trunc(x), trunc(y)-trunc(x)+1);
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(x));
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(Copy(StrEbx, trunc(x), trunc(y)-trunc(x)+1))); 
            end else Found := false;
        end;
        'String.positionFirst' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //ExtEax := Pos(StrEbx, StrEax);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(Pos(StrEbx, StrEax) -1+sets.StringStart)); 
        end;
        'String.remove' : begin 
          	if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
          	begin
          	  	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
              	StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
              	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
              	StrEax := stack_pop(pocz[sets.StackPointer]).Str;
              	if not (sets.Autoclear) then begin
              	  	stack_push(pocz[sets.StackPointer], buildString(StrEax));
              	  	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
              	end;
                Delete(StrEax, Pos(StrEbx, StrEax), Length(StrEbx));
              	stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
          	end else begin
          	  	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
              	y := stack_pop(pocz[sets.StackPointer]).Num; // check
          	  	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
              	x := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
              	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
              	StrEax := stack_pop(pocz[sets.StackPointer]).Str;
              	if not (sets.Autoclear) then begin
              	  	stack_push(pocz[sets.StackPointer], buildString(StrEax));
              	  	stack_push(pocz[sets.StackPointer], buildNumber(x));
              	  	stack_push(pocz[sets.StackPointer], buildNumber(y));
              	end;
                Delete(StrEax, trunc(x), trunc(y)); 
              	stack_push(pocz[sets.StackPointer], buildString(StrEax));
          	end;
        end;
        'String.insert' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            Insert(StrEbx, StrEax, trunc(y));
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.replace' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            Delete(StrEax, Length(StrEbx), trunc(y)); 
            Insert(StrEbx, StrEax, trunc(y));
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.nthOccur' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(NPos(StrEbx, StrEax, trunc(y)) -1+sets.StringStart)); 
        end;
        'String.positionFrom' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //ExtEax := PosEx(StrEbx, StrEax, trunc(y)) -1+sets.StringStart;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(PosEx(StrEbx, StrEax, trunc(y)) -1+sets.StringStart)); 
        end;
		'String.positionLast' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //ExtEax := LastDelimiter(StrEbx, StrEax) -1+sets.StringStart;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(LastDelimiter(StrEbx, StrEax) -1+sets.StringStart)); 
        end;
        'String.occurs' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //IntEax := 0;
            //repeat
            //	IntEbx := NPos(StrEbx, StrEax, IntEax+1);
            //	if (IntEbx <> 0) then Inc(IntEax);
            //until (IntEbx = 0);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            //stack_push(pocz[sets.StackPointer], buildNumber(IntEax)); 
            stack_push(pocz[sets.StackPointer], buildNumber(OccurrencesOfSubstring(StrEax, StrEbx))); 
        end;
        'String.eval' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            pocz := parseScoped(StrEbx, pocz, sets, vardb);
            //stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.system' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := executeCommand(StrEbx, sets.Shell);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
		'String.length' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
          	    //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEax := stack_get(pocz[sets.StackPointer]).Str;
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEax)));
            end else Found := false;
        end;
        'String.value' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_get(pocz[sets.StackPointer]).Str;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            val(StrEax, ExtEax, IntEax); 
            if (IntEax = 0) then begin
              stack_push(pocz[sets.StackPointer], buildNumber(ExtEax));
            end else begin
              stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
        end;
		'String.inC' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := string_toC(StrEbx);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        //
        'String.cutLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
                y := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                StrEax := RightStr(StrEbx, Length(StrEbx)-trunc(y));
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end else Found := false;
        end;
        'String.cutRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                StrEax := LeftStr(StrEbx, Length(StrEbx)-trunc(y));
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end else Found := false; 
        end;
        'String.cutBothSides' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                y := stack_pop(pocz[sets.StackPointer]).Num;
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                x := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                //stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax-IntEbx));
                StrEax := Copy(StrEbx, trunc(x)+1, Length(StrEbx)-trunc(y)-trunc(x));
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(x));
                	stack_push(pocz[sets.StackPointer], buildNumber(y));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
            end else Found := false; 
        end;
        'String.lower' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(LowerCase(StrEbx)));
        end;
        'String.upper' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(UpperCase(StrEbx)));
        end;
        'String.uplower' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            StrEcx := LowerCase(StrEbx);
            if Length(StrEcx) > 0 then StrEcx[1] := capitalize(StrEcx[1]);
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.setAt' : begin
          	if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //StrEcx := StrEax;
            //StrEcx[trunc(y)] := StrEbx[1];
            //stack_push(pocz[sets.StackPointer], buildString(StrEcx)); 
            StrEax[trunc(y)] := StrEbx[1];
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.getAt' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            StrEbx := Copy(StrEax, trunc(y), 1);
            stack_push(pocz[sets.StackPointer], buildString(StrEbx)); 
        end;
        'String.compare' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            stack_push(pocz[sets.StackPointer], buildNumber(CompareStr(StrEax, StrEbx)));
        end;
        'String.split' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                x := stack_pop(pocz[sets.StackPointer]).Num +1-sets.StringStart;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                StrEax := LeftStr(StrEbx, trunc(x)-1);
                StrEcx := RightStr(StrEbx, Length(StrEbx)-trunc(x)+1);
                if not (sets.Autoclear) then begin
                	stack_push(pocz[sets.StackPointer], buildNumber(x));
                	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                end;
                stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
                stack_push(pocz[sets.StackPointer], buildString(StrEcx));
            end else Found := false; 
        end;
        'String.apostrophed' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
            stack_push(pocz[sets.StackPointer], buildString(QuotedStr(StrEax)));
        end;
        'String.quoted' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
            stack_push(pocz[sets.StackPointer], buildString(AnsiQuotedStr(StrEax, '"')));
        end;
        'String.enclosed' : begin
            //if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            case Length(StrEax) of
                0 : stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                1 : stack_push(pocz[sets.StackPointer], buildString(StrEax[1] + StrEbx + StrEax[1]));
                2 : stack_push(pocz[sets.StackPointer], buildString(StrEax[1] + StrEbx + StrEax[2]));
                else stack_push(pocz[sets.StackPointer], raiseStringMaxLength(i, StrEax, 2));
            end;
        end;
        'String.map' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                StrEbx := '';
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := StrEbx + stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.filter' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                StrEcx := 'sfilt_'+IntToStr(DateTimeToUnix(Now));
                StrEbx := '';
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    vardb.setLocalVariable(StrEcx, buildString(StrEax[index]));
                    pocz := parseOpen('$' + StrEcx + ' ' + EntEax.Str, pocz, sets, vardb);
                    if (trunc(stack_pop(pocz[sets.StackPointer]).Num) = 0) 
                        then StrEbx := StrEbx + StrEax[index];
    		    end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.cut' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                StrEcx := 'scutt_'+IntToStr(DateTimeToUnix(Now));
                StrEbx := '';
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    vardb.setLocalVariable(StrEcx, buildString(StrEax[index]));
                    pocz := parseOpen('$' + StrEcx + ' ' + EntEax.Str, pocz, sets, vardb);
                    if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) 
                        then StrEbx := StrEbx + StrEax[index];
    		    end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.reduce' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                EntEbx := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], EntEbx);
            end else Found := False;
        end;
        'String.reduceFromFirst' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                EntEbx := buildString(StrEax[1]);
                vardb.addLayer();
                for index := 2 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], EntEbx);
            end else Found := False;
        end;
        'String.reduceLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
                EntEbx := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], EntEbx);
            end else Found := False;
        end;
        'String.reduceLeftFromFirst' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                EntEbx := buildString(StrEax[1]);
                vardb.addLayer();
                for index := 2 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], EntEbx);
            end else Found := False;
        end;
        'String.reduceRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
                EntEbx := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                vardb.addLayer();
                for index := Length(StrEax) downto 1 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], EntEbx);
            end else Found := False;
        end;
        'String.reduceRightFromLast' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                EntEbx := buildString(StrEax[Length(StrEax)]);
                vardb.addLayer();
                for index := Length(StrEax)-1 downto 1 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], EntEbx);
            end else Found := False;
        end;
        'String.strreduce' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.strreduceFromFirst' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                StrEbx := StrEax[1];
                vardb.addLayer();
                for index := 2 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.strreduceLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                vardb.addLayer();
                for index := 1 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.strreduceLeftFromFirst' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                StrEbx := StrEax[1];
                vardb.addLayer();
                for index := 2 to Length(StrEax) do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.strreduceRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                vardb.addLayer();
                for index := Length(StrEax) downto 1 do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.strreduceRightFromLast' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                StrEbx := StrEax[Length(StrEax)];
                vardb.addLayer();
                for index := Length(StrEax)-1 downto 1 do
                begin
                    stack_push(pocz[sets.StackPointer], buildString(StrEbx));
                    stack_push(pocz[sets.StackPointer], buildString(StrEax[index]));
                    doFunction(EntEax, pocz, sets, vardb);
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end else Found := False;
        end;
        'String.first' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax[1])); 
            end else Found := False;
        end;
        'String.last' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax[Length(StrEax)])); 
            end else Found := False;
        end;

        // String.-
        'String.delete' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TSTR) then
            begin
                // STR1 STR2 delete
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax - StrEbx));
            end else if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TNUM) then
            begin
                // STR1 NUM1 delete
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax - trunc(ExtEax)));
            end else Found := False;
        end;
        // String.*
        'String.copies' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TNUM) and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TSTR) then
            begin
                // NUM1 STR1 copies
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
                stack_push(pocz[sets.StackPointer], buildString(StrEax * trunc(ExtEax)));
            end else if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TNUM) then
            begin
                // STR1 NUM1 copies
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax * trunc(ExtEax)));
            end else Found := False;
        end;
        // String./
        'String.divide' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TSTR) then
            begin
                // STR1 STR2 delete
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax / StrEbx));
            end else if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TSTR) and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TNUM) then
            begin
                // STR1 NUM1 delete
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
                ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildString(StrEax / trunc(ExtEax)));
            end else Found := False;
        end;
        'String.removeMatching' : begin
            //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            stack_push(pocz[sets.StackPointer], buildString(StrEax / StrEbx));
        end;
        // logics
        'String.isEmpty' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildBoolean(StrEax = ''));
            end else Found := False;
        end;

        // others
        'String.translate' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEcx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (length(StrEbx) = length(StrEcx))
                then stack_push(pocz[sets.StackPointer], buildString(StringTranslate(StrEax, StrEbx, StrEcx)))
                else stack_push(pocz[sets.StackPointer], raiseStringSameLength(i));
        end;

        else begin
        	Found := false;
        end;
	end;
	Result := Found;
end;

function lib_directives(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found          : Boolean;
	StrEax         : String;
    IntEax, IntEbx : Integer;
begin
	Found := true;
    if (i[1] <> '@') then begin
        Found := false;
        Exit;
    end;
	case i of
        '@silent' : begin
           sets.Prevent := true;
        end;
        '@silent(true)' : begin
           sets.Prevent := true;
        end;
        '@silent(false)' : begin
           sets.Prevent := false;
        end;
        '@silent(TRUE)' : begin
           sets.Prevent := true;
        end;
        '@silent(FALSE)' : begin
           sets.Prevent := false;
        end;
        '@autoclear(true)' : begin
           sets.Autoclear := true;
        end;
        '@autoclear(false)' : begin
           sets.Autoclear := false;
        end;
        '@autoclear(TRUE)' : begin
           sets.Autoclear := true;
        end;
        '@autoclear(FALSE)' : begin
           sets.Autoclear := false;
        end;
        '@stricttype(true)' : begin
           sets.StrictType := true;
        end;
        '@stricttype(false)' : begin
           sets.StrictType := false;
        end;
        '@stricttype(TRUE)' : begin
           sets.StrictType := true;
        end;
        '@stricttype(FALSE)' : begin
           sets.StrictType := false;
        end;
        '@casesensitive(true)' : begin
           sets.CaseSensitive := true;
        end;
        '@casesensitive(false)' : begin
           sets.CaseSensitive := false;
        end;
        '@casesensitive(TRUE)' : begin
           sets.CaseSensitive := true;
        end;
        '@casesensitive(FALSE)' : begin
           sets.CaseSensitive := false;
        end;
        '@infmode' : begin
           sets.InfMode := true;
        end;
        '@infmode(TRUE)' : begin
           sets.InfMode := true;
        end;
        '@infmode(FALSE)' : begin
           sets.InfMode := false;
        end;
        '@infmode(true)' : begin
           sets.InfMode := true;
        end;
        '@infmode(false)' : begin
           sets.InfMode := false;
        end;
        '@real' : begin
           sets.Mask := '0.################';
        end;
        '@decimal' : begin
           sets.Mask := '#,###.################';
        end;
        '@milli' : begin
           sets.Mask := '0.000';
        end;
        '@float' : begin
           sets.Mask := '0.000000';
        end;
        '@double' : begin
           sets.Mask := '0.000000000000000';
        end;
        '@money' : begin
           sets.Mask := '0.00';
        end;
        '@amoney' : begin
           sets.Mask := '#,###.00';
        end;
        '@int' : begin
           sets.Mask := '0';
        end;
        '@scientific' : begin
           sets.Mask := '0.################E+00';
        end;
        '@scientific1' : begin
           sets.Mask := '0.000000000000000E+0000';
        end;
        '@sorttype(BUBBLESORT)' : begin
           sets.sorttype := 0;
        end;
        '@sorttype(QUICKSORT)' : begin
           sets.sorttype := 1;
        end;
        '@sorttype(MERGESORT)' : begin
           sets.sorttype := 2;
        end;
        '@sorttype(BOGOSORT)' : begin
           sets.sorttype := 3;
        end;
        '@sorttype(RANDOMSORT)' : begin
           sets.sorttype := 3;
        end;
        '@sorttype(BSORT)' : begin
           sets.sorttype := 0;
        end;
        '@sorttype(QSORT)' : begin
           sets.sorttype := 1;
        end;
        '@sorttype(MSORT)' : begin
           sets.sorttype := 2;
        end;
        '@sorttype(RSORT)' : begin
           sets.sorttype := 3;
        end;
        '@sorttype(0)' : begin
           sets.sorttype := 0;
        end;
        '@sorttype(1)' : begin
           sets.sorttype := 1;
        end;
        '@sorttype(2)' : begin
           sets.sorttype := 2;
        end;
        '@sorttype(3)' : begin
           sets.sorttype := 3;
        end;
        '@useshell(BASH)' : begin
           sets.Shell := SHELL_BASH;
        end;
        '@useshell(ZSH)' : begin
           sets.Shell := SHELL_ZSH;
        end;
        '@useshell(SH)' : begin
           sets.Shell := SHELL_SH;
        end;
        '@useshell(CMD)' : begin
           sets.Shell := SHELL_CMD;
        end;
        '@useshell(POWERSHELL)' : begin
           sets.Shell := SHELL_PWSH;
        end;
        '@useshell(PWSH)' : begin
           sets.Shell := SHELL_PWSH;
        end;

		'@use(Math)' : begin
           sets.Packages.UseMath := true;
		   sets.Packages.UseAnything := true;
        end;
		'@unuse(Math)' : begin
           sets.Packages.UseMath := false;
		   sets.Packages.UseAnything := verifyPackages(sets.Packages);
        end;
		'@use(String)' : begin
           sets.Packages.UseString := true;
		   sets.Packages.UseAnything := true;
        end;
		'@unuse(String)' : begin
           sets.Packages.UseString := false;
		   sets.Packages.UseAnything := verifyPackages(sets.Packages);
        end;
        '@use(Array)' : begin
           sets.Packages.UseArray := true;
		   sets.Packages.UseAnything := true;
        end;
		'@unuse(Array)' : begin
           sets.Packages.UseArray := false;
		   sets.Packages.UseAnything := verifyPackages(sets.Packages);
        end;
        '@use(Console)' : begin
           sets.Packages.UseConsole := true;
		   sets.Packages.UseAnything := true;
        end;
		'@unuse(Console)' : begin
           sets.Packages.UseConsole := false;
		   sets.Packages.UseAnything := verifyPackages(sets.Packages);
        end;
        '@use(Date)' : begin
           sets.Packages.UseDate := true;
		   sets.Packages.UseAnything := true;
        end;
		'@unuse(Date)' : begin
           sets.Packages.UseDate := false;
		   sets.Packages.UseAnything := verifyPackages(sets.Packages);
        end;


        '@stringmode' : begin
            sets.StringStart := 0;
            sets.StringMode := MCLIKE;
        end;
        '@stringmode(DEFAULT)' : begin
            sets.StringStart := 0;
            sets.StringMode := MCLIKE;
        end;
        '@stringmode(CLIKE)' : begin
            sets.StringStart := 0;
            sets.StringMode := MCLIKE;
        end;
        '@stringmode(PASCAL)' : begin
            sets.StringStart := 1;
            sets.StringMode := MPASCL;
        end;

        '@stringindex' : begin
            sets.StringStart := 0;
        end;
        '@stringindex(DEFAULT)' : begin
            sets.StringStart := 0;
        end;
        '@stringindex(0)' : begin
            sets.StringStart := 0;
        end;
        '@stringindex(1)' : begin
            sets.StringStart := 1;
        end;

        '@maxprecision(0)' : begin
            sets.Mask := '0';
        end;
        '@fixprecision(0)' : begin
            sets.Mask := '0';
        end;
        '@maxprecision(-1)' : begin
            sets.Mask := '0.################';
        end;
        '@fixprecision(-1)' : begin
            sets.Mask := '0.000000000000000';
        end;
        '@maxprecision(DEFAULT)' : begin
            sets.Mask := '0.################';
        end;
        '@fixprecision(DEFAULT)' : begin
            sets.Mask := '0.000000000000000';
        end;
		
        else begin
        	case LeftStr(i, 9) of
            	'@source("' : begin
              		if (RightStr(i, 2) = '")') then begin
                		StrEax := RightStr(i, Length(i)-9);
                		StrEax := LeftStr(StrEax, Length(StrEax)-2);
                		pocz := read_sourcefile(StrEax, pocz, sets, vardb);
              		end else begin
                        stack_push(pocz[sets.StackPointer], raiseException('ESyntax:CExpression: Syntax Error at expression "'+i+'".'));
              		end;
             	end;
             	else begin
                    case LeftStr(i, 14) of
                        '@maxprecision(' : begin
                            if (RightStr(i, 1) = ')') then begin
                                StrEax := RightStr(i, Length(i)-14);
                		        StrEax := LeftStr(StrEax, Length(StrEax)-1);
                                if (TryStrToInt(StrEax, IntEax)) 
                                then begin
                                    if IntEax > 0 then
                                    begin
                                        StrEax := '0.';
                                        for IntEbx := 1 to IntEax do
                                            StrEax := StrEax + '#';
                                        sets.Mask := StrEax;
                                    end else begin
                                        stack_push(pocz[sets.StackPointer], raiseException('EConstraint:CIntegerEx: a non-negative integer (or -1) expected at "'+i+'".'));  
                                    end;
                                end else begin
                                    stack_push(pocz[sets.StackPointer], raiseException('EConstraint:CIntegerEx: a non-negative integer (or -1) expected at "'+i+'".'));
                                end;
                            end else begin
                	        	stack_push(pocz[sets.StackPointer], raiseException('ESyntax:CExpression: Syntax Error at expression "'+i+'".'));
              		        end;
                        end;
                        '@fixprecision(' : begin
                            if (RightStr(i, 1) = ')') then begin
                                StrEax := RightStr(i, Length(i)-14);
                		        StrEax := LeftStr(StrEax, Length(StrEax)-1);
                                if (TryStrToInt(StrEax, IntEax)) 
                                then begin
                                    if IntEax > 0 then
                                    begin
                                        StrEax := '0.';
                                        for IntEbx := 1 to IntEax do
                                            StrEax := StrEax + '0';
                                        sets.Mask := StrEax;
                                    end else begin
                                        stack_push(pocz[sets.StackPointer], raiseException('EConstraint:CIntegerEx: a non-negative integer (or -1) expected at "'+i+'".'));  
                                    end;
                                end else begin
                                    stack_push(pocz[sets.StackPointer], raiseException('EConstraint:CIntegerEx: a non-negative integer (or -1) expected at "'+i+'".'));
                                end;
                            end else begin
                	        	stack_push(pocz[sets.StackPointer], raiseException('ESyntax:CExpression: Syntax Error at expression "'+i+'".'));
              		        end;
                        end;
                        else Found := false;
                    end;
             	end;
        	end;
        end;
    end;
    lib_directives := Found;
end;

function lib_constants(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found  : Boolean;
begin
	Found := true;
	case i of
        'NULL' : begin
            stack_push(pocz[sets.StackPointer], buildNull());
        end;
        'TRUE' : begin
            stack_push(pocz[sets.StackPointer], buildBoolean(True));
        end;
        'FALSE' : begin
            stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        else begin
        	Found := false;
        end;
	end;
    lib_constants := Found;
end;


function lib_variables(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found  : Boolean;
	StrEax, StrEbx : String;
	EntEax : Entity;
	LogEax : Boolean;
begin
	Found := true;
    case LeftStr(i, 1) of
    	'$' : begin
      		if (RightStr(i, Length(i)-1) <> '') then begin
        		StrEax := RightStr(i, Length(i)-1);
                if LeftStr(StrEax, 7) = 'global.'  
                    then EntEax := vardb.getGlobalVariable(StrEax)
                    else EntEax := vardb.getVariable(StrEax);
    			stack_push(pocz[sets.StackPointer], EntEax);
      		end else begin
        		raiserror('EVariable:CGet: You cannot get a value from an unnamed variable.');
      		end;
     	end;
     	'>' : begin 
     		if (RightStr(i, Length(i)-1) <> '') then begin
        		StrEax := RightStr(i, Length(i)-1);
        		if (sets.Autoclear) then EntEax := stack_pop(pocz[sets.StackPointer])
        		else EntEax := stack_get(pocz[sets.StackPointer]);
                if isValidForVariables(StrEax) then
                begin
                    if LeftStr(StrEax, 7) = 'global.' 
                        then vardb.setGlobalVariable(StrEax, EntEax)
                        else
    			            //vardb.setVariable(StrEax, EntEax);
                            vardb.setLocalVariable(StrEax, EntEax);
                        
                end else begin
                    raiserror('EVariable:CSetInvalid: Invalid variable string at "'+StrEax+'"');
                end;
      		end else begin
        		raiserror('EVariable:CSetUnnamed: Attempt of setting an unnamed variable.');
      		end;
     	end;
     	'?' : begin 
     		if (RightStr(i, Length(i)-1) <> '') then begin
        		StrEax := RightStr(i, Length(i)-1);
    			LogEax := vardb.isVarAssigned(StrEax);
    			stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
      		end else begin
        		raiserror('EVariable:CCheck: You cannot check nothing.');
      		end;
     	end;
     	'~' : begin 
     		if (RightStr(i, Length(i)-1) <> '') then begin
        		StrEax := RightStr(i, Length(i)-1);
    			vardb.removeVariable(StrEax);
      		end else begin
        		raiserror('EVariable:CDestroy: You cannot destroy an unnamed variable.');
      		end;
     	end;
     	else begin
            case LeftStr(i, 2) of
                '@@' : begin 
                    if (RightStr(i, Length(i)-2) <> '') then begin
                        StrEax := RightStr(i, Length(i)-2);
                        EntEax := vardb.getVariable(StrEax);
                        if LeftStr(StrEax, 7) = 'global.' 
                            then EntEax := vardb.getGlobalVariable(StrEax)
                            else EntEax := vardb.getVariable(StrEax);
                        if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], EntEax, TFUN, i)) then Exit;   
                        doFunction(EntEax, pocz, sets, vardb);
                    end else begin
                        raiserror('EVariable:CExecute: You cannot execute an unnamed function by this method.');
                    end;
                end;
                '->' : begin 
     		        if (RightStr(i, Length(i)-2) <> '') then begin
                		StrEax := RightStr(i, Length(i)-2);
                		if (sets.Autoclear) then EntEax := stack_pop(pocz[sets.StackPointer])
                		else EntEax := stack_get(pocz[sets.StackPointer]);
    	        		if isValidForVariables(StrEax) then
                        begin
                            if LeftStr(StrEax, 7) = 'global.' 
                                then vardb.setGlobalVariable(StrEax, EntEax) 
                                else
    			                    //vardb.setVariable(StrEax, EntEax);
                                    vardb.setLocalVariable(StrEax, EntEax);
                        end else begin
                            raiserror('EVariable:CSetInvalid: Invalid variable string at "'+StrEax+'"');
                        end;
      	        	end else begin
                		raiserror('EVariable:CSet: You cannot set a value to an unnamed variable.');
      	        	end;
     	        end;
                else begin
                    Found := false;
                end;
            end;
     	end;
    end;
    Result := Found;
end;

function lib_variables2(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found  : Boolean;
	StrEax, StrEbx : String;
	EntEax : Entity;
	LogEax : Boolean;
begin
	Found := true;
	case i of
		'vset' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if isValidForVariables(StrEax) then
            begin
                //vardb.setVariable(StrEax, EntEax);
                vardb.setLocalVariable(StrEax, EntEax);
            end else begin
                raiserror('EVariable:CSetInvalid: Invalid variable string at "'+StrEax+'"');
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], EntEax);
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
        end;
        'vlset' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if isValidForVariables(StrEax) then
            begin
                //vardb.setVariable(StrEax, EntEax);
                vardb.setLocalVariable(StrEax, EntEax);
            end else begin
                raiserror('EVariable:CSetInvalid: Invalid variable string at "'+StrEax+'"');
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], EntEax);
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
        end;
		'vget' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            EntEax := vardb.getVariable(StrEax);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'vexists' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            LogEax := vardb.isVarAssigned(StrEax);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
            stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
        end;
        'vdestroy' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            vardb.removeVariable(StrEax);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
        end;
        'vclear' : begin
            vardb.clearAllVariables();
        end;
        'vcall' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            EntEax := vardb.getVariable(StrEax);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], EntEax, TFUN, i)) then Exit;  
            //StrEbx := EntEax.Str;
            //pocz := parseScoped(StrEbx, pocz, sets, vardb);
            doFunction(EntEax, pocz, sets, vardb);
            if not (sets.Autoclear) then begin
                stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
        end;
        else begin
            Found := false;
        end;
    end;
    Result := Found;
end;

function lib_logics(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found                  : Boolean;
	EntEax, EntEbx         : Entity;
	LogEax, LogEbx, LogEcx : Boolean;
begin
	Found := true;
	case i of
		'=' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		LogEax := (EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num);
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
		end;
		'!=' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		LogEax := not ((EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num));
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
		end;
		'>' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		LogEax := EntEax.Num > EntEbx.Num;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
		end;
		'<' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		LogEax := EntEax.Num < EntEbx.Num;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
		end;
		'<=' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		LogEax := EntEax.Num <= EntEbx.Num;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
		end;
		'>=' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		LogEax := EntEax.Num >= EntEbx.Num;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
		end;
		'and' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		if EntEax.Num = 0 then LogEax := true else LogEax := false;
    		if EntEbx.Num = 0 then LogEbx := true else LogEbx := false;
    		LogEcx := LogEax and LogEbx;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEcx));
		end;
		'or' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		if EntEax.Num = 0 then LogEax := true else LogEax := false;
    		if EntEbx.Num = 0 then LogEbx := true else LogEbx := false;
    		LogEcx := LogEax or LogEbx;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEcx));
		end;
		'xor' : begin
    		EntEbx := stack_pop(pocz[sets.StackPointer]);
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		if EntEax.Num = 0 then LogEax := true else LogEax := false;
    		if EntEbx.Num = 0 then LogEbx := true else LogEbx := false;
    		LogEcx := LogEax xor LogEbx;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEcx));
		end;
		'not' : begin
    		EntEax := stack_pop(pocz[sets.StackPointer]);
    		if EntEax.Num = 0 then LogEax := true else LogEax := false;
    		LogEcx := not LogEax;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], EntEax);
    			stack_push(pocz[sets.StackPointer], EntEbx);
    		end;
    		stack_push(pocz[sets.StackPointer], buildBoolean(LogEcx));
		end;
		else begin
        	Found := false;
       	end;
    end;
    lib_logics := Found;
end;

function lib_consolemanipulators(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
    Found   : Boolean;
    x, y, z : ShortInt;
    StrEax  : String; 
    StrEbx  : String;
    StrEcx  : String;
    a       : Integer;
begin
    Found := true;
    case i of
        'Console.textColor' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextColor(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Console.textColour' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextColor(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Console.textBackground' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextBackground(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        {$IfNDef MSWINDOWS}
        'Console.textColorANSI' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextColorANSI(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Console.textBackgroundANSI' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextBackgroundANSI(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Console.textColorRGB' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            z := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextColorRGB(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Console.textColourRGB' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            z := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextColorRGB(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Console.textBackgroundRGB' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            z := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            TextBackgroundRGB(x, y, z);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Console.textReset' : begin
            TextReset();
        end;
        'Console.textBold' : begin
            TextBold();
        end;
        'Console.textItalic' : begin
            TextItalic();
        end;
        'Console.textUnderline' : begin
            TextUnderline();
        end;
        'Console.textBlink' : begin
            TextBlink();
        end;
        'Console.textFastBlink' : begin
            TextFastBlink();
        end;
        'Console.textInverse' : begin
            TextInverse();
        end;
        'Console.textBoldOff' : begin
            TextBoldOff();
        end;
        'Console.textItalicOff' : begin
            TextItalicOff();
        end;
        'Console.textUnderlineOff' : begin
            TextUnderlineOff();
        end;
        'Console.textBlinkOff' : begin
            TextBlinkOff();
        end;
        'Console.textFastBlinkOff' : begin
            TextFastBlinkOff();
        end;
        'Console.textInverseOff' : begin
            TextInverseOff();
        end;
        {$ENDIF}
        'Console.delay' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            a := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            Delay(a);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(a));
        end;
        {$IFDEF MSWINDOWS}
        'Console.whereX' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(WhereX()));
        end;
        'Console.whereY' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(WhereY()));
        end;
        'Console.startSound' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            a := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            Sound(a);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(a));
        end;
        'Console.stopSound' : begin
            NoSound();
        end;
        {$ENDIF}
        'Console.gotoXY' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            GotoXY(x,y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Console.clrscr' : begin
            clrscr();
        end;
        'Console.clearScreen' : begin
            clrscr();
        end;
        'Console.runCommand' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := executeCommand(StrEbx, sets.Shell);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        else begin
            Found := false;
        end;
    end;
    lib_consolemanipulators := Found;
end;

function lib_exceptions(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
    Found  : Boolean;
    ExcEax : Entity;
begin
    Found := true;
    case i of
        'EXC' : begin
            stack_push(pocz[sets.StackPointer], buildException(''));
        end;
        'toException' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            stack_push(pocz[sets.StackPointer], buildException(stack_pop(pocz[sets.StackPointer]).Str));
        end;
        'raiseException' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TEXC) then
            begin
                ExcEax := stack_pop(pocz[sets.StackPointer]);
                ExcEax.Num := 1;
                stack_push(pocz[sets.StackPointer], ExcEax);
            end else //if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                stack_push(pocz[sets.StackPointer], raiseException(stack_pop(pocz[sets.StackPointer]).Str));    
            end;
        end;
        else begin
            Found := false;
        end;
    end;
    lib_exceptions := Found;
end;

function lib_arrays(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found                  : Boolean;
    IntEax, IntEbx, IntEcx : LongInt;
    ArrEax, ArrEbx, ArrEcx : Entity;
    EntEax, EntEbx         : Entity;
    LogEax                 : Boolean;
    StrEax                 : String;
    ExtEax                 : Extended;
    index                  : Integer;
begin
	Found := true;
	case i of
        'Array.crush' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_reverse(pocz[trunc(ArrEax.Num)]);
                for index := 0 to stack_size(pocz[trunc(ArrEax.Num)])-1 do
                begin
                    EntEax := stack_getFront(pocz[trunc(ArrEax.Num)], index);
                    stack_push(pocz[sets.StackPointer], EntEax);
                end;
                //stack_reverse(pocz[trunc(ArrEax.Num)]);
            end else Found := False;
        end;
        'Array.destroy' : begin
            //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_reverse(pocz[trunc(ArrEax.Num)]);
            for index := 0 to stack_size(pocz[trunc(ArrEax.Num)])-1 do
            begin
                EntEax := stack_pop(pocz[trunc(ArrEax.Num)]);
                stack_push(pocz[sets.StackPointer], EntEax);
            end;
        end;
        'Array.getAt' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[IntEax]);
        end;
        'Array.getAt!' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_popFront(pocz[trunc(ArrEax.Num)], IntEax);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.setAt' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);

            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, stack_size(pocz[trunc(ArrEax.Num)])));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEbx := 0;
            while (IntEbx <= stack_size(pocz[trunc(ArrEax.Num)])-1) do
            begin
                if (IntEax = IntEbx)
                    then pocz[trunc(ArrEcx.Num)].Values[IntEbx] := EntEax
                    else pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx];
                IntEbx := IntEbx + 1;
            end;
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.setAt!' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            pocz[trunc(ArrEax.Num)].Values[IntEax] := EntEax;
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.setAt!!' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            pocz[trunc(ArrEax.Num)].Values[IntEax] := EntEax;
            //stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.push' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, stack_size(pocz[trunc(ArrEax.Num)])));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEbx := 0;
            while (IntEbx <= stack_size(pocz[trunc(ArrEax.Num)])-1) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx];
                IntEbx := IntEbx + 1;
            end;
            stack_push(pocz[trunc(ArrEcx.Num)], EntEax);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.push!' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[trunc(ArrEax.Num)], EntEax);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.push!!' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[trunc(ArrEax.Num)], EntEax);
            //stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.getTail' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_get(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.getTail!' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.pop' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, stack_size(pocz[trunc(ArrEax.Num)])-1));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEbx := 0;
            while (IntEbx <= stack_size(pocz[trunc(ArrEax.Num)])-2) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx];
                IntEbx := IntEbx + 1;
            end;
            //EntEax := stack_pop(pocz[trunc(ArrEcx.Num)]);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.pop!' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.pop!!' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[trunc(ArrEax.Num)]);
            //stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.getHead' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[0]);
        end;
        'Array.getHead!' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_firstpop(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;

        'Array.shift' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, stack_size(pocz[trunc(ArrEax.Num)])-1));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEbx := 0;
            while (IntEbx <= stack_size(pocz[trunc(ArrEax.Num)])-2) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx+1];
                IntEbx := IntEbx + 1;
            end;
            //EntEax := stack_pop(pocz[trunc(ArrEcx.Num)]);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.shift!' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_firstpop(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.shift!!' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_firstpop(pocz[trunc(ArrEax.Num)]);
            //stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.pushAt' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, stack_size(pocz[trunc(ArrEax.Num)])+1));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEbx := 0;
            while (IntEbx < IntEax) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx];
                IntEbx := IntEbx + 1;
            end;
            pocz[trunc(ArrEcx.Num)].Values[IntEax] := EntEax;
            IntEbx := IntEbx + 1;
            while (IntEbx <= stack_size(pocz[trunc(ArrEax.Num)])) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx-1];
                IntEbx := IntEbx + 1;
            end;
            //stack_push(pocz[trunc(ArrEcx.Num)], EntEax);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.pushAt!' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_pushFront(pocz[trunc(ArrEax.Num)], EntEax, IntEax);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.pushAt!!' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_pushFront(pocz[trunc(ArrEax.Num)], EntEax, IntEax);
        end;
        'Array.popAt' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, stack_size(pocz[trunc(ArrEax.Num)])-1));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEbx := 0;
            while (IntEbx < IntEax) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx] := pocz[trunc(ArrEax.Num)].Values[IntEbx];
                IntEbx := IntEbx + 1;
            end;
            //pocz[trunc(ArrEcx.Num)].Values[IntEax] := EntEax;
            IntEbx := IntEbx + 1;
            while (IntEbx <= stack_size(pocz[trunc(ArrEax.Num)])-1) do
            begin
                pocz[trunc(ArrEcx.Num)].Values[IntEbx-1] := pocz[trunc(ArrEax.Num)].Values[IntEbx];
                IntEbx := IntEbx + 1;
            end;
            //EntEax := stack_pop(pocz[trunc(ArrEcx.Num)]);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.popAt!' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_popFront(pocz[trunc(ArrEax.Num)], IntEax);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.popAt!!' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_popFront(pocz[trunc(ArrEax.Num)], IntEax);
            //stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        
        'Array.length' : begin
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, TVEC, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                IntEax := trunc(stack_get(pocz[sets.StackPointer]).Num);
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(stack_size(pocz[IntEax])));
            end else Found := False;
        end;
        'Array.toJSString' : begin
          	EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TVEC) then
            begin
                stack_push(pocz[sets.StackPointer], buildString(stack_showArrayFull(pocz[trunc(EntEax.Num)], pocz, sets.Mask))); 
            end else begin
                stack_push(pocz[sets.StackPointer], buildString(EntEax.Str));
            end;
        end;
        'Array.reduceSum' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_sum(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceProduct' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_product(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceAvg' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_avg(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMean' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_avg(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMeanGeom' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_avg_geom(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMeanHarm' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_avg_power(pocz[trunc(ArrEax.Num)].Values, -1)));
        end;
        'Array.reduceMeanSq' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_avg2(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reducePowerMean' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_avg_power(pocz[trunc(ArrEax.Num)].Values, ExtEax)));
        end;
        'Array.reduceVariance' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_variance(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceStddev' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_stddev(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceStdDev' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_stddev(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMedian' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_median(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMode' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], table_mode(pocz[trunc(ArrEax.Num)].Values));
        end;
        'Array.reduceModeStr' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], table_modeStr(pocz[trunc(ArrEax.Num)].Values));
        end;
        'Array.reduceGCD' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_gcd(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceLCM' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_lcm(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMin' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_min(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.reduceMax' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_max(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.findMin' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_min2(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.findMax' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(table_max2(pocz[trunc(ArrEax.Num)].Values)));
        end;
        'Array.sortNumbers' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.sorttype = 0) then bubblesort(pocz[trunc(ArrEax.Num)].Values);
            if (sets.sorttype = 1) then quicksort(pocz[trunc(ArrEax.Num)].Values);
            if (sets.sorttype = 2) then mergesort(pocz[trunc(ArrEax.Num)].Values);
            if (sets.sorttype = 3) then bogosort(pocz[trunc(ArrEax.Num)].Values);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.sortStrings' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            strings_sort(pocz[trunc(ArrEax.Num)].Values);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.belongs' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            LogEax := False;
            for EntEbx in pocz[trunc(ArrEax.Num)].Values do
            begin
                //if (EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num) then 
                if EntEax = EntEbx then 
                begin 
                    LogEax := True;
                    break;
                end;
    		end;
            stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
        end;
        'Array.map' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, Length(pocz[trunc(ArrEax.Num)].Values)));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    pocz[trunc(ArrEbx.Num)].Values[index] := stack_pop(pocz[sets.StackPointer]);
    		    end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.reduce' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                EntEbx := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
    		    end;
                stack_push(pocz[sets.StackPointer], EntEbx);
                vardb.removeLayer();
            end else Found := False;
        end;
        'Array.reduceLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                EntEbx := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
    		    end;
                stack_push(pocz[sets.StackPointer], EntEbx);
                vardb.removeLayer();
            end else Found := False;
        end;
        'Array.reduceFromFirst' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                EntEbx := pocz[trunc(ArrEax.Num)].Values[0];
                for index := 1 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
    		    end;
                stack_push(pocz[sets.StackPointer], EntEbx);
                vardb.removeLayer();
            end else Found := False;
        end;
        'Array.reduceLeftFromFirst' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                EntEbx := pocz[trunc(ArrEax.Num)].Values[0];
                for index := 1 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
    		    end;
                stack_push(pocz[sets.StackPointer], EntEbx);
                vardb.removeLayer();
            end else Found := False;
        end;
        'Array.reduceRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                EntEbx := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                for index := Length(pocz[trunc(ArrEax.Num)].Values)-1 downto 0 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
    		    end;
                stack_push(pocz[sets.StackPointer], EntEbx);
                vardb.removeLayer();
            end else Found := False;
        end;
        'Array.reduceRightFromLast' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                EntEbx := pocz[trunc(ArrEax.Num)].Values[Length(pocz[trunc(ArrEax.Num)].Values)-1];
                for index := Length(pocz[trunc(ArrEax.Num)].Values)-2 downto 0 do
                begin
                    stack_push(pocz[sets.StackPointer], EntEbx);
                    stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[index]);
                    doFunction(EntEax, pocz, sets, vardb);
                    EntEbx := stack_pop(pocz[sets.StackPointer]);
    		    end;
                stack_push(pocz[sets.StackPointer], EntEbx);
                vardb.removeLayer();
            end else Found := False;
        end;
        'Array.join' : begin
            if  (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) 
            and (stack_getback(pocz[sets.StackPointer], 1).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                IntEax := Length(pocz[trunc(ArrEax.Num)].Values);
                IntEbx := Length(pocz[trunc(ArrEbx.Num)].Values);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEax + IntEbx));
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to IntEax-1 do
                begin
                    pocz[trunc(ArrEcx.Num)].Values[index] := pocz[trunc(ArrEax.Num)].Values[index];
                end; 
                for index := 0 to IntEbx-1 do
                begin
                    pocz[trunc(ArrEcx.Num)].Values[IntEax+index] := pocz[trunc(ArrEbx.Num)].Values[index];
                end;
                stack_push(pocz[sets.StackPointer], ArrEcx);
            end else Found := False;
        end;
        'Array.split' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                IntEbx := Length(pocz[trunc(ArrEcx.Num)].Values) - IntEax;
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEax));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEbx));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to IntEax-1 do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[index];
                end; 
                for index := 0 to IntEbx-1 do
                begin
                    pocz[trunc(ArrEbx.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[IntEax+index];
                end;
                stack_push(pocz[sets.StackPointer], ArrEax);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.filter' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, i)) then Exit;
                if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := 'afilt_'+IntToStr(DateTimeToUnix(Now));
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    vardb.setLocalVariable(StrEax, pocz[trunc(ArrEax.Num)].Values[index]);
                    pocz := parseOpen('$' + StrEax + ' ' + EntEax.Str, pocz, sets, vardb);
                    if (trunc(stack_pop(pocz[sets.StackPointer]).Num) = 0) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.cut' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, i)) then Exit;
                if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, TFUN, i)) then Exit;
                EntEax := stack_pop(pocz[sets.StackPointer]);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                StrEax := 'acutt_'+IntToStr(DateTimeToUnix(Now));
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                vardb.addLayer();
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    vardb.setLocalVariable(StrEax, pocz[trunc(ArrEax.Num)].Values[index]);
                    pocz := parseOpen('$' + StrEax + ' ' + EntEax.Str, pocz, sets, vardb);
                    if (trunc(stack_pop(pocz[sets.StackPointer]).Num) <> 0) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                vardb.removeLayer();
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.splitByExpression' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TEXP, i)) then Exit;
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            StrEax := 'filt_'+IntToStr(DateTimeToUnix(Now));
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
            ArrEbx := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            vardb.addLayer();
            for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
            begin
                vardb.setLocalVariable(StrEax, pocz[trunc(ArrEax.Num)].Values[index]);
                pocz := parseOpen('$' + StrEax + ' ' + EntEax.Str, pocz, sets, vardb);
                if (trunc(stack_pop(pocz[sets.StackPointer]).Num) = 0) 
                    then stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index])
                    else stack_push(pocz[trunc(ArrEcx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		end;
            vardb.removeLayer();
            stack_push(pocz[sets.StackPointer], ArrEbx);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.unweave' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
            ArrEbx := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            vardb.addLayer();
            for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
            begin
                if (index mod 2 = 0) 
                    then stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index])
                    else stack_push(pocz[trunc(ArrEcx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		end;
            vardb.removeLayer();
            stack_push(pocz[sets.StackPointer], ArrEbx);
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.weave' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEbx := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            IntEax := Length(pocz[trunc(ArrEax.Num)].Values);
            IntEbx := Length(pocz[trunc(ArrEbx.Num)].Values);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEax + IntEbx));
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            IntEcx := 0;
            if (IntEax <= IntEbx) then
            begin
                for index := 0 to IntEax-1 do
                begin
                    pocz[trunc(ArrEcx.Num)].Values[IntEcx] := pocz[trunc(ArrEax.Num)].Values[index];
                    pocz[trunc(ArrEcx.Num)].Values[IntEcx+1] := pocz[trunc(ArrEbx.Num)].Values[index];
                    Inc(IntEcx, 2);
                end;
                for index := IntEax to IntEbx-1 do
                begin
                    pocz[trunc(ArrEcx.Num)].Values[IntEcx] := pocz[trunc(ArrEbx.Num)].Values[index];
                    Inc(IntEcx, 1);
                end;
            end else begin
                for index := 0 to IntEbx-1 do
                begin
                    pocz[trunc(ArrEcx.Num)].Values[IntEcx] := pocz[trunc(ArrEax.Num)].Values[index];
                    pocz[trunc(ArrEcx.Num)].Values[IntEcx+1] := pocz[trunc(ArrEbx.Num)].Values[index];
                    Inc(IntEcx, 2);
                end;
                for index := IntEbx to IntEax-1 do
                begin
                    pocz[trunc(ArrEcx.Num)].Values[IntEcx] := pocz[trunc(ArrEax.Num)].Values[index];
                    Inc(IntEcx, 1);
                end;
            end; 
            stack_push(pocz[sets.StackPointer], ArrEcx);
        end;
        'Array.left' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEax));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to IntEax-1 do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[index];
                end; 
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.right' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEax));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to IntEax-1 do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax+index];
                end; 
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.cutLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax-1 do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[index+IntEax];
                end; 
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.cutRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax-1 do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[index];
                end; 
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.between' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEbx := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEbx-IntEax+1));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to IntEbx-IntEax do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[IntEax+index];
                end; 
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.subarray' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEbx := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEcx := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, IntEbx));
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            for index := 0 to IntEbx-1 do
            begin
                pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[IntEax+index];
            end; 
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.cutBothSides' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEbx := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEcx := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax-IntEbx));
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEcx.Num)].Values)-IntEax-IntEbx-1 do
                begin
                    pocz[trunc(ArrEax.Num)].Values[index] := pocz[trunc(ArrEcx.Num)].Values[IntEax+index];
                end; 
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.crushBy' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
          	    if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
                IntEcx := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_reverse(pocz[trunc(ArrEax.Num)]);

                IntEax := 0;
                while (IntEax <= stack_size(pocz[trunc(ArrEax.Num)])-1) do
                begin
                    stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                    ArrEcx := stack_pop(pocz[sets.StackPointer]);
                    IntEbx := 0;
                    while (IntEbx < trunc(IntEcx)) and (IntEax <= stack_size(pocz[trunc(ArrEax.Num)])-1) do
                    begin
                        stack_push(pocz[trunc(ArrEcx.Num)], stack_getFront(pocz[trunc(ArrEax.Num)], IntEax));
                        IntEbx := IntEbx + 1;
                        IntEax := IntEax + 1;  
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEcx);
                end;
                stack_reverse(pocz[trunc(ArrEax.Num)]);

            end else Found := false;
        end;
        'Array.first' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[0]);
            end else Found := false;
        end;
        'Array.last' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], pocz[trunc(ArrEax.Num)].Values[Length(pocz[trunc(ArrEax.Num)].Values)-1]);
            end else Found := false;
        end;
        // crush, pushAt, popAt, swapAt, toString, size

         // logics
        'Array.isEmpty' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildBoolean(stack_size(pocz[trunc(ArrEax.Num)]) = 0));
            end else Found := False;
        end;

        // 052 functions
        'Array.cutNulls' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    if not (isNull(pocz[trunc(ArrEax.Num)].Values[index])) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
         'Array.cutZeros' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    if not (isZero(pocz[trunc(ArrEax.Num)].Values[index])) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.cutEmptyStrings' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    if not (isEmptyString(pocz[trunc(ArrEax.Num)].Values[index])) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.randomFrom' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], array_randomFrom(pocz[trunc(ArrEax.Num)].Values));
        end;
        'Array.distinct' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    if not (itemHappenedBefore(pocz[trunc(ArrEax.Num)].Values, index)) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.distinctNumbers' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    if not (stringHappenedBefore(pocz[trunc(ArrEax.Num)].Values, index)) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.distinctStrings' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    if not (stringHappenedBefore(pocz[trunc(ArrEax.Num)].Values, index)) then
                        stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.sort' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                if (pocz[trunc(ArrEax.Num)].Values[index].EntityType = TSTR) then
                begin
                    strings_sort(pocz[trunc(ArrEax.Num)].Values);
                end else begin
                    case sets.sorttype of
                        0 : bubblesort(pocz[trunc(ArrEax.Num)].Values);
                        1 : quicksort(pocz[trunc(ArrEax.Num)].Values);
                        2 : mergesort(pocz[trunc(ArrEax.Num)].Values);
                        3 : bogosort(pocz[trunc(ArrEax.Num)].Values);
                    end; 
                end;
                stack_push(pocz[sets.StackPointer], ArrEax);
            end else Found := False;
        end;
        'Array.pad' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        if (IntEbx mod 2 = 0) 
                            then stack_pushFront(pocz[trunc(ArrEbx.Num)], buildNull())
                            else stack_push(pocz[trunc(ArrEbx.Num)], buildNull());
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_pushFront(pocz[trunc(ArrEbx.Num)], buildNull());
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_push(pocz[trunc(ArrEbx.Num)], buildNull());
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padSpaces' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        if (IntEbx mod 2 = 0) 
                            then stack_pushFront(pocz[trunc(ArrEbx.Num)], buildString(' '))
                            else stack_push(pocz[trunc(ArrEbx.Num)], buildString(' '));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padSpacesLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_pushFront(pocz[trunc(ArrEbx.Num)], buildString(' '));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padSpacesRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_push(pocz[trunc(ArrEbx.Num)], buildString(' '));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padZeros' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        if (IntEbx mod 2 = 0) 
                            then stack_pushFront(pocz[trunc(ArrEbx.Num)], buildNumber(0.0))
                            else stack_push(pocz[trunc(ArrEbx.Num)], buildNumber(0.0));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padZerosLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_pushFront(pocz[trunc(ArrEbx.Num)], buildNumber(0.0));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padZerosRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_push(pocz[trunc(ArrEbx.Num)], buildNumber(0.0));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padNulls' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        if (IntEbx mod 2 = 0) 
                            then stack_pushFront(pocz[trunc(ArrEbx.Num)], buildNull())
                            else stack_push(pocz[trunc(ArrEbx.Num)], buildNull());
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padNullsLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_pushFront(pocz[trunc(ArrEbx.Num)], buildNull());
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padNullsRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_push(pocz[trunc(ArrEbx.Num)], buildNull());
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padEmptyStrings' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        if (IntEbx mod 2 = 0) 
                            then stack_pushFront(pocz[trunc(ArrEbx.Num)], buildString(''))
                            else stack_push(pocz[trunc(ArrEbx.Num)], buildString(''));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padEmptyStringsLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_pushFront(pocz[trunc(ArrEbx.Num)], buildString(''));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padEmptyStringsRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_push(pocz[trunc(ArrEbx.Num)], buildString(''));
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padEntities' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                EntEax := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        if (IntEbx mod 2 = 0) 
                            then stack_pushFront(pocz[trunc(ArrEbx.Num)], EntEax)
                            else stack_push(pocz[trunc(ArrEbx.Num)], EntEax);
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padEntitiesLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                EntEax := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_pushFront(pocz[trunc(ArrEbx.Num)], EntEax);
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.padEntitiesRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 3).EntityType = TVEC) then
            begin
                EntEax := stack_pop(pocz[sets.StackPointer]);
                if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
                IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                if IntEax < Length(pocz[trunc(ArrEax.Num)].Values) then begin
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end else begin
                    IntEbx := 0;
                    while IntEbx < (IntEax - Length(pocz[trunc(ArrEax.Num)].Values)) do
                    begin
                        stack_push(pocz[trunc(ArrEbx.Num)], EntEax);
                        IntEbx := IntEbx + 1;
                    end;
                    stack_push(pocz[sets.StackPointer], ArrEbx);
                end;
            end else Found := False;
        end;
        'Array.trim' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                while isNull(stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                while isNull(stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimLeft' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                //while isNull(stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                while isNull(stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimRight' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                while isNull(stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                //while isNull(stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimNulls' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                while isNull(stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                while isNull(stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimNullsLeft' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                //while isNull(stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                while isNull(stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimNullsRight' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                while isNull(stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                //while isNull(stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimEntities' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                EntEax := stack_pop(pocz[sets.StackPointer]);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                while EntEax = stack_get(pocz[trunc(ArrEbx.Num)]) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                while EntEax = stack_getFront(pocz[trunc(ArrEbx.Num)]) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimEntitiesLeft' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                EntEax := stack_pop(pocz[sets.StackPointer]);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                //while EntEax = stack_get(pocz[trunc(ArrEbx.Num)])) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                while EntEax = stack_getFront(pocz[trunc(ArrEbx.Num)]) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        'Array.trimEntitiesRight' : begin
            if (stack_getback(pocz[sets.StackPointer], 2).EntityType = TVEC) then
            begin
                EntEax := stack_pop(pocz[sets.StackPointer]);
                ArrEax := stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, 0));
                ArrEbx := stack_pop(pocz[sets.StackPointer]);
                for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
                begin
                    stack_push(pocz[trunc(ArrEbx.Num)], pocz[trunc(ArrEax.Num)].Values[index]);
    		    end;
                index := 0;
                while EntEax = stack_get(pocz[trunc(ArrEbx.Num)]) do stack_pop(pocz[trunc(ArrEbx.Num)]);
                //while EntEax = stack_getFront(pocz[trunc(ArrEbx.Num)])) do stack_popFront(pocz[trunc(ArrEbx.Num)]);
                stack_push(pocz[sets.StackPointer], ArrEbx);
            end else Found := False;
        end;
        else begin
            Found := false;
        end;
	end;
	lib_arrays := Found;
end;

function lib_files(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found  : Boolean;
    StrEax : String; 
begin
	Found := true;
	case i of
        'runFile' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_get(pocz[sets.StackPointer]).Str;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
        	pocz := read_sourcefile(StrEax, pocz, sets, vardb);
        end;    
        else begin
            Found := false;
        end;
	end;
	lib_files := Found;
end;

function lib_datetime(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found          : Boolean;
    StrEax         : String; 
    IntEax         : LongInt;
    ExtEax, ExtEbx : Extended;
    DatEax, DatEbx : TDateTime;
begin
	Found := true;
	case i of
        'Date.toDateTime' : begin
            if (stack_get(pocz[sets.StackPointer]).EntityType = TNUM) then
            begin
                ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
        	    //stack_push(pocz[sets.StackPointer], buildDateTime(TimestampToDateTime(ExtEax)));
                stack_push(pocz[sets.StackPointer], buildDateTime(ExtEax));
            end else begin
                StrEax := stack_pop(pocz[sets.StackPointer]).Str;
                stack_push(pocz[sets.StackPointer], buildDateTime(StringYMDToDateTime(StrEax)));
                //stack_push(pocz[sets.StackPointer], buildDateTime(VarToDateTime(StrEax)));
            end;
        end; 
        'Date.toTimestamp' : begin
            //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            if (stack_get(pocz[sets.StackPointer]).EntityType = TNUM) or (stack_get(pocz[sets.StackPointer]).EntityType = TDAT) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(stack_pop(pocz[sets.StackPointer]).Num));
            end else if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
            begin
                stack_push(pocz[sets.StackPointer], buildNumber(DateTimeToUnix(StringYMDToDateTime(stack_pop(pocz[sets.StackPointer]).Str))));
            end;
        end; 
        'Date.now' : begin
            stack_push(pocz[sets.StackPointer], buildDateTime(Now));
        end; 
        'Date.today' : begin
            stack_push(pocz[sets.StackPointer], buildDateTime(Today));
        end; 
        'Date.yesterday' : begin
            stack_push(pocz[sets.StackPointer], buildDateTime(Yesterday));
        end; 
        'Date.tomorrow' : begin
            stack_push(pocz[sets.StackPointer], buildDateTime(Tomorrow));
        end; 
        'Date.unixEpoch' : begin
            ExtEax := 0;
            stack_push(pocz[sets.StackPointer], buildDateTime(ExtEax));
        end; 
        'Date.truncDate' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            //StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            //DatEax := StringYMDToDateTime(StrEax));
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(DateOf(DatEax)));
        end;
        'Date.truncTime' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(TimeOf(DatEax)));
        end;
        'Date.getYear' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(YearOf(DatEax)));
        end;
        'Date.getMonth' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(MonthOf(DatEax)));
        end;
        'Date.getDay' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(DayOf(DatEax)));
        end;
        'Date.getHour' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(HourOf(DatEax)));
        end;
        'Date.getHour24' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(HourOf(DatEax)));
        end;
        'Date.getHour12' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(((HourOf(DatEax))-1) mod 12 + 1));
        end;
        'Date.getMinute' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(MinuteOf(DatEax)));
        end;
        'Date.getSecond' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(SecondOf(DatEax)));
        end;
        'Date.getMillisecond' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(MillisecondOf(DatEax)));
        end;
        'Date.getWeekDay' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(DayOfTheWeek(DatEax)));
        end;
        'Date.getYearDay' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(DayOfTheYear(DatEax)));
        end;
        'Date.isPM' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildBoolean(IsPM(DatEax)));
        end;
        'Date.isLeapYear' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            stack_push(pocz[sets.StackPointer], buildBoolean(IsLeapYear(IntEax)));
        end;
        'Date.isInLeapYear' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildBoolean(IsInLeapYear(DatEax)));
        end;
        'Date.setYear' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeYear(DatEax, IntEax)));
        end;
        'Date.setMonth' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeMonth(DatEax, IntEax)));
        end;
        'Date.setDay' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeDay(DatEax, IntEax)));
        end;
        'Date.setHour' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeHour(DatEax, IntEax)));
        end;
        'Date.setMinute' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeMinute(DatEax, IntEax)));
        end;
        'Date.setSecond' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeSecond(DatEax, IntEax)));
        end;
        'Date.setMillisecond' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(RecodeMillisecond(DatEax, IntEax)));
        end;
        'Date.addYear' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncYear(DatEax, IntEax)));
        end;
        'Date.addMonth' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncMonth(DatEax, IntEax)));
        end;
        'Date.addDay' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncDay(DatEax, IntEax)));
        end;
        'Date.addHour' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncHour(DatEax, IntEax)));
        end;
        'Date.addMinute' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncMinute(DatEax, IntEax)));
        end;
        'Date.addSecond' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncSecond(DatEax, IntEax)));
        end;
        'Date.addMillisecond' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildDateTime(IncMillisecond(DatEax, IntEax)));
        end;
        'Date.diffYear' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(YearsBetween(DatEax, DatEbx)));
        end;
        'Date.diffMonth' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(MonthsBetween(DatEax, DatEbx)));
        end;
        'Date.diffDay' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(DaysBetween(DatEax, DatEbx)));
        end;
        'Date.diffHour' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(HoursBetween(DatEax, DatEbx)));
        end;
        'Date.diffMinute' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(MinutesBetween(DatEax, DatEbx)));
        end;
        'Date.diffSecond' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(SecondsBetween(DatEax, DatEbx)));
        end;
        'Date.diffMillisecond' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(MillisecondsBetween(DatEax, DatEbx)));
        end;
        'Date.spanYear' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(YearSpan(DatEax, DatEbx)));
        end;
        'Date.spanMonth' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(MonthSpan(DatEax, DatEbx)));
        end;
        'Date.spanDay' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(DaySpan(DatEax, DatEbx)));
        end;
        'Date.spanHour' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(HourSpan(DatEax, DatEbx)));
        end;
        'Date.spanMinute' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(MinuteSpan(DatEax, DatEbx)));
        end;
        'Date.spanSecond' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(SecondSpan(DatEax, DatEbx)));
        end;
        'Date.spanMillisecond' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(MillisecondSpan(DatEax, DatEbx)));
        end;
        'Date.diffWeek' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(WeeksBetween(DatEax, DatEbx)));
        end;
        'Date.spanWeek' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(WeekSpan(DatEax, DatEbx)));
        end;
        'Date.compare' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(CompareDateTime(DatEbx, DatEax)));
        end;
        'Date.compareDate' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(CompareDate(DatEbx, DatEax)));
        end;
        'Date.compareTime' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEbx := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit; 
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            DatEbx := TimestampToDateTime(ExtEbx);
            stack_push(pocz[sets.StackPointer], buildNumber(CompareTime(DatEbx, DatEax)));
        end;
        'Date.getYearWeek' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(WeekOfTheYear(DatEax)));
        end;
        'Date.getMonthWeek' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildNumber(WeekOfTheMonth(DatEax)));
        end;
        'Date.format' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, i)) then Exit;
            //if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TDAT, TSTR, i)) then Exit;  
            ExtEax := stack_pop(pocz[sets.StackPointer]).Num;
            DatEax := TimestampToDateTime(ExtEax);
            stack_push(pocz[sets.StackPointer], buildString(FormatDateTime(StrEax, DatEax)));
        end;
        else begin
            Found := false;
        end;
	end;
	Result := Found;
end;

end.
