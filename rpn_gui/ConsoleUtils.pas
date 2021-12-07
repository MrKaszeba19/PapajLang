unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

function executeCommand(input, Shell : String) : String;
function getUser() : String;
function getHost() : String;
{$IFDEF LINUX}
function getShell() : String;
{$ENDIF}

implementation

uses 
    {$IFDEF MSWINDOWS}
		ShellApi, crt, Windows,
    {$ELSE}
        UnixCrt,
 	{$ENDIF}
    SysUtils, Process;

Function Is64Bit: Boolean;
Begin
  Result:= SizeOf(Pointer) > 4;
End;
 
Function Is32Bit: Boolean;
Begin
  Result:= SizeOf(Pointer) <= 4;
End;

function executeCommand(input, Shell : String) : String;
var
	s : String;
begin
	s := '';
	{$IFDEF MSWINDOWS}
	//RunCommand(Shell,['/c', input],s);
	if ShellExecute(0,nil, PChar('cmd'),PChar('/c '+input),nil,1) =0 then;
 	{$ELSE}
  	RunCommand(Shell,['-c', input], s);
 	{$ENDIF}
 	executeCommand := s;
end;

function getUser() : String;
{$IFDEF WINDOWS}
var
    buffer : array[0..255] of char;
    size   : dword;
{$ENDIF}
begin
    {$IFDEF WINDOWS}
    size := SizeOf(buffer);
    GetUserName(buffer, size);
    Result := buffer;
    {$ELSE}
    Result := GetEnvironmentVariable('USER'); 
    {$ENDIF}
end;

function getHost() : String;
{$IFDEF WINDOWS}
var
    buffer : array[0..255] of char;
    size   : dword;
begin
    size := SizeOf(buffer);//256;
    GetComputerName(buffer, size);
    Result := buffer;
end;
{$ELSE}
var
    F : textFile;
begin
    AssignFile(F,'/etc/hostname');
    Reset(F);
    Readln(F, Result);
    CloseFile(F);
end;
{$ENDIF}


{$IFDEF LINUX}
function getShell() : String;
begin
    Result := GetEnvironmentVariable('SHELL'); 
end;
{$ENDIF}

end.
