unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

function executeCommand(input, Shell : String) : String;
function getUser() : String;
function getHost() : String;
function getOS() : String;
function getOSVersion() : String;
function getOSDistribution() : String;
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

{$IFDEF UNIX}
// https://forum.lazarus.freepascal.org/index.php?topic=34271.0
function UnixSysInfo(str : String) : String;
var 
    P : TProcess;
 
function Exec(): String;
Begin
    P.Execute;
    SetLength(Result, 1000);
    SetLength(Result, P.Output.Read(Result[1], Length(Result)));
    while (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
        SetLength(Result, Length(Result) - 1);
End;

function ExecParam(Param: String): String;
Begin
    P.Parameters[0]:= '-' + Param;
    P.Execute;
    SetLength(Result, 1000);
    SetLength(Result, P.Output.Read(Result[1], Length(Result)));
    while (Length(Result) > 0) And (Result[Length(Result)] In [#8..#13,#32]) Do
        SetLength(Result, Length(Result) - 1);
End;
 
Begin
    P:= TProcess.Create(Nil);
    P.Options:= [poWaitOnExit, poUsePipes];
    P.Executable:= 'uname';
    P.Parameters.Add('');
    case str of
        'os' : Result := ExecParam('o');
        'kernel-name' : Result := ExecParam('s');
        'kernel-release' : Result := ExecParam('r');
        'kernel-version' : Result := ExecParam('v');
        'network-node' : Result := ExecParam('n');
        'architecture' : Result := ExecParam('m');
        else Result := Exec();
    end; 
    P.Free;
End;

function getOSDist(info : String) : String;
var
    F : textFile;
    L : String;
begin
    AssignFile(F,'/etc/os-release');
    Reset(F);
    Result := '';
    while not eof(F) do
    begin
        Readln(F, L);
        if (LeftStr(L, Length(info)) = info) then
        begin
            L := RightStr(L, Length(L) - Length(info) - 2); 
            Result := LeftStr(L, Length(L)-1);
            break;
        end;
    end;
    CloseFile(F);
end;
{$ENDIF}
{$IFDEF WINDOWS}
function getOSNickname(Release : String);
begin
    case Release of
        '1.1'  : Result := '1.01';
        '1.2'  : Result := '1.02';
        '1.3'  : Result := '1.03';
        '1.4'  : Result := '1.04';
        '2.3'  : Result := '2.03';
        '2.10' : Result := '2.10';
        '2.11' : Result := '2.11';
        '3.0'  : Result := '3.0';
        '3.10' : Result := '3.1 or Windows NT 3.1';
        '3.11' : Result := 'for Workgroups 3.11';
        '3.2'  : Result := '3.2';
        '3.50' : Result := 'NT 3.5';
        '3.51' : Result := 'NT 3.51';
        '4.0'  : Result := '95 or Windows NT 4.0';
        '4.10' : Result := '98';
        '5.0'  : Result := '2000';
        '4.90' : Result := 'ME';
        '5.1'  : Result := 'XP';
        '5.2'  : Result := 'XP Professional x64';
        '6.0'  : Result := 'Vista';
        '6.1'  : Result := '7';
        '6.2'  : Result := '8';
        '6.3'  : Result := '8.1';
        '10.0' : Result := '10';
        else Result := 'v'+Release;
    end;
 
  ShowMessage('Your OS is '+OS+' '+Version);
 
end;
{$ENDIF}

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

function getOS() : String;
begin
    Result := 'Other';
    {$IFDEF UNIX}
    Result := 'Unix';
    {$ENDIF}
    {$IFDEF LINUX}
    Result := 'Linux';
    {$ENDIF}
    {$ifdef FreeBSD}
    Result := 'FreeBSD';
    {$endif}
    {$ifdef NetBSD}
    Result := 'NetBSD';
    {$endif}
    {$ifdef Solaris}
    Result := 'Solaris';
    {$endif}
    {$ifdef QNX}
    Result := 'QNX';
    {$endif}
    {$IFDEF DOS}
    Result := 'DOS';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := 'Windows';
    {$ENDIF}
end;

function getOSVersion() : String;
begin
    {$IFDEF WINDOWS}
    // https://forum.lazarus.freepascal.org/index.php/topic,32847.msg212007.html#msg212007
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ELSE}
    Result := UnixSysInfo('kernel-release');
    {$ENDIF}
end;

function getOSDistribution() : String;
begin
    {$IFDEF WINDOWS}
    // https://forum.lazarus.freepascal.org/index.php/topic,32847.msg212007.html#msg212007
    Result := getOSNickname(IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion)));
    {$ELSE}
    Result := getOSDist('PRETTY_NAME');
    {$ENDIF}
end;


{$IFDEF LINUX}
function getShell() : String;
begin
    Result := GetEnvironmentVariable('SHELL'); 
end;
{$ENDIF}

end.
