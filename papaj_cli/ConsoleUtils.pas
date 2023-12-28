unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

function executeCommand(input, Shell : String) : String;
function getUser() : String;
function getHost() : String;
function getKernelName() : String;
function getKernelVersion() : String;
function getOS() : String;
function getOSVersion() : String;
function getOSDistribution() : String;
function getOSDistributionFull() : String;
function getCPUArch() : String;
function getCPUBits() : LongInt;
function isUnix() : Boolean;
function isFreeBSD() : Boolean;
function isMacOS() : Boolean;
function isLinux() : Boolean;
function isWindows() : Boolean;
{$IFDEF LINUX}
{$I ConsoleLinuxHeaders.fph}
{$ENDIF}
{$IFDEF WINDOWS}
{$I ConsoleWindowsHeaders.fph}
{$ENDIF}

implementation

uses 
    {$IFDEF MSWINDOWS}
		ShellApi, crt, Dos, Windows,
        registry,
    {$ELSE}
        UnixCrt,
 	{$ENDIF}
    SysUtils, StrUtils, Process;

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
function getOSNickname(Release : String) : String;
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
        '3.10' : Result := '3.1 or NT 3.1';
        '3.11' : Result := 'for Workgroups 3.11';
        '3.2'  : Result := '3.2';
        '3.50' : Result := 'NT 3.5';
        '3.51' : Result := 'NT 3.51';
        '4.0'  : Result := '95 or NT 4.0';
        '4.10' : Result := '98';
        '4.90' : Result := 'ME';
        '5.0'  : Result := '2000';
        '5.1'  : Result := 'XP';
        '5.2'  : Result := 'XP Professional x64';
        '6.0'  : Result := 'Vista';
        '6.1'  : Result := '7';
        '6.2'  : Result := '8';
        '6.3'  : Result := '8.1';
        '10.0' : Result := '10';
        else Result := 'v'+Release;
    end; 
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
    {$ifdef BSD}
    Result := 'BSD';
    {$endif}
    {$ifdef OpenBSD}
    Result := 'OpenBSD';
    {$endif}
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
    {$IFDEF HASAMIGA}
    Result := 'Amiga-like';
    {$ENDIF}
    {$IFDEF AMIGA}
    Result := 'Amiga';
    {$ENDIF}
    {$IFDEF MACOS}
    Result := 'Macintosh';
    {$ENDIF}
    {$IFDEF Darwin}
    Result := trim(executeCommand('sw_vers -productName','/bin/sh'));
    {$ENDIF}
    {$IFDEF DOS}
    Result := 'DOS';
    {$ENDIF}
    {$IFDEF MSDOS}
    Result := 'MS-DOS';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := 'Windows';
    {$ENDIF}
end;

function getKernelName() : String;
begin
    Result := '';
    {$IFDEF DOS}
    Result := 'DOS';
    {$ENDIF}
    {$IFDEF MSDOS}
    Result := 'MS-DOS';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := 'NT';
    {$ENDIF}
    {$IFDEF Win16}
    Result := 'MS-DOS';
    {$ENDIF}
    {$IFDEF Unix}
    Result := UnixSysInfo('kernel-name');
    {$ENDIF}
end;

function getKernelVersion() : String;
begin
    Result := '';
    {$IFDEF DOS}
    // https://forum.lazarus.freepascal.org/index.php/topic,32847.msg212007.html#msg212007
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF MSDOS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF Win16}
    // fix it for proper doses
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF Unix}
    Result := UnixSysInfo('kernel-release');
    {$ENDIF}
end;

function getOSVersion() : String;
begin
    Result := '';
    {$IFDEF WINDOWS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF DOS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF MSDOS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF Unix}
    Result := UnixSysInfo('kernel-release');
    {$ENDIF}
    {$IFDEF MacOS}
    Result := trim(executeCommand('sw_vers -productVersion','/bin/sh'));
    {$ENDIF}
    {$IFDEF Darwin}
    Result := trim(executeCommand('sw_vers -productVersion','/bin/sh'));
    {$ENDIF}
    {$IFDEF FreeBSD}
    Result := getOSDist('VERSION');
    {$ENDIF}
    {$IFDEF Linux}
    Result := getOSDist('VERSION');
    {$ENDIF}
end;

function getOSDistribution() : String;
begin
    Result := '';
    {$IFDEF DOS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF MSDOS}
    Result := IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := getOSNickname(IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion)));
    {$ENDIF}
    {$IFDEF Unix}
    Result := UnixSysInfo('os');
    {$ENDIF}
    {$IFDEF MacOS}
    // unsure
    Result := executeCommand('sw_vers -productVersion','/bin/sh');
    {$ENDIF}
    {$IFDEF Darwin}
    Result := trim(executeCommand('sw_vers -productName','/bin/sh'))+' '+trim(executeCommand('sw_vers -productVersion','/bin/sh'));
    {$ENDIF}
    {$IFDEF FreeBSD}
    Result := getOSDist('PRETTY_NAME');
    {$ENDIF}
    {$IFDEF Linux}
    Result := getOSDist('PRETTY_NAME');
    {$ENDIF}
end;

function getOSDistributionFull() : String;
begin
    //{$IFDEF WINDOWS}
    //Result := 'Windows ' + getOSNickname(IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion)));
    //{$ELSE}
    //Result := getOSDist('PRETTY_NAME');
    //{$ENDIF}
    Result := '';
    {$IFDEF DOS}
    Result := 'DOS '+IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF MSDOS}
    Result := 'MS-DOS '+IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion));
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := 'Windows '+getOSNickname(IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion)));
    {$ENDIF}
    {$IFDEF Unix}
    Result := UnixSysInfo('os');
    {$ENDIF}
    {$IFDEF MacOS}
    // unsure
    Result := 'Macintosh '+executeCommand('sw_vers -productVersion','/bin/sh');
    {$ENDIF}
    {$IFDEF Darwin}
    Result := trim(executeCommand('sw_vers -productName','/bin/sh'))+' '+trim(executeCommand('sw_vers -productVersion','/bin/sh'));
    {$ENDIF}
    {$IFDEF FreeBSD}
    Result := getOSDist('PRETTY_NAME');
    {$ENDIF}
    {$IFDEF Linux}
    Result := getOSDist('PRETTY_NAME');
    {$ENDIF}
end;

function getCPUArch() : String;
begin
    Result := 'Other';
    {$iFDEF CPU8}
    Result := '8-bit';
    {$ENDIF}
    {$iFDEF CPU16}
    Result := '16-bit';
    {$ENDIF}
    {$iFDEF CPU32}
    Result := '32-bit';
    {$ENDIF}
    {$IFDEF CPU64}
    Result := '64-bit';
    {$ENDIF}

    {$IFDEF CPU86}
    Result := '8086';
    {$ENDIF}
    {$IFDEF CPU386}
    Result := 'x86';
    {$ENDIF}
    {$IFDEF CPUi386}
    Result := 'i386';
    {$ENDIF}
    {$IFDEF CPUAMD64}
    Result := 'amd64';
    {$ENDIF}
    {$IFDEF CPUX86_64}
    Result := 'x86_64';
    {$ENDIF}
    {$IFDEF AMD64}
    Result := 'amd64';
    {$ENDIF}
    {$IFDEF CPUIA64}
    Result := 'IA-64';
    {$ENDIF}

    {$iFDEF CPUPOWERPC}
    Result := 'PowerPC';
    {$ENDIF}
    {$iFDEF CPUPOWERPC32}
    Result := 'PowerPC';
    {$ENDIF}
    {$IFDEF CPUPOWERPC64}
    Result := 'PowerPC';
    {$ENDIF}

    {$iFDEF CPUARM}
    Result := 'ARM';
    {$ENDIF}
    {$iFDEF CPUARM32}
    Result := 'ARM';
    {$ENDIF}
    {$IFDEF CPUAARCH64}
    Result := 'ARM';
    {$ENDIF}
    {$IFDEF CPUARM64}
    Result := 'ARM';
    {$ENDIF}
end;

function getCPUBits() : LongInt;
begin
    Result := -1;
    {$iFDEF CPU8}
    Result := 8;
    {$ENDIF}
    {$iFDEF CPU16}
    Result := 16;
    {$ENDIF}
    {$iFDEF CPU32}
    Result := 32;
    {$ENDIF}
    {$IFDEF CPU64}
    Result := 64;
    {$ENDIF}

    {$IFDEF CPU86}
    Result := 16;
    {$ENDIF}
    {$IFDEF CPU386}
    Result := 32;
    {$ENDIF}
    {$IFDEF CPUi386}
    Result := 32;
    {$ENDIF}
    {$IFDEF CPUAMD64}
    Result := 64;
    {$ENDIF}
    {$IFDEF CPUX86_64}
    Result := 64;
    {$ENDIF}
    {$IFDEF AMD64}
    Result := 64;
    {$ENDIF}
    {$IFDEF CPUIA64}
    Result := 64;
    {$ENDIF}

    {$iFDEF CPUPOWERPC32}
    Result := 32;
    {$ENDIF}
    {$IFDEF CPUPOWERPC64}
    Result := 64;
    {$ENDIF}

    {$iFDEF CPUARM}
    Result := 32;
    {$ENDIF}
    {$iFDEF CPUARM32}
    Result := 32;
    {$ENDIF}
    {$IFDEF CPUAARCH64}
    Result := 64;
    {$ENDIF}
    {$IFDEF CPUARM64}
    Result := 64;
    {$ENDIF}
end;

function isUnix() : Boolean;
begin
    {$IFDEF UNIX} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

function isFreeBSD() : Boolean;
begin
    {$IFDEF FreeBSD} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

function isMacOS() : Boolean;
begin
    {$IFDEF MacOS} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

function isLinux() : Boolean;
begin
    {$IFDEF LINUX} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

function isWindows() : Boolean;
begin
    {$IFDEF WINDOWS} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

{$IFDEF WINDOWS}
{$I ConsoleWindowsImpl.fph}
{$endif}

{$IFDEF LINUX}
{$I ConsoleLinuxImpl.fph}
{$ENDIF}

end.
