unit ConsoleUtils;

{$mode objfpc}{$H+}

interface

function executeCommand(input, Shell : String) : String;
function getUser() : String;
function getHost() : String;
function getOS() : String;
function getOSVersion() : String;
function getOSDistribution() : String;
function getOSDistributionFull() : String;
function getCPUArch() : String;
function getCPUBits() : LongInt;
//function getCPUName() : String;
{$ifndef FreeBSD}
function getCPUName(index : LongInt) : String;
function getRealCPUThreads() : LongInt;
function getCPUBaseFreq(index : LongInt) : Extended;
{$endif}
function isUnix() : Boolean;
function isFreeBSD() : Boolean;
function isLinux() : Boolean;
function isWindows() : Boolean;
{$IFDEF LINUX}
function getRealCPUArch() : String;
function getRealCPUBits() : LongInt;
function getUptime() : Extended;
function getRAMTotal() : Extended;
function getRAMFree() : Extended;
function getRAMUsage() : Extended;
function getRAMAvailable() : Extended;
function getSwapTotal() : Extended;
function getSwapFree() : Extended;
function getSwapUsage() : Extended;
function getScreenResolution() : String;
function getScreenWidth() : Extended;
function getScreenHeight() : Extended;
function getShell() : String;
function getCPUMaxFreq(index : LongInt) : Extended;
function getCPUMinFreq(index : LongInt) : Extended;
function getCPUCurFreq(index : LongInt) : Extended;
function getCPUUsage(index : LongInt = -1) : Extended;
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
    {$IFDEF HASAMIGA}
    Result := 'Amiga-like';
    {$ENDIF}
    {$IFDEF AMIGA}
    Result := 'Amiga';
    {$ENDIF}
    {$IFDEF MACOS}
    Result := 'MacOS';
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

function getOSDistributionFull() : String;
begin
    {$IFDEF WINDOWS}
    // https://forum.lazarus.freepascal.org/index.php/topic,32847.msg212007.html#msg212007
    Result := 'Windows ' + getOSNickname(IntToStr(Lo(DosVersion))+ '.'+IntToStr(Hi(DosVersion)));
    {$ELSE}
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

function isLinux() : Boolean;
begin
    {$IFDEF LINUX} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

function isWindows() : Boolean;
begin
    {$IFDEF WINDOWS} Result := True; {$ELSE} Result := False; {$ENDIF}
end;

{$IFDEF WINDOWS}

//function getCPUName() : String;
function getCPUName(index : LongInt) : String;
var
    CompileCommand: string='';
    Registry: TRegistry;
begin
    Registry := TRegistry.Create;
    try
        // Navigate to proper "directory":
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        //if Registry.OpenKeyReadOnly('\SOFTWARE\Classes\InnoSetupScriptFile\shell\Compile\Command') then
        if Registry.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System\CentralProcessor\'+IntToStr(index)) then
            //CompileCommand:=Registry.ReadString(''); //read the value of the default name
            CompileCommand:=Registry.ReadString('ProcessorNameString'); //read the value of the default name
    finally
        Registry.Free;  // In non-Windows operating systems this flushes the reg.xml file to disk
    end;
    Result := CompileCommand;
end;

function getCPUBaseFreq(index : LongInt) : Extended;
var
    CompileCommand: string='';
    Registry: TRegistry;
    fl : LongInt;
begin
    Registry := TRegistry.Create;
    try
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        if Registry.OpenKeyReadOnly('\HARDWARE\DESCRIPTION\System\CentralProcessor\'+IntToStr(index)) then
            fl := Registry.ReadInteger('~MHz');  
    finally
        Registry.Free; 
    end;
    Result := fl;
end;

function getRealCPUThreads() : LongInt;
begin
    Result := GetCPUCount;
    //Result := sysconf(83); 
end;
{$endif}

{$IFDEF LINUX}
function getRealCPUArch() : String;
begin
    Result := UnixSysInfo('architecture');
end;

function getRealCPUBits() : LongInt;
begin
    Result := StrToInt(executeCommand('getconf LONG_BIT', GetEnvironmentVariable('SHELL')));
end;

function getRealCPUThreads() : LongInt;
begin
    Result := StrToInt(trim(executeCommand('nproc --all', GetEnvironmentVariable('SHELL'))));
    //Result := GetCPUCount;
    //Result := sysconf(83); 
end;

function getRAMUsageByPID(pid : String) : String;
var
    fn : Text;
    s  : String;
begin
    // /proc/PID/status
    assignfile(fn, '/proc/'+ParamStr(1)+'/status');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 6) = 'VmRSS:') then 
        begin
            s := TrimLeft(RightStr(s, Length(s)-6));
            break;
        end;
    end;
    closefile(fn);
    Result := s;
end;

function getCPUName(index : LongInt) : String;
var
    fn : Text;
    s  : String;
    id : LongInt = 0;
begin
    assignfile(fn, '/proc/cpuinfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 10) = 'model name') then 
        begin
            if (id < index) then begin
                id := id + 1;
                continue;
            end;
            s := TrimLeft(RightStr(s, Length(s)-10));
            s := RightStr(s, Length(s)-2);
            if (id = index) then break;
        end;
    end;
    closefile(fn);
    Result := s;
end;

function getUptime() : Extended;
var
    fn : Text;
    s  : String;
    t  : Extended;
    fl : LongInt;
begin
    assignfile(fn, '/proc/uptime');
    reset(fn);
    readln(fn, s);
    s := s.Split([' '])[0];
    closefile(fn);
    val(s, t, fl);
    if fl = 0 
        then Result := t
        else Result := -1;
end;

function getRAMTotal() : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 9) = 'MemTotal:') then 
        begin
            //writeln(s);
            s := TrimLeft(RightStr(s, Length(s)-9));
            s := LeftStr(s, Length(s)-3);
            break;
        end;
    end;
    closefile(fn);
    Result := StrToInt(s);
end;

function getRAMAvailable() : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 13) = 'MemAvailable:') then 
        begin
            //writeln(s);
            s := TrimLeft(RightStr(s, Length(s)-13));
            s := LeftStr(s, Length(s)-3);
            break;
        end;
    end;
    closefile(fn);
    Result := StrToInt(s);
end;

function getRAMUsage() : Extended;
var
    fn   : Text;
    s, t : String;
    flag : Boolean = False;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 9) = 'MemTotal:') then 
        begin
            s := TrimLeft(RightStr(s, Length(s)-9));
            s := LeftStr(s, Length(s)-3);
            Flag := True;
            break;
        end;
    end;
    if Flag then
        while not eof(fn) do
        begin
            readln(fn, t);
            if (LeftStr(t, 13) = 'MemAvailable:') then 
            begin
                t := TrimLeft(RightStr(t, Length(t)-13));
                t := LeftStr(t, Length(t)-3);
                break;
            end;
        end;
    closefile(fn);
    if Flag 
        then Result := StrToInt(s) - StrToInt(t)
        else Result := -1;
end;


function getRAMFree() : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 8) = 'MemFree:') then 
        begin
            //writeln(s);
            s := TrimLeft(RightStr(s, Length(s)-8));
            s := LeftStr(s, Length(s)-3);
            break;
        end;
    end;
    closefile(fn);
    Result := StrToInt(s);
end;

function getSwapTotal() : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 10) = 'SwapTotal:') then 
        begin
            //writeln(s);
            s := TrimLeft(RightStr(s, Length(s)-10));
            s := LeftStr(s, Length(s)-3);
            break;
        end;
    end;
    closefile(fn);
    Result := StrToInt(s);
end;

function getSwapFree() : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 9) = 'SwapFree:') then 
        begin
            //writeln(s);
            s := TrimLeft(RightStr(s, Length(s)-9));
            s := LeftStr(s, Length(s)-3);
            break;
        end;
    end;
    closefile(fn);
    Result := StrToInt(s);
end;

function getSwapUsage() : Extended;
var
    fn   : Text;
    s, t : String;
    flag : Boolean = False;
begin
    assignfile(fn, '/proc/meminfo');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, 10) = 'SwapTotal:') then 
        begin
            s := TrimLeft(RightStr(s, Length(s)-10));
            s := LeftStr(s, Length(s)-3);
            Flag := True;
            break;
        end;
    end;
    if Flag then
        while not eof(fn) do
        begin
            readln(fn, t);
            if (LeftStr(t, 9) = 'SwapFree:') then 
            begin
                t := TrimLeft(RightStr(t, Length(t)-9));
                t := LeftStr(t, Length(t)-3);
                break;
            end;
        end;
    closefile(fn);
    if Flag 
        then Result := StrToInt(s) - StrToInt(t)
        else Result := -1;
end;

function getScreenResolution() : String;
var
    s  : String;
begin
    s := executeCommand('xrandr | fgrep ''*''', GetEnvironmentVariable('SHELL'));
    s := TrimLeft(s);
    s := s.Split([' '])[0];
    Result := s;
end;

function getScreenWidth() : Extended;
var
    s  : String;
begin
    s := executeCommand('xrandr | fgrep ''*''', GetEnvironmentVariable('SHELL'));
    s := TrimLeft(s);
    s := s.Split([' '])[0];
    s := s.Split(['x'])[0];
    Result := StrToInt(s);
end;

function getScreenHeight() : Extended;
var
    s  : String;
begin
    s := executeCommand('xrandr | fgrep ''*''', GetEnvironmentVariable('SHELL'));
    s := TrimLeft(s);
    s := s.Split([' '])[0];
    s := s.Split(['x'])[1];
    Result := StrToInt(s);
end;

// /proc/meminfo - ram and swap
// /proc/cpuinfo - cpu
// lscpu - better cpu
// /proc/uptime
// /proc/diskstats

//SwapTotal:       8299516 kB
//SwapFree:        8279292 kB


function getShell() : String;
begin
    Result := GetEnvironmentVariable('SHELL'); 
end;

// /sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq

function getCPUMaxFreq(index : LongInt) : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/sys/devices/system/cpu/cpu'+IntToStr(index)+'/cpufreq/scaling_max_freq');
    reset(fn);
    readln(fn, s);
    closefile(fn);
    Result := StrToInt(s);
end;

function getCPUMinFreq(index : LongInt) : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/sys/devices/system/cpu/cpu'+IntToStr(index)+'/cpufreq/scaling_min_freq');
    reset(fn);
    readln(fn, s);
    closefile(fn);
    Result := StrToInt(s);
end;

function getCPUCurFreq(index : LongInt) : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/sys/devices/system/cpu/cpu'+IntToStr(index)+'/cpufreq/scaling_cur_freq');
    reset(fn);
    readln(fn, s);
    closefile(fn);
    Result := StrToInt(s);
end;

function getCPUBaseFreq(index : LongInt) : Extended;
var
    fn : Text;
    s  : String;
begin
    assignfile(fn, '/sys/devices/system/cpu/cpu'+IntToStr(index)+'/cpufreq/base_frequency');
    reset(fn);
    readln(fn, s);
    closefile(fn);
    Result := StrToInt(s);
end;

function getCPUUsage(index : LongInt = -1) : Extended;
var
    fn   : Text;
    s    : String;
    t    : String;
    t0   : LongInt;
    ms   : TStringArray;
    ml   : LongInt;
    id   : LongInt;
    st   : array of LongInt; 
    sum1 : LongInt = 0;
    idl1 : LongInt = -1;
    usd1 : LongInt = -1;
    usg1 : Extended = -1;
    sum2 : LongInt = 0;
    idl2 : LongInt = -1;
    usd2 : LongInt = -1;
    usg2 : Extended = -1;
begin
    if index = -1 then
    begin
        t := 'cpu';
        t0 := 3;
    end else begin
        t := 'cpu'+IntToStr(index);
        t0 := Length(t);
    end;
    // test 1
    assignfile(fn, '/proc/stat');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, t0) = t) then 
        begin
            s := Trim(DelSpace1(RightStr(s, Length(s)-t0)));
            ms := s.Split([' ']);
            ml := Length(ms);
            SetLength(st, ml);
            for id := 0 to ml-1 do st[id] := StrToInt(ms[id]);
            // sum all 
            for id := 0 to ml-1 do sum1 := sum1 + st[id];
            // calc time spent idle
            idl1 := st[3];
            // Calc time spent working 
            usd1 := sum1 - idl1; 
            // calc ratio
            usg1 := 1.0*usd1 / sum1;
            break;
        end;
    end;
    closefile(fn);
    Delay(50);
    // test 2
    assignfile(fn, '/proc/stat');
    reset(fn);
    while not eof(fn) do
    begin
        readln(fn, s);
        if (LeftStr(s, t0) = t) then 
        begin
            s := Trim(DelSpace1(RightStr(s, Length(s)-t0)));
            ms := s.Split([' ']);
            ml := Length(ms);
            SetLength(st, ml);
            for id := 0 to ml-1 do st[id] := StrToInt(ms[id]);
            // sum all and subtract old sum
            for id := 0 to ml-1 do sum2 := sum2 + st[id];
            //writeln(sum2);
            //writeln(sum1);
            sum2 := sum2 - sum1;
            // calc time spent idle and subtract old idle
            idl2 := st[3];
            idl2 := idl2 - idl1;
            // Calc time spent working 
            usd2 := sum2 - idl2; 
            // calc ratio
            //writeln(sum2);
            usg2 := 1.0*usd2 / sum2;
            break;
        end;
    end;
    closefile(fn);
    //Result := usg1;
    Result := usg2;
    //SetLength(st, 0);
end;

{$ENDIF}

end.
