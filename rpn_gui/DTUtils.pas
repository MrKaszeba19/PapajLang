unit DTUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
    DATE_USA_DT = 'mm/dd/yyyy hh:nn:ss am/pm';
    DATE_USA_D = 'mm/dd/yyyy';
    DATE_USA_T = 'hh:nn:ss am/pm';
    DATE_USA_TP = 'hh:nn:ss.zzz am/pm';
    DATE_USA_DTP = 'mm/dd/yyyy hh:nn:ss.zzz am/pm';
    DATE_UK_DT = 'dd.mm.yyyy hh:nn:ss am/pm';
    DATE_UK_D = 'dd.mm.yyyy';
    DATE_UK_T = 'hh:nn:ss am/pm';
    DATE_UK_TP = 'hh:nn:ss.zzz am/pm';
    DATE_UK_DTP = 'dd.mm.yyyy hh:nn:ss.zzz am/pm';
    DATE_EU_DT = 'dd.mm.yyyy hh:nn:ss';
    DATE_EU_D = 'dd.mm.yyyy';
    DATE_EU_T = 'hh:nn:ss';
    DATE_EU_TP = 'hh:nn:ss.zzz';
    DATE_EU_DTP = 'dd.mm.yyyy hh:nn:ss.zzz';
    DATE_UN_DT = 'yyyy-mm-dd hh:nn:ss';
    DATE_UN_D = 'yyyy-mm-dd';
    DATE_UN_T = 'hh:nn:ss';
    DATE_UN_TP = 'hh:nn:ss.zzz';
    DATE_UN_DTP = 'yyyy-mm-dd hh:nn:ss.zzz';

function StringYMDToDateTime(input : String) : TDateTime;

function FormatYMD(input : TDateTime) : String;

function DateTimeToTimestamp(input : TDateTime) : Real;
function TimestampToDateTime(input : Real) : TDateTime;

implementation

uses DateUtils;

function StringYMDToDateTime(input : String) : TDateTime;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    //FS.DateSeparator := '/';
    //FS.ShortDateFormat := 'yyyy/mm/dd';
    //FS.ShortTimeFormat := 'hh:mm:ss';
    FS.DateSeparator := '-';
    FS.ShortDateFormat := DATE_UN_D;
    FS.ShortTimeFormat := DATE_UN_TP;
    Result := StrToDateTime(input, FS);
end;

function FormatYMD(input : TDateTime) : String;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    //FS.DateSeparator := '/';
    //FS.ShortDateFormat := 'yyyy/mm/dd';
    //FS.ShortTimeFormat := 'hh:mm:ss';
    FS.DateSeparator := '-';
    FS.ShortDateFormat := DATE_UN_D;
    FS.ShortTimeFormat := DATE_UN_TP;
    //writeln('chj', DateTimeToStr(input, FS));
    Result := DateTimeToStr(input, FS);
end;

function DateTimeToTimestamp(input : TDateTime) : Real;
var
    ts : LongInt;
    ms : Word;
    rs : Currency;
begin
    ts := DateTimeToUnix(RecodeMillisecond(input, 0));
    ms := MillisecondOf(input);
    rs := ts + ms/1000;
    Result := rs;
end;

function TimestampToDateTime(input : Real) : TDateTime;
begin
    if (input = int(input)) then
    begin
        Result := UnixToDateTime(trunc(input));
    end else begin
        if (input < 0)
            then Result := IncMillisecond(UnixToDateTime(trunc(input)), -trunc(frac(input)*1000))
            else Result := IncMillisecond(UnixToDateTime(trunc(input)), trunc(frac(input)*1000));
    end; 
end;


end.

