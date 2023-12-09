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
    DATE_US_DT = 'mm/dd/yyyy hh:nn:ss am/pm';
    DATE_US_D = 'mm-dd-yyyy';
    DATE_US_T = 'hh:nn:ss am/pm';
    DATE_US_TP = 'hh:nn:ss.zzz am/pm';
    DATE_US_DTP = 'mm-dd-yyyy hh:nn:ss.zzz am/pm';
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
function StringMDYToDateTime(input : String) : TDateTime;
function StringDMYToDateTime(input : String) : TDateTime;

function FormatYMD(input : TDateTime) : String;

function DateTimeToTimestamp(input : TDateTime) : Real;
function TimestampToDateTime(input : Real) : TDateTime;

function DateTimeInRange(date, dstart, dend : TDateTime; Inclusive : Boolean = True) : Boolean;
function DateInRange(date, dstart, dend : TDateTime; Inclusive : Boolean = True) : Boolean;
function TimeInRange(date, dstart, dend : TDateTime; Inclusive : Boolean = True) : Boolean;


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

function StringMDYToDateTime(input : String) : TDateTime;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.ShortDateFormat := DATE_US_D;
    FS.ShortTimeFormat := DATE_US_TP;
    FS.DateSeparator := '/';
    Result := StrToDateTime(input, FS);
end;

function StringDMYToDateTime(input : String) : TDateTime;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.ShortDateFormat := DATE_UK_D;
    FS.ShortTimeFormat := DATE_UK_TP;
    FS.DateSeparator := '.';
    Result := StrToDateTime(input, FS);
end;

function FormatYMD(input : TDateTime) : String;
//var
//    FS: TFormatSettings;
begin
    //FS := DefaultFormatSettings;
    ////FS.DateSeparator := '/';
    ////FS.ShortDateFormat := 'yyyy/mm/dd';
    ////FS.ShortTimeFormat := 'hh:mm:ss';
    //FS.DateSeparator := '-';
    //FS.ShortDateFormat := DATE_UN_D;
    //FS.ShortTimeFormat := DATE_UN_TP;
    ////writeln('chj', DateTimeToStr(input, FS));
    //Result := DateTimeToStr(input, FS);
    Result := FormatDateTime(DATE_UN_DTP, input);
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
    if (input = int(input)) 
        then Result := UnixToDateTime(trunc(input))
        else Result := IncMillisecond(UnixToDateTime(trunc(input)), trunc(frac(input)*1000));
end;

function DateTimeInRange(date, dstart, dend : TDateTime; Inclusive : Boolean = True) : Boolean;
begin
    if Inclusive then
    begin
        if CompareDateTime(dstart, dend) < 0 then
        begin
            Result := (CompareDateTime(dstart, date) <= 0) and (CompareDateTime(date, dend) <= 0);
        end else if SameDateTime(dstart, dend) then
        begin
            Result := SameDateTime(dstart, date);
        end else begin
            Result := (CompareDateTime(dend, date) <= 0) and (CompareDateTime(date, dstart) <= 0);
        end;
    end else begin
        if CompareDateTime(dstart, dend) < 0 then
        begin
            Result := (CompareDateTime(dstart, date) < 0) and (CompareDateTime(date, dend) < 0);
        end else if SameDateTime(dstart, dend) then
        begin
            Result := False;
        end else begin
            Result := (CompareDateTime(dend, date) < 0) and (CompareDateTime(date, dstart) < 0);
        end;
    end;
end;

function DateInRange(date, dstart, dend : TDateTime; Inclusive : Boolean = True) : Boolean;
begin
    if Inclusive then
    begin
        if CompareDate(dstart, dend) < 0 then
        begin
            Result := (CompareDate(dstart, date) <= 0) and (CompareDate(date, dend) <= 0);
        end else if SameDate(dstart, dend) then
        begin
            Result := SameDate(dstart, date);
        end else begin
            Result := (CompareDate(dend, date) <= 0) and (CompareDate(date, dstart) <= 0);
        end;
    end else begin
        if CompareDate(dstart, dend) < 0 then
        begin
            Result := (CompareDate(dstart, date) < 0) and (CompareDate(date, dend) < 0);
        end else if SameDate(dstart, dend) then
        begin
            Result := False;
        end else begin
            Result := (CompareDate(dend, date) < 0) and (CompareDate(date, dstart) < 0);
        end;
    end;
end;

function TimeInRange(date, dstart, dend : TDateTime; Inclusive : Boolean = True) : Boolean;
begin
    if Inclusive then
    begin
        if CompareTime(dstart, dend) < 0 then
        begin
            Result := (CompareTime(dstart, date) <= 0) and (CompareTime(date, dend) <= 0);
        end else if SameTime(dstart, dend) then
        begin
            Result := SameTime(dstart, date);
        end else begin
            Result := (CompareTime(dend, date) <= 0) and (CompareTime(date, dstart) <= 0);
        end;
    end else begin
        if CompareTime(dstart, dend) < 0 then
        begin
            Result := (CompareTime(dstart, date) < 0) and (CompareTime(date, dend) < 0);
        end else if SameTime(dstart, dend) then
        begin
            Result := False;
        end else begin
            Result := (CompareTime(dend, date) < 0) and (CompareTime(date, dstart) < 0);
        end;
    end;
end;

end.

