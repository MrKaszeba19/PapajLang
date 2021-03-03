unit DTUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
    DATE_USA_DT = 'mm/dd/yyyy hh:nn:ss am/pm';
    DATE_USA_D = 'mm/dd/yyyy';
    DATE_USA_T = 'hh:nn:ss am/pm';
    DATE_USA_TP = 'hh:nn:ss:zzz am/pm';
    DATE_USA_DTP = 'mm/dd/yyyy hh:nn:ss:zzz am/pm';
    DATE_UK_DT = 'dd.mm.yyyy hh:nn:ss am/pm';
    DATE_UK_D = 'dd.mm.yyyy';
    DATE_UK_T = 'hh:nn:ss am/pm';
    DATE_UK_TP = 'hh:nn:ss:zzz am/pm';
    DATE_UK_DTP = 'dd.mm.yyyy hh:nn:ss:zzz am/pm';
    DATE_EU_DT = 'dd.mm.yyyy hh:nn:ss';
    DATE_EU_D = 'dd.mm.yyyy';
    DATE_EU_T = 'hh:nn:ss';
    DATE_EU_TP = 'hh:nn:ss:zzz';
    DATE_EU_DTP = 'dd.mm.yyyy hh:nn:ss:zzz';
    DATE_UN_DT = 'yyyy-mm-dd hh:nn:ss';
    DATE_UN_D = 'yyyy-mm-dd';
    DATE_UN_T = 'hh:nn:ss';
    DATE_UN_TP = 'hh:nn:ss:zzz';
    DATE_UN_DTP = 'yyyy-mm-dd hh:nn:ss:zzz';

function StringYMDToDateTime(input : String) : TDateTime;

function FormatYMD(input : TDateTime) : String;

implementation

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
    FS.ShortTimeFormat := DATE_UN_T;
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
    FS.ShortTimeFormat := DATE_UN_T;
    //writeln('chj', DateTimeToStr(input, FS));
    Result := DateTimeToStr(input, FS);
end;


end.

