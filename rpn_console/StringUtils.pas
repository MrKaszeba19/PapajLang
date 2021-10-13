unit StringUtils;

{$mode objfpc}{$H+}

interface

const LineBreak = #10;

function OccurrencesOfChar(const S: string; const C: char) : integer;
function OccurrencesOfChar2(const S: string; const C: char) : integer;
function capitalize(dupa : char) : char;
function string_toC(dupa : String) : String;

function PadCharsCenter(str : String; size : LongInt; chr : Char) : String;
function OccurrencesOfSubstring(str, sub : String) : LongInt;
function TrimChars(str : String; chr : Char) : String;
function TrimCharsLeft(str : String; chr : Char) : String;
function TrimCharsRight(str : String; chr : Char) : String;
function RemoveCharset(a : String; b : String) : String;  

implementation

uses SysUtils, StrUtils;

function OccurrencesOfChar(const S: string; const C: char): integer;
var
    i: Integer;
begin
    result := 0;
    for i := 1 to Length(S) do
        if S[i] = C then
            inc(result);
end;

function OccurrencesOfChar2(const S: string; const C: char): integer;
var
    comment : Boolean;
    i       : Integer;
begin
    comment := False;
    result := 0;
    for i := 1 to Length(S) do
    begin
        if (S[i] = '"') then
            if (i > 1) and (not (S[i-1] = '\')) 
                then comment := not comment
                else if (i = 1) then
                    comment := not comment;
        if (not comment) and (S[i] = C) then
            inc(result);
    end;
end;

function capitalize(dupa : char) : char;
begin
    if (Ord(dupa) >= 97) and (Ord(dupa) <= 122) then Result := Chr(Ord(dupa)-32)
    else Result := dupa;
end;


function string_toC(dupa : String) : String;
begin
	dupa := StringReplace(dupa, '\a', #7, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\b', #8, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\e', #27, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\f', #12, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\n', #10, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\r', #13, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\t', #9, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\v', #11, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\\', '\', [rfReplaceAll]);
	dupa := StringReplace(dupa, '\''', #39, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\"', #34, [rfReplaceAll]);
	dupa := StringReplace(dupa, '\?', '?', [rfReplaceAll]);
	string_toC := dupa;
end;

function PadCharsCenter(str : String; size : LongInt; chr : Char) : String;
var
    index, limit : LongInt;
begin
    index := 0;
    limit := size - Length(str);
    while index < limit do
    begin
        if (index mod 2 = 0) 
            then str := chr + str
            else str := str + chr;
        index := index + 1;
    end; 
    Result := str;
end;

function OccurrencesOfSubstring(str, sub : String) : LongInt;
//var
//    cnt, tok : LongInt;
//begin
//    cnt := 0;
//    repeat
//    	tok := NPos(sub, str, cnt+1);
//    	if (tok > 0) then Inc(cnt);
//    until (tok <= 0);
//    Result := cnt;
//end;
var
    tok : LongInt;
begin
    Result := 0;
    repeat
    	tok := Pos(sub, str);
    	if (tok > 0) then 
        begin
            Inc(Result);
            Delete(str, 1, tok+length(sub)-1);
        end;
    until (tok <= 0);
end;

function TrimChars(str : String; chr : Char) : String;
begin
    while str[1] = chr do Delete(str, 1, 1);
    while str[Length(str)] = chr do SetLength(str, Length(str)-1);
    Result := str;
end;

function TrimCharsLeft(str : String; chr : Char) : String;
begin
    while str[1] = chr do Delete(str, 1, 1);
    Result := str;
end;

function TrimCharsRight(str : String; chr : Char) : String;
begin
    while str[Length(str)] = chr do SetLength(str, Length(str)-1);
    Result := str;
end;

function RemoveCharset(a : String; b : String) : String;  
var
    index : LongInt;
begin  
    for index := Length(b) downto 1 do
    begin
        a := DelChars(a, b[index]);
    end;
    Result := a;
end;

end.
