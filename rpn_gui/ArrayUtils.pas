unit ArrayUtils;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    ComplexNumbers,
    UnitStack, UnitEntity;

procedure bubblesort(var tab : TEntities);
procedure quicksort(var tab : TEntities);
procedure mergesort(var tab : TEntities);
procedure bogosort(var tab : TEntities);
procedure strings_sort(var tab : TEntities);

procedure table_reverse(var tab : TEntities);
function table_sum(tab : TEntities) : ComplexType;
function table_product(tab : TEntities) : ComplexType;
function table_avg(tab : TEntities) : ComplexType;
function table_avg2(tab : TEntities) : ComplexType;
function table_avg_geom(tab : TEntities) : ComplexType;
function table_min(tab : TEntities) : ComplexType;
function table_max(tab : TEntities) : ComplexType;
function table_min2(tab : TEntities) : LongInt;
function table_max2(tab : TEntities) : LongInt;
function table_avg_power(tab : TEntities; factor : Extended) : ComplexType;
function table_median(tab : TEntities) : ComplexType;
function table_mode(tab : TEntities) : Entity;
function table_modeStr(tab : TEntities) : Entity;
function table_abs(tab : TEntities) : ComplexType;
function table_variance(tab : TEntities) : ComplexType;
function table_stddev(tab : TEntities) : ComplexType;
function table_gcd(tab : TEntities) : Extended;
function table_lcm(tab : TEntities) : Extended;
function table_moment(tab : TEntities; k : LongInt) : ComplexType;
function table_quantile(tab : TEntities; factor : Extended = 0.5) : ComplexType;
function table_quantile2(tab : TEntities; num : Extended; denom : Extended) : ComplexType;
function table_skewness(tab : TEntities) : ComplexType;
function table_kurtosis(tab : TEntities) : ComplexType;

function itemHappenedBefore(tab : TEntities; position : LongInt) : Boolean;
function numberHappenedBefore(tab : TEntities; position : LongInt) : Boolean;
function stringHappenedBefore(tab : TEntities; position : LongInt) : Boolean;

procedure array_cutNulls(var tab : TEntities);
function array_randomFrom(tab : TEntities) : Entity;

implementation

uses MathUtils, Math;

// SORTS

procedure bubblesort(var tab : TEntities);
var
	i, j : Longint;
	pom  : Entity;
begin
	for j := Length(tab)-1 downto 1 do
		for i := 0 to j-1 do 
			if (tab[i].Num.Re > tab[i+1].Num.Re) then begin
				pom := tab[i];
				tab[i] := tab[i+1];
				tab[i+1] := pom;
      		end;
end;

procedure qs_engine(var AI: TEntities; ALo, AHi: Integer);
var
  Lo, Hi   : Integer;
  Pivot, T : Entity;
begin
	Lo := ALo;
	Hi := AHi;
	Pivot := AI[(Lo + Hi) div 2];
	repeat
		while AI[Lo].Num.Re < Pivot.Num.Re do
			Inc(Lo) ;
		while AI[Hi].Num.Re > Pivot.Num.Re do
			Dec(Hi) ;
		if Lo <= Hi then
		begin
			T := AI[Lo];
			AI[Lo] := AI[Hi];
			AI[Hi] := T;
			Inc(Lo);
			Dec(Hi);
		end;
	until Lo > Hi;
	if Hi > ALo then
		qs_engine(AI, ALo, Hi);
	if Lo < AHi then
		qs_engine(AI, Lo, AHi);
end;

procedure quicksort(var tab : TEntities);
begin
	qs_engine(tab, 0, Length(tab)-1);
end;

procedure ms_merge(var a : TEntities; l,r,x,y:Integer);
var 
  i,j,k,s : Integer;
  c       : TEntities;
begin
	i := l;
	j := y;
	k := 0;
	SetLength(c,r-l+1+y-x+1);
	while (l<=r) and (x<=y) do
	begin
		if a[l].Num.Re < a[x].Num.Re then
		begin
			c[k] := a[l];
			inc(l);
		end else begin
			c[k] := a[x];
      		inc(x);
    	end;
    	inc(k);
	end;

  	if l<=r then
    	for s:=l to r do
    	begin
    		c[k]:=a[s];
    		inc(k);
    	end 
	else
    	for s:=x to y do
    	begin
			c[k]:=a[s];
			inc(k);
		end;

	k:=0;
	for s:=i to j do
	begin
		a[s]:=c[k];
		inc(k);
	end;
end;

procedure ms_engine(var a : TEntities; l,r:Integer);
var 
  m : Integer;
begin
	if l=r then Exit;
	m:=(l+r) div 2;
	ms_engine(a, l, m);
	ms_engine(a, m+1, r);
	ms_merge(a, l, m, m+1, r);
end;

procedure mergesort(var tab : TEntities);
begin
	ms_engine(tab, 0, Length(tab)-1);
end;

function bs_isSorted(var data : TEntities) : Boolean;
var
  Count : Longint;
  res   : Boolean;
begin
	res := true;
	for count := Length(data)-1 downto 1 do
		if (data[count].Num.Re < data[count - 1].Num.Re) then res := false;
	bs_isSorted := res;
end;

procedure bs_shuffle(var data : TEntities);
var
	Count, rnd, i : Longint;
	pom           : Entity;
begin
	Count := Length(data);
	for i := 0 to Count-1 do
	begin   
		rnd := random(count);
		pom := data[i];
		data[i] := data[rnd];
		data[rnd] := pom;
	end;
end;

procedure bogosort(var tab : TEntities);
begin
	randomize();
	while not (bs_isSorted(tab)) do bs_shuffle(tab);
end;


// SORT STRINGS

procedure sortStringsPosition(var tab : TEntities; index : Integer); 
var
	i, j : Longint;
	pom  : Entity;
begin
	for j := Length(tab)-1 downto 1 do
	begin
		for i := 0 to j-1 do 
		begin
    		if (tab[i].Str[index] > tab[i+1].Str[index]) then begin
        		pom := tab[i];
        		tab[i] := tab[i+1];
        		tab[i+1] := pom;
      		end;
		end;
	end;
	
end;

procedure strings_sort(var tab : TEntities);
var
	i, min : Integer;
begin
	min := Length(tab[0].Str);
	for i := 0 to Length(tab)-1 do 
	begin
		if min < Length(tab[i].Str) then 
			min := Length(tab[i].Str);
	end;

	for i := min downto 1 do
		sortStringsPosition(tab, i); 
end;

// TABLES AGGREGATE

procedure table_reverse(var tab : TEntities);
var 
	pom : TEntities;
	i   : Integer;
begin
	SetLength(pom, Length(tab));
	for i := 0 to Length(tab)-1 do pom[i] := tab[Length(tab)-1-i];
	for i := 0 to Length(tab)-1 do tab[i] := pom[i];
	SetLength(pom, 0);
end;

function table_sum(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i].Num;
  table_sum := s;
end;

function table_product(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := 1.0;
  for i := 0 to Length(tab)-1 do
  	s := s * tab[i].Num;
  table_product := s;
end;

function table_avg(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := 0.0;
    for i := 0 to Length(tab)-1 do
        s := s + tab[i].Num;
    table_avg := s/Length(tab);
end;

function table_avg2(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := 0.0;
    for i := 0 to Length(tab)-1 do
        s := s + tab[i].Num * tab[i].Num;
    table_avg2 := sqrt(s/Length(tab));
end;

function table_avg_geom(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := 1.0;
    for i := 0 to Length(tab)-1 do
        s := s * tab[i].Num;
    table_avg_geom := ComplexNumbers.pow(s, Inv(Length(tab)));
end;

function table_min(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := tab[0].Num;
  for i := 1 to Length(tab)-1 do
  	if (tab[i].Num.Re < s.Re) then s := tab[i].Num;
  table_min := s;
end;

function table_max(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := tab[0].Num;
  for i := 1 to Length(tab)-1 do
  	if (tab[i].Num.Re > s.Re) then begin
  		s := tab[i].Num;
  	end; 
  table_max := s;
end;

function table_min2(tab : TEntities) : LongInt;
var
	i : LongInt;
	s : LongInt;
begin
	s := 0;
    for i := 1 to Length(tab)-1 do
        if (tab[i].Num.Re < tab[s].Num.Re) then s := i;
    table_min2 := s;
end;

function table_max2(tab : TEntities) : LongInt;
var
	i : LongInt;
	s : LongInt;
begin
	s := 0;
    for i := 1 to Length(tab)-1 do
        if (tab[i].Num.Re > tab[s].Num.Re) then s := i;
    table_max2 := s;
end;

function table_avg_power(tab : TEntities; factor : Extended) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
    if factor = 0 then begin
        table_avg_power := table_avg_geom(tab);
    end else if factor = 1 then begin
        table_avg_power := table_avg(tab);
    end else if factor = 2 then begin
        table_avg_power := table_avg2(tab);
    end else if factor = Infinity then begin
        table_avg_power := table_max(tab);
    end else if factor = -Infinity then begin
        table_avg_power := table_min(tab);
    end else begin
        s := 0.0;
        for i := 0 to Length(tab)-1 do
  	        s := s + pow(tab[i].Num, factor);
        table_avg_power := pow(s/Length(tab), 1/factor);
    end;
end;

function table_median(tab : TEntities) : ComplexType;
begin
	quicksort(tab);
	if (Length(tab) mod 2 = 0) then table_median := 0.5*(tab[Length(tab) div 2 - 1].Num + tab[Length(tab) div 2].Num)
	else table_median := tab[Length(tab) div 2].Num;
end;

function table_mode(tab : TEntities) : Entity;
var
    i        : LongInt;
    maxval   : Entity;
    maxcombo : Extended;
    curval   : Entity;
    curcombo : Extended;
begin
	if (Length(tab) = 0) then
    begin
        table_mode := buildNull();
    end else begin
        quicksort(tab);
        //todo: do it properly for complex numbers
        maxval := tab[0];
        maxcombo := 1;
        curval := tab[0];
        curcombo := 1;
        for i := 1 to Length(tab)-1 do
        begin
            if (curval.Num = tab[i].Num) then
            begin
                curcombo := curcombo + 1;
                //writeln(curval.Str, #9, curval.Num, #9, curcombo);
            end else begin
                if (maxcombo < curcombo) then
                begin
                    maxcombo := curcombo;
                    maxval := curval;
                end;
                curval := tab[i];
                curcombo := 1;
            end;
        end;
        table_mode := maxval;
    end;
end;

function table_modeStr(tab : TEntities) : Entity;
var
    i        : LongInt;
    maxval   : Entity;
    maxcombo : Extended;
    curval   : Entity;
    curcombo : Extended;
begin
	if (Length(tab) = 0) then
    begin
        table_modeStr := buildNull();
    end else begin
        strings_sort(tab);
        //writeln('chuj');
        maxval := tab[0];
        maxcombo := 1;
        curval := tab[0];
        curcombo := 1;
        for i := 1 to Length(tab)-1 do
        begin
            if (curval.Str = tab[i].Str) then
            begin
                curcombo := curcombo + 1;
                //writeln(curval.Num, ' ', curcombo);
            end else begin
                if (maxcombo < curcombo) then
                begin
                    maxcombo := curcombo;
                    maxval := curval;
                end;
                curval := tab[i];
                curcombo := 1;
            end;
        end;
        table_modeStr := maxval;
    end;
end;


function table_abs(tab : TEntities) : ComplexType;
var
	i : Integer;
	s : ComplexType;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i].Num * tab[i].Num;
  table_abs := sqrt(s);
end;

function table_variance(tab : TEntities) : ComplexType;
var
	i    : Integer;
	s    : ComplexType;
	mean : ComplexType;
begin
  mean := table_avg(tab);
  s := 0.0;
  for i := 0 to Length(tab)-1 do
    s := s + sqr(tab[i].Num - mean);
  table_variance := s/Length(tab);
end;

function table_stddev(tab : TEntities) : ComplexType;
begin
	table_stddev := sqrt(table_variance(tab));
end;

function table_gcd(tab : TEntities) : Extended;
var
	i    : Integer;
	s    : Extended;
begin
    s := tab[0].Num.Re;
    for i := 1 to Length(tab)-1 do
        s := gcd(s, tab[i].Num.Re);
    Result := s;
end;

function table_lcm(tab : TEntities) : Extended;
var
	i    : Integer;
	s    : Extended;
begin
    s := tab[0].Num.Re;
    for i := 1 to Length(tab)-1 do
        s := lcm(s, tab[i].Num.Re);
    Result := s;
end;

//function table_moment(tab : TEntities; k : LongInt) : Extended;
//var
//	i : Integer;
//	s : Extended;
//begin
//	s := 0.0;
//    for i := 0 to Length(tab)-1 do
//        s := s + pow(tab[i].Num, k);
//    Result := s/Length(tab);
//end;

function table_moment(tab : TEntities; k : LongInt) : ComplexType;
var
	i : Integer;
	s : ComplexType;
    m : ComplexType;
begin
    s := 0.0;
    m := table_avg(tab);
    for i := 0 to Length(tab)-1 do
        s := s + pow(tab[i].Num - m, k);
    Result := s/Length(tab);
end;

function table_quantile(tab : TEntities; factor : Extended = 0.5) : ComplexType;
begin
	quicksort(tab);
    if (factor = 0) then
    begin
        Result := tab[0].Num;
    end
    else if (factor = 1) then
    begin
        Result := tab[Length(tab)-1].Num;
    end else if (isInteger(Length(tab) * factor)) then
    begin
        Result := 0.5*(tab[Ceil(Length(tab) * factor) - 1].Num + tab[Ceil(Length(tab) * factor)].Num);
    end else begin 
        Result := tab[Ceil(Length(tab) * factor) - 1].Num;
    end;
end;

function table_quantile2(tab : TEntities; num : Extended; denom : Extended) : ComplexType;
begin
	quicksort(tab);
    if (num = 0) then
    begin
        Result := tab[0].Num;
    end
    else if (num > denom) or (num < 0) then
    begin
        Result := NaN;
    end
    else if (num = denom) then
    begin
        Result := tab[Length(tab)-1].Num;
    end else if (isInteger(Length(tab) * num / denom)) then
    begin
        Result := 0.5*(tab[Ceil(Length(tab) * num / denom) - 1].Num + tab[Ceil(Length(tab) * num / denom)].Num);
    end else begin 
        Result := tab[Ceil(Length(tab) * num / denom) - 1].Num;
    end;
end;

function table_skewness(tab : TEntities) : ComplexType;
var
    s : ComplexType;
begin
    s := table_stddev(tab);
    if s = 0
        then Result := 0
        else Result := (table_moment(tab, 3))/(pow(s,3));
end;

function table_kurtosis(tab : TEntities) : ComplexType;
var
    s : ComplexType;
begin
    s := table_stddev(tab);
    if s = 0
        then Result := 0
        else Result := (table_moment(tab, 4))/(pow(s,4));
end;

// booleans

function itemHappenedBefore(tab : TEntities; position : LongInt) : Boolean;
var
    index : LongInt;
begin
    Result := False;
    for index := 0 to position-1 do
    begin
        if (tab[index] = tab[position]) then
        begin
            Result := true;
            break;
        end;
    end;
end;

function numberHappenedBefore(tab : TEntities; position : LongInt) : Boolean;
var
    index : LongInt;
begin
    Result := False;
    for index := 0 to position-1 do
    begin
        if (tab[index].Num = tab[position].Num) then
        begin
            Result := true;
            break;
        end;
    end;
end;

function stringHappenedBefore(tab : TEntities; position : LongInt) : Boolean;
var
    index : LongInt;
begin
    Result := False;
    for index := 0 to position-1 do
    begin
        if (tab[index].Str = tab[position].Str) then
        begin
            Result := true;
            break;
        end;
    end;
end;

// Array utilities

procedure array_justpop(var pocz : TEntities; index : LongInt);
var
	i : LongInt;
begin
	for i := index to Length(pocz)-2 do pocz[i] := pocz[i+1];
	SetLength(pocz, Length(pocz)-1);
end;

procedure array_cutNulls(var tab : TEntities);
var
    index : LongInt;
    len   : LongInt;
begin
    index := 1;
    len := Length(tab);
    if (len > 0) then
    begin
        while (index < len) do
        begin
            if (isNull(tab[index])) then
            begin
                array_justpop(tab, index);
            end else begin
                index := index + 1;
            end;
        end;
    end;
end;

function array_randomFrom(tab : TEntities) : Entity;
begin
    Result := tab[random(Length(tab))];
end;


end.

