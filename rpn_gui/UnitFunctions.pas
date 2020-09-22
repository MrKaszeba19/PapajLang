unit UnitFunctions;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, StrUtils, Math, Process,
	UnitStack, UnitEntity, UnitVariables;

const
	PI = 3.1415926535897932384626433832795;
	EU = 2.7182818284590452353602874713526;
	FI = 1.6180339887498948482045868343656;
    EM = 0.5772156649015328606065120900824;

function OccurrencesOfChar(const S: string; const C: char): integer;
function OccurrencesOfChar2(const S: string; const C: char): integer;
function read_sourcefile(filename : String; var pocz : StackDB; var sets : TSettings; var vardb : VariableDB) : StackDB;

function lib_ultravanilla(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_math(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_strings(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_directives(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_constants(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_variables(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_logics(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_consolemanipulators(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_exceptions(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_arrays(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
function lib_files(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;

implementation

uses Unit5,
    {$IFDEF MSWINDOWS}
		ShellApi, crt,
    {$ELSE}
        ConsoleUtils,
 	{$ENDIF}
    UnitEnvironment, DateUtils;

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

procedure swapNumbers(var e1 : Extended; var e2 : Extended);
var
	pom : Extended;
begin
	pom := e1;
	e1 := e2;
	e2 := pom;
end;

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

// MATHEMATICAL FUNCTIONS

function pow(x,y:Extended):Extended;
var
        s : Extended;
        i : integer;
begin
        s := 1;
        for i := 1 to trunc(abs(y)) do s := s * x;
        if (y < 0) then s := 1 / s;
        pow := s;
end;

function pow2(x,y:Extended):Extended;
var
        s : Extended;
begin
        s := exp(y*ln(x));
        pow2 := s;
end;

function fact(x:Extended):Extended;
var
        s : Extended;
        i : integer;
begin
     s := 1;
     for i := 1 to trunc(abs(x)) do s := s * i;
     fact := s;
end;

function isPrime(x : LongInt) : Boolean;
var
    i : LongInt;
    s : Boolean;
begin
    case x of
        0 : isPrime := False;
        1 : isPrime := False;
        else begin
            s := True;
            for i := 2 to trunc(sqrt(x)) do
            begin
                if (x mod i = 0) then 
                begin
                    s := False;
                    break;
                end;
            end;
            isPrime := s;
        end;
    end;
end;

//function newton_int(n, k : Extended) : Extended;
//begin
//     //newton (n, 0) = 1;
//     //newton (n, n) = 1;
//     //newton (n, k) = newton (n-1, k-1) + newton (n-1, k);
//     //writeln('Counting (',trunc(n),',',trunc(k),')');
//     if (k > n/2) then newton_int := newton_int(n, n-k);
//     if (k = 0.0) or (k = n) then newton_int := 1.0
//     else newton_int := newton_int(n-1, k-1) + newton_int(n-1, k);
//end;

function newton_int(n, k : Extended) : Extended;
begin
    if(k > n) then newton_int := 1.0/0.0
    else if(k = 0) then 
        newton_int := 1
    else if (k > n/2) then
        newton_int := newton_int(n,n-k)
    else 
        newton_int := n * newton_int(n-1,k-1) / k;
end;

function newton_real(n, k : Extended) : Extended;
var s, j : Extended;
begin
	s := 1;
	if (n < 0) then
		newton_real := pow(-1.0, k) * newton_real(k-n-1, k)
	else begin
		j := 1.0;
		while (j <= k) do
		begin
			s := s * (n-k+j)/j;
			j := j + 1;
		end;
    	newton_real := s;
  	end;
end;

function gcd(a, b : Extended) : Extended;
begin
	while (a <> b) do
	begin
        if (a > b) then a := a - b
        else b := b - a;
    end;
	gcd := a;
end;

function lcm(a, b : Extended) : Extended;
begin
	lcm := (a*b)/gcd(a, b);
end;

//function fib(n: Extended) : Extended;
//begin
//     if n = 0.0 then fib := 0.0
//     else if n = 1.0 then fib := 1.0
//     else fib := fib(n-1.0) + fib(n-2.0);
//end;

function fib(n: Extended) : Extended;
var
    a, b : Extended;
    i    : LongInt;
begin
    if n = 0.0 then fib := 0.0
    else if n = 1.0 then fib := 1.0
    else begin
        a := 0.0;
        b := 1.0;
        for i := 2 to trunc(n) do
        begin
            a := a + b;
            swapNumbers(a, b);
        end;
        fib := b;
    end;
end;

function fgamma(x : Extended) : Extended;
var
	limit, n : Integer;
	s, s1    : Extended;
	epsilon  : Extended;
begin
	if (x = trunc(x)) then fgamma := fact(x-1) 
	else begin
		if (x > 100) then limit := trunc(100000*x)+1;
		limit := trunc(1000000*x)+1;
		n := 1;
		s := 1.0;
		epsilon := 50.0;
		while (n < limit) and (epsilon > 0.0000001) do
		//while (epsilon > 0.0000001) do
		begin
            //checkSIGINT();
			s1 := s;
			s := s * ((pow2(1+1/n, x))/(1+x/n));
			//writeln(s, #9, epsilon);
			epsilon := abs(s-s1);
			n := n + 1;
		end;
		fgamma := s/x;

		//s := exp(-EM*x)/x;
		//for n := 1 to 1000000*trunc(x)+1 do 
		//begin
		//	s1 := s;
		//	s := s * (1/(1 + x/n) * exp(x/n));
		//	if (abs(s1-s) < 0.000001) then break;
		//end;
		//fgamma := s;
	end;
end;

function dstdnorm(x : Extended) : Extended;
var
	sum    : Extended;
	//sum1   : Extended;
	t, eps : Extended;
	limit  : Extended;
begin
    //checkSIGINT();
	eps := 0.000001;
	limit := 5;
	if (x < -limit) then 
	begin 
		dstdnorm := 0.00000000001 
	end
	else if (x > limit) then 
	begin 
		dstdnorm := 0.99999999999 
	end else if (x = 0) then
	begin
		dstdnorm := 0.5;
	end
	else if (x = -1) then
	begin
		dstdnorm := 0.158655253931457;
	end
	else if (x = 1) then
	begin
		dstdnorm := 0.841344746068543;
	end else if (x <= -1) then
	begin
		eps := 0.0001;
		sum := 0;
		t := x;
		//sum1 := 50;
		while (t >= -limit) do
		//while (t >= -limit) and (abs(sum-sum1) > eps) do
		begin
			//sum1 := sum;
			sum := sum + eps*((exp(-(t*t/2)))+(exp(-((t-eps)*(t-eps)/2)))/2);
			t := t - eps;
		end; 
		dstdnorm := sum/sqrt(2*PI)*(2/3);
	end else if (x < 0) then
	begin
		sum := 0;
		t := x;
		while (t <= 0) do
		begin
			sum := sum + eps*((exp(-(t*t/2)))+(exp(-((t-eps)*(t-eps)/2)))/2);
			t := t + eps;
		end; 
		dstdnorm := 0.5 - sum/sqrt(2*PI)*(2/3);
	end else if (x <= 1) then begin
		sum := 0;
		t := x;
		while (t > 0) do
		begin
			sum := sum + eps*((exp(-(t*t/2)))+(exp(-((t-eps)*(t-eps)/2)))/2);
			t := t - eps;
		end; 
		dstdnorm := 0.5 + sum/sqrt(2*PI)*(2/3);
	end else begin
		eps := 0.0001;
		sum := 0;
		t := x;
		while (t <= limit) do
		//while (t <= limit) and (abs(sum-sum1) > eps) do
		begin
			//sum1 := sum;
			sum := sum + eps*((exp(-(t*t/2)))+(exp(-((t-eps)*(t-eps)/2)))/2);
			t := t + eps;
		end; 
		dstdnorm := 1 - sum/sqrt(2*PI)*(2/3);
	end;
end;

// 0.841344746068543
// 0.158655253931457

function dnorm(x, mu, si : Extended) : Extended;
begin
	dnorm := dstdnorm((x-mu)/si);
end;

function fnorm(x, mu, si : Extended) : Extended;
begin
    fnorm := 1/(si*sqrt(2*PI))*exp(-0.5*sqr((x-mu)/si));
end;

//function rnorm(mean, sd: Extended) : Extended;
//var
//    u1, u2: Extended;
//begin
//    u1 := random;
//    u2 := random;
//    rnorm := mean * abs(1 + sqrt(-2 * (ln(u1))) * cos(2 * pi * u2) * sd);
//end;

function fbinom(n : LongInt; k : LongInt; p : Extended) : Extended;
begin
    fbinom := newton_int(n, k)*pow(p, k)*pow(1-p, n-k);
end;

function dbinom(n, k : LongInt; p : Extended) : Extended;
var
    i : LongInt;
    s : Extended;
begin
    s := 0;
    //if (k <= n/2) then 
    //begin
        for i := 0 to k do
            s += fbinom(n, i, p);
        dbinom := s; 
    //end else begin
    //    for i := n downto n-k+1 do
    //        s += fbinom(n, i, p);
    //    dbinom := s; 
    //end;
end; 

function rbinom(n : LongInt; p : Extended) : Extended;
var
    i, res : LongInt;
begin
    res := 0;
    for i := 1 to n do
        if (random <= p) then Inc(res);
    rbinom := res;
end;

function fgeom(k : LongInt; p : Extended) : Extended;
begin
    fgeom := pow(1-p, k-1)*p;
end;

function dgeom(k : LongInt; p : Extended) : Extended;
begin
    dgeom := 1 - pow(1-p, k);
end;

function rgeom(p : Extended) : Extended;
var
    x      : Extended;
    i, res : LongInt;
begin
    res := 1;
    if (p <= 0) 
        then rgeom := Infinity 
        else begin
            while (random >= p) do 
                Inc(res);
            rgeom := res;
        end;
end;

function fexp(x : Extended; lambda : Extended) : Extended;
begin
    fexp := lambda * exp(-x*lambda);
end;

function dexp(x : Extended; lambda : Extended) : Extended;
begin
    dexp := 1 - exp(-x*lambda);
end;

function rexp(lambda : Extended) : Extended;
begin
    rexp := -ln(random)/lambda;
end;

function fpoisson(x, lambda : Extended): Extended;
begin
    fpoisson := exp(-lambda)*pow(lambda, x)/fact(x);
end;

function dpoisson(x, lambda : Extended): Extended;
var 
    s : Extended;
    i : LongInt;
begin
    s := 0;
    for i := 0 to trunc(x) do
        s += fpoisson(i, lambda);
    dpoisson := s; 
end;

function rpoisson(mean: Extended): Extended;
{ Generator for Poisson distribution (Donald Knuth's algorithm) }
const
  RESOLUTION = 1000;
var
  k    : Extended;
  b, l : Extended;
begin
  //assert(mean > 0, 'mean < 1');
  k := 0;
  b := 1;
  l := exp(-mean);
  while b > l do
  begin
    k := k + 1;
    b := b * random(RESOLUTION) / RESOLUTION;
  end;
  rpoisson := k - 1;
end;

// SORTS

procedure bubblesort(var tab : TEntities);
var
	i, j : Longint;
	pom  : Entity;
begin
	for j := Length(tab)-1 downto 1 do
		for i := 0 to j-1 do 
			if (tab[i].Num > tab[i+1].Num) then begin
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
		while AI[Lo].Num < Pivot.Num do
			Inc(Lo) ;
		while AI[Hi].Num > Pivot.Num do
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
		if a[l].Num < a[x].Num then
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
		if (data[count].Num < data[count - 1].Num) then res := false;
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

function table_sum(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i].Num;
  table_sum := s;
end;

function table_product(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 1.0;
  for i := 0 to Length(tab)-1 do
  	s := s * tab[i].Num;
  table_product := s;
end;

function table_avg(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
    for i := 0 to Length(tab)-1 do
        s := s + tab[i].Num;
    table_avg := s/Length(tab);
end;

function table_avg2(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
    for i := 0 to Length(tab)-1 do
        s := s + tab[i].Num * tab[i].Num;
    table_avg2 := sqrt(s/Length(tab));
end;

function table_avg_geom(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 1.0;
    for i := 0 to Length(tab)-1 do
        s := s * tab[i].Num;
    table_avg_geom := pow2(s, 1/Length(tab));
end;

function table_min(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := tab[0].Num;
  for i := 1 to Length(tab)-1 do
  	if (tab[i].Num < s) then s := tab[i].Num;
  table_min := s;
end;

function table_max(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := tab[0].Num;
  for i := 1 to Length(tab)-1 do
  	if (tab[i].Num > s) then begin
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
        if (tab[i].Num < tab[s].Num) then s := i;
    table_min2 := s;
end;

function table_max2(tab : TEntities) : LongInt;
var
	i : LongInt;
	s : LongInt;
begin
	s := 0;
    for i := 1 to Length(tab)-1 do
        if (tab[i].Num > tab[s].Num) then s := i;
    table_max2 := s;
end;

function table_avg_power(tab : TEntities; factor : Extended) : Extended;
var
	i : Integer;
	s : Extended;
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
        if (factor = trunc(factor)) then
        begin
            for i := 0 to Length(tab)-1 do
  	            s := s + pow(tab[i].Num, factor);
        end else begin
            for i := 0 to Length(tab)-1 do
  	            s := s + pow2(tab[i].Num, factor);
        end;
        table_avg_power := pow2(s/Length(tab), 1/factor);
    end;
end;

function table_median(tab : TEntities) : Extended;
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
        //writeln('chuj');
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


function table_abs(tab : TEntities) : Extended;
var
	i : Integer;
	s : Extended;
begin
	s := 0.0;
  for i := 0 to Length(tab)-1 do
  	s := s + tab[i].Num * tab[i].Num;
  table_abs := sqrt(s);
end;

function table_variance(tab : TEntities) : Extended;
var
	i    : Integer;
	s    : Extended;
	mean : Extended;
begin
  mean := table_avg(tab);
  s := 0.0;
  for i := 0 to Length(tab)-1 do
    s := s + sqr(tab[i].Num - mean);
  table_variance := s/Length(tab);
end;

function table_stddev(tab : TEntities) : Extended;
begin
	table_stddev := sqrt(table_variance(tab));
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

// COMMANDS' EXECUTION

function executeCommand(input, Shell : String) : String;
var
	s : String;
begin
	s := '';
	{$IFDEF MSWINDOWS}
	//RunCommand(Shell,['/c', input],s);
	if ShellExecute(0,nil, PChar('cmd'),PChar('/c '+input),nil,1) =0 then;
 	{$ELSE}
  	RunCommand(Shell,['-c', input],s);
 	{$ENDIF}
 	executeCommand := s;
end;



function lib_ultravanilla(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found          : Boolean;
	x, y, z, a     : Extended;
	Size           : Longint;
	index          : Longint;
	IntEax         : LongInt;
	StrEax, StrEbx : String;
	StrEcx         : String;
	EntEax         : Entity;
	ExtEax         : Extended;
    LogEax         : Boolean;
	HelpETable     : array of Entity;
begin
	Found := true;
	case i of
    	// binary
    	'+' : begin
    		if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
    		y := stack_pop(pocz[sets.StackPointer]).Num;
    		if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
    		x := stack_pop(pocz[sets.StackPointer]).Num;
    		z := x+y;
    		if not (sets.Autoclear) then begin
    			stack_push(pocz[sets.StackPointer], buildNumber(x));
    			stack_push(pocz[sets.StackPointer], buildNumber(y));
    		end;
    		stack_push(pocz[sets.StackPointer], buildNumber(z));
    	end;
    	'-' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (not sets.InfMode) then
            begin 
                z := x-y;
            end else begin 
                if ((y = Infinity) or (y = -Infinity)) and ((x = Infinity) or (x = -Infinity)) then
                begin
                    z := NaN;
                end else begin
                    z := x-y;
                end;
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        '*' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := x*y;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        '/' : begin
            if (not sets.InfMode) and (sets.StrictType) and (assertNonZeroLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (not sets.InfMode) then
            begin 
                z := x/y;
            end else begin 
                if (y = 0) then
                begin
                    z := x*Infinity;
                end else if (y = 0) and (x = 0) then
                begin
                    z := NaN;
                end else if ((y = Infinity) or (y = -Infinity)) and ((x = Infinity) or (x = -Infinity)) then
                begin
                    z := NaN;
                end else if (y = Infinity) then
                begin
                    z := 0;
                end else if (y = -Infinity) then
                begin
                    z := 0;
                end else begin
                    z := x/y;
                end;
            end;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        '^' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (y = trunc(y)) then begin
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
            if (y = trunc(y)) then begin
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
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := trunc(x) mod trunc(y);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'div' : begin
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertIntegerLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            x := stack_pop(pocz[sets.StackPointer]).Num;
            z := trunc(x) div trunc(y);
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
            if trunc(x/y) < 0 then begin
            	z := trunc(x/y)-1;
            end else begin
            	z := trunc(x/y);
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
            z := trunc(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
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
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            //pocz := parseScoped(StrEbx, pocz, sets, vardb);
            doFunction(StrEbx, pocz, sets, vardb);
        end;
        'callIf' : begin
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, TBOO, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TFUN) then
            begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num = 0 then 
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(StrEbx, pocz, sets, vardb);
            end else begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num = 0 then begin
                    if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(StrEbx, pocz, sets, vardb);
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
                StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num <> 0 then 
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(StrEbx, pocz, sets, vardb);
            end else begin
                if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TBOO, i)) then Exit;  
                if stack_pop(pocz[sets.StackPointer]).Num <> 0 then begin
                    if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;  
                    StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
                    //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                    doFunction(StrEbx, pocz, sets, vardb);
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
            	if (sets.StrictType) and (EntEax.EntityType <> TBOO) 
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
        'length' : begin
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, TVEC, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                IntEax := trunc(stack_get(pocz[sets.StackPointer]).Num);
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(stack_size(pocz[IntEax])));
            end else begin
                StrEax := stack_get(pocz[sets.StackPointer]).Str;
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEax)));
            end;
        end;
        'len' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_get(pocz[sets.StackPointer]).Str;
            //if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEax)));
        end;
        'print' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writeOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writeOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
        end;
        'println' : begin
            EntEax := stack_get(pocz[sets.StackPointer]);
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writelnOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writelnOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
        end;
        'rprint' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writeOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writeOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writeOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
        end;
        'rprintln' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (EntEax.EntityType = TBOO) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNUM) then writelnOnConsole(FormatFloat(sets.Mask, EntEax.Num));
            if (EntEax.EntityType = TSTR) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TNIL) then writelnOnConsole(EntEax.Str);
            if (EntEax.EntityType = TVEC) then writelnOnConsole(stack_showArrayPS(pocz[trunc(EntEax.Num)], pocz, sets.Mask));
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
	Found      : Boolean;
	x, y, z, w : Extended;
    index      : LongInt;
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
            z := ln(x)/ln(2);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.abs' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := abs(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
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
        'Math.arcsinh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arcsinh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arccosh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arccosh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arctanh' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := arctanh(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.arccoth' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := 0.5*ln((y+1)/(x-1));
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
        'Math.floor' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
			if (y = trunc(y)) then z := trunc(y)
			else if (y < 0) then z := trunc(y)-1 else z := trunc(y);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.ceiling' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (y = trunc(y)) then z := trunc(y)
			else if (y < 0) then z := trunc(y) else z := trunc(y)+1;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.round' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := round(y);
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
            stack_push(pocz[sets.StackPointer], buildBoolean(isPrime(trunc(y))));
        end;
		'Math.isEven' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(trunc(y) mod 2 = 0));
        end;
		'Math.isOdd' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(trunc(y) mod 2 = 1));
        end;
		'Math.isInteger' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            stack_push(pocz[sets.StackPointer], buildBoolean(trunc(y) = y));
        end;

		'Math.Gamma' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TNUM, i)) then Exit;
            y := stack_pop(pocz[sets.StackPointer]).Num;
            z := fgamma(y);
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
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(z));
            stack_push(pocz[sets.StackPointer], buildNumber(w));
        end;
        'Math.randomNormStd' : begin
            z := randg(0, 1);
            stack_push(pocz[sets.StackPointer], buildNumber(z));
        end;
        'Math.genNormStd' : begin
            x := stack_pop(pocz[sets.StackPointer]).Num;
            for index := 1 to trunc(x) do
            begin
                //checkSIGINT();
                w := randg(0, 1);
                stack_push(pocz[sets.StackPointer], buildNumber(w));
            end;
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(x));
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
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
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
            StrEcx := concat(StrEax, StrEbx);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.crush' : begin
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
        end;
        'String.crushBy' : begin
          	SetLength(HelpSTable, 0);
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            IntEax := 1;
            IntEbx := 1;
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
        end;
        'String.left' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEax := LeftStr(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax));
        end;
        'String.right' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEax := RightStr(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax));
        end;
        'String.trim' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := Trim(StrEbx);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.trimLeft' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := TrimLeft(StrEbx);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.trimRight' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := TrimRight(StrEbx);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.padLeft' : begin
        	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := PadLeft(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.padRight' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := PadRight(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.pad' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := PadCenter(StrEbx, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.despace' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := DelChars(StrEbx, ' ');
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.onespace' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := DelSpace1(StrEbx);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.dechar' : begin
            if (sets.StrictType) and (assertCharLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            StrEcx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEcx := DelChars(StrEbx, StrEcx[1]);
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
            StrEcx := StrEax + ' ' + StrEbx;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEcx));
        end;
        'String.bindBy' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEcx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            StrEdx := StrEax + StrEcx + StrEbx;
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEdx));
        end;
        'String.split' : begin
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
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEax := Copy(StrEbx, trunc(x), trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.between' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            x := stack_pop(pocz[sets.StackPointer]).Num;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            StrEax := Copy(StrEbx, trunc(x), trunc(y)-trunc(x)+1);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildNumber(x));
            	stack_push(pocz[sets.StackPointer], buildNumber(y));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.positionFirst' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            ExtEax := Pos(StrEbx, StrEax);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(ExtEax)); 
        end;
        'String.remove' : begin 
          	if (stack_get(pocz[sets.StackPointer]).EntityType = TSTR) then
          	begin
          	  	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
              	StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
              	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
              	StrEax := stack_pop(pocz[sets.StackPointer]).Str;
              	Delete(StrEax, Pos(StrEbx, StrEax), Length(StrEbx));
              	if not (sets.Autoclear) then begin
              	  	stack_push(pocz[sets.StackPointer], buildString(StrEax));
              	  	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
              	end;
              	stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
          	end else begin
          	  	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
              	y := stack_pop(pocz[sets.StackPointer]).Num;
          	  	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
              	x := stack_pop(pocz[sets.StackPointer]).Num;
              	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
              	StrEax := stack_pop(pocz[sets.StackPointer]).Str;
              	Delete(StrEax, trunc(x), trunc(y)); 
              	if not (sets.Autoclear) then begin
              	  	stack_push(pocz[sets.StackPointer], buildString(StrEax));
              	  	stack_push(pocz[sets.StackPointer], buildNumber(x));
              	  	stack_push(pocz[sets.StackPointer], buildNumber(y));
              	end;
              	stack_push(pocz[sets.StackPointer], buildString(StrEax));
          	end;
        end;
        'String.insert' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            Insert(StrEbx, StrEax, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.replace' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit; 
            y := stack_pop(pocz[sets.StackPointer]).Num;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            Delete(StrEax, Length(StrEbx), trunc(y)); 
            Insert(StrEbx, StrEax, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildString(StrEax)); 
        end;
        'String.nthOccur' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            ExtEax := NPos(StrEbx, StrEax, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(ExtEax)); 
        end;
        'String.positionFrom' : begin
          	if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;  
            y := stack_pop(pocz[sets.StackPointer]).Num;
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            ExtEax := PosEx(StrEbx, StrEax, trunc(y));
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(ExtEax)); 
        end;
		'String.positionLast' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            ExtEax := LastDelimiter(StrEbx, StrEax);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(ExtEax)); 
        end;
        'String.occurs' : begin
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEax := stack_pop(pocz[sets.StackPointer]).Str;
            IntEax := 0;
            repeat
            	IntEbx := NPos(StrEbx, StrEax, IntEax+1);
            	if (IntEbx <> 0) then Inc(IntEax);
            until (IntEbx = 0);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEax));
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
            stack_push(pocz[sets.StackPointer], buildNumber(IntEax)); 
        end;
        'String.run' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit;  
            StrEbx := stack_pop(pocz[sets.StackPointer]).Str;
            pocz := parseScoped(StrEbx, pocz, sets, vardb);
            if not (sets.Autoclear) then begin
            	stack_push(pocz[sets.StackPointer], buildString(StrEbx));
            end;
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
          	if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, i)) then Exit; 
            StrEax := stack_get(pocz[sets.StackPointer]).Str;
            if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEax)));
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
		else Found := false;
	end;
	lib_strings := Found;
end;

function lib_directives(i : String; var pocz : StackDB; var Steps : Integer; var sets : TSettings; var vardb : VariableDB) : Boolean;
var
	Found  : Boolean;
	StrEax : String;
begin
	Found := true;
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
		
        else begin
        	case LeftStr(i, 9) of
            	'@source("' : begin
              		if (RightStr(i, 2) = '")') then begin
                		StrEax := RightStr(i, Length(i)-9);
                		StrEax := LeftStr(StrEax, Length(StrEax)-2);
                		pocz := read_sourcefile(StrEax, pocz, sets, vardb);
              		end else begin
                		raiserror('Exception when attempting to read the file stream: Syntax error');
              		end;
             	end;
             	else begin
              		Found := false;
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
		'PI' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(PI));
        end;
        'EU' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(EU));
        end;
        'FI' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(FI));
        end;
        'NULL' : begin
            stack_push(pocz[sets.StackPointer], buildNull());
        end;
        'TRUE' : begin
            stack_push(pocz[sets.StackPointer], buildBoolean(True));
        end;
        'FALSE' : begin
            stack_push(pocz[sets.StackPointer], buildBoolean(False));
        end;
        '\\' : begin
            stack_push(pocz[sets.StackPointer], buildString('\'));
        end;
        '\>' : begin
            stack_push(pocz[sets.StackPointer], buildString('>'));
        end;
        '\<' : begin
            stack_push(pocz[sets.StackPointer], buildString('<'));
        end;
        '\>>' : begin
          stack_push(pocz[sets.StackPointer], buildString('>>'));
        end;
        '\<<' : begin
          stack_push(pocz[sets.StackPointer], buildString('>>'));
        end;
        '\?' : begin
          stack_push(pocz[sets.StackPointer], buildString('?'));
        end;
        '\$' : begin
          stack_push(pocz[sets.StackPointer], buildString('$'));
        end;
        '\@' : begin
          stack_push(pocz[sets.StackPointer], buildString('@'));
        end;
        '\@@' : begin
          stack_push(pocz[sets.StackPointer], buildString('@@'));
        end;
        '\+' : begin
          stack_push(pocz[sets.StackPointer], buildString('+'));
        end;
        '\-' : begin
          stack_push(pocz[sets.StackPointer], buildString('-'));
        end;
        '\*' : begin
          stack_push(pocz[sets.StackPointer], buildString('*'));
        end;
        '\/' : begin
          stack_push(pocz[sets.StackPointer], buildString('/'));
        end;
        '\^' : begin
          stack_push(pocz[sets.StackPointer], buildString('^'));
        end;
        '\=' : begin
          stack_push(pocz[sets.StackPointer], buildString('='));
        end;
        '\<=' : begin
          stack_push(pocz[sets.StackPointer], buildString('<='));
        end;
        '\>=' : begin
          stack_push(pocz[sets.StackPointer], buildString('>='));
        end;
        '\!=' : begin
          stack_push(pocz[sets.StackPointer], buildString('!='));
        end;
        '\{' : begin
          stack_push(pocz[sets.StackPointer], buildString('{'));
        end;
        '\}' : begin
          stack_push(pocz[sets.StackPointer], buildString('}'));
        end;
        '\[' : begin
          stack_push(pocz[sets.StackPointer], buildString('['));
        end;
        '\]' : begin
          stack_push(pocz[sets.StackPointer], buildString(']'));
        end;
        '\[]' : begin
          stack_push(pocz[sets.StackPointer], buildString('[]'));
        end;
        '\t' : begin
          stack_push(pocz[sets.StackPointer], buildString(''+#9));
        end;
        '\n' : begin
          stack_push(pocz[sets.StackPointer], buildString(''+#10));
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
            doFunction(EntEax.Str, pocz, sets, vardb);
            if not (sets.Autoclear) then begin
                stack_push(pocz[sets.StackPointer], buildString(StrEax));
            end;
        end;
        else begin
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
                                //StrEbx := EntEax.Str;
                                //pocz := parseScoped(StrEbx, pocz, sets, vardb);
                                doFunction(EntEax.Str, pocz, sets, vardb);
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
        end;
    end;
    lib_variables := Found;
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
        {$IFDEF MSWINDOWS}
        'Console.whereX' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(WhereX()));
        end;
        'Console.whereY' : begin
            stack_push(pocz[sets.StackPointer], buildNumber(WhereY()));
        end;
        'Console.delay' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            a := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            Delay(a);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
        end;
        'Console.startSound' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            a := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            Sound(a);
            if not (sets.Autoclear) then stack_push(pocz[sets.StackPointer], buildNumber(y));
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
	Found  : Boolean;
    IntEax : LongInt;
    ArrEax, ArrEbx : Entity;
    EntEax, EntEbx : Entity;
    LogEax : Boolean;
    StrEax, StrEbx : String;
    ExtEax : Extended;
    index  : Integer;
begin
	Found := true;
	case i of
        'Array.crush' : begin
            //if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_reverse(pocz[trunc(ArrEax.Num)]);
            for index := 0 to stack_size(pocz[trunc(ArrEax.Num)])-1 do
            begin
                EntEax := stack_getFront(pocz[trunc(ArrEax.Num)], index);
                stack_push(pocz[sets.StackPointer], EntEax);
            end;
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
        'Array.setAt' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            pocz[trunc(ArrEax.Num)].Values[IntEax] := EntEax;
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.push' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_push(pocz[trunc(ArrEax.Num)], EntEax);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.pop' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_pop(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.pushAt' : begin
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            stack_pushFront(pocz[trunc(ArrEax.Num)], EntEax, IntEax);
            stack_push(pocz[sets.StackPointer], ArrEax);
        end;
        'Array.popAt' : begin
            if (sets.StrictType) and (assertNaturalLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), i)) then Exit;
            IntEax := trunc(stack_pop(pocz[sets.StackPointer]).Num);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_popFront(pocz[trunc(ArrEax.Num)], IntEax);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.shift' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            EntEax := stack_firstpop(pocz[trunc(ArrEax.Num)]);
            stack_push(pocz[sets.StackPointer], EntEax);
        end;
        'Array.length' : begin
            if (sets.StrictType) and (assertEitherLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TSTR, TVEC, i)) then Exit; 
            if (stack_get(pocz[sets.StackPointer]).EntityType = TVEC) then
            begin
                IntEax := trunc(stack_get(pocz[sets.StackPointer]).Num);
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(stack_size(pocz[IntEax])));
            end else begin
                StrEax := stack_get(pocz[sets.StackPointer]).Str;
                if (sets.Autoclear) then stack_pop(pocz[sets.StackPointer]);
                stack_push(pocz[sets.StackPointer], buildNumber(Length(StrEax)));
            end;
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
                if (EntEax.Str = EntEbx.Str) and (EntEax.Num = EntEbx.Num) then 
                begin 
                    LogEax := True;
                    break;
                end;
    		end;
            stack_push(pocz[sets.StackPointer], buildBoolean(LogEax));
        end;
        'Array.map' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            StrEax := 'mapi_'+IntToStr(DateTimeToUnix(Now));
            stack_push(pocz[sets.StackPointer], buildNewEmptyArray(pocz, sets, Length(pocz[trunc(ArrEax.Num)].Values)));
            ArrEbx := stack_pop(pocz[sets.StackPointer]);
            vardb.addLayer();
            for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
            begin
                vardb.setLocalVariable(StrEax, pocz[trunc(ArrEax.Num)].Values[index]);
                pocz := parseOpen('$' + StrEax + ' ' + EntEax.Str + ' >' + StrEax, pocz, sets, vardb);
                pocz[trunc(ArrEbx.Num)].Values[index] := vardb.getLocalVariable(StrEax);
    		end;
            vardb.removeLayer();
            stack_push(pocz[sets.StackPointer], ArrEbx);
        end;
        'Array.reduce' : begin
            EntEbx := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            StrEax := 'redlacc_'+IntToStr(DateTimeToUnix(Now));
            StrEbx := 'redlstp_'+IntToStr(DateTimeToUnix(Now));
            vardb.addLayer();
            vardb.setLocalVariable(StrEax, EntEbx);
            for index := 0 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
            begin
                vardb.setLocalVariable(StrEbx, pocz[trunc(ArrEax.Num)].Values[index]);
                pocz := parseOpen('$' + StrEax + ' $' + StrEbx + ' ' + EntEax.Str + ' >' + StrEax, pocz, sets, vardb);
    		end;
            stack_push(pocz[sets.StackPointer], vardb.getLocalVariable(StrEax));
            vardb.removeLayer();
        end;
        'Array.reduceFromFirst' : begin
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TFUN, i)) then Exit;
            EntEax := stack_pop(pocz[sets.StackPointer]);
            if (sets.StrictType) and (assertEntityLocated(pocz[sets.StackPointer], stack_get(pocz[sets.StackPointer]), TVEC, i)) then Exit; 
            ArrEax := stack_pop(pocz[sets.StackPointer]);
            StrEax := 'redlacc_'+IntToStr(DateTimeToUnix(Now));
            StrEbx := 'redlstp_'+IntToStr(DateTimeToUnix(Now));
            vardb.addLayer();
            vardb.setLocalVariable(StrEax, pocz[trunc(ArrEax.Num)].Values[0]);
            for index := 1 to Length(pocz[trunc(ArrEax.Num)].Values)-1 do
            begin
                vardb.setLocalVariable(StrEbx, pocz[trunc(ArrEax.Num)].Values[index]);
                pocz := parseOpen('$' + StrEax + ' $' + StrEbx + ' ' + EntEax.Str + ' >' + StrEax, pocz, sets, vardb);
    		end;
            stack_push(pocz[sets.StackPointer], vardb.getLocalVariable(StrEax));
            vardb.removeLayer();
        end;

        // crush, pushAt, popAt, swapAt, toString, size

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

end.
