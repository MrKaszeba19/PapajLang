unit MathUtils;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils;

procedure swapNumbers(var e1 : Extended; var e2 : Extended);
function pow(x, y : Extended) : Extended;
function pow2(x, y : Extended) : Extended;
function fmod(x, y : Extended) : Extended;
function fdiv(x, y : Extended) : Extended;
function fact(x : Extended) : Extended;

function ftrunc(x : Extended) : Extended;
function fround(x : Extended) : Extended;
function ffloor(x : Extended) : Extended;
function fceiling(x : Extended) : Extended;

function isPrime(x : LongInt) : Boolean;
function isInteger(x : Extended) : Boolean;

function newton_int(n, k : Extended) : Extended;
function newton_real(n, k : Extended) : Extended;
function gcd(a, b : Extended) : Extended;
function lcm(a, b : Extended) : Extended;
function fib(n: Extended) : Extended;

function fgamma(x : Extended) : Extended;
function dstdnorm(x : Extended) : Extended;
function dnorm(x, mu, si : Extended) : Extended;
function fnorm(x, mu, si : Extended) : Extended;
function fbinom(n : LongInt; k : LongInt; p : Extended) : Extended;
function dbinom(n, k : LongInt; p : Extended) : Extended;
function rbinom(n : LongInt; p : Extended) : Extended;
function fgeom(k : LongInt; p : Extended) : Extended;
function dgeom(k : LongInt; p : Extended) : Extended;
function rgeom(p : Extended) : Extended;
function fexp(x : Extended; lambda : Extended) : Extended;
function dexp(x : Extended; lambda : Extended) : Extended;
function rexp(lambda : Extended) : Extended;
function fpoisson(x, lambda : Extended): Extended;
function dpoisson(x, lambda : Extended): Extended;
function rpoisson(mean: Extended): Extended;

implementation

uses Math;

procedure swapNumbers(var e1 : Extended; var e2 : Extended);
var
	pom : Extended;
begin
	pom := e1;
	e1 := e2;
	e2 := pom;
end;

// MATHEMATICAL FUNCTIONS

function pow(x, y : Extended) : Extended;
var
    s : Extended;
    i : integer;
begin
    s := 1;
    for i := 1 to trunc(abs(y)) do s := s * x;
    if (y < 0) then s := 1 / s;
    Result := s;
end;

function pow2(x, y : Extended) : Extended;
var
    s : Extended;
begin
    if (x = 0) and (y <> 0) then s := 0
    else if (x = 0) and (y = 0) then s := NaN
    else s := exp(y*ln(x));
    Result := s;
end;

function fmod(x,y:Extended):Extended;
begin
    Result := x - y * Int(x/y);
end;

function fdiv(x, y : Extended) : Extended;
begin
    Result := Int(x/y);
end;

function fact(x:Extended):Extended;
var
    s : Extended;
    i : integer;
begin
    s := 1;
    for i := 1 to trunc(abs(x)) do s := s * i;
    Result := s;
end;

function ftrunc(x : Extended) : Extended;
begin
    Result := fdiv(x,1);
end;

function fround(x : Extended) : Extended;
begin
    if (x < 0) 
        then Result := fdiv(x-0.5,1)
        else Result := fdiv(x+0.5,1);
end;

function ffloor(x : Extended) : Extended;
begin
    if (isInteger(x)) 
        then Result := fdiv(x,1)
	    else if (x < 0) 
            then Result := fdiv(x,1)-1 
            else Result := fdiv(x,1);
end;
function fceiling(x : Extended) : Extended;
begin
    if (isInteger(x)) 
        then Result := fdiv(x,1)
	    else if (x < 0) 
            then Result := fdiv(x,1) 
            else Result := fdiv(x,1)+1;
end;

// ======== booleans

function isPrime(x : LongInt) : Boolean;
var
    i : LongInt;
    s : Boolean;
begin
    case x of
        0 : Result := False;
        1 : Result := False;
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
            Result := s;
        end;
    end;
end;

function isInteger(x : Extended) : Boolean;
begin
    Result := (x = int(x));
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
    if (k > n) then 
        Result := 1.0/0.0
    else if (k = 0) then 
        Result := 1
    else if (k > n/2) then
        Result := newton_int(n,n-k)
    else 
        Result := n * newton_int(n-1,k-1) / k;
end;

function newton_real(n, k : Extended) : Extended;
var 
    s, j : Extended;
begin
	s := 1;
	if (n < 0) then
		Result := pow(-1.0, k) * newton_real(k-n-1, k)
	else begin
		j := 1.0;
		while (j <= k) do
		begin
			s := s * (n-k+j)/j;
			j := j + 1;
		end;
    	Result := s;
  	end;
end;

function gcd(a, b : Extended) : Extended;
begin
	while (a <> b) do
	begin
        if (a > b) then a := a - b
        else b := b - a;
    end;
	Result := a;
end;

function lcm(a, b : Extended) : Extended;
begin
	Result := (a*b)/gcd(a, b);
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
    if n = 0.0 then Result := 0.0
    else if n = 1.0 then Result := 1.0
    else begin
        a := 0.0;
        b := 1.0;
        for i := 2 to trunc(n) do
        begin
            a := a + b;
            swapNumbers(a, b);
        end;
        Result := b;
    end;
end;

function fgamma(x : Extended) : Extended;
var
	limit, n : Integer;
	s, s1    : Extended;
	epsilon  : Extended;
begin
	if (isInteger(x)) then Result := fact(x-1) 
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
		Result := s/x;

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
		Result := 0.00000000001 
	end
	else if (x > limit) then 
	begin 
		Result := 0.99999999999 
	end else if (x = 0) then
	begin
		Result := 0.5;
	end
	else if (x = -1) then
	begin
		Result := 0.158655253931457;
	end
	else if (x = 1) then
	begin
		Result := 0.841344746068543;
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
		Result := sum/sqrt(2*PI)*(2/3);
	end else if (x < 0) then
	begin
		sum := 0;
		t := x;
		while (t <= 0) do
		begin
			sum := sum + eps*((exp(-(t*t/2)))+(exp(-((t-eps)*(t-eps)/2)))/2);
			t := t + eps;
		end; 
		Result := 0.5 - sum/sqrt(2*PI)*(2/3);
	end else if (x <= 1) then begin
		sum := 0;
		t := x;
		while (t > 0) do
		begin
			sum := sum + eps*((exp(-(t*t/2)))+(exp(-((t-eps)*(t-eps)/2)))/2);
			t := t - eps;
		end; 
		Result := 0.5 + sum/sqrt(2*PI)*(2/3);
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
		Result := 1 - sum/sqrt(2*PI)*(2/3);
	end;
end;

// 0.841344746068543
// 0.158655253931457

function dnorm(x, mu, si : Extended) : Extended;
begin
	Result := dstdnorm((x-mu)/si);
end;

function fnorm(x, mu, si : Extended) : Extended;
begin
    Result := 1/(si*sqrt(2*PI))*exp(-0.5*sqr((x-mu)/si));
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
    Result := newton_int(n, k)*pow(p, k)*pow(1-p, n-k);
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
        Result := s; 
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
    Result := res;
end;

function fgeom(k : LongInt; p : Extended) : Extended;
begin
    Result := pow(1-p, k-1)*p;
end;

function dgeom(k : LongInt; p : Extended) : Extended;
begin
    Result := 1 - pow(1-p, k);
end;

function rgeom(p : Extended) : Extended;
var
    x      : Extended;
    i, res : LongInt;
begin
    res := 1;
    if (p <= 0) 
        then Result := Infinity 
        else begin
            while (random >= p) do 
                Inc(res);
            Result := res;
        end;
end;

function fexp(x : Extended; lambda : Extended) : Extended;
begin
    Result := lambda * exp(-x*lambda);
end;

function dexp(x : Extended; lambda : Extended) : Extended;
begin
    Result := 1 - exp(-x*lambda);
end;

function rexp(lambda : Extended) : Extended;
begin
    Result := -ln(random)/lambda;
end;

function fpoisson(x, lambda : Extended): Extended;
begin
    Result := exp(-lambda)*pow(lambda, x)/fact(x);
end;

function dpoisson(x, lambda : Extended): Extended;
var 
    s : Extended;
    i : LongInt;
begin
    s := 0;
    for i := 0 to trunc(x) do
        s += fpoisson(i, lambda);
    Result := s; 
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
    Result := k - 1;
end;

end.

