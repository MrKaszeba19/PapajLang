unit MathUtils;

{$mode objfpc}{$H+}

interface

uses
    ComplexNumbers,
	Classes, SysUtils;

procedure swapNumbers(var e1 : Extended; var e2 : Extended);
//function pow(x, y : Extended) : Extended;
//function pow2(x, y : Extended) : Extended;
//function root(x, y : Extended) : Extended;
function fmod(x, y : Extended) : Extended;
function fdiv(x, y : Extended) : Extended;
function fact(x : Extended) : Extended;
function subfact(x : Extended) : Extended;
function minusOneTo(x : LongInt) : LongInt;

function ftrunc(x : Extended) : Extended;
function ffrac(x : Extended) : Extended;
function fround(x : Extended) : Extended;
function ffloor(x : Extended) : Extended;
function fceiling(x : Extended) : Extended;
function ftruncTo(x : Extended; digits : LongInt = 0) : Extended;
function froundTo(x : Extended; digits : LongInt = 0) : Extended;
function ffloorTo(x : Extended; digits : LongInt = 0) : Extended;
function fceilingTo(x : Extended; digits : LongInt = 0) : Extended;
function randomIntRange(x, y : Extended) : Extended;
function randomRealRange(x, y : Extended) : Extended;

function isPrime(x : Extended) : Boolean;
//function isInteger(x : Extended) : Boolean;
function divides(x, y : Extended) : Boolean;

//function newton_int(n, k : Extended) : Extended;
//function newton_real(n, k : Extended) : Extended;
function gcd(a, b : Extended) : Extended;
function lcm(a, b : Extended) : Extended;
function gcdExtended(a, b : LongInt; var x : LongInt; var y : LongInt) : LongInt;
function fib(n: Extended) : Extended;

//function LogGamma(x : Extended) : Extended;
//function vgamma(x : Extended) : Extended;
//function vlowergamma(s, x : Extended) : Extended;

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
function fgamma(x, alpha, beta : Extended) : Extended;
function dgamma(x, alpha, beta : Extended) : Extended;
function rgamma1(a, b : Extended) : Extended;
function rgengamma(a, b, c: Extended): Extended;
function fchisq(x : Extended; v : LongInt) : Extended;
function dchisq(x : Extended; v : LongInt) : Extended;
function rchisq(df : Extended) : Extended;
function ferlang(x, k, l : Extended) : Extended;
function derlang(x, k, l : Extended) : Extended;
function rerlang(k, l : Extended) : Extended;
//function vbeta(x, y : Extended) : Extended;
//function vinbeta(x, a, b : Extended) : Extended;
//function vrinbeta(x, a, b : Extended) : Extended;
function fstudentt(x, nu : Extended) : Extended;
function dstudentt(x, nu : Extended) : Extended;
function rstudentt(df: Extended) : Extended;
function fbeta(x, alpha, beta : Extended) : Extended;
function dbeta(x, alpha, beta : Extended) : Extended;
function rbeta(a, b : Extended) : Extended;
function ffischerf(x, d1, d2 : Extended) : Extended;
function dfischerf(x, d1, d2 : Extended) : Extended;
function rfischerf(v, w: Extended) : Extended;
function farcsine(x : Extended) : Extended;
function darcsine(x : Extended) : Extended;
function rarcsine() : Extended;

function num_tau(n : Extended) : Extended;
function num_sigma(n : Extended) : Extended;
function num_mobius(n : Extended) : Extended;
function num_euler(n : Extended) : Extended;
function num_pi(n : Extended) : Extended;
function num_omega(n : Extended) : LongInt;
function num_omega2(n : Extended) : LongInt;
function num_liouville(n : Extended) : LongInt;
function num_isSquareFree(n : Extended) : Boolean;
function num_isCarmichael(n : LongInt) : Boolean;

//function modularPower(a, b, m : Extended) : Extended;
function modularPower(a, b, m : LongInt) : LongInt;
function modularInverse(a, m : LongInt) : LongInt;

function convertDecToBase(a : LongInt; base : Integer) : String;
function convertBaseToDec(str : String; base : Integer) : LongInt;

function toNumberString(input : Extended; decimal : String; thousand : String = '') : String;
function getNumberFromNumberString(input : String; decimal : String; thousand : String = '') : Extended;


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

function isInteger(x : Extended) : Boolean;
begin
    Result := (x = int(x));
end;

{*
function pow(x, y : Extended) : Extended;
var
    s : Extended;
    i : LongInt;
    d : LongInt;
begin
    if (x = 0) then 
    begin
        if (y <= 0) then s := NaN else s := 0;
    end
    else if (x*x = 1) then 
    begin 
        if (x = -1) 
            then s := minusOneTo(trunc(y))
            else s := 1;
    end else begin
        d := abs(trunc(y));
        s := 1;
        if (d > 0) then
        begin
            if (d mod 2 = 0) 
                then s := sqr(pow(x, d div 2))
                else s := x * pow(x, d-1);
            if (y < 0) then s := 1 / s;
        end;
    end;
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

function pow_int(a, f : longint) : longint;
begin
	if f = 0 then Result := 1
	else Result := a * pow_int(a, f-1);
end;

function root(x, y : Extended) : Extended;
begin
    if (x < 0) then 
    begin
        if (isInteger(y)) then
        begin
            if (trunc(y) mod 2 = 0) 
                then Result := NaN // so far
                else Result := -pow2(abs(x),1/y);
        end else Result := pow2(x,1/y); 
    end else Result := pow2(x,1/y);
end;
*}

function fmod(x, y : Extended) : Extended;
begin
    Result := x - y * Int(x/y);
end;

function fdiv(x, y : Extended) : Extended;
begin
    Result := Int(x/y);
end;

function fact(x : Extended) : Extended;
var
    s : Extended;
    i : LongInt;
begin
    s := 1;
    i := 1; 
    while i <= abs(x) do begin
        s := s * i;
        i := i + 1;
    end;
    Result := s;
end;

function subfact(x : Extended) : Extended;
begin
    if x = 0 
        then Result := 1
        else Result := ffloor(fact(x)/system.exp(1) + 0.5);
end;

function minusOneTo(x : LongInt) : LongInt;
begin
    if (x mod 2 = 0) 
        then Result := 1
        else Result := -1;
end;


function ftrunc(x : Extended) : Extended;
begin
    //{$IFDEF cpu32} writeln( 'cpu32' ); 
    if abs(x) <= High(LongInt) 
        then Result := trunc(x)
        else Result := fdiv(x,1);
    //{$ENDIF}
end;

function ffrac(x : Extended) : Extended;
begin
    //{$IFDEF cpu32} writeln( 'cpu32' ); 
    if abs(x) <= High(LongInt) 
        then Result := frac(x)
        else Result := fmod(x,1);
    //{$ENDIF}
end;

function fround(x : Extended) : Extended;
begin
    if abs(x) <= High(LongInt)
        then 
            //Result := Trunc(x+0.5)
            if (x <= 0) 
                then Result := Trunc(x-0.5)
                else Result := Trunc(x+0.5)
        else 
            if (x <= 0) 
                then Result := fdiv(x-0.5,1)
                else Result := fdiv(x+0.5,1);
end;

function ffloor(x : Extended) : Extended;
begin
    if abs(x) <= High(LongInt)
        then Result := Floor(x)
        else if (isInteger(x)) 
            then Result := fdiv(x,1)
	        else if (x < 0) 
                then Result := fdiv(x,1)-1 
                else Result := fdiv(x,1);
end;
function fceiling(x : Extended) : Extended;
begin
    if abs(x) <= High(LongInt)
        then Result := Ceil(x)
        else if (isInteger(x)) 
            then Result := fdiv(x,1)
	        else if (x < 0) 
                then Result := fdiv(x,1) 
                else Result := fdiv(x,1)+1;
end;

function ftruncTo(x : Extended; digits : LongInt = 0) : Extended;
var i : LongInt;
begin
    if digits > 0 then
    begin
        for i := 1 to digits do x := x * 10;
        x := ftrunc(x);
        for i := 1 to digits do x := x / 10;
    end else begin
        for i := 1 to -digits do x := x / 10;
        x := ftrunc(x);
        for i := 1 to -digits do x := x * 10;
    end;
    Result := x;
end;

function froundTo(x : Extended; digits : LongInt = 0) : Extended;
var i : LongInt;
begin
    if digits > 0 then
    begin
        for i := 1 to digits do x := x * 10;
        x := fround(x);
        for i := 1 to digits do x := x / 10;
    end else begin
        for i := 1 to -digits do x := x / 10;
        x := fround(x);
        for i := 1 to -digits do x := x * 10;
    end;
    Result := x;
end;


function ffloorTo(x : Extended; digits : LongInt = 0) : Extended;
var i : LongInt;
begin
    if digits > 0 then
    begin
        for i := 1 to digits do x := x * 10;
        x := ffloor(x);
        for i := 1 to digits do x := x / 10;
    end else begin
        for i := 1 to -digits do x := x / 10;
        x := ffloor(x);
        for i := 1 to -digits do x := x * 10;
    end;
    Result := x;
end;

function fceilingTo(x : Extended; digits : LongInt = 0) : Extended;
var i : LongInt;
begin
    if digits > 0 then
    begin
        for i := 1 to digits do x := x * 10;
        x := fceiling(x);
        for i := 1 to digits do x := x / 10;
    end else begin
        for i := 1 to -digits do x := x / 10;
        x := fceiling(x);
        for i := 1 to -digits do x := x * 10;
    end;
    Result := x;
end;


function randomIntRange(x, y : Extended) : Extended;
begin
    Result := trunc(x) + random(trunc(y)-trunc(x)+1);
end;

function randomRealRange(x, y : Extended) : Extended;
begin
    Result := x + random * (y - x);
end;

// ======== booleans

function isPrime(x : Extended) : Boolean;
var
    i : LongInt;
    s : Boolean;
begin
    if x <= 1 
        then Result := False
        else begin
            s := True;
            if (x <> 5) and (x <> 11) and (x >= 4) then
            begin
                if (fmod(x, 2) = 0) then 
                begin
                    s := False;
                end else if (fmod(x, 5) = 0) then 
                begin
                    s := False;
                end else begin
                    i := 3;
                    //while i <= sqrt(x) do
                    while i*i <= x do
                    begin
                        if (fmod(x, i) = 0) or (fmod(x, i+4) = 0) or (fmod(x, i+6) = 0) or (fmod(x, i+8) = 0) then 
                        begin
                            s := False;
                            break;
                        end;
                        i := i + 10;
                    end;
                end;
            end;
            Result := s;
        end;
end;

function divides(x, y : Extended) : Boolean;
begin
    Result := fmod(x,y) = 0
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

{*
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
*}

function gcd(a, b : Extended) : Extended;
var
    temp : Extended;
begin
    while b <> 0 do
    begin
      temp := b;
      b := fmod(a, b);
      a := temp;
    end;
    result := a;
end;

function gcd2(a, b : Extended) : Extended;
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

function gcdExtended(a, b : LongInt; var x : LongInt; var y : LongInt) : LongInt;
var 
    c, q, r, s, r1, s1 : LongInt;
begin
    x := 1;
    y := 0;
    r := 0;
    s := 1;
    while (b <> 0) do
    begin
        c := a mod b;
        q := a div b;
        a := b;
        b := c;
        r1 := r;
        s1 := s;
        r := x - q * r;
        s := y - q * s;
        x := r1;
        y := s1;
    end;
    Result := a;
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

// ====== STATISTICS

{*
function LogGamma(x : Extended) : Extended;
{ Log of Gamma(x), exponentiate this to get Gamma(x), x! =
Gamma(x+1),
  very accurate approximation: 1+epsilon, abs(epsilon) < 2.1E-10
  Based on Numerical Recipes, by Press, Flannery, Teukolsky,
  and Vetterling; first edition, page 157 and 704; but greatly
cleaned up, by Jud McCranie }
const stp =   2.50662827465;
      c1  =  76.18009173;
      c2  = -86.50532033;
      c3  =  24.01409822;
      c4  =  -1.231739516;
      c5  =   1.20858003E-3;
      c6  =  -5.36382E-6;
var ser : float;
begin { --- log gamma --- }
    ser := 1.0 + c1 / x + c2 / (x + 1.0) + c3 / (x + 2.0) +
             c4 / (x + 3.0) + c5 / (x + 4.0) + c6 / (x + 5.0);
    LogGamma := (x - 0.5) * ln( x + 4.5) - x - 4.5 + ln( stp * ser);
end; { --- log gamma --- }

function vgamma(x : Extended) : Extended;
var
	limit, n : Integer;
begin
    //writeln('x=',x:2:5);
	if (isInteger(x)) then Result := fact(x-1) 
    else if (x = 0.5) then
    begin
        Result := sqrt(PI);
    end else if (x > 0) and (fmod(x,1) = 0.5) then
    begin
        Result := (x-1)*vgamma(x-1);
    end 
    else begin
		Result := exp(LogGamma(x));
	end;
end;

function vlowergamma(s, x : Extended) : Extended;
var
    t, sum  : Extended;
	epsilon : Extended;
begin
    if (s = 1) then
    begin
        //writeln('chuj3');
        //writeln('s=',s:2:5,' x=',x:2:5);
        Result := 1.0 - exp(-x);
    end else if (x = 0) then begin
        //writeln('chuj2');
        Result := 0;
    end else begin
        //writeln('chuj');
	    epsilon := 0.0001*trunc(x+1);
        //epsilon := 0.0001;
        sum := 0;
        //writeln('s=',s:2:5,' x=',x:2:5);
        t := 0;
        while (t <= x) do
        begin
            //writeln('s=',s:2:5,' x=',t:2:5);
            //writeln(pow2(t, s-1));
            sum := sum + epsilon*(pow2(t, s-1)*exp(-t));
            //sum := sum + (pow2(t, s-1)*exp(-t));
            t := t + epsilon;
            Result := sum;
        end;
    end;
end;
*}

function dstdnorm(x : Extended) : Extended;
//var
	//sum    : Extended;
	//sum1   : Extended;
	//t, eps : Extended;
	//limit  : Extended;
begin
    //writeln('dnorm');
    Result := 0.5 * (1 + Real(Erf(x*sqrt(2)/2)));
    //checkSIGINT();
	{*
    eps := 0.00001;
	limit := 10;
    // add +/- infinity and increase precision
	if (x = -Infinity) then 
	begin 
		Result := 0 
	end 
    else if (x = Infinity) then 
	begin 
		Result := 1 
	end
    else if (x < -limit) then 
	begin 
		Result := 0.0000000000000001 
	end
	else if (x > limit) then 
	begin 
		Result := 0.9999999999999999 
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
    *}
end;

// 0.841344746068543
// 0.158655253931457

function dnorm(x, mu, si : Extended) : Extended;
begin
	Result := dstdnorm((x-mu)/si);
end;

function fnorm(x, mu, si : Extended) : Extended;
begin
    Result := Real(Inv(si*Sqrt(2*PI))*Exp(-0.5*Sqr((x-mu)/si)));
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
    //Result := newton_int(n, k)*pow(p, k)*pow(1-p, n-k);
    Result := Real(Newton(n, k)*pow(p, k)*pow(1-p, n-k));
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
    Result := Real(pow(1-p, k-1)*p);
end;

function dgeom(k : LongInt; p : Extended) : Extended;
begin
    Result := Real(1 - pow(1-p, k));
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
    Result := lambda * system.exp(-x*lambda);
end;

function dexp(x : Extended; lambda : Extended) : Extended;
begin
    Result := 1 - system.exp(-x*lambda);
end;

function rexp(lambda : Extended) : Extended;
begin
    Result := -system.ln(random)/lambda;
end;

function fpoisson(x, lambda : Extended): Extended;
begin
    Result := Real(exp(-lambda)*pow(lambda, x))/fact(x);
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
    l := system.exp(-mean);
    while b > l do
    begin
        k := k + 1;
        b := b * random(RESOLUTION) / RESOLUTION;
    end;
    Result := k - 1;
end;

function fgamma(x, alpha, beta : Extended) : Extended;
begin
    if alpha = 1 
        then Result := fexp(x, beta)
        else if isInteger(alpha) 
            then Result := ferlang(x, alpha, beta)
            else Result := Real((Pow(beta, alpha)*Pow(x, alpha-1)*Exp(-beta*x))/Gamma(alpha));
end;

function dgamma(x, alpha, beta : Extended) : Extended;
begin
    //writeln('VL: ', vlowergamma(alpha, beta*x):2:5);
    //writeln('VG: ', vgamma(alpha):2:5);
    if x = 0.0 
        then Result := 0.0
        else if isInteger(alpha) 
            then Result := derlang(x, alpha, beta)
            //else Result := vlowergamma(alpha, beta*x)/vgamma(alpha);
            else Result := Real(LowerGamma(alpha, beta*x)/Gamma(alpha));
end;

function rgamma1(a, b : Extended) : Extended;
var
    d, c    : Extended;
    Z, V, U : Extended;
    flag    : Boolean;
begin
    if (a >= 1) then
    begin
        d := a - 1.0/3; 
        c := 1.0/system.sqrt(9*d); 
        flag := False;
        repeat
            Z := randg(0, 1);
            V := Real(pow((1+c*Z), 3)); 
            if Z > -1.0/c then
            begin
                U := random();
                flag := system.ln(U) < (0.5*Z*Z + d - d*V + d*system.ln(V));
            end;
        until flag = True;
        Result := d*V/b;
    end else begin
        c := rgamma1(a+1, b);
        c := c * Real(pow(random(), (1/a)));
        Result := c;
    end;
end;


function rgamma2(a, b : Extended) : Extended;
var
    d, c    : Extended;
    Z, V, U : Extended;
    flag    : Boolean;
begin
    if (a > 1) then
    begin
        d := a - 1.0/3; 
        c := 1.0/system.sqrt(9*d); 
        flag := True;
        while flag do
        begin
            Z := randg(0,1);
            if Z > -1/c then
            begin
                V := Real(pow((1+c*Z), 3)); 
                U := random;
                flag := system.ln(U) > (0.5*Z*Z + d - d*V + d*system.ln(V));
            end;
        end;
        Result := d*V/b;
    end else begin
        c := rgamma2(a+1, b);
        c := c * Real(pow(random(), (1/a)));
        Result := c;
    end;
end;

function randomExp(a, rate: Extended): Extended;
const
  RESOLUTION = 1000;
var
  unif: Extended;
begin
  if rate = 0 then
    randomExp := NaN
  else
  begin
    repeat
      unif := random(RESOLUTION) / RESOLUTION;
    until unif <> 0;
    randomExp := a - rate * system.ln(unif);
  end;
end;

function rgengamma(a, b, c: Extended): Extended;
const
    RESOLUTION = 1000;
    T = 4.5;
    D = 1 + system.ln(T);
var
    unif: Extended;
    A2, B2, C2, Q, p, y: Extended;
    p1, p2, v, w, z: Extended;
    found: boolean;
begin
    A2 := 1 / system.sqrt(2 * c - 1);
    B2 := c - system.ln(4);
    Q := c + 1 / A2;
    C2 := 1 + c / system.exp(1);
    found := False;
    if c < 1 then
    begin
        repeat
            repeat
                unif := random(RESOLUTION) / RESOLUTION;
            until unif > 0;
            p := C2 * unif;
            if p > 1 then
            begin
                repeat
                    unif := random(RESOLUTION) / RESOLUTION;
                until unif > 0;
                y := -system.ln((C2 - p) / c);
                if unif <= power(y, c - 1) then
                begin
                    Result := a + b * y;
                    found := True;
                end;
            end else begin
                y := power(p, 1 / c);
                if unif <= system.exp(-y) then
                begin
                    Result := a + b * y;
                    found := True;
                end;
            end;
        until found;
    end
    else if c = 1 then
      { Gamma distribution becomes exponential distribution, if c = 1 }
    begin
        Result := randomExp(a, b);
    end else begin
        repeat
            repeat
                p1 := random(RESOLUTION) / RESOLUTION;
            until p1 > 0;
            repeat
                p2 := random(RESOLUTION) / RESOLUTION;
            until p2 > 0;
                v := A2 * system.ln(p1 / (1 - p1));
                y := c * system.exp(v);
                z := p1 * p1 * p2;
                w := B2 + Q * v - y;
            if (w + D - T * z >= 0) or (w >= system.ln(z)) then
            begin
                Result := a + b * y;
                found := True;
            end;
        until found;
    end;
end;

function fchisq(x : Extended; v : LongInt) : Extended;
begin
    //Result := (pow2(x, v/2.0-1.0)*exp(-x/2))/(pow2(2, v/2)*vgamma(v/2.0));
    case v of
        2 : Result := fexp(x, 1/2);
        else begin
                 if (v = 1) and (x = 0) then Result := infinity
            else if (x = 0) then Result := 0
            else Result := fgamma(x, v/2, 0.5);
        end;
    end;
end;

function dchisq(x : Extended; v : LongInt) : Extended;
begin
    //Result := vlowergamma(v/2.0, x/2.0)/vgamma(v/2.0);
    case v of
        2 : Result := dexp(x, 1/2);
        else begin
            if (x = 0) 
            then Result := 0 
            else Result := dgamma(x, v/2, 0.5);
        end;
    end;
end;

function rchisq(df : Extended) : Extended;
begin
    if df < 1 
        then Result := NaN
        else Result := rgamma1(df/2, 0.5);
        //else if ftrunc(df) = 1 
        //    then Result := rgamma1(df/2, 0.5)
        //    else Result := rgengamma(0, 2, df/2);
end;

//function rchisq(df: Extended) : Extended;
//begin
//    if df < 1 then 
//        Result := NaN
//    else
//        Result := rgengamma(0, 2, 0.5 * df);
//end;

function ferlang(x, k, l : Extended) : Extended;
begin
    Result := Real(pow(l, k)*pow(x, k-1)*exp(-l*x))/fact(k-1); 
end;

function derlang(x, k, l : Extended) : Extended;
var
    i : LongInt;
    s : Extended;
begin
    s := 0;
    for i := 0 to trunc(k)-1 
        do s := s + (Real(exp(-l*x)*pow(l*x, i))/fact(i));
    Result := 1 - s;
end;

function rerlang(k, l : Extended) : Extended;
const
    RESOLUTION = 1000;
var
    i          : Integer;
    unif, prod : Extended;
begin
    if (l <= 0) or (k < 1) then
        Result := NaN
    else
    begin
        prod := 1;
        for i := 1 to trunc(k) do
        begin
            repeat
                unif := random(RESOLUTION) / RESOLUTION;
            until unif <> 0;
            prod := prod * unif;
        end;
        Result := -1.0/l * system.ln(prod);
    end;
end;

{*
function vbeta(x, y : Extended) : Extended;
//var
//    eps  : Extended;
//    t, s : Extended;
begin
    if (isInteger(x)) and (isInteger(y)) then
    begin
        Result := ((x+y)/(x*y))*(1/(newton_int(x+y, x)))
    end else begin
        //eps := 0.0001;
        //s := 0;
        //t := 0;
        //while (t < 1) do
        //begin
        //    s := s + eps*(pow2(t, x-1) * pow2(1-t, y-1));
        //    t := t + eps;
        //end;
        //Result := s;
        Result := exp(LogGamma(x) + LogGamma(y) - LogGamma(x+y));
    end;
end;

function vinbeta(x, a, b : Extended) : Extended;
var
    eps  : Extended;
    t, s : Extended;
begin
    eps := 0.0001;
    s := 0;
    t := 0;
    while (t < x) do
    begin
        s := s + eps*(pow2(t, a-1) * pow2(1-t, b-1));
        t := t + eps;
    end;
    Result := s;
end;

function vrinbeta(x, a, b : Extended) : Extended;
begin
    //writeln(x:2:6);
    //writeln(vinbeta(x, a, b):2:8, #9, vbeta(a, b):2:8);
    //writeln(vinbeta(x, a, b)/vbeta(a, b):2:8);
         if (x = 0)   then Result := 0
    else if (x = 1)   then Result := 1
    else if (b = 1)   then Result := pow2(x, a)
    else if (a = 1)   then Result := 1 - pow2(1-x, b)
    //else if (x > 0.5) then Result := 1 - vrinbeta(1.0-x, b, a)
    else Result := vinbeta(x, a, b)/vbeta(a, b);
    //Result := vinbeta(x, a, b)/vbeta(a, b);
end;
*}

function gammat(nu : Extended) : Extended;
var
    s : Extended;
begin
    if (fmod(nu, 2) = 0) 
        then s := 1/(2*system.sqrt(nu))
        else s := 1/(PI*system.sqrt(nu));
    nu := nu - 1;
    while (nu >= 2) do
    begin
        s := s * nu/(nu-1);
        nu := nu - 2;
    end;
    Result := s;
end;

function fstudentt(x, nu : Extended) : Extended;
begin
    if nu = Infinity then Result := fnorm(x, 0, 1)
    else if isInteger(nu) then
    begin
             if nu = 1 then Result := Real(1/(PI*(x*x+1)))
        else if nu = 2 then Result := Real(1/(2*sqrt(2)*sqrt(pow(x*x/2+1, 3))))
        else if nu = 3 then Result := Real(2/(PI*sqrt(3)*pow(x*x/3+1, 2)))
        else if nu = 4 then Result := Real(3/(8*sqrt(pow(x*x/4+1, 5))))
        else if nu = 5 then Result := Real(8/(3*PI*sqrt(5)*pow(x*x/5+1, 3)))
        else if nu > 0 then 
        begin
             if (nu > 1) 
                then Result := gammat(nu)/Real(sqrt(pow(x*x/nu+1, nu+1)))
                else Result := Real(Inv(sqrt(nu)*Beta(0.5, nu/2))/sqrt(pow(x*x/nu+1, nu+1)));
        end 
        else Result := NaN;
    end
    else if nu > 0 then Result := Real(1/(sqrt(nu)*Beta(0.5, nu/2))*pow(x*x/nu+1, -(nu+1)/2))
    else Result := NaN;
end;

function dstudentt(x, nu : Extended) : Extended;
begin
    //writeln(nu/(x*x+nu):2:5);
         if nu = Infinity then Result := dstdnorm(x)
    else if nu = 1 then Result := 0.5 + system.arctan(x)/PI
    else if nu = 2 then Result := 0.5 + Real(x/(2*sqrt(2*(x*x*0.5+1))))
    else if nu = 3 then Result := 0.5 + Real((x/(sqrt(3)*(x*x/3+1)) + arctan(x/sqrt(3)))/PI)
    else if nu = 4 then Result := 0.5 + Real(3/8*(x/sqrt(x*x/4+1))*(1-1/12*((x*x)/(x*x/4+1))))
    else if nu = 5 then Result := 0.5 + Real((x/(sqrt(5)*(x*x/5+1)) * (1 + (2/(3*(x*x/5+1)))) + arctan(x/sqrt(5)))/PI)
    else if nu > 0 then begin
        if x < 0 then
        begin
            Result := 1 - dstudentt(-x, nu);
        end else begin
            Result := 1 - 0.5*Real(RegIncBeta(nu/(x*x+nu), nu/2, 0.5));
        end;
    end else Result := NaN;
end;

function rstudentt(df: Extended) : Extended;
begin
    if df = Infinity then
        Result := randg(0, 1)
    else if df < 1 then 
        Result := NaN
    else if df = 1 then
        Result := randg(0, 1) / system.sqrt(rchisq(1))
    else begin
        Result := randg(0, 1) / system.sqrt(rchisq(df) / df);
    end;
end;

function ffischerf(x, d1, d2 : Extended) : Extended;
begin
    Result := Real(sqrt((pow(d1 * x, d1) * pow(d2, d2)) / (pow(d1*x + d2, d1+d2)))/(x * Beta(d1/2, d2/2)));
end;

function dfischerf(x, d1, d2 : Extended) : Extended;
begin
    Result := Real(RegIncBeta((d1*x)/(d1*x+d2), d1/2, d2/2));
end;

function rfischerf(v, w: Extended) : Extended;
begin
    if (v < 1) or (w < 1) then
        Result := NaN
    else
        Result := rchisq(v) / v / (rchisq(w) / w);
end;

function fbeta(x, alpha, beta : Extended) : Extended;
begin
    if (alpha >= 1) or (beta >= 1) then
    begin
        if (abs(x-0.5) > 0.5) then
        begin
            Result := NaN;
        end else begin
            Result := Real(pow(x, alpha-1) * pow(1-x, beta-1) / ComplexNumbers.Beta(alpha, beta));
        end;
    end else begin
        if (abs(x-0.5) >= 0.5) then
        begin
            Result := NaN;
        end else begin
            Result := Real(pow(x, alpha-1) * pow(1-x, beta-1) / ComplexNumbers.Beta(alpha, beta));
        end;
    end;
end;

function dbeta(x, alpha, beta : Extended) : Extended;
begin
    //if (abs(x-0.5) >= 0.5) then
    if (system.abs(x-0.5) > 0.5) then
    //if (abs(x) > 1) then
    begin
        Result := NaN;
    end else begin
        Result := Real(RegIncBeta(x, alpha, beta));
    end;
end;

function rbeta(a, b : Extended) : Extended;
var
    x, y : Extended;
begin
    if (a = 1) and (b = 1) then
    begin
        Result := random();
    end else begin
        //x := rgengamma(0, 1, a);
        //y := rgengamma(0, 1, b);
        x := rgamma1(a, 1);
        y := rgamma1(b, 1);
        Result := x / (x+y);
    end;
end;

function farcsine(x : Extended) : Extended;
begin
    if (abs(x-0.5) >= 0.5) then
    begin
        Result := NaN;
    end else begin
        Result := 1/(PI * system.sqrt(x * (1-x)));
    end;
end; 

function darcsine(x : Extended) : Extended;
begin
    if (abs(x-0.5) > 0.5) then
    begin
        Result := NaN;
    end else begin
        Result := Math.arcsin(2*x-1)/PI + 0.5;
    end;
end;

function rarcsine() : Extended;
var
    x, y : Extended;
begin
    //x := rgengamma(0, 1, 0.5);
    //y := rgengamma(0, 1, 0.5);
    //Result := x / (x+y);
    Result := rbeta(0.5, 0.5);
end;

// ------------------------------------------------------------------------
// ====== NUMBER THEORY

function num_tau(n : Extended) : Extended;
var
    s, i : Extended;
begin
    s := 0;
    i := 1;
    while (i*i <= n) do
    begin
        if divides(n, i) then
        begin
            s := s + 1;
            if (i*i <> n) then s := s + 1;
        end; 
        i := i + 1;
    end;
    Result := s;
end;

function num_sigma(n : Extended) : Extended;
var
    s, i : Extended;
begin
    s := 0;
    i := 1;
    while (i*i <= n) do
    begin
        if divides(n, i) then
        begin
            s := s + i;
            if (i*i <> n) then s := s + fdiv(n, i);
        end; 
        i := i + 1;
    end;
    Result := s;
end;

function num_mobius(n : Extended) : Extended;
var
    i, p : Extended;
begin
    if (n = 1) then Result := 1
    else begin
        p := 0;
        i := 1;
        Result := 1;
        while (i*i < n) do
        begin
            if divides(n, i) and (isPrime(i)) then
            begin
                if divides(n, i*i) then
                begin
                    Result := 0;
                    break;
                end else p := p + 1;
            end;
            i := i + 1;
        end;
        if (Result = 1) then
        begin
            if (fmod(p, 2) = 0)
                then Result := 1
                else Result := -1;
        end;
    end
end;

function num_euler(n : Extended) : Extended;
var
    p : LongInt;
begin
    Result := n;
    p := 2;
    while (p * p <= n) do
    begin
        if (fmod(n,p) = 0) then
        begin
            while (fmod(n,p) = 0) do n := n / p;
            Result := Result * (1.0 - (1.0 / p));
        end;
        p := p + 1;
    end;
    if (n > 1) then Result := fround(Result - (1.0 - (1.0 / n)));
end;


function num_pi(n : Extended) : Extended;
var
    s, i : Extended;
begin
    s := 0;
    i := 1;
    while (i <= n) do
    begin
        if (isPrime(i)) then s := s + 1;
        i := i + 1;
    end;
    Result := s;
end;

function num_omega(n : Extended) : LongInt;
var
    s    : LongInt;
    x, y : Extended;
begin
    s := 0;
    x := 1;
    while (n > 1) do
    begin
        if (isPrime(n)) then
        begin
            if (n <> x) then
            begin
                s := s + 1;
            end;
            n := 1;
        end else begin
            y := 2;
            while not (divides(n, y)) do y := y + 1;
            if (x <> y) then
            begin
                s := s + 1;
                x := y;
            end;
            n := n / y;
        end;
    end;
    Result := s;
end;

function num_omega2(n : Extended) : LongInt;
var
    s    : LongInt;
    x, y : Extended;
begin
    s := 0;
    x := 1;
    while (n > 1) do
    begin
        if (isPrime(n)) then
        begin
            s := s + 1;
            n := 1;
        end else begin
            y := 2;
            while not (divides(n, y)) do y := y + 1;
            s := s + 1;
            n := n / y;
        end;
    end;
    Result := s;
end;

function num_liouville(n : Extended) : LongInt;
begin
    Result := minusOneTo(num_omega2(n));
end;

function num_isSquareFree(n : Extended) : Boolean;
var
    s    : LongInt;
    x, y : Extended;
begin
    s := 0;
    x := 1;
    Result := True;
    if (isPrime(n)) then
    begin
        Result := True;
    end else begin
        while (n > 1) do
        begin
            if (isPrime(n)) then
            begin
                if (n <> x) then
                begin
                    s := s + 1;
                end else begin
                     Result := False;
                     break;
                end;
                n := 1;
            end else begin
                y := 2;
                while not (divides(n, y)) do y := y + 1;
                if (x <> y) then
                begin
                    s := s + 1;
                    x := y;
                end else begin
                    Result := False;
                    break;
                end;
                n := n / y;
            end;
        end;
    end;
end;


function num_isCarmichael(n : LongInt) : Boolean;
var
    i     : LongInt;
begin
    if (n = 1) then
    begin
        Result := True;
    end else if (isPrime(n)) then
    begin
        Result := False;
    end else begin
        Result := True;
        for i := 2 to n-1 do
        begin
            if (gcd(n, i) = 1) then
            begin
                if (modularPower(i, n-1, n) <> 1) then
                begin
                    Result := False;
                    break;
                end;
            end;
        end;
    end;
end;


// junk to remove
function num_isCarmichael2(n : LongInt) : Boolean;
var
    i     : LongInt;
    prime : Boolean;// = False; 
begin
    if (n = 1) then
    begin
        Result := True;
    end else begin
        prime := True;
        Result := True;
        for i := 2 to n-1 do
        begin
            if (gcd(n, i) = 1) then
            begin
                prime := False;
                if (modularPower(i, n-1, n) <> 1) then
                begin
                    writeln(i);
                    Result := False;
                    break;
                end;
            end;
        end;
        writeln(prime);
        writeln(Result);
        Result := (not prime) and Result;
    end;
end;

//function modularPower(a, b, m : Extended) : Extended;
function modularPower(a, b, m : LongInt) : LongInt;
var
    r : LongInt;
begin
    if m = 1 then
    begin
        Result := 0;
    end else begin
        r := 1;
        a := a mod m;
        while (b > 0) do
        begin
            if (b mod 2 = 1) then begin
                r := (r * a) mod m;
            end;
            a := (a*a) mod m;
            b := b shr 1;
        end;
        Result := r;
    end;
end;

function modularInverseNaive(a, m : LongInt) : LongInt;
var
    i : LongInt;
begin
    Result := -1;
    for i := 1 to m-1 do
        if (((a mod m) * (i mod m)) mod m = 1) then
        begin
            Result := i;
            break;
        end;
end;

function modularInverse(a, m : LongInt) : LongInt;
var
    x, y, g : LongInt;
begin
    g := gcdExtended(a, m, x, y);
    if (g <> 1) then
        Result := modularInverseNaive(a, m)
    else
        Result := (x mod m + m) mod m;
end;

function getBaseValue(x : Char) : LongInt;
var
    val : LongInt;
begin
    val := Ord(x);
         if (val in [48..57])  then Result := val-48
    else if (val in [65..90])  then Result := val-65+10
    else if (val in [97..122]) then Result := val-97+10
    else Result := -1;
end;

function getBaseLetter(x : LongInt) : Char;
begin
         if (x in [0..9])   then Result := Chr(x+48)
    else if (x in [10..35]) then Result := Chr(x-10+65)
    else Result := Chr(0);
end;

//function HexToDec(x : String) : longint;
function convertBaseToDec(str : String; base : Integer) : LongInt;
var
	i, f, s	: longint;
begin
	s := 0;
	f := 0;
	if str[1] = '-' then
	begin
		for i := length(str) downto 2 do
        begin
            s := s - getBaseValue(str[i])*Int(pow(base, f));
            inc(f);
        end;
	end else begin
		for i := length(str) downto 1 do
        begin
            s := s + getBaseValue(str[i])*Int(pow(base, f));
            inc(f);
        end;
	end;
	Result := s;
end;

function convertDecToBase(a : LongInt; base : Integer) : String;
var
	temp, liczba : longint;
	hex 		 : String;
begin
	liczba := system.abs(a);
	hex := ''; 
    if (base = 1) then 
    begin
        for temp := 1 to liczba do hex := '1' + hex; 
    end else begin
	    repeat
	    	temp := liczba MOD base;
            hex := getBaseLetter(temp) + hex;
	    	liczba := liczba DIV base;
	    until liczba = 0;
    end;
	if a < 0 then hex := '-' + hex;
	Result := hex;
end;

// -------------------------------------------
// print numbers in strings

function toNumberString(input : Extended; decimal : String; thousand : String = '') : String;
var
    FS: TFormatSettings;
begin
    FS := DefaultFormatSettings;
    FS.DecimalSeparator := decimal[1];
    if (thousand = '')
        then Result := FormatFloat('0.################', input, FS)
        else begin
            FS.ThousandSeparator := thousand[1];
            Result := FormatFloat('#,##0.################', input, FS);
        end;
end;

function getNumberFromNumberString(input : String; decimal : String; thousand : String = '') : Extended;
var
    t  : Extended;
    fl : LongInt;
begin
    input := StringReplace(input, '''', '', [rfReplaceAll]);
    input := StringReplace(input, ' ', '', [rfReplaceAll]);
    if thousand <> '' then
        input := StringReplace(input, thousand[1], '', [rfReplaceAll]);
    if decimal[1] = ',' then
    begin
        input := StringReplace(input, ',', '.', [rfReplaceAll]);
    end;
    system.val(input, t, fl);
    if fl = 0 
        then Result := t
        else Result := NaN;
end;

end.

