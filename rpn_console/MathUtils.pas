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

function isPrime(x : Extended) : Boolean;
function isInteger(x : Extended) : Boolean;

function newton_int(n, k : Extended) : Extended;
function newton_real(n, k : Extended) : Extended;
function gcd(a, b : Extended) : Extended;
function lcm(a, b : Extended) : Extended;
function fib(n: Extended) : Extended;

function LogGamma(x : Extended) : Extended;
function vgamma(x : Extended) : Extended;
function vlowergamma(s, x : Extended) : Extended;
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
function ferlang(x, k, l : Extended) : Extended;
function derlang(x, k, l : Extended) : Extended;
function rerlang(k, l : Extended) : Extended;


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
    i := 1;
    while i <= abs(y) do begin
        s := s * x;
        i := i + 1;
    end;
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
    i := 1; 
    while i <= abs(x) do begin
        s := s * i;
        i := i + 1;
    end;
    Result := s;
end;

function ftrunc(x : Extended) : Extended;
begin
    //{$IFDEF cpu32} writeln( 'cpu32' ); 
    if x <= High(LongInt) 
        then Result := trunc(x)
        else Result := fdiv(x,1);
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
            i := 2;
            while i <= sqrt(x) do
            begin
                //writeln('i=', i);
                if (fmod(x, i) = 0) then 
                begin
                    //writeln('nope');
                    s := False;
                    break;
                end;
                i := i + 1;
            end;
            Result := s;
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

function fgamma2(x : Extended) : Extended;
var
	limit, n : Integer;
	s, s1    : Extended;
	epsilon  : Extended;
begin
	if (isInteger(x)) then Result := fact(x-1) 
    else if (x = 0.5) then
    begin
        Result := sqrt(PI);
    end else if (x > 0) and (fmod(x,1) = 0.5) then
    begin
        Result := (x-1)*fgamma2(x-1);
    end else begin
		if (x > 100) 
            then limit := trunc(100000*x)+1
		    else limit := trunc(1000000*x)+1;
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

function fgamma(x, alpha, beta : Extended) : Extended;
begin
    if alpha = 1 
        then Result := fexp(x, beta)
        else if isInteger(alpha) 
            then Result := ferlang(x, alpha, beta)
            else Result := (pow2(beta, alpha)*pow2(x, alpha-1)*exp(-beta*x))/vgamma(alpha);
end;

function dgamma(x, alpha, beta : Extended) : Extended;
begin
    //writeln('VL: ', vlowergamma(alpha, beta*x):2:5);
    //writeln('VG: ', vgamma(alpha):2:5);
    if x = 0.0 
        then Result := 0.0
        else if isInteger(alpha) 
            then Result := derlang(x, alpha, beta)
            else Result := vlowergamma(alpha, beta*x)/vgamma(alpha);
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
        c := 1.0/sqrt(9*d); 
        flag := False;
        repeat
            Z := randg(0, 1);
            V := pow((1+c*Z), 3); 
            if Z > -1.0/c then
            begin
                U := random();
                flag := ln(U) < (0.5*Z*Z + d - d*V + d*ln(V));
            end;
        until True;
        Result := d*V/b;
    end else begin
        c := rgamma1(a+1, b);
        c := c * pow2(random(), (1/a));
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
        c := 1.0/sqrt(9*d); 
        flag := True;
        while flag do
        begin
            Z := randg(0,1);
            if Z > -1/c then
            begin
                V := pow((1+c*Z), 3); 
                U := random;
                flag := ln(U) > (0.5*Z*Z + d - d*V + d*ln(V));
            end;
        end;
        Result := d*V/b;
    end else begin
        c := rgamma2(a+1, b);
        c := c * pow2(random(), (1/a));
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
    randomExp := a - rate * ln(unif);
  end;
end;

function rgengamma(a, b, c: Extended): Extended;
const
    RESOLUTION = 1000;
    T = 4.5;
    D = 1 + ln(T);
var
    unif: Extended;
    A2, B2, C2, Q, p, y: Extended;
    p1, p2, v, w, z: Extended;
    found: boolean;
begin
    A2 := 1 / sqrt(2 * c - 1);
    B2 := c - ln(4);
    Q := c + 1 / A2;
    C2 := 1 + c / exp(1);
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
                y := -ln((C2 - p) / c);
                if unif <= power(y, c - 1) then
                begin
                    Result := a + b * y;
                    found := True;
                end;
            end else begin
                y := power(p, 1 / c);
                if unif <= exp(-y) then
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
                v := A2 * ln(p1 / (1 - p1));
                y := c * exp(v);
                z := p1 * p1 * p2;
                w := B2 + Q * v - y;
            if (w + D - T * z >= 0) or (w >= ln(z)) then
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
        else Result := fgamma(x, v/2, 0.5);
    end;
end;

function dchisq(x : Extended; v : LongInt) : Extended;
begin
    //Result := vlowergamma(v/2.0, x/2.0)/vgamma(v/2.0);
    case v of
        2 : Result := dexp(x, 1/2);
        else Result := dgamma(x, v/2, 0.5);
    end;
end;

function ferlang(x, k, l : Extended) : Extended;
begin
    Result := (pow(l, k)*pow(x, k-1)*exp(-l*x))/fact(k-1); 
end;

function derlang(x, k, l : Extended) : Extended;
var
    i : LongInt;
    s : Extended;
begin
    s := 0;
    for i := 0 to trunc(k)-1 
        do s := s + (exp(-l*x)*pow(l*x, i)/fact(i));
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
        Result := -1.0/l * ln(prod);
    end;
end;


end.

