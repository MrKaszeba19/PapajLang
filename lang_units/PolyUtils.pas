unit PolyUtils;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    ComplexNumbers, ArrayUtils,
    UnitStack, UnitEntity;

function buildLinearPoly(x : ComplexType) : TEntities;

function polynomial_value(poly : TEntities; x : ComplexType) : ComplexType;
function polynomial_value(poly : TEntities; x : Entity) : Entity;
function polynomial_degree(poly : TEntities) : LongInt;
procedure polynomial_truncate(var poly : TEntities);
function polynomial_isofIntegerCoefs(poly : TEntities) : Boolean;
function polynomial_isofIntegerComplexCoefs(poly : TEntities) : Boolean;
function polynomial_isTrivial(poly : TEntities) : Boolean;
function polynomial_isPowerOfPoly(poly : TEntities) : LongInt;

function polynomial_sum(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_diff(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_mul(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_div(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_mod(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_mul(poly : TEntities; x : ComplexType) : TEntities;
function polynomial_mul(poly : TEntities; x : Entity) : TEntities;
function polynomial_pow(poly : TEntities; grade : LongInt) : TEntities;
function polynomial_hornerdiv(poly : TEntities; x : ComplexType) : TEntities;

//function polynomial_roots(poly : TEntities; distinct : Boolean = True; realonly : Boolean = False) : TEntities;
procedure polynomial_roots(var poly : TEntities; var res : TEntities; distinct : Boolean = True; realonly : Boolean = False);
//procedure polynomial_roots(poly : TEntities; var res : TEntities; distinct : Boolean = True; realonly : Boolean = False);

function polynomial_derivative(poly : TEntities; grade : LongInt = 1) : TEntities;
function polynomial_integral(poly : TEntities; grade : LongInt = 1) : TEntities;

implementation

uses MathUtils, Math;

const C_EPS15 = 0.000000000000001;

procedure genIntegerDivisors(var res : TEntities; x : ComplexType);
var
    i, j : LongInt;
    size : LongInt = 0;
begin
    if x = 1 then
    begin
        SetLength(res, 2);
        res[0] := buildNumber(1);
        res[1] := buildNumber(-1);
        //Result := res;
    end else begin
        SetLength(res, size);
        i := 1;
        j := Int(x);
        while (i*i <= j) do
        begin
            if (divides(j, i)) then 
            begin
                SetLength(res, size+2);
                res[size]   := buildNumber(i);
                res[size+1] := buildNumber(-i);
                size := size+2;
                if (i*i <> j) then begin 
                    SetLength(res, size+2);
                    res[size] := buildNumber(j div i);
                    res[size+1] := buildNumber(-(j div i));
                    size := size+2;
                end;
            end;
            i := i + 1;
        end;
    end;
end;

// polynomials

procedure writePoly(poly : TEntities);
var
    i : LongInt;
begin
    for i := 0 to Length(poly)-1 do
        write(AnsiString(poly[i].Num)+' ');
    writeln();
end;

function buildConstPoly(x : ComplexType) : TEntities;
var
    res  : TEntities;
begin
    SetLength(res, 1);
    res[0] := buildNumber(1);
    Result := res;
end;

function buildLinearPoly(x : ComplexType) : TEntities;
var
    res  : TEntities;
begin
    SetLength(res, 2);
    res[0] := buildNumber(x);
    res[1] := buildNumber(1);
    Result := res;
end;

function polynomial_value(poly : TEntities; x : ComplexType) : ComplexType;
var
    res : ComplexType;
    i   : LongInt;
begin
    case polynomial_degree(poly) of
        -1 : begin
            Result := 0;
        end;
        0 : begin
            Result := poly[0].Num; 
        end;
        else begin
            res := poly[Length(poly)-1].Num;
            i := Length(poly)-2;
            while (i >= 0) do
            begin
                res := res * x + poly[i].Num;
                i := i-1;
            end;
            Result := res;
        end;
    end;
end;

function polynomial_value(poly : TEntities; x : Entity) : Entity;
var
    res : Entity;
    i   : LongInt;
begin
    case polynomial_degree(poly) of
        -1 : begin
            Result := buildNumber(0);
        end;
        0 : begin
            Result := poly[0]; 
        end;
        else begin
            res := poly[Length(poly)-1];
            i := Length(poly)-2;
            while (i >= 0) do
            begin
                res := res * x + poly[i];
                i := i-1;
            end;
            Result := res;
        end;
    end;
end;

function polynomial_degree(poly : TEntities) : LongInt;
var
    i : LongInt;
begin
    i := Length(poly)-1;
    while (i >= 0) do
    begin
        if not (poly[i].Num = 0) then break;
        i := i-1;
    end;
    Result := i;
    //if Length(poly) = 0 
    //    then Result := 0
    //    else Result := i;
end;

procedure polynomial_truncate(var poly : TEntities);
var
    n : LongInt;
begin
    n := polynomial_degree(poly);
    SetLength(poly, n+1);
end;

function polynomial_isofIntegerCoefs(poly : TEntities) : Boolean;
var
    i   : LongInt;
begin
    Result := True;
    for i := 0 to polynomial_degree(poly) do
        if not (isInteger(poly[i].Num)) then 
        begin
            Result := False;
            break;
        end;
end;

function polynomial_isofRealCoefs(poly : TEntities) : Boolean;
var
    i   : LongInt;
begin
    Result := True;
    for i := 0 to polynomial_degree(poly) do
        if not (isReal(poly[i].Num)) then 
        begin
            Result := False;
            break;
        end;
end;

function polynomial_isofIntegerComplexCoefs(poly : TEntities) : Boolean;
var
    i   : LongInt;
begin
    Result := True;
    for i := 0 to polynomial_degree(poly) do
        if not (isIntegerComplex(poly[i].Num)) then 
        begin
            Result := False;
            break;
        end;
end;

// is like ax^n + b
function polynomial_isTrivial(poly : TEntities) : Boolean;
var
    i   : LongInt;
begin
    Result := True;
    for i := 1 to polynomial_degree(poly)-1 do
        if not (ComplexNumbers.isZero(poly[i].Num)) then 
        begin
            Result := False;
            break;
        end;
end;

// is like ax^2n + bx^n + c
function polynomial_isPowerOfPoly(poly : TEntities) : LongInt;
var
    i, n : LongInt;
begin
    n := polynomial_degree(poly) div 2;
    if (i = 0) then 
    begin
        Result := -1;
        Exit;
    end;
    Result := n;
    for i := 1 to 2*n do
    begin
        if ((i mod n = 0) and (poly[i].Num = 0)) 
        or ((i mod n <> 0) and (poly[i].Num <> 0))
        then begin
            Result := -1;
            Exit;
        end;
    end;
end;

function getMaxCoef(tab : TEntities) : ComplexType;
var
	i   : LongInt;
	sum : ComplexType;
begin
	sum := tab[0].Num;
    for i := 1 to Length(tab)-1 do
  	    if (Abs(tab[i].Num) > Abs(sum)) 
            then sum := tab[i].Num;
    Result := sum;
end;

// can be scaled to integers
function polynomial_scalableToIntegers(poly : TEntities) : LongInt;
var
    res    : LongInt;
    maxi   : ComplexType;
begin
    res := 0;
    maxi := getMaxCoef(poly);
    //while (not (polynomial_isofIntegerCoefs(poly))) 
    while (not (polynomial_isofIntegerComplexCoefs(poly))) 
    //or (maxi < Hi(RealType))
    and (Abs(maxi) <= 1000000000) 
    // todo: scale it to fit maximum possible root (vide cauchy bound in https://en.wikipedia.org/wiki/Geometrical_properties_of_polynomial_roots)
    do begin
        poly := polynomial_mul(poly, 10);
        res := res + 1;
        maxi := maxi * 10;
    end;
    Result := res;
end;

// check if w(x) = (x - b/n*a)^n
function isSingleMultiRootPoly(poly : TEntities) : Boolean;
var
    n : LongInt;
    p : TEntities;
begin
    //writeln('hello');
    //writePoly(poly);
    n := polynomial_degree(poly);
    p := buildLinearPoly(poly[n-1].Num/(n*poly[n].Num));
    Result := polynomial_degree(polynomial_diff(polynomial_pow(p, n), poly)) < 0;
    //writeln(Result);
end;

function polynomial_sum(poly1 : TEntities; poly2 : TEntities) : TEntities;
var
    res    : TEntities;
    flag   : ShortInt;
    mx, mn : LongInt;
    i      : LongInt;
begin
    if Length(poly1) > Length(poly2) then
    begin
        flag := 0;
        mx := Length(poly1);
        mn := Length(poly2);
    end else begin
        flag := 1;
        mx := Length(poly2);
        mn := Length(poly1);
    end; 
    SetLength(res, mx);
    for i := 0 to mn-1 do
        //res[i] := buildNumber(poly1[i].Num + poly2[i].Num);
        res[i] := poly1[i] + poly2[i];
    if flag = 0 then
    begin
        for i := mn to mx-1 do
            //res[i] := buildNumber(poly1[i].Num);
            res[i] := poly1[i];
    end else begin
        for i := mn to mx-1 do
            //res[i] := buildNumber(poly2[i].Num);
            res[i] := poly2[i];
    end;
    polynomial_truncate(res);
    Result := res;
end;

function polynomial_diff(poly1 : TEntities; poly2 : TEntities) : TEntities;
var
    res    : TEntities;
    flag   : ShortInt;
    mx, mn : LongInt;
    i      : LongInt;
begin
    if Length(poly1) > Length(poly2) then
    begin
        flag := 0;
        mx := Length(poly1);
        mn := Length(poly2);
    end else begin
        flag := 1;
        mx := Length(poly2);
        mn := Length(poly1);
    end; 
    SetLength(res, mx);
    for i := 0 to mn-1 do
        //res[i] := buildNumber(poly1[i].Num - poly2[i].Num);
        res[i] := poly1[i] - poly2[i];
    if flag = 0 then
    begin
        for i := mn to mx-1 do
            //res[i] := buildNumber(poly1[i].Num);
            res[i] := poly1[i];
    end else begin
        for i := mn to mx-1 do
            //res[i] := buildNumber(-poly2[i].Num);
            res[i] := poly2[i] * buildNumber(-1);
    end;
    polynomial_truncate(res);
    Result := res;
end;

function polynomial_mul(poly1 : TEntities; poly2 : TEntities) : TEntities;
// todo:
// make mul by constant
var
    res        : TEntities;
    m1, m2, mx : LongInt;
    i, j       : LongInt;
begin
    m1 := Length(poly1);
    m2 := Length(poly2);
    mx := m1 + m2 - 1;
    SetLength(res, mx);
    // todo: examine for other entities
    for i := 0 to mx-1 do
        res[i] := buildNumber(0);
    for i := 0 to m1-1 do
        for j := 0 to m2-1 do
            //res[i+j] := buildNumber(res[i+j].Num + poly1[i].Num * poly2[j].Num);
            res[i+j] := res[i+j] + poly1[i] * poly2[j];
    polynomial_truncate(res);
    Result := res;
end;

function divideLeads(poly1 : TEntities; poly2 : TEntities) : TEntities;
var
    n1, n2, n, i : LongInt; 
    res          : TEntities;
begin
    n1 := polynomial_degree(poly1);
    n2 := polynomial_degree(poly2);
    n := n1 - n2;
    SetLength(res, n + 1);
    for i := 0 to n-1 do
        res[i] := buildNumber(0);
    res[n] := buildNumber(poly1[n1].Num / poly2[n2].Num);
    Result := res;
end;


function polynomial_div(poly1 : TEntities; poly2 : TEntities) : TEntities;
var
    r, q, t : TEntities;
begin
    SetLength(q, 0);
    r := poly1;
    while (polynomial_degree(r) > 0) and (polynomial_degree(r) >= polynomial_degree(poly2)) do
    begin
        t := divideLeads(r, poly2);
        q := polynomial_sum(q, t);
        r := polynomial_diff(r, polynomial_mul(t, poly2));
    end;
    polynomial_truncate(q);
    Result := q;
end;

function polynomial_mod(poly1 : TEntities; poly2 : TEntities) : TEntities;
var
    r, q, t : TEntities;
begin
    SetLength(q, 0);
    r := poly1;
    while (polynomial_degree(r) > 0) and (polynomial_degree(r) >= polynomial_degree(poly2)) do
    begin
        t := divideLeads(r, poly2);
        q := polynomial_sum(q, t);
        r := polynomial_diff(r, polynomial_mul(t, poly2));
    end;
    polynomial_truncate(r);
    Result := r;
end;

function polynomial_mul(poly : TEntities; x : ComplexType) : TEntities;
var
    res  : TEntities;
    i, n : LongInt;
begin
    n := polynomial_degree(poly);
    SetLength(res, n+1);
    for i := 0 to n do
    begin
        res[i] := buildNumber(poly[i].Num * x);
    end;
    Result := res;
end;

function polynomial_mul(poly : TEntities; x : Entity) : TEntities;
var
    res  : TEntities;
    i, n : LongInt;
begin
    n := polynomial_degree(poly);
    SetLength(res, n+1);
    for i := 0 to n do
    begin
        res[i] := poly[i] * x;
    end;
    Result := res;
end;

function polynomial_pow(poly : TEntities; grade : LongInt) : TEntities;
var
    res : TEntities;
    i   : LongInt;
begin
    if (grade = 0) then Result := buildConstPoly(1)
    else if (grade = 1) then Result := poly
    else begin
        res := buildConstPoly(1);
        for i := 1 to grade do
            res := polynomial_mul(res, poly);
        Result := res;
    end;
end;

// finding roots

function getClosestToZero(a : TEntities) : LongInt;
var
    res, i : LongInt;
begin
    res := 0;
    for i := 1 to Length(a)-1 do
        if 
        //(Abs(a[i].Num) > 0) 
        (Abs(a[i].Num) > C_EPS15)
        and (Abs(a[i].Num) < Abs(a[res].Num)) then
            res := i;
    Result := res;
end;

function isComplexDivisible(x, y : ComplexType) : Boolean;
begin
    Result := x/y = Int(x/y);
end;

// todo: improve it
function getPolyGCD(poly : TEntities) : ComplexType;
var
    res : ComplexType;
    i   : LongInt;
begin
    res := poly[getClosestToZero(poly)].Num;
    //if res = 0 then 
    //begin
    //    Result := 1;
    //end else begin
        for i := 0 to Length(poly)-1 do
        begin
            //writeln(AnsiString(poly[i].Num)+' / '+AnsiString(res));
            //writeln(AnsiString(poly[i].Num/res));
            if (poly[i].Num = 0) 
            //or (Abs(poly[i].Num) < C_EPS15) 
            then continue;
            //writeln(AnsiString(poly[i].Num));
            if (Abs(poly[i].Num/res) > 1000000000) or (not isComplexDivisible(poly[i].Num, res))
            then begin
                res := 1;
                break;
            end;
        end;
        Result := res;
    //end;
end;

// Cauchy bound for roots
function getRootsBound(poly : TEntities) : ComplexType;
var
    res : ComplexType;
    i   : LongInt;
    n   : LongInt;
begin
    res := 0;
    n := polynomial_degree(poly);
    for i := 0 to n-1 do
        if (Abs(poly[i].Num/poly[n].Num) > Real(res)) 
            then res := Abs(poly[i].Num/poly[n].Num);
    Result := 1+res;
end;

function polynomial_hornerdiv(poly : TEntities; x : ComplexType) : TEntities;
var
    res  : TEntities;
    i, n : LongInt;
begin
    n := polynomial_degree(poly);
    SetLength(res, n);
    res[n-1] := buildNumber(poly[n].Num);
    for i := n-2 downto 0 do
        res[i] := buildNumber(res[i+1].Num*x+poly[i+1].Num);
    Result := res;
end;

// assume P(x) is divisible by (x-x0)
procedure polynomial_phornerdiv(var poly : TEntities; x : ComplexType);
var
    i, n : LongInt;
begin
    n := polynomial_degree(poly);
    for i := 0 to n-1 do
        poly[i] := buildNumber(poly[i+1].Num);
    SetLength(poly, n);
    for i := n-2 downto 0 do
        poly[i] := buildNumber(poly[i+1].Num*x+poly[i].Num);
end;


//procedure polynomial_tryFindNewtonRoots(var poly : TEntities; var res : TEntities; distinct : Boolean = True; realonly : Boolean = False);
//var
//    size  : LongInt;
//    bound : ComplexType;
//    limit : LongInt;
//begin
//    size := Length(res);
//    bound := getRootsBound(poly);
//    limit := polynomial_degree(poly);
//end;

procedure polynomial_roots(var poly : TEntities; var res : TEntities; distinct : Boolean = True; realonly : Boolean = False);
//procedure polynomial_roots(poly : TEntities; var res : TEntities; distinct : Boolean = True; realonly : Boolean = False);
var
    //x, y, d : TEntities;
    x     : TEntities;
    y, d  : TEntities;
    a, b  : TEntities;
    delta : ComplexType;
    p, q  : ComplexType;
    u, v  : ComplexType;
    e, f  : ComplexType;
    n, i  : LongInt; 
    j, k  : LongInt;
    l     : LongInt;
    size  : LongInt;
    flag  : Boolean;
begin
    size := Length(res);

    //writePoly(poly);
    //writeln('scale: ', polynomial_scalableToIntegers(poly));
    //writeln('gcd  : ', AnsiString(getPolyGCD(poly)));


    //if (polynomial_degree(poly) >= 1) and (getPolyGCD(poly) <> 1) then begin
    //    p := getPolyGCD(poly);
    //    poly := polynomial_mul(poly, Inv(p));
    //    //polynomial_roots(poly, res, distinct, realonly); 
    //    //Exit;
    //end;

    if (polynomial_degree(poly) >= 2) then begin
        n := polynomial_scalableToIntegers(poly);
        if (n > 0) then
        begin
            poly := polynomial_mul(poly, Pow(10, n));
            //writePoly(poly);
        end;
        if (getPolyGCD(poly) <> 1) then
        begin
            p := getPolyGCD(poly);
            poly := polynomial_mul(poly, Inv(p));
        end;
    end;

    if (polynomial_degree(poly) >= 2) and (poly[0].Num = 0) then
    begin
        repeat
            SetLength(res, size+1);
            res[size] := buildNumber(0);
            a := buildLinearPoly(0);
            poly := polynomial_div(poly, a);
        until (poly[0].Num <> 0);
        polynomial_roots(poly, res, distinct, realonly); 
        Exit;
    end else if (polynomial_degree(poly) >= 3) and (polynomial_isTrivial(poly)) then
    begin
        n := polynomial_degree(poly);
        SetLength(res, size+n);
        if (isReal(poly[0].Num)) and (isReal(poly[n].Num))
            then res[size] := buildNumber(RealRoot(-Real(poly[0].Num)/Real(poly[n].Num), n))
            else res[size] := buildNumber(Root(-poly[0].Num/poly[n].Num, n));
        e := ComplexNumPolar(1, 2*C_PI/n);
        for i := 2 to n do
        begin
            res[size+i-1] := buildNumber(res[size+i-2].Num * e);
        end;
        if (Length(res) > 0) and (distinct) then
            res := table_distinct(res); 
        if (Length(res) > 0) and (realonly) then
            res := table_filterReals(res); 
        Exit;
    end else if (polynomial_degree(poly) >= 4) and (polynomial_degree(poly) mod 2 = 0) and (polynomial_isPowerOfPoly(poly) > -1) then begin
        n := polynomial_isPowerOfPoly(poly);
        poly[0] := buildNumber(poly[0].Num);
        poly[1] := buildNumber(poly[n].Num);
        poly[2] := buildNumber(poly[2*n].Num);
        for i := 3 to 2*n do
            poly[i] := buildNumber(0);
        polynomial_truncate(poly);
        polynomial_roots(poly, res, False, False);
        //polynomial_roots(poly, res, distinct, realonly); 
        size := Length(res);
        SetLength(res, size+2*n-2);
        //res[size-2] := buildNumber(Sqrt(res[size-2].Num));
        //res[size-1] := buildNumber(Sqrt(res[size-1].Num));
        if (isReal(poly[0].Num)) and (isReal(poly[1].Num)) and (isReal(poly[2].Num)) then 
        begin
            res[size-2] := buildNumber(RealRoot(Real(res[size-2].Num), n));
            res[size-1] := buildNumber(RealRoot(Real(res[size-1].Num), n));
        end else begin 
            res[size-2] := buildNumber(Root(res[size-2].Num, n));
            res[size-1] := buildNumber(Root(res[size-1].Num, n));
        end;
        e := ComplexNumPolar(1, 2*C_PI/n);
        for i := 1 to n-1 do
        begin
            res[size+2*i-2] := buildNumber(res[size+2*i-4].Num * e);
            res[size+2*i-1] := buildNumber(res[size+2*i-3].Num * e);
        end;
        if (Length(res) > 0) and (distinct) then
            res := table_distinct(res); 
        if (Length(res) > 0) and (realonly) then
            res := table_filterReals(res); 
        Exit;
    // todo: maybe scale polynomials to integers, e.g. 2.5x^2 + x + 126 to 5x^2 + 2x + 252 
    end else if (polynomial_degree(poly) >= 3) 
    //and (polynomial_isofIntegerCoefs(poly)) 
    and (polynomial_isofIntegerComplexCoefs(poly)) 
    then begin
        //writeln('integer coefs');
        // todo: fix it
        n := polynomial_degree(poly);
        p := poly[0].Num; 
        q := poly[n].Num;
        // -------------------------------------------------------------------------
        //{*
        SetLength(x, 0);
        SetLength(y, 0);
        genIntegerDivisors(x, Abs(p));
        genIntegerDivisors(y, Abs(q));
        flag := True;
        e := 0;
        f := 1;
        k := 0;
        // todo: do something if nothing is found
        while flag and (k < n) do
        begin
            l := 0;
            while (l < 2) do
            begin
                i := 0;
                while flag and (i < Length(x)) do
                begin
                    j := 0;
                    while flag and (j < Length(y)) do
                    begin
                        if (y[j].Num = 0) then
                        begin
                            j := j+1;
                            continue;
                        end;
                        if l = 0 
                            then e := x[i].Num/y[j].Num * f
                            else e := RealRoot(Real(x[i].Num/y[j].Num), n) * f;
                        if (polynomial_value(poly, e) = 0) then
                        begin
                            flag := False;
                            break;
                        end;
                        j := j+1;
                    end;
                    i := i+1;
                end;
                l := l+1;
            end; 
            f := f * ComplexNumPolar(1, 2*C_PI/n);
            k := k+1;
        end;
        SetLength(x, 0);
        SetLength(y, 0);
        //*}
        // -------------------------------------------------------------------------
        // -----------------------------------------------------------------
        if not Flag then
        begin
            repeat
                polynomial_phornerdiv(poly, e);
                SetLength(res, size+1);
                res[size] := buildNumber(e);
                size := size+1;
            until (polynomial_value(poly, e) <> 0) or (polynomial_degree(poly) = 2);
            //polynomial_roots(poly, res, false, false);
            polynomial_roots(poly, res, distinct, realonly); 
            Flag := True;
            if (Length(res) > 0) and (distinct) then
                res := table_distinct(res); 
            if (Length(res) > 0) and (realonly) then
                res := table_filterReals(res); 
            Exit;
        end;
    end;

    //size := Length(res);
    //writeln('size ', size);

    case polynomial_degree(poly) of
        -1 : begin
            // all complex numbers
            SetLength(res, size+1);
            //res[size] := buildNumber(NaN);
            if realonly 
                then res[size] := buildString('real_numbers')
                else res[size] := buildString('complex_numbers');
            size := size+1;
        end;
        0 : begin
            // no solutions
            SetLength(res, size);
        end;
        1 : begin
            SetLength(res, size+1);
            res[0] := buildNumber(-poly[0].Num/poly[1].Num);
        end;
        2 : begin
            delta := Sqr(poly[1].Num) - 4 * poly[2].Num * poly[0].Num;
            SetLength(res, size+2);
            res[size] := buildNumber((-poly[1].Num-Sqrt(delta))/(2*poly[2].Num));
            res[size+1] := buildNumber((-poly[1].Num+Sqrt(delta))/(2*poly[2].Num));
        end;
        3 : begin
            if (isReal(poly[0].Num)) and (isReal(poly[1].Num))
            and (isReal(poly[2].Num)) and (isReal(poly[3].Num)) then
            begin
                if (poly[3].Num = 1) and (poly[2].Num = 0) then
                begin
                    p := poly[1].Num;
                    q := poly[0].Num;
                end else begin
                    p := poly[1].Num/poly[3].Num - Sqr(poly[2].Num)/(3*Sqr(poly[3].Num));
                    q := (2*Cub(poly[2].Num))/(27*Cub(poly[3].Num)) + poly[0].Num/poly[3].Num - (poly[2].Num*poly[1].Num)/(3*Sqr(poly[3].Num));
                end;
                delta := Sqr(q)/4 + Cub(p)/27;
                if 
                //(isReal(p)) and (isReal(q)) and 
                (Real(delta) >= 0)
                then begin
                    u := RealRoot(Real(Sqrt(delta)-0.5*q), 3);
                    v := RealRoot(Real(-Sqrt(delta)-0.5*q), 3);
                end else begin
                    u := Root(Sqrt(delta)-0.5*q, 3);
                    v := Root(-Sqrt(delta)-0.5*q, 3);
                end;
                e := ComplexNum(-0.5, -system.sqrt(3)/2);
                f := ComplexNum(-0.5, system.sqrt(3)/2);
                //e := ComplexNumPolar(1, 2*C_PI/3);
                //f := ComplexNumPolar(1, -2*C_PI/3);
                SetLength(res, size+3);
                res[size]   := buildNumber(u+v     -poly[2].Num/(3*poly[3].Num));
                res[size+1] := buildNumber(u*e+v*f -poly[2].Num/(3*poly[3].Num));
                res[size+2] := buildNumber(u*f+v*e -poly[2].Num/(3*poly[3].Num));
            end else begin
                e := Sqr(poly[2].Num) - 3*poly[3].Num*poly[1].Num; // delta_0
                f := 2*Cub(poly[2].Num) - 9*poly[3].Num*poly[2].Num*poly[1].Num + 27*Sqr(poly[3].Num)*poly[0].Num; // delta_1
                if (e = 0) and (f = 0) 
                then begin
                    u := -poly[2].Num/(3*poly[3].Num);
                    SetLength(res, size+3);
                    res[size]   := buildNumber(u);
                    res[size+1] := buildNumber(u);
                    res[size+2] := buildNumber(u);
                end else begin
                    delta := ComplexNumbers.Root((f + ComplexNumbers.Sqrt(Sqr(f) - 4*Cub(e)))/2, 3);
                    if delta = 0 then delta := ComplexNumbers.Root((f - ComplexNumbers.Sqrt(Sqr(f) - 4*Cub(e)))/2, 3);
                    u := -Inv(3*poly[3].Num)*(poly[2].Num + delta + e/delta);
                    SetLength(res, size+3);
                    // todo: try best to find real roots
                    res[size]   := buildNumber(u);
                    v := ComplexNum(-0.5, system.sqrt(3)/2);
                    delta := delta*v;
                    u := -Inv(3*poly[3].Num)*(poly[2].Num + delta + e/delta);
                    res[size+1] := buildNumber(u);
                    delta := delta*v;
                    u := -Inv(3*poly[3].Num)*(poly[2].Num + delta + e/delta);
                    res[size+2] := buildNumber(u);
                end;
            end;
        end;
        4 : begin 
            e := Sqr(poly[2].Num) - 3*poly[1].Num*poly[3].Num + 12 * poly[4].Num*poly[0].Num; // delta_0
            f := 2*Cub(poly[2].Num) - 9*poly[3].Num*poly[2].Num*poly[1].Num + 27*Sqr(poly[3].Num)*poly[0].Num + 27*poly[4].Num*Sqr(poly[1].Num) - 72*poly[4].Num*poly[2].Num*poly[0].Num; // delta_1
            // check if w(x) = (x - b/4a)^4
            // todo: maybe move it to general cases
            if (isSingleMultiRootPoly(poly)) then
            begin
                //writeln('xd');
                SetLength(res, size+4);
                u := -poly[3].Num/(4*poly[4].Num);
                res[size]   := buildNumber(u);
                res[size+1] := buildNumber(u);
                res[size+2] := buildNumber(u);
                res[size+3] := buildNumber(u);
            end else 
            if (-(Sqr(f) - 4*Cub(e))/27 = 0) and (e = 0) then
            begin
                a := polynomial_derivative(poly, 2);
                if polynomial_degree(polynomial_mod(poly, a)) = -1 then
                begin
                    b := polynomial_div(poly, a);
                    polynomial_roots(a, res, distinct, realonly); 
                    polynomial_roots(b, res, distinct, realonly); 
                end else begin
                    b := polynomial_div(poly, polynomial_mod(poly, a));
                    //writeln('ok');
                    SetLength(res, size+4);
                    u := -b[0].Num/b[1].Num;
                    res[size]   := buildNumber(u);
                    res[size+1] := buildNumber(u);
                    res[size+2] := buildNumber(u);
                    res[size+3] := buildNumber(-poly[3].Num/poly[4].Num - 3*u);
                end;
            end else begin
                //writeln('hhhh2');
                //if (Sqr(f) - 4*Cub(e)/27 <> 0) and (e = 0)
                delta := ComplexNumbers.Root((f + ComplexNumbers.Sqrt(Sqr(f) - 4*Cub(e)))/2, 3); // Q
                if delta = 0 then delta := ComplexNumbers.Root((f - ComplexNumbers.Sqrt(Sqr(f) - 4*Cub(e)))/2, 3);
                p := (8*poly[4].Num*poly[2].Num - 3*Sqr(poly[3].Num))/(8*Sqr(poly[4].Num));
                q := (Cub(poly[3].Num) - 4*poly[4].Num*poly[3].Num*poly[2].Num + 8*Sqr(poly[4].Num)*poly[1].Num)/(8*Cub(poly[4].Num));
                u := 0.5*Sqrt(-2*p/3 + Inv(3*poly[4].Num) * (delta + e/delta)); // S
                i := 0;
                while (u = 0) and (i < 3) do // change root of Q
                begin
                    delta := delta * ComplexNum(-0.5, system.sqrt(3)/2);
                    u := 0.5*Sqrt(-2*p/3 + Inv(3*poly[4].Num) * (delta + e/delta));
                    i := i + 1;
                end;
                SetLength(res, size+4);
                res[size]   := buildNumber(-poly[3].Num/(4*poly[4].Num) - u + 0.5*Sqrt(-4*Sqr(u) - 2*p + q/u));
                res[size+1] := buildNumber(-poly[3].Num/(4*poly[4].Num) - u - 0.5*Sqrt(-4*Sqr(u) - 2*p + q/u));
                res[size+2] := buildNumber(-poly[3].Num/(4*poly[4].Num) + u + 0.5*Sqrt(-4*Sqr(u) - 2*p - q/u));
                res[size+3] := buildNumber(-poly[3].Num/(4*poly[4].Num) + u - 0.5*Sqrt(-4*Sqr(u) - 2*p - q/u));
            end;
        end;
        else begin
            n := polynomial_degree(poly);
            v := getRootsBound(poly);
            a := polynomial_derivative(poly);
            flag := False;
            u := 0;
            for k := 0 to n-1 do
            begin
                for i in [0, 1, -1] do
                begin
                    if (i = 0) and (k <> 0) then continue;
                    u := i*v * ComplexNumPolar(1, k*2*C_PI/n);
                    f := u*1000;
                    j := 1;
                    while (not (polynomial_value(poly, u) = 0)) and (j < 1000) 
                    and (Abs(u-f) > C_EPS15) 
                    and (Abs(u) < 2*Abs(v))
                    do
                    begin
                        f := u;
                        u := u - polynomial_value(poly, u)/polynomial_value(a, u);
                        if (polynomial_value(poly, u) = 0) or ((Abs(u-f) < C_EPS15) 
                        //and (Abs(polynomial_value(poly, u)) < C_EPS15)
                        ) 
                        then
                        begin
                            flag := True;
                            break;
                        end;
                        j := j+1;
                    end;
                    if flag then break;
                end;
                if flag then break;
            end;
            if Flag then
            begin
                repeat
                    polynomial_phornerdiv(poly, u);
                    SetLength(res, size+1);
                    res[size] := buildNumber(u);
                    size := size+1;
                until (polynomial_value(poly, u) <> 0) or (polynomial_degree(poly) <= 2);
                polynomial_roots(poly, res, distinct, realonly);
            end else begin
                SetLength(res, size+1);
                res[size] := buildString('unknown_roots');
                size := size+1;
            end;
        end;
    end;
    if (Length(res) > 0) and (distinct) then
        res := table_distinct(res); 
    if (Length(res) > 0) and (realonly) then
        res := table_filterReals(res); 
end;

function polynomial_derivative(poly : TEntities; grade : LongInt = 1) : TEntities;
var
    res  : TEntities;
    i, n : LongInt;
begin
    if polynomial_degree(poly) <= 0 then
    begin
        SetLength(res, 0);
        Result := res;
    end else if grade < 0 then
    begin
        Result := polynomial_integral(poly, -grade);
    end else begin
        n := polynomial_degree(poly);
        SetLength(res, n+1);
        for i := n downto 0 do
            res[i] := poly[i];
        while (grade >= 1) do
        begin
            i := 1;
            while (i <= n) do
            begin
                res[i-1] := buildNumber(res[i].Num * ComplexNum(i, 0));
                i := i + 1;
            end;
            SetLength(res, n);
            grade := grade - 1;
        end;
        polynomial_truncate(res);
        Result := res;
    end;
end;

function polynomial_integral(poly : TEntities; grade : LongInt = 1) : TEntities;
var
    res  : TEntities;
    i, n : LongInt;
begin
    if polynomial_degree(poly) < 0 then
    begin
        SetLength(res, 1);
        res[0] := buildNumber(0);
        Result := res;
    end else if grade < 0 then
    begin
        Result := polynomial_derivative(poly, -grade);
    end else begin
        n := polynomial_degree(poly);
        SetLength(res, n+1);
        for i := n downto 0 do
            res[i] := poly[i];
        while (grade >= 1) do
        begin
            SetLength(res, n+2);
            i := n+1;
            while (i > 0) do
            begin
                res[i] := buildNumber(res[i-1].Num / ComplexNum(i, 0));
                i := i - 1;
            end;
            res[i] := buildNumber(0);
            grade := grade - 1;
        end;
        polynomial_truncate(res);
        Result := res;
    end;
end;

end.