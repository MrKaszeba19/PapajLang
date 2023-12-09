unit PolyUtils;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    ComplexNumbers, ArrayUtils,
    UnitStack, UnitEntity;

function polynomial_value(poly : TEntities; x : ComplexType) : ComplexType;
function polynomial_degree(poly : TEntities) : LongInt;
procedure polynomial_truncate(var poly : TEntities);
function polynomial_sum(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_diff(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_mul(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_div(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_mod(poly1 : TEntities; poly2 : TEntities) : TEntities;
function polynomial_roots(poly : TEntities; distinct : Boolean = True; realonly : Boolean = False) : TEntities;

implementation

uses MathUtils, Math;

// polynomials
// move it to separate file

function polynomial_value(poly : TEntities; x : ComplexType) : ComplexType;
var
    res : ComplexType;
    i   : LongInt;
begin
    res := poly[Length(poly)-1].Num;
    i := Length(poly)-2;
    while (i >= 0) do
    begin
        res := res * x + poly[i].Num;
        i := i-1;
    end;
    Result := res;
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

{*
procedure polynomial_sum(poly1 : TEntities; poly2 : TEntities; var res : TEntities);
var
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
        res[i].Num := poly1[i].Num + poly2[i].Num;
    if flag = 0 then
    begin
        for i := mn to mx-1 do
            res[i].Num := poly1[i].Num;
    end else begin
        for i := mn to mx-1 do
            res[i].Num := poly2[i].Num;
    end;
    writeln(Length(res));
end;

procedure polynomial_diff(poly1 : TEntities; poly2 : TEntities; var res : TEntities);
var
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
        res[i].Num := poly1[i].Num - poly2[i].Num;
    if flag = 0 then
    begin
        for i := mn to mx-1 do
            res[i].Num := poly1[i].Num;
    end else begin
        for i := mn to mx-1 do
            res[i].Num := -poly2[i].Num;
    end;
end;
*}

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
        res[i] := buildNumber(poly1[i].Num + poly2[i].Num);
    if flag = 0 then
    begin
        for i := mn to mx-1 do
            res[i] := buildNumber(poly1[i].Num);
    end else begin
        for i := mn to mx-1 do
            res[i] := buildNumber(poly2[i].Num);
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
        res[i] := buildNumber(poly1[i].Num - poly2[i].Num);
    if flag = 0 then
    begin
        for i := mn to mx-1 do
            res[i] := buildNumber(poly1[i].Num);
    end else begin
        for i := mn to mx-1 do
            res[i] := buildNumber(-poly2[i].Num);
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
    for i := 0 to mx-1 do
        res[i] := buildNumber(0);
    for i := 0 to m1-1 do
        for j := 0 to m2-1 do
            res[i+j] := buildNumber(res[i+j].Num + poly1[i].Num * poly2[j].Num);
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

function polynomial_roots(poly : TEntities; distinct : Boolean = True; realonly : Boolean = False) : TEntities;
var
    res     : TEntities;
    x, y, d : TEntities;
    delta   : ComplexType;
    p, q    : ComplexType;
    u, v    : ComplexType;
    e, f    : ComplexType;
begin
    case polynomial_degree(poly) of
        -1 : begin
            // all complex numbers
            SetLength(res, 1);
            res[0] := buildNumber(NaN);
        end;
        0 : begin
            // no solutions
            SetLength(res, 0);
        end;
        1 : begin
            // all complex numbers
            SetLength(res, 1);
            res[0] := buildNumber(-poly[0].Num/poly[1].Num);
        end;
        2 : begin
            delta := Sqr(poly[1].Num) - 4 * poly[2].Num * poly[0].Num;
            if (realonly) and (isReal(delta)) and (Real(delta) < 0) then
            begin
                SetLength(res, 0);
            end else if (delta = 0) and (distinct) then
            begin
                SetLength(res, 1);
                res[0] := buildNumber(-poly[1].Num/(2*poly[2].Num));
            end else begin
                SetLength(res, 2);
                res[0] := buildNumber((-poly[1].Num-Sqrt(delta))/(2*poly[2].Num));
                res[1] := buildNumber((-poly[1].Num+Sqrt(delta))/(2*poly[2].Num));
            end;
        end;
        //3 : begin
        //    // https://pl.wikipedia.org/wiki/R%C3%B3wnanie_sze%C5%9Bcienne
        //    // work it out later
        //    if (poly[3].Num = 1) and (poly[2].Num = 0) then
        //    begin
        //        p := poly[1].Num;
        //        q := poly[0].Num;
        //    end else begin
        //        p := poly[1].Num/poly[3].Num + Sqr(poly[2].Num)/(3*Sqr(poly[3].Num));
        //        q := (2*Cub(poly[2].Num))/(27*Cub(poly[3].Num)) + poly[0].Num/poly[3].Num - (poly[2].Num*poly[1].Num)/(3*Sqr(poly[3].Num));
        //    end;
        //    //writeln(ANsiString(p));
        //    //writeln(ANsiString(q));
        //    delta := Sqr(q)/4 + Cub(p)/27;
        //    u := Root(Sqrt(delta)-0.5*q, 3);
        //    v := Root(-Sqrt(delta)-0.5*q, 3);
        //    //e := ComplexNum(-0.5, -system.sqrt(3)/2);
        //    //f := ComplexNum(-0.5, system.sqrt(3)/2);
        //    //SetLength(res, 3);
        //    //res[0] := buildNumber(u+v     -poly[2].Num/(3*poly[3].Num));
        //    //res[1] := buildNumber(u*e+v*f -poly[2].Num/(3*poly[3].Num));
        //    //res[2] := buildNumber(u*f+v*e -poly[2].Num/(3*poly[3].Num));
        //    e := ComplexNum(-0.5, system.sqrt(3)/2);
        //    SetLength(res, 3);
        //    res[0] := buildNumber(u+v     -poly[2].Num/(3*poly[3].Num));
        //    res[1] := buildNumber(u*e+v/e -poly[2].Num/(3*poly[3].Num));
        //    res[2] := buildNumber(u/e+v*e -poly[2].Num/(3*poly[3].Num));
        //    if (distinct) then
        //        res := table_distinct(res);
        //end;
        else begin
            // work it out later
            SetLength(res, 0);
        end;
    end;
    Result := res;
end;

end.