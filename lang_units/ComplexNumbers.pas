unit ComplexNumbers;

interface

{$mode objfpc}{$H+}

uses RPNAbout;

const
    C_PI       = 3.1415926535897932384626433832795;       // pi
    C_HALFPI   = 1.57079632679489661923132169163975;      // pi/2
    C_QURTPI   = 0.7853981633974483096156608458198757;    // pi/4
    C_SQRTPI   = 1.7724538509055160272981674833411;       // sqrt(pi)
    C_2DSQPI   = 1.1283791670955125738961589031215;       // 2/sqrt(pi)
    C_LN2      = 0.693147180559945309417232121458176568;  // ln(2)
    C_LNSQPI   = 0.5723649429247000870717136756765;       // ln(sqrt(pi))
    C_MINV12   = -0.08333333333333333333333333333333333;  // -1/12
    C_SQPID6   = 1.644934066848226436472415166646025;     // Pi*Pi/6
    C_SQPID12  = 0.822467033424113218236207583323012594;  // Pi*Pi/12
    C_QUPI90   = 1.0823232337111381915160036965411679;    // pi^4 / 90
    C_7QUPI720 = 0.9470328294972459175765032344735219149; // 7pi^4 / 720
    C_APERY    = 1.20205690315959428539973816151145;      // Apery's constant
    C_EXP      = 2.718281828459045235360287471352662;     // Euler constant
    C_PHI      = 1.6180339887498948482045868343656;       // phi - Golden ratio
    C_EM       = 0.5772156649015328606065120900824;       // Euler-Mascheroni constant
    C_OMEGA    = 0.56714329040978387299996866221035554;   // Omega constant
    C_EXPTOXP1 = 41.193555674716123563188287684364331978; // e^(e+1)

type ComplexType = record
    Re : RealType;
    Im : RealType;
end;

function ComplexNum(a, b : RealType) : ComplexType;
function ComplexNumPolar(r, a : RealType) : ComplexType;

operator := (a : RealType) res : ComplexType;
operator := (a : Extended) res : ComplexType;
operator := (a : Real) res : ComplexType;
operator := (a : IntegerType) res : ComplexType;
operator := (a : LongInt) res : ComplexType;
operator := (a : Int64) res : ComplexType;
//operator := (a : Integer) res : ComplexType;
operator := (a : ShortInt) res : ComplexType;

operator := (a : ComplexType) res : String;
function toString(a : ComplexType) : String;
function toStringFormat(a : ComplexType) : String;
function toStringFormat(a : ComplexType; mask : String) : String;
procedure Val(x : String; var num : ComplexType; var posError : ShortInt);
operator Explicit(a : ComplexType) : String;

operator Explicit(a : RealType) res : ComplexType;
operator Explicit(a : Extended) res : ComplexType;
operator Explicit(a : Real) res : ComplexType;
operator Explicit(a : IntegerType) res : ComplexType;
operator Explicit(a : LongInt) res : ComplexType;
operator Explicit(a : Int64) res : ComplexType;
operator Explicit(a : ShortInt) res : ComplexType;

function Int(z : ComplexType) : IntegerType;
function Int(z : ComplexType) : Int64;
function Int(z : ComplexType) : LongInt;
function Real(z : ComplexType) : RealType;
function Real(z : ComplexType) : Extended;

operator = (a : ComplexType; b : ComplexType) : Boolean;
operator <> (a : ComplexType; b : ComplexType) : Boolean;
operator - (a : ComplexType) res : ComplexType;
operator + (a : ComplexType) res : ComplexType;

operator + (a : ComplexType; b : ComplexType) res : ComplexType;
operator - (a : ComplexType; b : ComplexType) res : ComplexType;
operator * (a : ComplexType; b : ComplexType) res : ComplexType;
operator / (a : ComplexType; b : ComplexType) res : ComplexType;

function Abs(a : ComplexType) : RealType;
function Arg(a : ComplexType) : RealType;
function Arg2(a : ComplexType) : RealType;
function Conj(a : ComplexType) : ComplexType;
function RePart(a : ComplexType) : RealType;
function ImPart(a : ComplexType) : RealType;

function Imag() : ComplexType;
function Imag(a : ComplexType) : ComplexType;
function EulerNum() : RealType;
function Pi() : RealType;
function EulerMascheroni() : RealType;
function GoldenNum() : RealType;

function isZero(z : ComplexType) : Boolean;
function isNatural(z : ComplexType) : Boolean;
function isInteger(z : ComplexType) : Boolean;
function isReal(z : ComplexType) : Boolean;
function isNotReal(z : ComplexType) : Boolean;
function isImaginary(z : ComplexType) : Boolean;
function isComplex(z : ComplexType) : Boolean;
function isIntegerComplex(z : ComplexType) : Boolean;

function Sqr(z : ComplexType) : ComplexType;
function Cub(z : ComplexType) : ComplexType;
function Pow4(z : ComplexType) : ComplexType;
function Pow5(z : ComplexType) : ComplexType;
function Inv(z : ComplexType) : ComplexType;
function Exp(z : ComplexType) : ComplexType;
function Ln(z : ComplexType) : ComplexType;
function Log(a,b : ComplexType) : ComplexType;
function IntPow(x : ComplexType; y : IntegerType) : ComplexType;
function Pow(a,b : ComplexType) : ComplexType;
function RealRoot(a,b : RealType) : ComplexType;
function Root(a,b : ComplexType) : ComplexType;
function Sqrt(a : ComplexType) : ComplexType;
function MinusOneTo(z : ComplexType) : ComplexType;
function ImagTo(z : ComplexType) : ComplexType;
function MinusImagTo(z : ComplexType) : ComplexType;

function Sin(z : ComplexType) : ComplexType;
function Cos(z : ComplexType) : ComplexType;
function Tan(z : ComplexType) : ComplexType;
function Cot(z : ComplexType) : ComplexType;
function Csc(z : ComplexType) : ComplexType;
function Sec(z : ComplexType) : ComplexType;
function ArcSin(z : ComplexType) : ComplexType;
function ArcCos(z : ComplexType) : ComplexType;
function ArcTan(z : ComplexType) : ComplexType;
function ArcCot(z : ComplexType) : ComplexType;
function ArcCsc(z : ComplexType) : ComplexType;
function ArcSec(z : ComplexType) : ComplexType;

function Sinh(z : ComplexType) : ComplexType;
function Cosh(z : ComplexType) : ComplexType;
function Tanh(z : ComplexType) : ComplexType;
function Coth(z : ComplexType) : ComplexType;
function Csch(z : ComplexType) : ComplexType;
function Sech(z : ComplexType) : ComplexType;
function ArSinh(z : ComplexType) : ComplexType;
function ArCosh(z : ComplexType) : ComplexType;
function ArTanh(z : ComplexType) : ComplexType;
function ArCoth(z : ComplexType) : ComplexType;
function ArCsch(z : ComplexType) : ComplexType;
function ArSech(z : ComplexType) : ComplexType;

function Sinc(z : ComplexType) : ComplexType;
function Tanc(z : ComplexType) : ComplexType;
function LogInt(z : ComplexType) : ComplexType; // li(z)
function LogInt2(z : ComplexType) : ComplexType; // Li(z) = li(z) - li(2)
function ExpIntC1(z : ComplexType) : ComplexType; // E_1(z) exponential integral
function ExpIntC(z : ComplexType; n : IntegerType) : ComplexType; // E_n(z)
function ExpInt(z : ComplexType) : ComplexType; // Ei(z)
function SinInt(z : ComplexType) : ComplexType; // Si(z)
function SinInt2(z : ComplexType) : ComplexType; // si(z)
function CosInt(z : ComplexType) : ComplexType; // Ci(z)
function CosInt2(z : ComplexType) : ComplexType; // Cin(z)

function FresnelC(z : ComplexType) : ComplexType; // Fresnel integral C(x)
function FresnelS(z : ComplexType) : ComplexType; // Fresnel integral S(x)

function Gamma(z : ComplexType) : ComplexType;
function GammaLn(z : ComplexType) : ComplexType;
function Erf(z : ComplexType) : ComplexType;
function Erfc(z : ComplexType) : ComplexType;
function Erfi(z : ComplexType) : ComplexType;
function LowerGamma(s, x : ComplexType) : ComplexType;
function UpperGamma(s, x : ComplexType) : ComplexType;
function LowerRegGamma(s, x : ComplexType) : ComplexType;
function Beta(x, y : ComplexType) : ComplexType;
function IncBeta(x, a, b : ComplexType) : ComplexType;
function RegIncBeta(x, a, b : ComplexType) : ComplexType;

function bernoulli_num(n : IntegerType) : ComplexType;
function Newton(n, k : ComplexType) : ComplexType;
function DirichletEta(z : ComplexType) : ComplexType;
function RiemannZeta(z : ComplexType) : ComplexType;

function LambertW0(z : ComplexType) : ComplexType; // W_0
function LambertWn1(z : ComplexType) : ComplexType; // W_-1
function LambertW(z : ComplexType; k : IntegerType = 0) : ComplexType; // W_k

function InfPowerTower(z : ComplexType) : ComplexType; // h(z) = z^z^z^...

function isInfinite(z : ComplexType) : Boolean; 
function isReInfinite(z : ComplexType) : Boolean; 
function isImInfinite(z : ComplexType) : Boolean; 
function isTotalInfinite(z : ComplexType) : Boolean;     // is like inf+inf*i 
function isFinite(z : ComplexType) : Boolean; 
function RePosInfinity(im : RealType = 0) : ComplexType; // inf + im*i
function ReNegInfinity(im : RealType = 0) : ComplexType; // -inf + im*i
function ImPosInfinity(re : RealType = 0) : ComplexType; // re + inf*i
function ImNegInfinity(re : RealType = 0) : ComplexType; // re - inf*i
function ComplexInfinity1() : ComplexType;               // inf + inf*i
function ComplexInfinity2() : ComplexType;               // -inf + inf*i
function ComplexInfinity3() : ComplexType;               // -inf - inf*i
function ComplexInfinity4() : ComplexType;               // inf - inf*i

function ComplexRound(z : ComplexType) : ComplexType;
function ComplexTrunc(z : ComplexType) : ComplexType;
function ComplexFloor(z : ComplexType) : ComplexType;
function ComplexCeil(z : ComplexType) : ComplexType;

implementation

uses Math, SysUtils;


// construction

function ComplexNum(a, b : RealType) : ComplexType;
begin
    Result.Re := a;
    Result.Im := b;
end;

function ComplexNumPolar(r, a : RealType) : ComplexType;
begin
    Result := system.abs(r) * (Cos(a) + Imag(Sin(a)));
end;

// assigments

operator := (a : RealType) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

operator := (a : Extended) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

operator := (a : Real) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

operator := (a : IntegerType) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

operator := (a : LongInt) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

operator := (a : Int64) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

//operator := (a : Integer) res : ComplexType;
//begin
//    res.Re := a;
//    res.Im := 0;
//end;

operator := (a : ShortInt) res : ComplexType;
begin
    res.Re := a;
    res.Im := 0;
end;

operator := (a : ComplexType) res : String;
begin
    if (a.Im = 0) then
    begin
        res := FloatToStr(a.Re);
    end else if (a.Im >0) then
    begin
        res := FloatToStr(a.Re)+'+'+FloatToStr(a.Im)+'i';
    end else begin
        res := FloatToStr(a.Re)+'-'+FloatToStr(-a.Im)+'i';
    end;
end;

function toString(a : ComplexType) : String;
begin
    if (a.Im = 0) then
    begin
        Result := FloatToStr(a.Re);
    end else if (a.Im > 0) then
    begin
        Result := FloatToStr(a.Re)+'+'+FloatToStr(a.Im)+'i';
    end else begin
        Result := FloatToStr(a.Re)+'-'+FloatToStr(-a.Im)+'i';
    end;
end;

function toStringFormat(a : ComplexType) : String;
begin
    if (a.Im = 0) then
    begin
        Result := FormatFloat('0.###############', a.Re);
    end else if (a.Im > 0) then
    begin
        //Result := FormatFloat('0.###############', a.Re)+'+'+FormatFloat('0.###############', a.Im)+'i';
        Result := FormatFloat('0.######', a.Re)+'+'+FormatFloat('0.######', a.Im)+'i';
    end else begin
        //Result := FormatFloat('0.###############', a.Re)+'-'+FormatFloat('0.###############', a.Re)+'i';
        Result := FormatFloat('0.######', a.Re)+'-'+FormatFloat('0.######', -a.Im)+'i';
    end;
end;

function toStringFormat(a : ComplexType; mask : String) : String;
begin
    if (a.Im = 0) then
    begin
        Result := FormatFloat(mask, a.Re);
    end else if (a.Im > 0) then
    begin
        //Result := FormatFloat('0.###############', a.Re)+'+'+FormatFloat('0.###############', a.Im)+'i';
        Result := FormatFloat(mask, a.Re)+'+'+FormatFloat(mask, a.Im)+'i';
    end else begin
        //Result := FormatFloat('0.###############', a.Re)+'-'+FormatFloat('0.###############', a.Re)+'i';
        Result := FormatFloat(mask, a.Re)+'-'+FormatFloat(mask, -a.Im)+'i';
    end;
end;

//procedure Val(x : String; var num : ComplexType; var posError : IntegerType);
procedure Val(x : String; var num : ComplexType; var posError : ShortInt);
//procedure Val(x : String; var num : ComplexType; var posError : LongInt);
var
    rea, ima : RealType;
    x1, x2   : String;
    i        : IntegerType;
begin
    system.val(x, rea, posError);
    if (posError = 0)
    then
    begin
        //writeln('reading a real number');
        num := rea;
    end else begin
        //writeln('reading a complex number');
        // read a complex number
        posError := 0;
        x := StringReplace(x, 'j', 'i', [rfReplaceAll]);
        x := StringReplace(x, ' ', '', [rfReplaceAll]);
        if (x.Length > 1)
        then
        begin
            // retrieve a complex number
            x1 := ''+x[1];
            i := 2;
            while (i <= Length(x)) do
            begin
                if (x[i] in ['+', '-', 'i', 'j']) then break;
                x1 := x1 + x[i];
                i := i+1;
            end;
            if (x[i] in ['i', 'j']) then
            begin
                if (Length(x1) = 0) 
                    then num := ComplexNum(0, 1)
                else if (x1 = '-') 
                    then num := ComplexNum(0, -1) 
                else if (Length(x1) = 1) and (x1[1] in ['0'..'9']) 
                        then num := ComplexNum(0, Ord(x1[1])-48)
                else if (x1 = '+') 
                    then num := ComplexNum(0, 1)
                else if (x1 = '.') or (x1 = '-.') or (x1 = '+.')
                    then posError := 1
                else begin
                    posError := 1;
                    system.val(x1, rea, posError);   
                    if (posError = 0) then num := ComplexNum(0, rea);
                end;
                if (RightStr(x, Length(x)-i) <> '') then posError := 1;
                exit; 
            end;
            posError := 1;
            system.val(x1, rea, posError);
            if (posError = 0) then
            begin
                //writeln('read real part ', rea);
                x2 := ''+x[i];
                while (i < Length(x)) do
                begin
                    i := i+1;
                    if (x[i] in ['i', 'j']) then break;
                    x2 := x2 + x[i];
                end;
                if (x2 = '-') 
                    then num := ComplexNum(rea, -1) 
                else if (x2 = '+') 
                    then num := ComplexNum(rea, 1)
                else if (x2 = '-.') or (x2 = '+.')
                    then posError := 1
                else begin
                    posError := 1;
                    system.val(x2, ima, posError);   
                    if (posError = 0) then num := ComplexNum(rea, ima);
                end;
                if (RightStr(x, Length(x)-i) <> '') then posError := 1;
                exit; 
            end;
        end else begin
            posError := 1;
        end;
    end;
end;

operator Explicit(a : ComplexType) : String;
begin
    if (a.Im = 0) then
    begin
        Result := FloatToStr(a.Re);
    end else if (a.Im > 0) then
    begin
        Result := FloatToStr(a.Re)+'+'+FloatToStr(a.Im)+'i';
    end else begin
        Result := FloatToStr(a.Re)+'-'+FloatToStr(-a.Im)+'i';
    end;
end;

operator Explicit(a : RealType) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

operator Explicit(a : Extended) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

operator Explicit(a : Real) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

operator Explicit(a : IntegerType) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

operator Explicit(a : LongInt) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

operator Explicit(a : Int64) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

//operator Explicit(a : Integer) res : ComplexType;
//begin
//    res := ComplexNum(a, 0);
//end;

operator Explicit(a : ShortInt) res : ComplexType;
begin
    res := ComplexNum(a, 0);
end;

function Int(z : ComplexType) : IntegerType;
begin
    Result := system.trunc(z.Re);
end;

function Int(z : ComplexType) : Int64;
begin
    Result := system.trunc(z.Re);
end;

function Int(z : ComplexType) : LongInt;
begin
    Result := system.trunc(z.Re);
end;

function Real(z : ComplexType) : RealType;
begin
    Result := z.Re;
end;

function Real(z : ComplexType) : Extended;
begin
    Result := z.Re;
end;

// logical

operator = (a : ComplexType; b : ComplexType) : Boolean;
begin
    Result := (a.Re = b.Re) and (a.Im = b.Im)
end;

operator <> (a : ComplexType; b : ComplexType) : Boolean;
begin
    Result := not (a = b);
end;

// arithmetic

operator - (a : ComplexType) res : ComplexType;
begin
    res.Re := -a.Re;
    res.Im := -a.Im;
end;

operator + (a : ComplexType) res : ComplexType;
begin
    res.Re := a.Re;
    res.Im := a.Im;
end;

operator + (a : ComplexType; b : ComplexType) res : ComplexType;
begin
    res.Re := a.Re + b.Re;
    res.Im := a.Im + b.Im;
end;

operator - (a : ComplexType; b : ComplexType) res : ComplexType;
begin
    res.Re := a.Re - b.Re;
    res.Im := a.Im - b.Im;
end;

operator * (a : ComplexType; b : ComplexType) res : ComplexType;
begin
    if (a.Im = 0) and (b.Im = 0) then begin
        res.Re := a.Re * b.Re;
        res.Im := 0;
    end else begin
        res.Re := a.Re * b.Re - a.Im * b.Im;
        res.Im := a.Im * b.Re + a.Re * b.Im;
    end;
end;

operator / (a : ComplexType; b : ComplexType) res : ComplexType;
begin
    if (a.Im = 0) and (b.Im = 0) then begin
        res.Re := a.Re / b.Re;
        res.Im := 0;
    end else begin
        res.Re := (a.Re * b.Re + a.Im * b.Im) / (b.Re * b.Re + b.Im * b.Im);
        res.Im := (a.Im * b.Re - a.Re * b.Im) / (b.Re * b.Re + b.Im * b.Im);
    end;
end;

// basic functions

function Abs(a : ComplexType) : RealType;
begin
    Result := system.sqrt(a.Re * a.Re + a.Im * a.Im);
end;

function Arg(a : ComplexType) : RealType;
begin
    if (Abs(a) = 0) then
    begin
        Result := NaN;
    end else begin
        if (a.Im >= 0) then
        begin
            Result := Math.arccos(a.Re / Abs(a));
        end else begin
            Result := -Math.arccos(a.Re / Abs(a));
        end;
        //Result := system.arctan(a.Im/a.Re);
    end;
end;

function Arg2(a : ComplexType) : RealType;
begin
    if (Abs(a) = 0) then
    begin
        Result := NaN;
    end else begin
        if (a.Im >= 0) then
        begin
            Result := Math.arccos(a.Re / Abs(a));
        end else begin
            Result := 2*C_PI-Math.arccos(a.Re / Abs(a));
        end;
    end;
end;

function Conj(a : ComplexType) : ComplexType;
begin
    Result := ComplexNum(a.Re, -a.Im);
end;

function RePart(a : ComplexType) : RealType;
begin
    Result := a.Re;
end;

function ImPart(a : ComplexType) : RealType;
begin
    Result := a.Im;
end;

// constants

function Imag() : ComplexType;
begin
    Result := ComplexNum(0,1);
end;

function Imag(a : ComplexType) : ComplexType;
begin
    Result := ComplexNum(-a.Im,a.Re);
end;

function EulerNum() : RealType;
begin
    Result := Exp(1).Re;
end;

function Pi() : RealType;
begin
    //Result := 3.1415926535897932384626433832795;
    //Result := Arg(-1);
    //Result := system.Pi;
    Result := C_PI;
end;

function GoldenNum() : RealType;
begin
    //Result := 1.6180339887498948482045868343656;
    Result := C_PHI;
end;

function EulerMascheroni() : RealType;
begin
    //Result := 0.5772156649015328606065120900824;
    Result := C_EM;
end;

function Sqr(z : ComplexType) : ComplexType;
begin
    Result := z*z;
end;

function Cub(z : ComplexType) : ComplexType;
begin
    Result := z*z*z;
end;

function Pow4(z : ComplexType) : ComplexType;
begin
    Result := z*z*z*z;
end;

function Pow5(z : ComplexType) : ComplexType;
begin
    Result := z*z*z*z*z;
end;

function Inv(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := 1.0/z.Re
        else Result := ComplexNum((z.Re)/(z.Re * z.Re + z.Im * z.Im), (-z.Im)/(z.Re * z.Re + z.Im * z.Im));
end;

function isZero(z : ComplexType) : Boolean;
begin
    Result := (z.Re = 0) and (z.Im = 0);
end;

function isNatural(z : ComplexType) : Boolean;
begin
    Result := (isInteger(z)) and (z.Re >= 0);
end;

function isInteger(z : ComplexType) : Boolean;
begin
    Result := (z.Im = 0) and (z.Re = Int(z.Re));
end;


function isReal(z : ComplexType) : Boolean;
begin
    Result := (z.Im = 0);
end;

function isNotReal(z : ComplexType) : Boolean;
begin
    Result := (z.Im <> 0);
end;

function isImaginary(z : ComplexType) : Boolean;
begin
    Result := (z.Re = 0) and (z.Im <> 0);
end;

function isComplex(z : ComplexType) : Boolean;
begin
    Result := True;
end;

function isIntegerComplex(z : ComplexType) : Boolean;
begin
    Result := (z.Re = Int(z.Re)) and (z.Im = Int(z.Im));
end;


// more functions

function Exp(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := system.exp(z.Re)
        else Result := system.exp(z.Re)*(system.cos(z.Im) + Imag(system.sin(z.Im)));
end;

function Ln(z : ComplexType) : ComplexType;
begin
    if (z = 0) 
        then Result := ReNegInfinity()
    else if (z.Im = 0) and (z.Re > 0)
        then Result := system.ln(z.Re)
        else Result := system.ln(Abs(z)) + Imag(Arg(z));
end;

function Log(a,b : ComplexType) : ComplexType;
begin
    Result := Ln(b)/Ln(a);
end;

function IntPow(x : ComplexType; y : IntegerType) : ComplexType;
var
    s : ComplexType;
    d : IntegerType;
begin
    if isZero(x) then
    begin
        if (y <= 0) then s := NaN else s := 0;
    end else begin
        d := system.abs(y);
        s := 1;
        if (d > 0) then
        begin
            if (d mod 3 = 0)
                then s := Cub(intPow(x, d div 3))
                else if (d mod 2 = 0)
                         then s := Sqr(IntPow(x, d div 2))
                         else s := x * IntPow(x, d-1);
            if (y < 0) then s := Inv(s);
        end;
    end;
    Result := s;
end;

function unitcirclepowTo(a,b : ComplexType) : ComplexType;
begin
    if (a = 1)
    then Result := 1
    else if (a = -1)
    then Result := MinusOneTo(b)
    else if (a = Imag)
    then Result := ImagTo(b)
    else if (a = -Imag)
    then Result := MinusImagTo(b)
    else Result := Exp(b * Ln(a));
end;

function Pow(a,b : ComplexType) : ComplexType;
begin
    if (a = 0) then begin
        if (b.Re <= 0) 
            then Result := NaN
            else Result := 0;
    end else if (isInteger(b))
    then begin
        if (a*a*a*a = 1)
            then Result := unitcirclepowTo(a,b)
            else Result := IntPow(a, trunc(b.Re));
    end else begin
        if (a*a*a*a = 1)
            then Result := unitcirclepowTo(a,b)
            else Result := Exp(b * Ln(a));
    end;
end;

function Sqrt(a : ComplexType) : ComplexType;
begin
    if (a.Im = 0) and (a.Re >= 0) then
    begin
        Result := system.sqrt(a.Re);
    end else if (a.Im = 0) and (a.Re < 0) then
    begin
        Result := system.sqrt(-a.Re)*Imag;
    end else if (a.Im > 0) then begin
        Result := ComplexNum(system.sqrt((a.Re + Abs(a))/2), system.sqrt((-a.Re + Abs(a))/2));
    end else begin
        Result := ComplexNum(system.sqrt((a.Re + Abs(a))/2), -system.sqrt((-a.Re + Abs(a))/2));
    end;
end;

function RealRoot(a,b : RealType) : ComplexType;
begin
    if (b = 1) then
    begin
        Result := a;
    end else if (b = 2) then begin
        Result := Sqrt(a);
    end else if (b < 0) then begin
        Result := Inv(RealRoot(a,b));
    end else if (a < 0) and (isInteger(b)) and (trunc(b) mod 2 = 1)
        then Result := -Pow(system.abs(a),Inv(b))
        else begin
            Result := Pow(a,Inv(b));
        end;
end;

function Root(a,b : ComplexType) : ComplexType;
begin
    if (b = 1) then
    begin
        Result := a;
    end else if (b = 2) then begin
        Result := Sqrt(a);
    end else if (isReal(b)) then begin
        if (b.Re < 0) then begin
            Result := Inv(Root(a,-b.Re));
        //end else if isInteger(b) then
        //begin
        //    writeln('hello');
        //    Result := ComplexNumPolar(RealRoot(Abs(a), b.Re).Re, Arg(a)/b.Re);
        //    //Result := Pow(a,Inv(b.Re));
        end else begin
            Result := Pow(a,Inv(b.Re));
        end;
    end else begin
        Result := Pow(a,Inv(b));
    end;
end;

function MinusOneTo(z : ComplexType) : ComplexType;
begin
    Result := ImagTo(2*z);
end;

function ImagTo(z : ComplexType) : ComplexType;
var
    a : Integer;
begin
    if (isInteger(z))
        then begin
            a := trunc(z.Re);
            a := ((a mod 4) + 4) mod 4;
            case a of
                0 : Result := 1;
                1 : Result := Imag;
                2 : Result := -1;
                3 : Result := -Imag;
            end;
        end else Result := Exp(z * Imag * Pi * 0.5);
end;

function MinusImagTo(z : ComplexType) : ComplexType;
var
    a : Integer;
begin
    if (isInteger(z))
        then begin
            a := trunc(z.Re);
            a := ((a mod 4) + 4) mod 4;
            case a of
                0 : Result := 1;
                1 : Result := -Imag;
                2 : Result := -1;
                3 : Result := Imag;
            end;
        end else Result := Exp(- z * Imag * Pi * 0.5);
end;

function Sin(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then if (isInteger(z/Pi))
                 then Result := 0
                 else Result := system.sin(z.Re)
        else Result := (Exp(Imag(z)) - Exp(Imag(-z))) / Imag(2);
end;

function Cos(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then if (isInteger(z/Pi-0.5))
                 then Result := 0
                 else Result := system.cos(z.Re)
        else Result := (Exp(Imag(z)) + Exp(Imag(-z))) / 2.0;
end;

function Tan(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then if (isInteger(z/Pi))
                 then Result := 0
                 else Result := system.sin(z.Re)/system.cos(z.Re)
        else Result := -Imag * (Exp(Imag(z)) - Exp(Imag(-z))) / (Exp(Imag(z)) + Exp(Imag(-z)));
end;

function Cot(z : ComplexType) : ComplexType;
begin
    Result := Inv(Tan(z));
end;

function Csc(z : ComplexType) : ComplexType;
begin
    Result := Inv(Sin(z));
end;

function Sec(z : ComplexType) : ComplexType;
begin
    Result := Inv(Cos(z));
end;

function ArcSin(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0) and (Abs(z) <= 1)
        then Result := Math.arcsin(z.Re)
        else Result := -Imag * Ln(Imag(z) + Sqrt(Abs(1-z*z)) * Exp((Imag*Arg(1-z*z))/2));
end;

function ArcCos(z : ComplexType) : ComplexType;
begin
    Result := C_HALFPI - ArcSin(z);
end;

function ArcTan(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := system.arctan(z.Re)
        else Result := Inv(Imag(2))*Ln((Imag-z)/(Imag+z));
end;

function ArcCot(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := C_HALFPI - system.arctan(z.Re)
        else Result := Inv(Imag(2))*Ln((z+Imag)/(z-Imag));
end;

function ArcCsc(z : ComplexType) : ComplexType;
begin
    Result := ArcSin(Inv(z));
end;

function ArcSec(z : ComplexType) : ComplexType;
begin
    Result := ArcCos(Inv(z));
end;

// hyperbolic functions

function Sinh(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := Math.sinh(z.Re)
        else Result := (Exp(z) - Exp(-z)) / 2.0;
end;

function Cosh(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := Math.cosh(z.Re)
        else Result := (Exp(z) + Exp(-z)) / 2.0;
end;

function Tanh(z : ComplexType) : ComplexType;
begin
    Result := (Exp(2*z) - 1) / (Exp(2*z) + 1);
end;

function Coth(z : ComplexType) : ComplexType;
begin
    Result := (Exp(2*z) + 1) / (Exp(2*z) - 1);
end;

function Csch(z : ComplexType) : ComplexType;
begin
    Result := Inv(Sinh(z));
end;

function Sech(z : ComplexType) : ComplexType;
begin
    Result := Inv(Cosh(z));
end;

function ArSinh(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0)
        then Result := Math.arsinh(z.Re)
        else Result := Ln(z + Sqrt(z*z + 1));
end;

function ArCosh(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0) and (z.Re >= 1)
        then Result := Math.arsinh(z.Re)
        else Result := Ln(z + Sqrt(z*z - 1));
end;

function ArTanh(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0) and (Abs(z) < 1)
        then Result := Math.artanh(z.Re)
        else Result := Ln((1+z)/(1-z)) / 2.0;
end;

function ArCoth(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0) and (Abs(z) > 1)
        then Result := system.ln((1+z.Re)/(1-z.Re)) / 2.0
        else Result := Ln((1+z)/(1-z)) / 2.0;
end;

function ArCsch(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0) and (z.Re > 0) and (z.Re <= 1)
        then Result := system.ln(1.0/z.Re + system.sqrt(1.0/(z.Re*z.Re) + 1))
        else Result := Ln(Inv(z) + Sqrt(Inv(z*z) + 1));
end;

function ArSech(z : ComplexType) : ComplexType;
begin
    if (z.Im = 0) and (z.Re <> 0)
        then Result := system.ln(1.0/z.Re + system.sqrt(1.0/(z.Re*z.Re) - 1))
        else Result := Ln(Inv(z) + Sqrt(Inv(z*z) - 1));
end;

function Sinc(z : ComplexType) : ComplexType;
begin
    if (z = 0)
    then Result := 1
    else Result := Sin(z)/z;
end;

function Tanc(z : ComplexType) : ComplexType;
begin
    if (z = 0)
    then Result := 1
    else Result := Tan(z)/z;
end;

// gamma function

function fmod(x, y : RealType) : RealType;
begin
    Result := x - y * Int(x/y);
end;

function fdiv(x, y : RealType) : RealType;
begin
    Result := Int(x/y);
end;

function fact(x : RealType) : RealType;
var
    s : RealType;
    i : IntegerType;
begin
    s := 1;
    i := 1;
    while i <= abs(x) do begin
        s := s * i;
        i := i + 1;
    end;
    Result := s;
end;

function factln(x : RealType) : RealType;
var
    s : RealType;
    i : IntegerType;
begin
    s := 0;
    i := 1;
    while i <= abs(x) do begin
        s := s + system.ln(i);
        i := i + 1;
    end;
    Result := s;
end;

function powbyfact(z : ComplexType; n : IntegerType) : ComplexType; // z^n/n!
var
    s : ComplexType;
    i : IntegerType;
begin
    s := 1;
    i := 1;
    while i <= abs(n) do begin
        s := s * z / i;
        i := i + 1;
    end;
    Result := s;
end;

function LogInt(z : ComplexType) : ComplexType; // li(z)
//var
//    sum : ComplexType;
//    n   : IntegerType = 50;
begin
    if (z = 0) then Result := 0
    else if (z = 1) then Result := -Infinity
    else if (isReal(z)) and (z.Re > 1) then
    begin
        Result := ExpInt(Ln(z));
    end else begin
        // TODO check this piece of code, probably does not work
        Result := ExpInt(Ln(z));
        //sum := (2*n+1)-Ln(z)-Sqr(n+1)/((2*n+3)-Ln(z));
        //while (n > 0) do
        //begin
        //    sum := (2*n-1)-Ln(z)-Sqr(n)/sum;
        //    n := n-1;
        //end;
        //Result := -z/sum;
    end;
end;

function LogInt2(z : ComplexType) : ComplexType; // Li(z) = li(z) - li(2)
begin
    Result := LogInt(z) - LogInt(2); // make the latter one as a constant maybe later
end;

function ExpIntC1(z : ComplexType) : ComplexType; // E_1(z) exponential integral
var
    sum : ComplexType;
    n   : IntegerType = 100;
begin
    if (z = 0) then Result := Infinity
    else if (isReal(z)) and (z.Re < 0) then
        Result := -ExpInt(-z.Re) - Imag(Pi)
    else begin
        sum := z+(n+1)/(n+2);
        while (n > 0) do
        begin
            sum := z+n/(1+n/sum);
            n := n-1;
        end;
        Result := Exp(-z)/sum;
    end;
end;

function ExpIntC(z : ComplexType; n : IntegerType) : ComplexType;
var
    sum : ComplexType;
    k   : IntegerType = 50;
begin
    if (n = 0)
    then Result := Exp(-z)/z
    else if (n = 1)
    then Result := ExpIntC1(z)
    else if (z = 0)
    then Result := Inv(n-1)
    else begin
        sum := z+(n+k)/(k+2);
        while (k > 0) do
        begin
            sum := z+(n+k-1)/(1+k/sum);
            k := k-1;
        end;
        Result := Exp(-z)/sum;
    end;
end;

function ExpInt(z : ComplexType) : ComplexType;
var
    limit, n : IntegerType;
    sum      : ComplexType;
begin
    if (isReal(z) and (z.Re <= 0))
    then Result := -ExpIntC1(-z)
    else begin
        sum := 0;
        if (z.Re >= 1000)
        then limit := trunc(Abs(z)+1)
        else if (z.Re >= 100)
        then limit := 2 * trunc(Abs(z)+1)
        else if (z.Re >= 10)
        then limit := 5 * trunc(Abs(z)+1)
        else limit := 10 * trunc(Abs(z)+1);
        for n := 1 to limit do
            sum := sum + Inv(n) * powbyfact(z, n);
        Result := C_EM + Ln(z) + sum;
    end;
end;


function SinInt(z : ComplexType) : ComplexType; // Si(x)
var
    limit, n : IntegerType;
    sum      : ComplexType;
begin
    if (isZero(z)) then Result := 0
    else if (z.Re < 0)
    then Result := -SinInt(-z)
    else if (isReal(z))
    then Result := C_HALFPI + ExpIntC1(Imag(z)).Im
    else begin
        //writeln('hello');
        sum := 0;
        limit := 100;
        for n := 0 to limit do
            sum := sum + MinusOneTo(n) * powbyfact(z, 2*n+1) * Inv(2*n+1);
        Result := sum;
    end;
end;

function SinInt2(z : ComplexType) : ComplexType; // si(x)
begin
    Result := SinInt(z) - C_HALFPI;
end;

function CosInt(z : ComplexType) : ComplexType; // Ci(x)
var
    limit, n : IntegerType;
    sum      : ComplexType;
begin
    if (isReal(z)) then
    begin
        if (z.Re < 0)
        then Result := CosInt(-z) + Imag(C_PI)
        else Result := -ExpIntC1(Imag(z)).Re;
    end else begin
        sum := 0;
        limit := 100;
        for n := 1 to limit do
            sum := sum + MinusOneTo(n) * powbyfact(z, 2*n) * Inv(2*n);
        Result := C_EM + Ln(z) + sum;
    end;
end;

function CosInt2(z : ComplexType) : ComplexType; // Cin(x)
begin
    Result := C_EM + Ln(z) - CosInt(z);
end;

function Gamma(z : ComplexType) : ComplexType;
begin
    if (isInteger(z)) then
        if (z.Re > 0)
            then Result := fact(z.Re-1)
            else Result := NaN
    else if (z = 0.5)
    then Result := C_SQRTPI // sqrt(pi)
    else if (z.Re > 1)
    then Result := (z-1)*Gamma(z-1)
    else if (z.Re < 0)
    then Result := Gamma(z+1)/z
    else Result := Exp(GammaLn(z));
end;

// from http://numerical.recipes/book.html
function LogGamma_real(z : ComplexType) : ComplexType;
var
    j              : IntegerType;
    x, y, tmp, ser : ComplexType;
    cof            : Array[0..13] of ComplexType;
begin
    cof[0] := 57.1562356658629235;
    cof[1] := -59.5979603554754912;
    cof[2] := 14.1360979747417471;
    cof[3] := -0.491913816097620199;
    cof[4] := 0.339946499848118887e-4;
    cof[5] := 0.465236289270485756e-4;
    cof[6] := -0.983744753048795646e-4;
    cof[7] := 0.158088703224912494e-3;
    cof[8] := -0.210264441724104883e-3;
    cof[9] := 0.217439618115212643e-3;
    cof[10] := -0.164318106536763890e-3;
    cof[11] := 0.844182239838527433e-4;
    cof[12] := -0.261908384015814087e-4;
    cof[13] := 0.368991826595316234e-5;
    x := z;
    y := x;
    tmp := x + 5.24218750000000000; //rational 671/128.
    tmp := (x+0.5)*Ln(tmp)-tmp;
    ser := 0.999999999999997092;
    for j := 0 to 13 do
    begin
        ser := ser + cof[j]/(y+1);
        y := y+1;
    end;
    Result := tmp+Ln(2.5066282746310005*ser/x);
end;

function GammaLn(z : ComplexType) : ComplexType;
begin
    if (z = 1)
    then Result := 0
    else if (z = 0.5)
    then
        Result := C_LNSQPI // ln(sqrt(pi))
    else if (z.Re > 1)
    then Result := Ln(z-1) + GammaLn(z-1)
    else if (z.Re < 0)
    then Result := GammaLn(z+1) - Ln(z)
    else begin
        Result := LogGamma_real(z);
    end;
end;

// error functions

//{*
function Erf2(z : ComplexType) : ComplexType;
var
    limit, n : IntegerType;
    s1, s, p : ComplexType;
	epsilon  : RealType;
    k        : IntegerType;
begin
    //writeln('erf');
    if z = 0 then
    begin
        Result := 0;
    end else begin
        if (Abs(z) > 100)
            then limit := trunc(10000*Abs(z))+1
		    else limit := trunc(100000*Abs(z))+1;
        //writeln('lim ' , limit);
        s := 0;
        epsilon := 50.0;
        n := 0;
        while (n < limit)
        and (epsilon > 0.0000000000001) do
        begin
            s1 := s;
            p := 1.0;
            for k := 1 to n do
                p := p * (-z*z)/k;
            s := s + p * (z/(2*n+1));
            epsilon := Abs(s-s1);
            n := n + 1;
        end;
        s := s * C_2DSQPI; // 2/sqrt(pi)
        Result := s;
    end;
end;

function Erfc(z : ComplexType) : ComplexType;
var
    sum : ComplexType;
    n   : IntegerType = 500;
begin
    if z.Re < 0 then
        Result := 2 - Erfc(-z)
    else if z = 0 then
        Result := 1
    else if z.Re = 0 then
    begin
        Result := 1-Erf2(z);
    end else begin
        sum := (4*n+1) + 2*Sqr(z) - ((2*n+1)*(2*n+2));
        while (n > 0) do
        begin
            sum := (4*n-3) + 2*Sqr(z) - ((2*n-1)*(2*n))/sum;
            n := n-1;
        end;
        Result := (C_2DSQPI * z * Exp(-Sqr(z)))/sum;
    end;
end;

function Erf(z : ComplexType) : ComplexType;
begin
    Result := 1.0 - Erfc(z);
end;


function Erfi(z : ComplexType) : ComplexType;
begin
    Result := -Imag(Erf(Imag(z)));
end;

// TODO
// beta functions

// s.Re > 0
//
function UpperGamma(s, x : ComplexType) : ComplexType;
var
    sum : ComplexType;
    n   : IntegerType = 50;
begin
    if (s.Re < 0)
    then Result := NaN
    else if (s = 0) and ((not isReal(x)) or (x.Re <= 0))
    then Result := NaN
    else if (s = 1) then
    begin
        Result := Exp(-x);
    end else if (x = 0) then begin
        Result := Gamma(s);
    end else if (s = 0.5) then begin
        Result := Erfc(sqrt(x)) * C_SQRTPI; // sqrt(pi)
    end else begin
        sum := (2*n+1)+x-s;
        while (n > 0) do
        begin
            sum := (2*n-1)+x-s + (n*(s-n))/sum;
            n := n-1;
        end;
        Result := (Pow(x,s) * Exp(-x))/sum;
    end;
end;

function LowerGamma(s, x : ComplexType) : ComplexType;
begin
    if (s.Re <= 0)
    then Result := NaN
    else
    if (s = 1) then
    begin
        Result := 1.0 - Exp(-x);
    end else if (x = 0) then begin
        Result := 0;
    end else if (s = 0.5) then begin
        Result := Erf(sqrt(x)) * C_SQRTPI; // sqrt(pi)
    end else begin
        Result := Gamma(s) - UpperGamma(s,x);
    end;
end;

function LowerRegGamma(s, x : ComplexType) : ComplexType;
begin
    Result := LowerGamma(s,x)/Gamma(s);
end;

// beta function with some help
function newton_intt(n, k : ComplexType) : ComplexType;
var
    i : IntegerType;
begin
    if (k.Re > n.Re) then
        Result := 1.0/0.0
    else if (k = 0) then
        Result := 1
    else if (k.Re > n.Re/2) then
        Result := newton_intt(n,n-k)
    else begin
        Result := 1;
        for i := 1 to trunc(k.Re) do
            Result := Result * (n - i + 1) / i;
    end;
end;

function Newton(n, k : ComplexType) : ComplexType;
var
    s : ComplexType;
    j : IntegerType;
begin
	if (n.Re < 0)
    then Result := MinusOneTo(k) * Newton(k-n-1, k)
    else if (isInteger(n)) and (isInteger(k))
    then Result := newton_intt(n,k)
	else begin
        s := 1;
		j := 1;
		while (j <= k.Re) do
		begin
			s := s * (n-k+j)/j;
			j := j + 1;
		end;
    	Result := s;
  	end;
end;


function Beta(x, y : ComplexType) : ComplexType;
begin
    if (isInteger(x)) and (isInteger(y))
    then Result := ((x+y)/(x*y))*Inv(newton_intt(x+y, x))
    else begin
        Result := Exp(GammaLn(x) + GammaLn(y) - GammaLn(x+y));
    end;
end;

// incomplete beta function
function IncBeta(x, a, b : ComplexType) : ComplexType;
var
    sum : ComplexType;
    n   : IntegerType = 50;
    m   : IntegerType;
begin
    if (x = 1)
    then Result := Beta(a,b)
    else if (x = 0)
    then Result := 0
    else if (x.Re > ((a+1)/(a+b+2)).Re)
    then Result := Beta(a,b) - IncBeta(1-x,b,a)
    else begin
        m := (n+1) div 2;
        sum := (-1.0*(a+b+m)*(a+m)*x)/((a + 2*m + 1)*(a + 2*m));
        while (n > 0) do
        begin
            m := n div 2;
            if (n mod 2 = 0)
                then sum := 1 + ((m*(b-m)*x)/((a + 2*m - 1)*(a + 2*m)))/sum
                else sum := 1 + ((-(a+b+m)*(a+m)*x)/((a + 2*m + 1)*(a + 2*m)))/sum;
            n := n-1;
        end;
        Result := (Pow(x,a) * Pow(1-x, b))/(a*sum);
    end;
end;

function RegIncBeta(x, a, b : ComplexType) : ComplexType;
begin
         if (x = 0)   then Result := 0
    else if (x = 1)   then Result := 1
    else if (b = 1)   then Result := Pow(x, a)
    else if (a = 1)   then Result := 1 - Pow(1-x, b)
    else Result := IncBeta(x, a, b)/Beta(a, b);
end;

// --------------------------------------------------------
// Fresnel integrals

function FresnelC(z : ComplexType) : ComplexType;
begin
    if (isReal(z))
        then Result := ((Imag+1)/2 * Erf(C_SQRTPI/2 * z * (1-Imag))).Re
        else Result := (1-Imag)/4 * (Erf(C_SQRTPI*z*(1+Imag)/2) + Imag(Erf(C_SQRTPI*z*(1-Imag)/2)));
end;

function FresnelS(z : ComplexType) : ComplexType;
begin
    if (isReal(z))
        then Result := ((Imag+1)/2 * Erf(C_SQRTPI/2 * z * (1-Imag))).Im
        else Result := (1+Imag)/4 * (Erf(C_SQRTPI*z*(1+Imag)/2) - Imag(Erf(C_SQRTPI*z*(1-Imag)/2)));
end;


// --------------------------------------------------------
// riemann zeta function

// compute Bernoulli number B_n - works badly on large ones
function bernoulli_num(n : IntegerType) : ComplexType;
var
    i, k : IntegerType;
    s    : ComplexType;
    B    : array of ComplexType;
begin
    if (n > 10)
        then SetLength(B, n+1)
        else SetLength(B, 10+1);
    B[0] := 1;
    B[1] := 0.5;
    B[2] := 0.16666666666666666666666666666667;  // 1/6
    B[4] := -0.0333333333333333333333333333333; // -1/30
    B[6] := 0.02380952380952380952380952380952; // 1/42
    B[8] := -0.0333333333333333333333333333333; // -1/30
    B[10] := 0.0757575757575757575757575757576; // 5/66
    i := 3;
    while (i <= n) do
    begin
        B[i] := 0;
        i := i + 2;
    end;
    if (n >= 12) and (n mod 2 = 0) then
    begin
        i := 12;
        while (i <= n) do
        begin
            B[i] := 1;
            s := 0.0;
            for k := 0 to i-1 do
                s := s + 1.0 * B[k] * newton_intt(i,k) / (i - k + 1.0);
            B[i] := 1-s;
            i := i+2;
        end;
    end;
    Result := B[n];
end;

function DirichletEta(z : ComplexType) : ComplexType;
var
    i, n, k : IntegerType;
    d, dn, s : ComplexType;
begin
         if (z = -1) then Result := 1.0/4
    else if (z = 0) then Result := 1.0/2
    else if (z = 0.5) then Result := 0.6048986434216303702472659142359555 // just for convenience
    else if (z = 1) then Result := C_LN2
    else if (z = 2) then Result := C_SQPID12
    else if (z = 3) then Result := 0.901542677369695714049803621133587493
    else if (z = 4) then Result := C_7QUPI720
    else if (z = Infinity) then Result := 1
    else if (z = Imag) then Result := ComplexNum(0.532593181763096166570965008197319044727785768143492192239748725, 0.229384857728525892457886733558081938225195415266121034625072393)
    else if (z = -Imag) then Result := ComplexNum(0.532593181763096166570965008197319044727785768143492192239748725, -0.229384857728525892457886733558081938225195415266121034625072393)
    else begin
        if (isInteger(z)) and (z.Re < 0) then
        begin
            n := trunc(1 - z.Re);
            Result := (Pow(2, n) - 1) * bernoulli_num(n) / n;
        end else begin
            n := 50;
            dn := 0;
            for i := 0 to n do
                dn := dn + fact(n+i-1)*Pow(4,i)/(fact(n-i) * fact(2*i));
            dn := n * dn;
            s := 0;
            for k := 0 to n-1 do
            begin
                d := 0;
                for i := 0 to k do
                    d := d + fact(n+i-1)*Pow(4,i)/(fact(n-i) * fact(2*i));
                d := n * d;
                s := s + MinusOneTo(k) * (d - dn) * Inv(Pow(k+1, z))
            end;
            s := -1/dn * s;
            Result := s;
        end;
    end;
end;

function RiemannZeta(z : ComplexType) : ComplexType;
begin
         if (z = -1) then Result := -1/12
    else if (z = 0) then Result := -1.0/2
    else if (z = 0.5) then Result := -1.460354508809586812889499152515298 // just for convenience
    else if (z = 1) then Result := Infinity
    else if (z = 2) then Result := C_SQPID6 // Pi*Pi/6
    else if (z = 3) then Result := C_APERY  // Apery's constant
    else if (z = 4) then Result := C_QUPI90 // pi^4 / 90
    else if (z = Imag) then Result := ComplexNum(0.003300223685324102874217114210134565971489647240278355024692396, -0.4181554491413216766892742398433610608359501869010386208171983)
    else if (z = -Imag) then Result := ComplexNum(0.003300223685324102874217114210134565971489647240278355024692396, 0.4181554491413216766892742398433610608359501869010386208171983)
    else begin
        Result := DirichletEta(z) * Inv(1-Pow(2,1-z));
    end;
end;

// --------------------------------------------------------
// lambert lambert ty k
// W function

// helpers

function LambertW0_exp(z : ComplexType) : ComplexType;
var
    sum : ComplexType;
    n   : IntegerType = 300;
begin
    sum := z/Exp(z);
    while (n >= 0) do
    begin
        sum := z/Exp(sum);
        n := n-1;
    end;
    Result := sum;
end;

function LambertW0_ln(z : ComplexType) : ComplexType;
var
    sum : ComplexType;
    n   : IntegerType = 100;
begin
    sum := Ln(z);
    while (n >= 0) do
    begin
        sum := Ln(z/sum);
        n := n-1;
    end;
    Result := sum;
end;


function xex(x : RealType) : RealType;
begin
    Result := x * system.exp(x);
end;

function xex(x : ComplexType) : ComplexType;
begin
    Result := x * Exp(x);
end;

function xexd(x : ComplexType) : ComplexType;
begin
    Result := (x+1) * Exp(x);
end;

function xexdd(x : ComplexType) : ComplexType;
begin
    Result := (x+2) * Exp(x);
end;

// approx for reals between e and e^(1+e)
function LambertW0_realapprox(z, a, b : RealType) : RealType;
var
    res, dif : RealType;
    res0     : RealType = 0;
    epsilon  : RealType = 0.000000000000001;
    n        : IntegerType = 1000;
begin
    dif := 2137;
    res := 1;
    while (n > 0) 
    and (dif >= epsilon) 
    do
    begin
        if (xex(a) = z) then
        begin
            res := a;
            dif := 0;
        end else if (xex(b) = z) then
        begin
            res := b;
            dif := 0;
        end else begin
            res := (a+b)/2;
            if (xex(res) = z) then
            begin
                dif := 0;
            end else if (xex(res) > z) then
            begin
                b := res;
                dif := system.abs(res - res0);
                res0 := res;
            end else begin
                a := res;
                dif := system.abs(res - res0);
                res0 := res;
            end;
        end;
        n := n-1;
    end;
    Result := res;
end;

// approximation for complex numbers
function LambertW_newtonHailey(z : ComplexType; w0 : ComplexType) : ComplexType;
var
    n       : IntegerType = 10000;
    epsilon : RealType = 0.000000000000001;
begin
    while (n > 0) and (Abs(xex(w0) - z) > epsilon) do
    begin
        w0 := w0 - (xex(w0) - z)/(xexd(w0) - ((xexdd(w0) - z*(w0+2)))/(2*w0+2));
        n := n - 1;
    end;
    Result := w0;
end;

function LambertW0_newton2(z : RealType) : RealType;
const
    limit = 10;
var
    w0      : RealType;
    n       : IntegerType = 0;
    epsilon : RealType = 0.000000000000001;
begin
    w0 := system.ln(z) - system.ln(system.ln(z));
    while (n < limit) and (system.abs(xex(w0) - z) > epsilon) do
    begin
        w0 := (w0)/(1+w0) * (1 + system.ln(z/w0));
        n := n + 1;
    end;
    if (n < limit) // for some reason newton2 either does its job in 3-5 steps, or it can't finish within 10000 steps
        then Result := w0
        else Result := LambertW0_realapprox(z, 1, C_EXP); // this approximation takes at most 50 steps in general
end;

function LambertWn1_realnewton(z : RealType) : RealType;
const
    limit = 10000;
var
    w0      : RealType;
    n       : IntegerType = 0;
    epsilon : RealType = 0.000000000000001;
begin
    if (z <= -0.25) 
        then w0 := -1 - system.sqrt(2)*system.sqrt(1 + C_EXP*z)
        else w0 := system.ln(-z) - system.ln(-system.ln(-z));
    while (n < limit) and (system.abs(xex(w0) - z) > epsilon) do
    begin
        w0 := (w0)/(1+w0) * (1 + system.ln(z/w0));
        n := n + 1;
    end;
    Result := w0;
end;

// huge koszonom to Istvan Mezo
// https://github.com/IstvanMezo/LambertW-function/blob/master/complex%20Lambert.cpp
function LW_InitPoint(z : ComplexType; k : IntegerType) : ComplexType;
var
    ip, p : ComplexType;
    two_pi_k_I : ComplexType;
begin
	two_pi_k_I := ComplexNum(0, 2 * C_PI * k);
	ip := Ln(z) + two_pi_k_I - Ln(Ln(z) + two_pi_k_I);
	p := Sqrt(2 * (C_EXP * z + 1));

	if (Abs(z - (-exp(-1))) <= 1) then
	begin
		if (k = 0) 
            then ip := -1 + p - Sqr(p)/3 + 11./72. * Cub(p);
		if ((k = 1) and (z.Im < 0))
		or ((k = -1) and (z.Im > 0)) 
            then ip := -1 - p - Sqr(p)/3 - 11*Cub(p)/72;
	end;

	if  (k = 0) 
    and (Abs(z - 0.5) <= 0.5)
    then ip := (0.35173371 * (0.1237166 + 7.061302897 * z)) / (2.827184 * (2*z + 1));

	if (k = -1) and (Abs(z - 0.5) <= 0.5) 
    then ip := -(((2.2591588985 + Imag(4.22096)) * ((-14.073271 - Imag(33.767687754)) 
            * z - (12.7127 - Imag(19.071643)) * (2*z + 1))) 
            / (2.0 - (17.23103 - Imag(10.629721)) * (2*z + 1)));

	Result := ip;
end;

// lambert W function main definitions

function LambertW0(z : ComplexType) : ComplexType;
begin
         if (z = 0) then Result := 0
    else if (z = 1) then Result := C_OMEGA
    else if (z = C_EXP) then Result := 1
    else if (z = Exp(1+C_EXP)) then Result := C_EXP
    else if (z = Sqrt(C_EXP)/2) then Result := 0.5
    else if (z = 2*C_LN2) then Result := C_LN2
    else if (z = -Inv(C_EXP)) then Result := -1
    else if (z = -C_HALFPI) then Result := Imag(C_HALFPI)
    else if (Abs(z) > C_EXPTOXP1) then Result := LambertW0_ln(z)
    else if (
        (not ((isReal(z)) and (z.Re <= -Inv(C_EXP).Re))) 
        and (Abs(z) < C_EXP) 
        ) then Result := LambertW0_exp(z)
        //) then Result := LambertW0_realapprox(z.Re, -Inv(C_EXP).Re, C_EXP)
    else if ((isReal(z)) and (z.Re > C_EXP) and (z.Re < C_EXPTOXP1)) then Result := LambertW0_newton2(z.Re)
    else Result := LambertW_newtonHailey(z, Ln(z));
end;

function LambertWn1(z : ComplexType) : ComplexType;
begin
         if (z = -Inv(C_EXP)) then Result := -1
    else if (z = 0) then Result := -Infinity
    else if (isReal(z)) and (z.Re > -Inv(C_EXP).Re) and (z.Re < 0) then
    begin
        Result := LambertWn1_realnewton(z.Re)
    end else begin
        Result := LambertW_newtonHailey(z, LW_InitPoint(z,-1));
    end;
end;

// Lambert W function for all branches
function LambertW(z : ComplexType; k : IntegerType = 0) : ComplexType;
begin
    if (z = 0) then 
    begin
        if (k = 0) then Result := 0 else Result := -Infinity; 
    end 
    else if (z = -Inv(C_EXP)) and ((k = 0) or (k = -1)) then Result := -1
	else if (z = C_EXP) and (k = 0) then Result := 1
    else case k of
        -1 : Result := LambertWn1(z);
        0  : Result := LambertW0(z);
        else Result := LambertW_newtonHailey(z, LW_InitPoint(z,k));
    end;
end;

// -----------------------------------------------------------
// Euler power tower h(x) = x^x^x^x^...
// Eisenstein (1844)
function InfPowerTower(z : ComplexType) : ComplexType;
begin
         if (z = 1) then Result := 1
    else if (z = 0) then Result := 0
    else if (z = Pow(C_EXP, Inv(C_EXP))) then Result := C_EXP
    else Result := -LambertW(-Ln(z))/Ln(z);
end;

// -----------------------------------------------------------
// infinities

function isInfinite(z : ComplexType) : Boolean; 
begin
    Result := (Math.isInfinite(z.Re)) or (Math.isInfinite(z.Im));
end;

function isReInfinite(z : ComplexType) : Boolean; 
begin
    Result := (Math.isInfinite(z.Re));
end;

function isImInfinite(z : ComplexType) : Boolean; 
begin
    Result := (Math.isInfinite(z.Im));
end;

function isTotalInfinite(z : ComplexType) : Boolean; 
begin
    Result := (Math.isInfinite(z.Re)) and (Math.isInfinite(z.Im));
end;

function isFinite(z : ComplexType) : Boolean; 
begin
    Result := not (isInfinite(z));
end;

function RePosInfinity(im : RealType = 0) : ComplexType;
begin
    Result.Re := Infinity;
    Result.Im := im;
end;

function ReNegInfinity(im : RealType = 0) : ComplexType;
begin
    Result.Re := -Infinity;
    Result.Im := im;
end;

function ImPosInfinity(re : RealType = 0) : ComplexType;
begin
    Result.Re := re;
    Result.Im := Infinity;
end;

function ImNegInfinity(re : RealType = 0) : ComplexType;
begin
    Result.Re := re;
    Result.Im := -Infinity;
end;

function ComplexInfinity1() : ComplexType;
begin
    Result.Re := Infinity;
    Result.Im := Infinity;
end;

function ComplexInfinity2() : ComplexType;
begin
    Result.Re := -Infinity;
    Result.Im := Infinity;
end;

function ComplexInfinity3() : ComplexType;
begin
    Result.Re := -Infinity;
    Result.Im := -Infinity;
end;

function ComplexInfinity4() : ComplexType;
begin
    Result.Re := Infinity;
    Result.Im := -Infinity;
end;

// --------------------------------------------------------
// rounds

function ComplexRound(z : ComplexType) : ComplexType;
begin
    Result.Re := Round(z.Re);
    Result.Im := Round(z.Im);
end;

function ComplexTrunc(z : ComplexType) : ComplexType;
begin
    Result.Re := Trunc(z.Re);
    Result.Im := Trunc(z.Im);
end;

function ComplexFloor(z : ComplexType) : ComplexType;
begin
    Result.Re := Floor(z.Re);
    Result.Im := Floor(z.Im);
end;

function ComplexCeil(z : ComplexType) : ComplexType;
begin
    Result.Re := Ceil(z.Re);
    Result.Im := Ceil(z.Im);
end;

end.
