unit RPNAbout;

interface

const RPN_version = '0.5.4';
const RPN_update = 0;
const RPN_codename = 'Nun';
const RPN_generation = 3;
const RPN_isStable = True;
const RPN_date = {$I %DATE%};
const RPN_updated = '';
const RPN_targetCPU = {$I %FPCTARGETCPU%};
{$ifdef WINDOWS}
const RPN_targetOS = 'Windows';
{$else}
const RPN_targetOS = {$I %FPCTARGETOS%};
{$endif}
const RPN_apptype = 'GUI';
const RPN_isGUI = true;

{$IFDEF cpu64}
type LongInt = type Int64;
type IntegerType = type Int64;
{$ELSE}
type IntegerType = type LongInt;
{$ENDIF}
type RealType = type Extended;

implementation

end.

