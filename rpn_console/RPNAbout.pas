unit RPNAbout;

interface

const RPN_version = '0.5.3';
const RPN_update = 2;
const RPN_codename = 'Mem';
const RPN_generation = 3;
const RPN_isStable = True;
//const RPN_date = {$I %DATE%};
const RPN_date = '2023-04-08';
const RPN_updated = '2023-11-26';
const RPN_targetCPU = {$I %FPCTARGETCPU%};
{$ifdef WINDOWS}
const RPN_targetOS = 'Windows';
{$else}
const RPN_targetOS = {$I %FPCTARGETOS%};
{$endif}
const RPN_apptype = 'Console';
const RPN_isGUI = false;

{$IFDEF cpu64}
type LongInt = Int64;
{$ENDIF}

implementation

end.

