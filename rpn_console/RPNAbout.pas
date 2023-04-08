unit RPNAbout;

interface

const RPN_version = '0.5.3';
const RPN_update = 0;
const RPN_codename = 'Mem';
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
const RPN_apptype = 'Console';
const RPN_isGUI = false;

{$IFDEF cpu64}
type LongInt = Int64;
{$ENDIF}

implementation

end.

