unit RPNAbout;

interface

const RPN_version = '0.5.2';
const RPN_update = 0;
const RPN_codename = 'Lamed';
const RPN_generation = 3;
const RPN_isStable = True;
const RPN_date = '2021-12-18'; //{$I %DATE%};
const RPN_updated = '';
const RPN_targetCPU = {$I %FPCTARGETCPU%};
const RPN_targetOS = {$I %FPCTARGETOS%};
const RPN_apptype = 'Console';
const RPN_isGUI = false;

{$IFDEF cpu64}
type LongInt = Int64;
{$ENDIF}

implementation

end.

