unit RPNAbout;

interface

const RPN_version = 'X.X.X';
const RPN_update = 0;
const RPN_codename = 'Leviathan';
const RPN_generation = 3;
const RPN_isStable = False;
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
type LongInt = Int64;
{$ENDIF}

implementation

end.

