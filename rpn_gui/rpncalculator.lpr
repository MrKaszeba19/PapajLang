program rpncalculator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, crt, // this includes the LCL widgetset
  SysUtils, 
  Forms, Unit1, Unit2, Unit3, Unit4, Unit5, RPNAbout
  { you can add units after this };

function convertToMDY(date : String) : String;
begin
    Result := Copy(date, 6, 2)+'/'+Copy(date, 9, 2)+'/'+Copy(date, 1, 4);
end;

procedure show_version();
begin
    writeln('RPN CALCULATOR - PapajScript Interpreter.');
    writeln('GUI Application.'); 
    if (RPN_update <= 0) 
        then write('Version '+RPN_version+' ('+RPN_codename+') for '+RPN_targetCPU+'.')
        else write('Version '+RPN_version+' ('+RPN_codename+'), update #'+IntToStr(RPN_update)+' for '+RPN_targetCPU+'.');
    if (RPN_isStable)
        then writeln(' Gen'+IntToStr(RPN_generation)+' build.')
        else writeln(' May be more unstable than usual. 3:)');
    writeln('by Paul Lipkowski & his fiancee Rosie. ');
    //if (RPN_updated = '')
    //    then writeln('Released on '+RPN_date+'.')
    //    else writeln('Released on '+RPN_date+', updated on '+RPN_updated+'.');
    if (RPN_updated = '')
        then writeln('Released on '+convertToMDY(RPN_date)+'.')
        else writeln('Released on '+convertToMDY(RPN_date)+', updated on '+convertToMDY(RPN_date)+'.');
    writeln('Since 11/24/2017. Proudly written in FreePascal. :)');
    writeln('');
end;

{$R *.res}

begin
    show_version();
    write('Turning on the randomizer... ');
    Randomize();
    writeln('Done.');
    RequireDerivedFormResource:=True;
    write('App initializing... ');
    Application.Initialize;
    writeln('Done.');
    writeln('Loading main form... ');
    Application.CreateForm(TForm1, Form1);
    writeln('Done.');
    write('Loading I/O form... ');
    Application.CreateForm(TForm2, Form2);
    writeln('Done.');
    write('Running...');
    Application.Run;
    writeln('Done.');
end.

