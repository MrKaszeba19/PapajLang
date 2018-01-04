unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1    : TForm1;
  language : string;

{$IFDEF MSWINDOWS}
function GetLocaleInformation(Flag: integer): string;
{$ENDIF}
function GetLocaleLanguage: string;

implementation
uses Unit2, Unit3;

{$R *.lfm}

{ TForm1 }

{$IFDEF MSWINDOWS}
function GetLocaleInformation(Flag: integer): string;
var
 pcLCA: array[0..20] of char;
begin
 if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
 begin
   pcLCA[0] := #0;
 end;
 Result := pcLCA;
end;

{$ENDIF}

function GetLocaleLanguage: string;
begin
 {$IFDEF MSWINDOWS}
  //Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
  Result := '';
 {$ELSE}
  Result := SysUtils.GetEnvironmentVariable('LANG');
 {$ENDIF}
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
     try
        Edit2.Text := FloatToStr(calc_parseRPN(Edit1.Text));
     except
     on E : EAccessViolation do begin
       Edit1.Text := '';
       Edit2.Text := 'Wrong RPN.';
     end;
     on E : Exception do begin
       Edit1.Text := '';
       Edit2.Text := '';
     end;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     // https://docs.moodle.org/dev/Table_of_locales
     // showmessage(GetLocaleLanguage);
     case (GetLocaleLanguage) of
          'en.UTF-8' : language := 'eng';
          'English_Australia.1252' : language := 'eng';
          'pl.UTF-8' : language := 'pol';
          'Polish_Poland.1250' : language := 'pol';
          else language := 'eng';
     end;
     case language of
          'eng' : set1ENG();
          'pol' : set1POL();
          else set1ENG();
     end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
     Close;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
     Unit3.set1ENG();
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
     Unit3.set1POL();
end;

end.

