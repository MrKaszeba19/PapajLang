unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Menus, ShellCtrls, UnitEntity;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    SynAnySyn1: TSynAnySyn;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1    : TForm1;
  language : string;

{$IFDEF MSWINDOWS}
//function GetLocaleInformation(Flag: integer): string;
{$ENDIF}
function GetLocaleLanguage: string;

implementation
uses Unit2, Unit3;

{$R *.lfm}

{ TForm1 }

{$IFDEF MSWINDOWS}
//function GetLocaleInformation(Flag: integer): string;
//var
// pcLCA: array[0..20] of char;
//begin
// if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
// begin
//   pcLCA[0] := #0;
// end;
// Result := pcLCA;
//end;

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
var
  prevent : Integer;
  mask    : String;
begin
     try
        Edit2.Text := PS_parseString(Edit1.Text);
     except
     on E : EAccessViolation do begin
       Edit1.Text := '';
       Edit2.Text := 'Wrong RPN.';
     end;
     on E : Exception do begin
       Edit1.Text := '';
       Edit2.Text := E.Message;
     end;
     end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
     prevent : Integer;
     mask    : String;
begin
     try
        Memo1.Text := '';
        Memo1.Append(PS_parseString(SynEdit1.Text));
     except
           on E : EAccessViolation do begin
              Memo1.Text := 'Wrong RPN.';
           end;
           on E : Exception do begin
              Memo1.Text := E.Message;
           end;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     // https://docs.moodle.org/dev/Table_of_locales
     // showmessage(GetLocaleLanguage);
     case (GetLocaleLanguage) of
          'da_DK.UTF-8' : language := 'den';
          'Danish_Denmark.1252' : language := 'den';
          'en.UTF-8' : language := 'eng';
          'English_Australia.1252' : language := 'eng';
          'pl.UTF-8' : language := 'pol';
          'Polish_Poland.1250' : language := 'pol';
          'he_IL.utf8' : language := 'hbr';
          'Hebrew_Israel.1255' : language := 'hbr';
          else language := 'eng';
     end;
     case language of
          'den' : set1DEN();
          'eng' : set1ENG();
          'pol' : set1POL();
	  'hbr' : set1HBR();
          else set1ENG();
     end;
     {$IFDEF MSWINDOWS}
     Memo1.Font.Name := 'Consolas';
     {$ENDIF}
     {$IFDEF UNIX}
     Memo1.Font.Name := 'Monospace';
     {$ENDIF}
     SynEdit1.Text := '3 2 + times {'+#13#10+'  "Hello world!" println '+#13#10+'}'+#13#10+'10 times rand'+#13#10+'all sum';
     Memo1.Text := '';
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

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
     Unit3.set1DEN();
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
     Unit3.set1HBR();
end;

end.
