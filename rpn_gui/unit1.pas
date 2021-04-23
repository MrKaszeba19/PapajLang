unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Menus, ShellCtrls, ComCtrls,
  UnitEntity;

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
    MenuLangCSB: TMenuItem;
    MenuLangCSB2: TMenuItem;
    MenuQuit: TMenuItem;
    MenuLangENG: TMenuItem;
    MenuLangPOL: TMenuItem;
    MenuLangDEN: TMenuItem;
    MenuLangHBR: TMenuItem;
    MenuLoad: TMenuItem;
    MenuSave: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SynAnySyn1: TSynAnySyn;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    procedure MenuLangCSB2Click(Sender: TObject);
    procedure MenuLangCSBClick(Sender: TObject);
    procedure OpenOfflineFile();
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuQuitClick(Sender: TObject);
    procedure MenuLangENGClick(Sender: TObject);
    procedure MenuLangPOLClick(Sender: TObject);
    procedure MenuLangDENClick(Sender: TObject);
    procedure MenuLangHBRClick(Sender: TObject);
    procedure MenuLoadClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
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

{$IFDEF MSWINDOWS}
uses Unit2, Unit3, Windows;
{$ELSE}
uses Unit2, Unit3;
{$ENDIF}


{$R *.lfm}

procedure TForm1.OpenOfflineFile();
var fn : String;
begin
     if OpenDialog1.Execute then
     begin
          try
             fn := OpenDialog1.FileName;
             SynEdit1.Lines.LoadFromFile(fn);
             SynEdit1.Modified := false;
          except
                ShowMessage('Error when loading a file.');
          end;
     end;
end;

procedure TForm1.MenuLangCSBClick(Sender: TObject);
begin
     language := 'csb';
     Unit3.set1CSB();
end;

procedure TForm1.MenuLangCSB2Click(Sender: TObject);
begin
    language := 'csb2';
    Unit3.set1CSB2();
end;

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
        Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
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
     //showmessage(GetLocaleLanguage);
     case (GetLocaleLanguage) of
          'csb.UTF-8' : language := 'csb';
          'cab_PL.UTF-8' : language := 'csb';
          'da_DK.UTF-8' : language := 'den';
          'Danish_Denmark.1252' : language := 'den';
          'Danish' : language := 'den';
          'en.UTF-8' : language := 'eng';
          'en_GB.UTF-8' : language := 'eng';
          'en_US.UTF-8' : language := 'eng';
          'English_Australia.1252' : language := 'eng';
          'English' : language := 'eng';
          'pl.UTF-8' : language := 'pol';
          'pl_PL.UTF-8' : language := 'pol';
          'Polish_Poland.1250' : language := 'pol';
          'Polish' : language := 'pol';
          'he_IL.utf8' : language := 'hbr';
          'Hebrew_Israel.1255' : language := 'hbr';
          'Hebrew' : language := 'hbr';
          else language := 'eng';
     end;
     case language of
          'den' : set1DEN();
          'eng' : set1ENG();
          'pol' : set1POL();
	  'hbr' : set1HBR();
          'csb' : set1CSB();
          'csb2' : set1CSB2();
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

procedure TForm1.MenuQuitClick(Sender: TObject);
begin
     Close;
end;

procedure TForm1.MenuLangENGClick(Sender: TObject);
begin
     language := 'eng';
     Unit3.set1ENG();
end;

procedure TForm1.MenuLangPOLClick(Sender: TObject);
begin
     language := 'poi';
     Unit3.set1POL();
end;

procedure TForm1.MenuLangDENClick(Sender: TObject);
begin
     language := 'den';
     Unit3.set1DEN();
end;

procedure TForm1.MenuLangHBRClick(Sender: TObject);
begin
     language := 'hbr';
     Unit3.set1HBR();
end;

procedure TForm1.MenuLoadClick(Sender: TObject);
begin
     if SynEdit1.Modified then begin
        if mrOK=MessageDlg('Are you sure you want to exit without saving a file?',mtConfirmation,[mbOK,mbCancel],0) then
        begin
             OpenOfflineFile();
             //SetStyles();
        end;
     end else begin
         OpenOfflineFile();
         //SetStyles();
     end;
end;

procedure TForm1.MenuSaveClick(Sender: TObject);
begin
     if SaveDialog1.Execute then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        //SetFileName(SaveDialog1.FileName);
        SynEdit1.Modified := false;
     end else begin
         Abort;
     end;
end;

end.
