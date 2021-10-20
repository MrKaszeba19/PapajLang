unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Menus, ShellCtrls, ComCtrls, ExtCtrls,
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
    LabelFileName: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuNewFile: TMenuItem;
    MenuLangCSB3: TMenuItem;
    MenuLangGER: TMenuItem;
    MenuLangNED: TMenuItem;
    MenuLangFRA: TMenuItem;
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
    CodePanel: TPanel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    SynAnySyn1: TSynAnySyn;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    procedure MenuLangCSB3Click(Sender: TObject);
    procedure MenuLangFRAClick(Sender: TObject);
    procedure MenuLangCSB2Click(Sender: TObject);
    procedure MenuLangCSBClick(Sender: TObject);
    procedure MenuLangGERClick(Sender: TObject);
    procedure MenuLangNEDClick(Sender: TObject);
    procedure MenuNewFileClick(Sender: TObject);

    procedure SetFileName(x : String);
    function GetFileName() : String;
    procedure InitializeNewFile();

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
    procedure Splitter1Moved(Sender: TObject);
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
             LabelFileName.Caption := fn;
             SynEdit1.Modified := false;
          except
                ShowMessage('Error when loading a file.');
          end;
     end;
end;

procedure TForm1.InitializeNewFile();
begin
     //SynEdit1.Text := '';
     //SynEdit1.CommandProcessor(TSynEditorCommand(ecSelectAll), ' ', nil);
     //SynEdit1.CommandProcessor(TSynEditorCommand(ecCopy), ' ', nil);
     //SynEdit1.CommandProcessor(TSynEditorCommand(ecCut), ' ', nil);
     //SynEdit1.CommandProcessor(TSynEditorCommand(ecPaste), ' ', nil);
     SynEdit1.Text := '';
     {$IFDEF MSWINDOWS}
     SetFileName('.\Untitled.ppsc');
     {$ELSE}
     SetFileName('./Untitled.ppsc');
     {$ENDIF}
     LabelFileName.Caption := GetFileName();
     //SetStyles();
     SynEdit1.Modified := false;
end;

procedure TForm1.MenuLangCSBClick(Sender: TObject);
begin
     language := 'csb';
     Unit3.set1CSB();
end;

procedure TForm1.MenuLangGERClick(Sender: TObject);
begin
     language := 'ger';
     Unit3.set1GER();
end;

procedure TForm1.MenuLangNEDClick(Sender: TObject);
begin
     language := 'ned';
     Unit3.set1NED();
end;

procedure TForm1.MenuNewFileClick(Sender: TObject);
begin
     if SynEdit1.Modified then begin
        if mrOK=MessageDlg('Are you sure you want to exit without saving a file?',mtConfirmation,[mbOK,mbCancel],0) then
        begin
             InitializeNewFile();
        end;
     end else begin
         InitializeNewFile();
     end;
end;

procedure TForm1.SetFileName(x: String);
begin
     LabelFileName.Caption := x;
end;

function TForm1.GetFileName(): String;
begin
     Result := LabelFileName.Caption;
end;

procedure TForm1.MenuLangCSB2Click(Sender: TObject);
begin
    language := 'csb2';
    Unit3.set1CSB2();
end;

procedure TForm1.MenuLangFRAClick(Sender: TObject);
begin
    language := 'fra';
    Unit3.set1FRA();
end;

procedure TForm1.MenuLangCSB3Click(Sender: TObject);
begin
     language := 'csb3';
     Unit3.set1CSB3();
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
       Edit2.Text := 'Wrong PS code.';
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
        {$IFDEF LINUX}
        // scroll down:
        Memo1.SelStart:=Length(Memo1.lines.Text);
        Memo1.VertScrollBar.Position:=1000000;
         {$ELSE }
         {$ENDIF}
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
          'csb_PL.UTF-8' : language := 'csb';
          'da_DK.UTF-8' : language := 'den';
          'Danish_Denmark.1252' : language := 'den';
          'Danish' : language := 'den';
          'en.UTF-8' : language := 'eng';
          'en_GB.UTF-8' : language := 'eng';
          'en_US.UTF-8' : language := 'eng';
          'English_Australia.1252' : language := 'eng';
          'English' : language := 'eng';
          'de_DE.UTF-8' : language := 'ger';
          'German_Germany.1252' : language := 'ger';
          'German' : language := 'ger';
          'nl_NL.UTF-8' : language := 'ned';
          'Dutch_Netherlands.1252' : language := 'ned';
          'Dutch' : language := 'ned';
          'pl.UTF-8' : language := 'pol';
          'pl_PL.UTF-8' : language := 'pol';
          'Polish_Poland.1250' : language := 'pol';
          'Polish' : language := 'pol';
          'he_IL.utf8' : language := 'hbr';
          'Hebrew_Israel.1255' : language := 'hbr';
          'Hebrew' : language := 'hbr';
          'French' : language := 'fra';
          'French (Canada)' : language := 'fra';
          'fr.UTF-8' : language := 'fra';
          'fr_FR.UTF-8' : language := 'fra';
          'fr_BE.UTF-8' : language := 'fra';
          'fr_CH.UTF-8' : language := 'fra';
          'fr_LU.UTF-8' : language := 'fra';
          'fr_CA.UTF-8' : language := 'fra';
          'French_France.1252' : language := 'fra';
          else language := 'eng';
     end;
     case language of
          'den' : set1DEN();
          'eng' : set1ENG();
          'fra' : set1FRA();
          'ger' : set1GER();
          'ned' : set1NED();
          'pol' : set1POL();
	  'hbr' : set1HBR();
          'csb' : set1CSB();
          'csb2' : set1CSB2();
          'csb3' : set1CSB3();
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
     language := 'pol';
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
     SaveDialog1.FileName := GetFileName();
     if SaveDialog1.Execute then begin
        SynEdit1.Lines.SaveToFile(SaveDialog1.FileName);
        SetFileName(SaveDialog1.FileName);
        SynEdit1.Modified := false;
     end else begin
         Abort;
     end;
end;

procedure TForm1.Splitter1Moved(Sender: TObject);
begin
     SynEdit1.Height := Splitter1.Top-SynEdit1.Top;
     Memo1.Top := Splitter1.Top+Splitter1.Height;
     Memo1.Height := CodePanel.Height-(Splitter1.Top+Splitter1.Height);
end;

end.
