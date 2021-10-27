unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, SynCompletion, Forms,
  Controls, Graphics, Dialogs, StdCtrls, Menus, ShellCtrls, ComCtrls, ExtCtrls,
  Unit3,
  UnitEntity;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonTerminal: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LabelFileName: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuView: TMenuItem;
    MenuDarkMode: TMenuItem;
    MenuAutoClear: TMenuItem;
    MenuRunScript: TMenuItem;
    MenuRunHere: TMenuItem;
    MenuRunExternal: TMenuItem;
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
    procedure ButtonTerminalClick(Sender: TObject);
    procedure MenuLangCSB3Click(Sender: TObject);
    procedure MenuLangFRAClick(Sender: TObject);
    procedure MenuLangCSB2Click(Sender: TObject);
    procedure MenuLangCSBClick(Sender: TObject);
    procedure MenuLangGERClick(Sender: TObject);
    procedure MenuLangNEDClick(Sender: TObject);
    procedure MenuNewFileClick(Sender: TObject);
    procedure MenuRunHereClick(Sender: TObject);
    procedure MenuRunScriptClick(Sender: TObject);

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
  locale   : LangMap;

implementation

{$IFDEF MSWINDOWS}
uses Unit2, Windows;
{$ELSE}
uses Unit2;
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
                ShowMessage(locale.ErrorLoadFile+locale.ExclamationMark);
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
    locale := GetLocale(L_CSB);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangGERClick(Sender: TObject);
begin
    locale := GetLocale(L_GER);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangNEDClick(Sender: TObject);
begin
    locale := GetLocale(L_NED);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuNewFileClick(Sender: TObject);
begin
     if SynEdit1.Modified then begin
        if mrOK=MessageDlg(locale.AreYouSureQuitSave, mtConfirmation, [mbOK,mbCancel],0) then
        begin
             InitializeNewFile();
        end;
     end else begin
         InitializeNewFile();
     end;
end;

procedure TForm1.MenuRunHereClick(Sender: TObject);
begin

end;

procedure TForm1.MenuRunScriptClick(Sender: TObject);
begin

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
    locale := GetLocale(L_CSB2);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangFRAClick(Sender: TObject);
begin
    locale := GetLocale(L_FRA);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangCSB3Click(Sender: TObject);
begin
    locale := GetLocale(L_CSB3);
    ApplyLocaleMain(locale);
end;

procedure TForm1.ButtonTerminalClick(Sender: TObject);
begin

end;

{ TForm1 }


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
       Edit2.Text := locale.WrongPS+'.';
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
              Memo1.Text := locale.WrongPS+'.';
           end;
           on E : Exception do begin
              Memo1.Text := E.Message;
           end;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     locale := GetLocale(DetermineLanguage());
     ApplyLocaleMain(locale);
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
    locale := GetLocale(L_ENG);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangPOLClick(Sender: TObject);
begin
    locale := GetLocale(L_POL);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangDENClick(Sender: TObject);
begin
    locale := GetLocale(L_DEN);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangHBRClick(Sender: TObject);
begin
    locale := GetLocale(L_HBR);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLoadClick(Sender: TObject);
begin
     if SynEdit1.Modified then begin
        if mrOK=MessageDlg(locale.AreYouSureQuitSave, mtConfirmation, [mbOK,mbCancel], 0) then
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
