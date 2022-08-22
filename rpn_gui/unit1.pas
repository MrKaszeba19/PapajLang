unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, UTF8Process, SynEdit, SynHighlighterAny,
  SynCompletion, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ShellCtrls, ComCtrls, ExtCtrls, Unit3, UnitEntity;

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
    MenuLangSRB1: TMenuItem;
    MenuLangSRB2: TMenuItem;
    MenuLangCSB1: TMenuItem;
    MenuLangCSB2: TMenuItem;
    MenuLangCSB3: TMenuItem;
    MenuRunPause: TMenuItem;
    MenuLangPOR1: TMenuItem;
    MenuLangPOR2: TMenuItem;
    MenuLangNOR1: TMenuItem;
    MenuLangNOR2: TMenuItem;
    MenuLangSRB: TMenuItem;
    MenuLangPOR: TMenuItem;
    MenuLangSLO: TMenuItem;
    MenuLangCRO: TMenuItem;
    MenuLangNOR: TMenuItem;
    MenuLangSWE: TMenuItem;
    MenuLangMKD: TMenuItem;
    MenuLangRUS: TMenuItem;
    MenuLangITA: TMenuItem;
    MenuLangAFR: TMenuItem;
    MenuView: TMenuItem;
    MenuDarkMode: TMenuItem;
    MenuAutoClear: TMenuItem;
    MenuRunScript: TMenuItem;
    MenuRunHere: TMenuItem;
    MenuRunExternal: TMenuItem;
    MenuNewFile: TMenuItem;
    MenuLangGER: TMenuItem;
    MenuLangNED: TMenuItem;
    MenuLangFRA: TMenuItem;
    MenuLangCSB: TMenuItem;
    MenuQuit: TMenuItem;
    MenuLangENG: TMenuItem;
    MenuLangPOL: TMenuItem;
    MenuLangDEN: TMenuItem;
    MenuLangHBR: TMenuItem;
    MenuLoad: TMenuItem;
    MenuSave: TMenuItem;
    OpenDialog1: TOpenDialog;
    CodePanel: TPanel;
    ProcessExt: TProcessUTF8;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    SynAnySyn1: TSynAnySyn;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    procedure ButtonTerminalClick(Sender: TObject);
    procedure MenuAutoClearClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuLangCSB1Click(Sender: TObject);
    procedure MenuLangNOR1Click(Sender: TObject);
    procedure MenuLangCROClick(Sender: TObject);
    procedure MenuLangNOR2Click(Sender: TObject);
    procedure MenuLangNORClick(Sender: TObject);
    procedure MenuLangPOR1Click(Sender: TObject);
    procedure MenuLangPOR2Click(Sender: TObject);
    procedure MenuLangPORClick(Sender: TObject);
    procedure MenuLangRUSClick(Sender: TObject);
    procedure MenuLangAFRClick(Sender: TObject);
    procedure MenuLangCSB3Click(Sender: TObject);
    procedure MenuLangFRAClick(Sender: TObject);
    procedure MenuLangCSB2Click(Sender: TObject);
    procedure MenuLangCSBClick(Sender: TObject);
    procedure MenuLangGERClick(Sender: TObject);
    procedure MenuLangITAClick(Sender: TObject);
    procedure MenuLangMKDClick(Sender: TObject);
    procedure MenuLangNEDClick(Sender: TObject);
    procedure MenuLangSLOClick(Sender: TObject);
    procedure MenuLangSRB1Click(Sender: TObject);
    procedure MenuLangSRB2Click(Sender: TObject);
    procedure MenuLangSRBClick(Sender: TObject);
    procedure MenuLangSWEClick(Sender: TObject);
    procedure MenuNewFileClick(Sender: TObject);
    procedure MenuRunHereClick(Sender: TObject);
    procedure MenuRunPauseClick(Sender: TObject);
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
                ShowMessage(locale.ExclamationMark2 + locale.ErrorLoadFile + locale.ExclamationMark);
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

procedure TForm1.MenuLangITAClick(Sender: TObject);
begin
    locale := GetLocale(L_ITA);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangMKDClick(Sender: TObject);
begin
    locale := GetLocale(L_MKD);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangNEDClick(Sender: TObject);
begin
    locale := GetLocale(L_NED);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangSLOClick(Sender: TObject);
begin
    locale := GetLocale(L_SLO);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangSRB1Click(Sender: TObject);
begin
    locale := GetLocale(L_SRB);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangSRB2Click(Sender: TObject);
begin
    locale := GetLocale(L_SRB2);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangSRBClick(Sender: TObject);
begin
    locale := GetLocale(L_SRB);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangSWEClick(Sender: TObject);
begin
    locale := GetLocale(L_SWE);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuNewFileClick(Sender: TObject);
begin
     if SynEdit1.Modified then begin
        if mrOK=MessageDlg(locale.QuestionMark2 + locale.AreYouSureContSave + locale.QuestionMark, 
                           mtConfirmation, 
                           [mbOK,mbCancel]
                           ,0) then
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

procedure TForm1.MenuRunPauseClick(Sender: TObject);
begin
    MenuRunPause.Checked := not MenuRunPause.Checked;
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

{$IFNDEF WINDOWS}
function isDebianBased() : Boolean;
begin
    if FileExists('/etc/debian_version') then Result := True else Result := False;
    //Result := False;
end;

function findRPN() : String;
var
    s : String;
begin
	s := '';
	if FileExists(GetUserDir()+'bin/rpn') then s := GetUserDir()+'bin/rpn';
	if (s = '') and (FileExists(GetUserDir()+'rpn')) then s := GetUserDir()+'rpn';
	if s = '' then s := FindDefaultExecutablePath('rpn');
	Result := s;
end;

function detectTerminal() : String;
var
    s : String;
begin
    //RunCommand(shell, ['-c', 'ps -o comm= -p "$(($(ps -o ppid= -p "$(($(ps -o sid= -p "$$")))")))"'], terminal);
    s := '';
    if (isDebianBased()) then 
    begin
        s := 'x-terminal-emulator';
    end else begin
	    if FindDefaultExecutablePath('xdg-terminal') <> '' then s := 'xdg-terminal';
        if (s = '') and (FindDefaultExecutablePath('xfce4-terminal') <> '') then s := 'xfce4-terminal';
        if (s = '') and (FindDefaultExecutablePath('mate-terminal') <> '') then s := 'mate-terminal';
        if (s = '') and (FindDefaultExecutablePath('gnome-terminal') <> '') then s := 'gnome-terminal';
        if (s = '') and (FindDefaultExecutablePath('konsole') <> '') then s := 'konsole';
        if (s = '') and (FindDefaultExecutablePath('guake') <> '') then s := 'guake';
        if (s = '') and (FindDefaultExecutablePath('terminator') <> '') then s := 'terminator';
        if (s = '') and (FindDefaultExecutablePath('tilda') <> '') then s := 'tilda';
        if (s = '') and (FindDefaultExecutablePath('yakuake') <> '') then s := 'yakuake';
        if (s = '') and (FindDefaultExecutablePath('xterm') <> '') then s := 'xterm';
        if (s = '') then s := GetEnvironmentVariable('TERMINAL');
        if (s = '') then s := GetEnvironmentVariable('TERM');
        if (s = '') then s := 'xterm';
        if (LeftStr(s, 5) = 'xterm') then s := 'xterm';
    end;
    Result := trim(s);
end;
{$ENDIF}

procedure TForm1.ButtonTerminalClick(Sender: TObject);
{$IFDEF MSWINDOWS}
var
    Pause : String = '';
begin
    SynEdit1.Lines.SaveToFile('temp.ppsc');
    if (MenuRunPause.Checked) 
        then Pause := ' -P'
        else Pause := '';
    if FileExists('rpn.exe') then
    begin
        if ShellExecute(0,nil, PChar('cmd'),PChar('/c rpn.exe temp.ppsc'+Pause),nil,1) = 0 then;
    end else begin
        writeln('No RPN console app found.'+#13#10+'Add RPN Calculator console app to directory where the GUI app is located.');
        ShowMessage(locale.NoAppFound+#13#10+locale.AddAppDir);
    end;
end;
{$ELSE}
var
    dir, command         : String;
    Pause                : String = '';
    terminal, shell, out : String;
begin
    //dir := GetAppConfigDir(false)+'/';
    //if not DirectoryExists(dir) then
    //begin
    //    if not CreateDir(dir) then
    //    begin
    //        raiserror('Error: Failed to set up a directory for the config file.');
    //        isDir := false;  
    //    end else begin
    //        isDir := true;
    //    end;
    //end else begin
    //    isDir := true;
    //end;
    dir := GetTempDir();
    SynEdit1.Lines.SaveToFile(dir+'rpng_temp.ppsc');
    if (MenuRunPause.Checked) 
        then Pause := ' -P'
        else Pause := '';
    shell := GetEnvironmentVariable('SHELL');
    terminal := detectTerminal();
    writeln('Shell:      ', shell);
    writeln('Terminal:   ', terminal);
    writeln('Temp dir:   ', dir);
    writeln('App dir:    ', GetCurrentDir);
    if FileExists('./rpn') then
    begin
        writeln('Attempting to run RPN Calculator console app from GUI app local directory.');
        command := './rpn '+dir+'rpng_temp.ppsc'+Pause;
        writeln('Command:    ', command);
        if (terminal = 'xdg-terminal') 
            then RunCommand(terminal, [command], out)
            else if (terminal = 'tilda') 
                then RunCommand(terminal, ['-c', command], out)
                else RunCommand(terminal, ['-e', command], out);
        //RunCommand(terminal, ['-e', command], out);
        writeln(out);
    end else if (findRPN() <> '') then begin
        writeln('Attempting to run RPN Calculator console app from $PATH.');
        writeln('If the script does not run, then rpn is not installed in your $PATH.');
        writeln('Executable: ', findRPN());
        command := findRPN()+' '+dir+'rpng_temp.ppsc'+Pause;
        writeln('Command:    ', command);
        if (terminal = 'xdg-terminal') 
            then RunCommand(terminal, [command], out)
            else if (terminal = 'tilda') 
                then RunCommand(terminal, ['-c', command], out)
                else RunCommand(terminal, ['-e', command], out);
        writeln(out);
    end else begin
        writeln('No RPN console app found.'+#13#10+'Add RPN Calculator console app to the $PATH or the directory where the GUI app is located.');
        ShowMessage(locale.NoAppFound+#13#10+locale.AddAppDirPath);
    end;
    //DeleteFile(dir+'rpng_temp.ppsc');
end;
{$ENDIF}

procedure TForm1.MenuAutoClearClick(Sender: TObject);
begin
     MenuAutoClear.Checked := not MenuAutoClear.Checked;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin

end;

procedure TForm1.MenuLangCSB1Click(Sender: TObject);
begin
    locale := GetLocale(L_CSB);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangNOR1Click(Sender: TObject);
begin
    locale := GetLocale(L_NOR);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangCROClick(Sender: TObject);
begin
    locale := GetLocale(L_CRO);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangNOR2Click(Sender: TObject);
begin
    locale := GetLocale(L_NOR2);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangNORClick(Sender: TObject);
begin

end;

procedure TForm1.MenuLangPOR1Click(Sender: TObject);
begin
    locale := GetLocale(L_POR);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangPOR2Click(Sender: TObject);
begin
    locale := GetLocale(L_POR2);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangPORClick(Sender: TObject);
begin

end;

procedure TForm1.MenuLangRUSClick(Sender: TObject);
begin
    locale := GetLocale(L_RUS);
    ApplyLocaleMain(locale);
end;

procedure TForm1.MenuLangAFRClick(Sender: TObject);
begin
    locale := GetLocale(L_AFR);
    ApplyLocaleMain(locale);
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
        if (MenuAutoClear.Checked) then Memo1.Text := '';
        Memo1.Append(PS_parseString(SynEdit1.Text));
        {$IFDEF LINUX}
        // scroll down:
        Memo1.SelStart:=Length(Memo1.lines.Text);
        Memo1.VertScrollBar.Position:=1000000;
         {$ELSE }
         {$ENDIF}
     except
           on E : EAccessViolation do begin
              Memo1.Text := Memo1.Text + locale.WrongPS+'.';
           end;
           on E : Exception do begin
              Memo1.Text := Memo1.Text + E.Message;
           end;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    write(' - Applying locale... ');
    locale := GetLocale(DetermineLanguage());
    ApplyLocaleMain(locale);
    writeln('Done.');
    write(' - Setting up fonts... ');
    {$IFDEF MSWINDOWS}
    Memo1.Font.Name := 'Consolas';
    ProcessExt.CommandLine := 'rpn.exe "2 2 +"';
    //{$ENDIF}
    //{$IFDEF UNIX}
    {$ELSE}
    Memo1.Font.Name := 'Monospace';
    ProcessExt.CommandLine := 'rpn "2 2 +"';
    {$ENDIF}
    writeln('Done.');
    write(' - Setting up text... ');
    SynEdit1.Text := '3 2 + times {'+#13#10+'  "Hello world!" println '+#13#10+'}'+#13#10+'10 times rand'+#13#10+'all sum';
    Memo1.Text := '';
    writeln('Done.');
end;

procedure TForm1.MenuQuitClick(Sender: TObject);
begin
     //Close;
     if SynEdit1.Modified then begin
        if mrOK=MessageDlg(locale.QuestionMark2 + locale.AreYouSureQuitSave + locale.QuestionMark,
                           mtConfirmation,
                           [mbOK,mbCancel],
                           0) then
        begin
             Close();
        end;
     end else begin
         Close();
     end;
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
        if mrOK=MessageDlg(locale.QuestionMark2 + locale.AreYouSureContSave + locale.QuestionMark, 
                           mtConfirmation, 
                           [mbOK,mbCancel], 
                           0) then
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
