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
  Form1: TForm1;

implementation
uses Unit2, Unit3;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
     try
        Edit2.Text := FloatToStr(calc_parseRPN(Edit1.Text));
     except
     on Exception do begin
       Edit1.Text := '';
       Edit2.Text := '';
     end;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     Unit3.setENG();
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
     Close;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
     Unit3.setENG();
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
     Unit3.setPOL();
end;

end.

