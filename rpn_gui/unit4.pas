unit Unit4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UnitEntity;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
    Unit1, Unit2, Unit3, Unit5;

{$R *.lfm}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
var 
	prevent : Integer;
	mask    : String;
begin
     try
        //Unit5.arax := Unit2.PS_parseString(Form2.Edit1.Text);
        Unit5.arax := Form2.Edit1.Text;
        Close;
     except
        ShowMessage(locale.ExclamationMark2 + Unit1.locale.Error + Unit1.locale.ExclamationMark);
     end;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
     Edit1.Text := '';
     ApplyLocaleScan(Unit1.locale);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
     Edit1.Text := '';
end;

end.

