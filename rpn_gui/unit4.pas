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
        Unit5.arax := Unit2.PS_parseString(Form2.Edit1.Text);
        Close;
     except
        ShowMessage('Error!');
     end;
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
     Edit1.Text := '';
     case Unit1.language of
          'den' : Unit3.set2DEN();
          'eng' : Unit3.set2ENG();
          'pol' : Unit3.set2POL();
          else Unit3.set2ENG();
     end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
     Edit1.Text := '';
end;

end.

