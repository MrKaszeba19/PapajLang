unit Unit6;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RPNAbout;

type

  { TForm3 }

  TForm3 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form3: TForm3;

implementation

uses Unit3, Unit1;

{$R *.lfm}

{ TForm3 }

function convertToMDY(date : String) : String;
begin
    Result := Copy(date, 6, 2)+'/'+Copy(date, 9, 2)+'/'+Copy(date, 1, 4);
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
    Close();
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
    if not RPN_isStable
       then Label2.Caption := 'Version '+RPN_version+' ('+RPN_codename+'), unstable testing build'
       else if RPN_update > 0
            then Label2.Caption := 'Version '+RPN_version+' ('+RPN_codename+'), update #'+IntToStr(RPN_update)
            else Label2.Caption := 'Version '+RPN_version+' ('+RPN_codename+')';
    Label6.Caption := 'for '+RPN_targetOS+' on '+RPN_targetCPU;
    if RPN_update > 0
       then Label4.Caption := 'Released on '+convertToMDY(RPN_date)+', updated on '+convertToMDY(RPN_updated)
       else Label4.Caption := 'Released on '+convertToMDY(RPN_date);
    ApplyLocaleAbout(Unit1.locale);
end;

end.

