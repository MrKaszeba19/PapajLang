unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure set1DEN();
procedure set1ENG();
procedure set1POL();

procedure set2DEN();
procedure set2ENG();
procedure set2POL();

implementation
uses Unit1, Unit4;

procedure set1DEN();
begin
     Unit1.Form1.Caption := 'OPN-Lommeregner';
     Unit1.Form1.Label1.Caption := 'OPN-udtryk';
     Unit1.Form1.Edit1.TextHint  := 'Indtast et OPN-udtryk afgrænset af mellemrum, f.eks. "2 3 +" eller "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Resultat';
     Unit1.Form1.Button1.Caption := 'Tæl det!';
     Unit1.Form1.MenuItem1.Caption := 'Applikation';
     Unit1.Form1.MenuItem2.Caption := 'Sprog';
     Unit1.Form1.MenuItem3.Caption := 'Luk';
end;

procedure set1ENG();
begin
     Unit1.Form1.Caption := 'RPN Calculator';
     Unit1.Form1.Label1.Caption := 'RPN Expression';
     Unit1.Form1.Edit1.TextHint  := 'Type a RPN expression delimited by spaces, e.g. "2 3 +" or "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Result';
     Unit1.Form1.Button1.Caption := 'Count it!';
     Unit1.Form1.MenuItem1.Caption := 'Application';
     Unit1.Form1.MenuItem2.Caption := 'Language';
     Unit1.Form1.MenuItem3.Caption := 'Close';
end;

procedure set1POL();
begin
     Unit1.Form1.Caption := 'Kalkulator ONP';
     Unit1.Form1.Label1.Caption := 'Wyrażenie ONP';
     Unit1.Form1.Edit1.TextHint  := 'Napisz wyrażenie ONP rozdzielone spacjami, np. "2 3 +" or "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Wynik';
     Unit1.Form1.Button1.Caption := 'Policz!';
     Unit1.Form1.MenuItem1.Caption := 'Aplikacja';
     Unit1.Form1.MenuItem2.Caption := 'Język';
     Unit1.Form1.MenuItem3.Caption := 'Zamknij';
end;

procedure set2ENG();
begin
     Unit4.Form2.Caption := 'Scan an expression';
     Unit4.Form2.Label1.Caption := 'Expression';
     Unit4.Form2.Button1.Caption := 'Submit';
end;

procedure set2DEN();
begin
     Unit4.Form2.Caption := 'Scan en expression';
     Unit4.Form2.Label1.Caption := 'Expression';
     Unit4.Form2.Button1.Caption := 'Bekræft';
end;

procedure set2POL();
begin
     Unit4.Form2.Caption := 'Wczytaj wyrażenie';
     Unit4.Form2.Label1.Caption := 'Wyrażenie';
     Unit4.Form2.Button1.Caption := 'Zatwierdź';
end;

end.
