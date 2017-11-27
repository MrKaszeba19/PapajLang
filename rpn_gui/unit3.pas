unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure setENG();
procedure setPOL();

implementation
uses Unit1;

procedure setENG();
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

procedure setPOL();
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

end.

