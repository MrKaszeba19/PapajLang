unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure set1DEN();
procedure set1ENG();
procedure set1POL();
procedure set1HBR();

procedure set2DEN();
procedure set2ENG();
procedure set2POL();
procedure set2HBR();

implementation
uses Unit1, Unit4;

procedure set1DEN();
begin
     Unit1.Form1.Caption := 'OPN-Lommeregner – PapajScript';
     Unit1.Form1.Label1.Caption := 'OPN-udtryk';
     Unit1.Form1.Edit1.TextHint  := 'Indtast et PS-udtryk afgrænset af mellemrum, f.eks. "2 3 +" eller "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Resultat';
     Unit1.Form1.Button1.Caption := 'Tæl det!';
     Unit1.Form1.MenuItem1.Caption := 'Applikation';
     Unit1.Form1.MenuItem2.Caption := 'Sprog';
     Unit1.Form1.MenuQuit.Caption := 'Luk';
     Unit1.Form1.Button2.Caption := 'Kør scripten';
     Unit1.Form1.Label2.Caption := 'PapajScript-kode';
     Unit1.Form1.MenuLoad.Caption := 'Hent scripten fra en fil';
     Unit1.Form1.MenuSave.Caption := 'Gem scripten som en fil';
     Unit1.Form1.OpenDialog1.Title := 'Hent scripten fra en fil';
     Unit1.Form1.SaveDialog1.Title := 'Gem scripten som en fil';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1ENG();
begin
     Unit1.Form1.Caption := 'RPN Calculator – PapajScript';
     Unit1.Form1.Label1.Caption := 'PS Expression';
     Unit1.Form1.Edit1.TextHint  := 'Type a PS expression delimited by spaces, e.g. "2 3 +" or "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Result';
     Unit1.Form1.Button1.Caption := 'Count it!';
     Unit1.Form1.MenuItem1.Caption := 'Application';
     Unit1.Form1.MenuItem2.Caption := 'Language';
     Unit1.Form1.MenuQuit.Caption := 'Close';
     Unit1.Form1.Button2.Caption := 'Run script';
     Unit1.Form1.Label2.Caption := 'PapajScript code';
     Unit1.Form1.MenuLoad.Caption := 'Load script from file';
     Unit1.Form1.MenuSave.Caption := 'Save script to file';
     Unit1.Form1.OpenDialog1.Title := 'Load script from file';
     Unit1.Form1.SaveDialog1.Title := 'Save script to file';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1POL();
begin
     Unit1.Form1.Caption := 'Kalkulator ONP – PapajScript';
     Unit1.Form1.Label1.Caption := 'Wyrażenie PS';
     Unit1.Form1.Edit1.TextHint  := 'Napisz wyrażenie ONP rozdzielone spacjami, np. "2 3 +" or "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Wynik';
     Unit1.Form1.Button1.Caption := 'Policz!';
     Unit1.Form1.MenuItem1.Caption := 'Aplikacja';
     Unit1.Form1.MenuItem2.Caption := 'Język';
     Unit1.Form1.MenuQuit.Caption := 'Zamknij';
     Unit1.Form1.Button2.Caption := 'Uruchom';
     Unit1.Form1.Label2.Caption := 'Kod PapajScript';
     Unit1.Form1.MenuLoad.Caption := 'Wczytaj skrypt z pliku';
     Unit1.Form1.MenuSave.Caption := 'Zapisz skrypt do pliku';
     Unit1.Form1.OpenDialog1.Title := 'Wczytaj skrypt z pliku';
     Unit1.Form1.SaveDialog1.Title := 'Zapisz skrypt do pliku';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1HBR();
begin
     Unit1.Form1.Caption := 'מחשבון RPN – PapajScript';
     Unit1.Form1.Label1.Caption := 'ביטוי PS';
     Unit1.Form1.Edit1.TextHint  := 'הקלד ביטוי שמופרד במרווחים, לדוגמה "2 3 +" או "20 4 / 5 +", כאן.';
     Unit1.Form1.Edit2.TextHint  := 'תוצאה';
     Unit1.Form1.Button1.Caption := 'תחשיב!';
     Unit1.Form1.MenuItem1.Caption := 'אפליקציה';
     Unit1.Form1.MenuItem2.Caption := 'שפה';
     Unit1.Form1.MenuQuit.Caption := 'סגור';
     Unit1.Form1.Button2.Caption := 'הפעל';
     Unit1.Form1.Label2.Caption := 'קוד PapajScript';
     Unit1.Form1.MenuLoad.Caption := 'העלה מקובץ';
     Unit1.Form1.MenuSave.Caption := 'שמור לקובץ';
     Unit1.Form1.OpenDialog1.Title := 'העלה מקובץ';
     Unit1.Form1.SaveDialog1.Title := 'שמור לקובץ';
     Unit1.Form1.BiDiMode := bdRightToLeft;
     Unit1.Form1.Label1.BiDiMode := bdRightToLeft;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set2ENG();
begin
     Unit4.Form2.Caption := 'Scan an expression';
     Unit4.Form2.Label1.Caption := 'Expression';
     Unit4.Form2.Button1.Caption := 'Submit';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2DEN();
begin
     Unit4.Form2.Caption := 'Scan en expression';
     Unit4.Form2.Label1.Caption := 'Expression';
     Unit4.Form2.Button1.Caption := 'Bekræft';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2POL();
begin
     Unit4.Form2.Caption := 'Wczytaj wyrażenie';
     Unit4.Form2.Label1.Caption := 'Wyrażenie';
     Unit4.Form2.Button1.Caption := 'Zatwierdź';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2HBR();
begin
     Unit4.Form2.Caption := 'העלה ביטוי';
     Unit4.Form2.Label1.Caption := 'ביטוי';
     Unit4.Form2.Button1.Caption := 'אישור';
     Unit4.Form2.BiDiMode := bdRightToLeft;
     Unit4.Form2.Label1.BiDiMode := bdRightToLeft;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

end.
