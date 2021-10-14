unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure set1DEN();
procedure set1ENG();
procedure set1FRA();
procedure set1GER();
procedure set1NED();
procedure set1POL();
procedure set1HBR();
procedure set1CSB();
procedure set1CSB2();
procedure set1CSB3();

procedure set2DEN();
procedure set2ENG();
procedure set2FRA();
procedure set2GER();
procedure set2NED();
procedure set2POL();
procedure set2HBR();
procedure set2CSB();
procedure set2CSB2();
procedure set2CSB3();

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

procedure set1FRA();
begin
     Unit1.Form1.Caption := 'La calculatrice NPI – Interpréteur PapajScript';
     Unit1.Form1.Label1.Caption := 'PS Expression';
     Unit1.Form1.Edit1.TextHint  := 'Tapez une expression délimitée par des espaces, ex. "2 3 +" ou "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Résultat';
     Unit1.Form1.Button1.Caption := 'Comptons!';
     Unit1.Form1.MenuItem1.Caption := 'Application';
     Unit1.Form1.MenuItem2.Caption := 'Langue';
     Unit1.Form1.MenuQuit.Caption := 'Fermer';
     Unit1.Form1.Button2.Caption := 'Exécuter';//'Exécuter un script';
     Unit1.Form1.Label2.Caption := 'PapajScript code';
     Unit1.Form1.MenuLoad.Caption := 'Charger un script à partir d''un fichier';
     Unit1.Form1.MenuSave.Caption := 'Enregistrer un script dans un fichier';
     Unit1.Form1.OpenDialog1.Title := 'Charger un script à partir d''un fichier';
     Unit1.Form1.SaveDialog1.Title := 'Enregistrer un script dans un fichier';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1GER();
begin
     Unit1.Form1.Caption := 'UPN-Rechner – PapajScript';
     Unit1.Form1.Label1.Caption := 'PS-Ausdruck';
     Unit1.Form1.Edit1.TextHint  := 'Geben Sie einen durch Leerzeichen getrennten PS-Ausdruck ein, e.g. "2 3 +" oder "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Resultat';
     Unit1.Form1.Button1.Caption := 'Zähle!';
     Unit1.Form1.MenuItem1.Caption := 'Anwendung';
     Unit1.Form1.MenuItem2.Caption := 'Sprache';
     Unit1.Form1.MenuQuit.Caption := 'Schließen';
     Unit1.Form1.Button2.Caption := 'Skript ausführen';
     Unit1.Form1.Label2.Caption := 'PapajScript-Code';
     Unit1.Form1.MenuLoad.Caption := 'Skript aus Datei laden';
     Unit1.Form1.MenuSave.Caption := 'Skript in Datei speichern';
     Unit1.Form1.OpenDialog1.Title := 'Skript aus Datei laden';
     Unit1.Form1.SaveDialog1.Title := 'Skript in Datei speichern';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1NED();
begin
     Unit1.Form1.Caption := 'OPN-calculator – PapajScript';
     Unit1.Form1.Label1.Caption := 'PS-uitdrukking';
     Unit1.Form1.Edit1.TextHint  := 'Typ een PS-uitdrukking die wordt gescheiden door spaties, bijv. "2 3 +" of "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Resultaat';
     Unit1.Form1.Button1.Caption := 'Bereken het!';
     Unit1.Form1.MenuItem1.Caption := 'Toepassing';
     Unit1.Form1.MenuItem2.Caption := 'Sprache';
     Unit1.Form1.MenuQuit.Caption := 'Sluiten';
     Unit1.Form1.Button2.Caption := 'Script uitvoeren';
     Unit1.Form1.Label2.Caption := 'PapajScript-code';
     Unit1.Form1.MenuLoad.Caption := 'Script uit bestand laden';
     Unit1.Form1.MenuSave.Caption := 'Script in bestand opslaan';
     Unit1.Form1.OpenDialog1.Title := 'Script uit bestand laden';
     Unit1.Form1.SaveDialog1.Title := 'Script in bestand opslaan';
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

procedure set1CSB();
begin
     Unit1.Form1.Caption := 'Kalkùlatór OPN – PapajScript';
     Unit1.Form1.Label1.Caption := 'Wësłów PS';
     Unit1.Form1.Edit1.TextHint  := 'Nôpisze wësłów OPN rozdzélony spacëjama, np. "2 3 +" lub "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Windzenié';
     Unit1.Form1.Button1.Caption := 'Rëchuj!';
     Unit1.Form1.MenuItem1.Caption := 'Aplikacëjô';
     Unit1.Form1.MenuItem2.Caption := 'Jãzëk';
     Unit1.Form1.MenuQuit.Caption := 'Zamkni';
     Unit1.Form1.Button2.Caption := 'Zrësz';
     Unit1.Form1.Label2.Caption := 'PapajScriptòwi kòd';
     Unit1.Form1.MenuLoad.Caption := 'Wczëtôj skript z lopkù';
     Unit1.Form1.MenuSave.Caption := 'Zôpisze skript do lopkù';
     Unit1.Form1.OpenDialog1.Title := 'Wczëtôj skript z lopkù';
     Unit1.Form1.SaveDialog1.Title := 'Zôpisze skript do lopkù';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1CSB2();
begin
     Unit1.Form1.Caption := 'Kalkulator OPN – PapajScript';
     Unit1.Form1.Label1.Caption := 'Vësłóv PS';
     Unit1.Form1.Edit1.TextHint  := 'Napiše vësłóv OPN rozdzeloni spacijama, np. "2 3 +" lub "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'Vindzenje';
     Unit1.Form1.Button1.Caption := 'Rëchuj!';
     Unit1.Form1.MenuItem1.Caption := 'Aplikacijô';
     Unit1.Form1.MenuItem2.Caption := 'Jãzik';
     Unit1.Form1.MenuQuit.Caption := 'Zamkni';
     Unit1.Form1.Button2.Caption := 'Zreš';
     Unit1.Form1.Label2.Caption := 'PapajScriptovi kod';
     Unit1.Form1.MenuLoad.Caption := 'Včëtô skript z lopka';
     Unit1.Form1.MenuSave.Caption := 'Zapiše skript do lopka';
     Unit1.Form1.OpenDialog1.Title := 'Včëtô skript z lopka';
     Unit1.Form1.SaveDialog1.Title := 'Zapiše skript do lopka';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
end;

procedure set1CSB3();
begin
     Unit1.Form1.Caption := 'Каљкулатор ОПН – PapajScript';
     Unit1.Form1.Label1.Caption := 'Вёслóв PS';
     Unit1.Form1.Edit1.TextHint  := 'Напише вёслóв ОПН розѕељони спацијама, нп. "2 3 +" луб "20 4 / 5 +"';
     Unit1.Form1.Edit2.TextHint  := 'винѕење';
     Unit1.Form1.Button1.Caption := 'Рёхуј!';
     Unit1.Form1.MenuItem1.Caption := 'Апликацијô';
     Unit1.Form1.MenuItem2.Caption := 'Јãзик';
     Unit1.Form1.MenuQuit.Caption := 'Замкни';
     Unit1.Form1.Button2.Caption := 'Зреш';
     Unit1.Form1.Label2.Caption := 'ПапајСцриптови код';
     Unit1.Form1.MenuLoad.Caption := 'Вчётô скрипт з лопка';
     Unit1.Form1.MenuSave.Caption := 'Запише скрипт до лопка';
     Unit1.Form1.OpenDialog1.Title := 'Вчётô скрипт з лопка';
     Unit1.Form1.SaveDialog1.Title := 'Запише скрипт до лопка';
     Unit1.Form1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
     Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
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

procedure set2ENG();
begin
     Unit4.Form2.Caption := 'Scan an expression';
     Unit4.Form2.Label1.Caption := 'Expression';
     Unit4.Form2.Button1.Caption := 'Submit';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2FRA();
begin
     Unit4.Form2.Caption := 'Mettez une expression';
     Unit4.Form2.Label1.Caption := 'Expression';
     Unit4.Form2.Button1.Caption := 'Soumettre';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2GER();
begin
     Unit4.Form2.Caption := 'Scanne einen Ausdruck hier';
     Unit4.Form2.Label1.Caption := 'Ausdruck';
     Unit4.Form2.Button1.Caption := 'Senden';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2NED();
begin
     Unit4.Form2.Caption := 'Een uitdrukking scannen';
     Unit4.Form2.Label1.Caption := 'Uitdrukking';
     Unit4.Form2.Button1.Caption := 'Indienen';
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

procedure set2CSB();
begin
     Unit4.Form2.Caption := 'Wczëtôj wësłów';
     Unit4.Form2.Label1.Caption := 'Wësłów';
     Unit4.Form2.Button1.Caption := 'Zacwierdze';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2CSB2();
begin
     Unit4.Form2.Caption := 'Včëtô vësłóv';
     Unit4.Form2.Label1.Caption := 'Vësłóv';
     Unit4.Form2.Button1.Caption := 'Zacvjerdze';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;

procedure set2CSB3();
begin
     Unit4.Form2.Caption := 'Вчётô вёслóв';
     Unit4.Form2.Label1.Caption := 'вёслóв';
     Unit4.Form2.Button1.Caption := 'Зацвјерѕе';
     Unit4.Form2.BiDiMode := bdLeftToRight;
     Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
     Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
end;



end.
