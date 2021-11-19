unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type Language = (
    L_AFR,
    L_CSB,
    L_CSB2,
    L_CSB3,
    L_DEN,
    L_ENG,
    L_FRA,
    L_GER,
	L_HBR,
    L_ITA,
    L_MKD,
    L_NED,
    L_NOR,
    L_NOR2,
    L_POL,
    L_RUS,
    L_SWE
);

type LangMap = record
    AreYouSureQuit     : String;
    AreYouSureContSave : String;
    AreYouSureQuitSave : String;
    AutoClearTerminal  : String;
    DarkMode           : String;
    Error              : String;
    ErrorLoadFile      : String;
    Expression         : String;
    Load               : String;
    MenuApplication    : String;
    MenuLanguage       : String;
    MenuLoadFile       : String;
    MenuNewFile        : String;
    MenuSaveFile       : String;
    MenuClose          : String;
    MenuView           : String;
    MenuRun            : String;
    No                 : String;
    PSCode             : String;
    Result             : String;
    RunInt             : String;
    RunExt             : String;
    RunScriptInt       : String;
    RunScriptExt       : String;
    SampleCaption      : String;
    SampleCountIt      : String;
    SampleHint         : String;
    Save               : String;
    Submit             : String;
    WindowMainName     : String;
    WindowScanName     : String;
    WrongPS            : String;
    Yes                : String;
    isRightToLeft      : Boolean;
    ExclamationMark    : String;
    ExclamationMark2   : String;
    QuestionMark       : String;
    QuestionMark2      : String;
end;

function DetermineLanguage() : Language;
function GetLocale(lang : Language) : LangMap;
procedure ApplyLocaleMain(lang : LangMap);
procedure ApplyLocaleScan(lang : LangMap);

implementation
uses Unit1, Unit4;

// NEW LANG ENGINE

// translation

function langDanish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Er du sikker på, at du vil afslutte programmet';
    Result.AreYouSureContSave := 'Er du sikker på, at du vil fortsætte uden at gemme';
    Result.AreYouSureQuitSave := 'Er du sikker på, at du vil lukke filen uden at gemme';
    Result.AutoClearTerminal  := 'Tøm terminal, før du kører et script';
    Result.DarkMode           := 'Mørkt tema';
    Result.Error              := 'Fejl';
    Result.ErrorLoadFile      := 'Fejl ved indlæsning af en fil';
    Result.Expression         := 'Udtryk';
    Result.Load               := 'Hent';
    Result.MenuApplication    := 'Applikation';
    Result.MenuLanguage       := 'Sprog';
    Result.MenuLoadFile       := 'Hent scripten fra en fil';
    Result.MenuNewFile        := 'Ny fil';
    Result.MenuSaveFile       := 'Gem scripten som en fil';
    Result.MenuClose          := 'Luk';
    Result.MenuView           := 'Vis';
    Result.MenuRun            := 'Kør';
    Result.No                 := 'Nej';
    Result.PSCode             := 'PapajScript-kode';
    Result.Result             := 'Resultat';
    Result.RunInt             := 'Kør';
    Result.RunExt             := 'Kør eksternt';
    Result.RunScriptInt       := 'Kør scripten';
    Result.RunScriptExt       := 'Kør scripten i et eksternt vindue';
    Result.SampleCaption      := 'PS-udtryk';
    Result.SampleCountIt      := 'Tæl det!';
    Result.SampleHint         := 'Indtast et PS-udtryk afgrænset af mellemrum, f.eks. "2 3 +" eller "20 4 / 5 +"';
    Result.Save               := 'Gem';
    Result.Submit             := 'Bekræft';
    Result.WindowMainName     := 'OPN-Lommeregner – PapajScript';
    Result.WindowScanName     := 'Scan en expression';
    Result.WrongPS            := 'Forkert PS-udtryk';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langEnglish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Are you sure you want to close the application';
    Result.AreYouSureContSave := 'Are you sure you want to conitinue without saving the file';
    Result.AreYouSureQuitSave := 'Are you sure you want to close the application without saving the file';
    Result.AutoClearTerminal  := 'Clear terminal before running a script';
    Result.DarkMode           := 'Dark theme';
    Result.Error              := 'Error';
    Result.ErrorLoadFile      := 'Error when loading a file';
    Result.Expression         := 'Expression';
    Result.Load               := 'Load';
    Result.MenuApplication    := 'Application';
    Result.MenuLanguage       := 'Language';
    Result.MenuLoadFile       := 'Load script from file';
    Result.MenuNewFile        := 'New file';
    Result.MenuSaveFile       := 'Save script to file';
    Result.MenuClose          := 'Close';
    Result.MenuView           := 'View';
    Result.MenuRun            := 'Run';
    Result.No                 := 'No';
    Result.PSCode             := 'PapajScript code';
    Result.Result             := 'Result';
    Result.RunInt             := 'Run';
    Result.RunExt             := 'Run in terminal';
    Result.RunScriptInt       := 'Run script';
    Result.RunScriptExt       := 'Run script in an external window';
    Result.SampleCaption      := 'PS Expression';
    Result.SampleCountIt      := 'Count it!';
    Result.SampleHint         := 'Type a PS expression delimited by spaces, e.g. "2 3 +" or "20 4 / 5 +"';
    Result.Save               := 'Save';
    Result.Submit             := 'Submit';
    Result.WindowMainName     := 'RPN Calculator – PapajScript interpreter';
    Result.WindowScanName     := 'Scan an expression';
    Result.WrongPS            := 'Wrong PS expression';
    Result.Yes                := 'Yes';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langFrench() : LangMap;
begin
    Result.AreYouSureQuit     := 'Êtes-vous sûr de vouloir quitter';
    Result.AreYouSureContSave := 'Êtes-vous sûr de vouloir continuer sans l''enregistrer';
    Result.AreYouSureQuitSave := 'Êtes-vous sûr de vouloir fermer le fichier sans l''enregistrer';
    Result.AutoClearTerminal  := 'Effacer le terminal avant d''exécuter un script';
    Result.DarkMode           := 'Thème sombre';
    Result.Error              := 'Erreur';
    Result.ErrorLoadFile      := 'Erreur lors du chargement d''un fichier';
    Result.Expression         := 'Expression';
    Result.Load               := 'Charger';
    Result.MenuApplication    := 'Application';
    Result.MenuLanguage       := 'Langue';
    Result.MenuLoadFile       := 'Charger un script à partir d''un fichier';
    Result.MenuNewFile        := 'Nouveau fichier';
    Result.MenuSaveFile       := 'Enregistrer un script dans un fichier';
    Result.MenuClose          := 'Fermer';
    Result.MenuView           := 'Voir'; // vue
    Result.MenuRun            := 'Exécution';
    Result.No                 := 'Non';
    Result.PSCode             := 'PapajScript code';
    Result.Result             := 'Résultat';
    Result.RunInt             := 'Exécuter';
    Result.RunExt             := 'Exécuter externement';
    Result.RunScriptInt       := 'Exécuter le script';
    Result.RunScriptExt       := 'Exécuter le script dans une fenêtre externe';
    Result.SampleCaption      := 'Expression PS';
    Result.SampleCountIt      := 'Comptons !';
    Result.SampleHint         := 'Tapez une expression délimitée par des espaces, ex. "2 3 +" ou "20 4 / 5 +"';
    Result.Save               := 'Enregistrer';
    Result.Submit             := 'Soumettre';
    Result.WindowMainName     := 'La calculatrice NPI – Interpréteur du PapajScript';
    Result.WindowScanName     := 'Mettez une expression';
    Result.WrongPS            := 'Expression PS incorrecte';
    Result.Yes                := 'Yes';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := ' !';
    Result.QuestionMark       := ' ?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langGerman() : LangMap;
begin
    Result.AreYouSureQuit     := 'Möchten Sie die Anwendung wirklich schließen';
    Result.AreYouSureContSave := 'Möchten Sie wirklich ohne Speichern fortsetzen.';
    Result.AreYouSureQuitSave := 'Möchten Sie die Datei wirklich ohne Speichern schließen';
    Result.AutoClearTerminal  := 'Bevor ein Skript ausgeführt wird, das Terminal leeren';
    Result.DarkMode           := 'Dunkles Thema';
    Result.Error              := 'Error';
    Result.ErrorLoadFile      := 'Fehler beim Laden einer Datei';
    Result.Expression         := 'Ausdruck';
    Result.Load               := 'Laden';
    Result.MenuApplication    := 'Anwendung';
    Result.MenuLanguage       := 'Sprache';
    Result.MenuLoadFile       := 'Skript aus Datei laden';
    Result.MenuNewFile        := 'Neue Datei';
    Result.MenuSaveFile       := 'Skript in Datei speichern';
    Result.MenuClose          := 'Schließen';
    Result.MenuView           := 'Ansicht';
    Result.MenuRun            := 'Ausführung';
    Result.No                 := 'Nein';
    Result.PSCode             := 'PapajScript-Code';
    Result.Result             := 'Resultat';
    Result.RunInt             := 'Ausführen';
    Result.RunExt             := 'Extern ausführen';
    Result.RunScriptInt       := 'Skript ausführen';
    Result.RunScriptExt       := 'Skript in einem externen Fenster ausführen';
    Result.SampleCaption      := 'PS-Ausdruck';
    Result.SampleCountIt      := 'Zähle!';
    Result.SampleHint         := 'Geben Sie einen durch Leerzeichen getrennten PS-Ausdruck ein, e.g. "2 3 +" oder "20 4 / 5 +"';
    Result.Save               := 'Speichern';
    Result.Submit             := 'Senden';
    Result.WindowMainName     := 'UPN-Rechner – PapajScript';
    Result.WindowScanName     := 'Scanne einen Ausdruck hier';
    Result.WrongPS            := 'Falscher PS-Ausdruck';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langHebrew() : LangMap;
begin
    Result.AreYouSureQuit     := 'האם את/ה בטוח/ה שברצונך לסגור את האפליקציה';
    Result.AreYouSureContSave := 'האם את/ה בטוח/ה שברצונך להמשיך מבלי לשמור';
    Result.AreYouSureQuitSave := 'האם את/ה בטוח/ה שברצונך לסגור את הקובץ מבלי לשמור';
    Result.AutoClearTerminal  := 'נקה את הטרמינל לפני הפעלת סקריפט';
    Result.DarkMode           := 'ערכת נושא כהה';
    Result.Error              := 'שגיאה';
    Result.ErrorLoadFile      := 'שגיאה בעת טעינת קובץ';
    Result.Expression         := 'ביטוי';
    Result.Load               := 'העלה';
    Result.MenuApplication    := 'אפליקציה';
    Result.MenuLanguage       := 'שפה';
    Result.MenuLoadFile       := 'העלה מקובץ';
    Result.MenuNewFile        := 'קובץ חדש';
    Result.MenuSaveFile       := 'שמור לקובץ';
    Result.MenuClose          := 'סגור';
    Result.MenuView           := 'הופעה';
    Result.MenuRun            := 'הפעלה';
    Result.No                 := 'לא';
    Result.PSCode             := 'קוד פאפייסקריפט';
    Result.Result             := 'תוצאה';
    Result.RunInt             := 'הפעל';
    Result.RunExt             := 'הפעל חיצונית';
    Result.RunScriptInt       := 'הפעל סקריפט';
    Result.RunScriptExt       := 'הפעל סקריפט בחלון חיצוני';
    Result.SampleCaption      := 'ביטוי PS';
    Result.SampleCountIt      := 'תחשיב!';
    Result.SampleHint         := 'הקלד ביטוי שמופרד במרווחים, לדוגמה "2 3 +" או "20 4 / 5 +", כאן.';
    Result.Save               := 'שמור';
    Result.Submit             := 'אישור';
    Result.WindowMainName     := 'מחשבון RPN – PapajScript';
    Result.WindowScanName     := 'העלה ביטוי';
    Result.WrongPS            := 'ביטוי PS שגוי';
    Result.Yes                := 'כן';
    Result.isRightToLeft      := true;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langDutch() : LangMap;
begin
    Result.AreYouSureQuit     := 'Weet u zeker dat u wilt afsluiten';
    Result.AreYouSureContSave := 'Weet u zeker dat u wilt doorgaan zonder op te slaan';
    Result.AreYouSureQuitSave := 'Weet u zeker dat u het bestand wilt sluiten zonder op te slaan';
    Result.AutoClearTerminal  := 'Terminal wissen voordat u een script uitvoert';
    Result.DarkMode           := 'Donker thema';
    Result.Error              := 'Fout';
    Result.ErrorLoadFile      := 'Fout bij het laden van een bestand';
    Result.Expression         := 'Uitdrukking';
    Result.Load               := 'Laden';
    Result.MenuApplication    := 'Toepassing';
    Result.MenuLanguage       := 'Taal';
    Result.MenuLoadFile       := 'Script uit bestand laden';
    Result.MenuNewFile        := 'Nieuw bestand';
    Result.MenuSaveFile       := 'Script in bestand opslaan';
    Result.MenuClose          := 'Sluiten';
    Result.MenuView           := 'Weergave';
    Result.MenuRun            := 'Uitvoeren';
    Result.No                 := 'Nee';
    Result.PSCode             := 'PapajScript-code';
    Result.Result             := 'Resultaat';
    Result.RunInt             := 'Uitvoeren';
    Result.RunExt             := 'Extern uitvoeren';
    Result.RunScriptInt       := 'Script uitvoeren';
    Result.RunScriptExt       := 'Script uitvoeren in een extern venster';
    Result.SampleCaption      := 'PS-uitdrukking';
    Result.SampleCountIt      := 'Bereken het!';
    Result.SampleHint         := 'Typ een PS-uitdrukking die wordt gescheiden door spaties, bijv. "2 3 +" of "20 4 / 5 +"';
    Result.Save               := 'Opslaan';
    Result.Submit             := 'Indienen';
    Result.WindowMainName     := 'OPN-calculator – PapajScript';
    Result.WindowScanName     := 'Een uitdrukking scannen';
    Result.WrongPS            := 'Verkeerde PS-uitdrukking';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;


function langPolish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Czy jesteś pewny/a, że chcesz zamknąć aplikację';
    Result.AreYouSureContSave := 'Czy jesteś pewny/a, że chcesz kontynuować bez zapisywania pliku';
    Result.AreYouSureQuitSave := 'Czy jesteś pewny/a, że chcesz zamknąć aplikację bez zapisywania pliku';
    Result.AutoClearTerminal  := 'Wyczyść terminal przed uruchomieniem skryptu';
    Result.DarkMode           := 'Tryb ciemny';
    Result.Error              := 'Błąd';
    Result.ErrorLoadFile      := 'Błąd przy wczytywaniu pliku';
    Result.Expression         := 'Wyrażenie';
    Result.Load               := 'Wczytaj';
    Result.MenuApplication    := 'Aplikacja';
    Result.MenuLanguage       := 'Język';
    Result.MenuLoadFile       := 'Wczytaj skrypt z pliku';
    Result.MenuNewFile        := 'Nowy plik';
    Result.MenuSaveFile       := 'Zapisz skrypt do pliku';
    Result.MenuClose          := 'Zamknij';
    Result.MenuView           := 'Widok';
    Result.MenuRun            := 'Uruchomienie';
    Result.No                 := 'Nie';
    Result.PSCode             := 'Kod PapajScript';
    Result.Result             := 'Wynik';
    Result.RunInt             := 'Uruchom';
    Result.RunExt             := 'Uruchom zewnętrznie';
    Result.RunScriptInt       := 'Uruchom skrypt';
    Result.RunScriptExt       := 'Uruchom skrypt w terminalu zewnętrznym';
    Result.SampleCaption      := 'Wyrażenie PS';
    Result.SampleCountIt      := 'Liczymy!';
    Result.SampleHint         := 'Napisz wyrażenie ONP rozdzielone spacjami, np. "2 3 +" or "20 4 / 5 +"';
    Result.Save               := 'Zapisz';
    Result.Submit             := 'Zatwierdź';
    Result.WindowMainName     := 'Kalkulator ONP – Interpreter PapajScript';
    Result.WindowScanName     := 'Wczytaj wyrażenie';
    Result.WrongPS            := 'Błędne wyrażenie PS';
    Result.Yes                := 'Tak';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langKashubian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Jes të pewny/ô, że të chcész aplikacëjã zamknąc';
    Result.AreYouSureContSave := 'Jes të pewny/ô, że të chcész jisc dali bez zôpisënkù lopka';
    Result.AreYouSureQuitSave := 'Jes të pewny/ô, że të chcész aplikacëjã zamknąc bez zôpisënkù lopka';
    Result.AutoClearTerminal  := 'Wëcziszczë terminala przed zreszëniém skripta';
    Result.DarkMode           := 'Cemni trib';
    Result.Error              := 'Féla';
    Result.ErrorLoadFile      := 'Féla przë wczëtônkù lopka';
    Result.Expression         := 'Wësłów';
    Result.Load               := 'Wczëtôj';
    Result.MenuApplication    := 'Aplikacëjô';
    Result.MenuLanguage       := 'Jãzëk';
    Result.MenuLoadFile       := 'Wczëtôj skripta z lopka';
    Result.MenuNewFile        := 'Nowi lopk';
    Result.MenuSaveFile       := 'Zôpisze skripta do lopka';
    Result.MenuClose          := 'Zamkni';
    Result.MenuView           := 'Wëzdrzatk';
    Result.MenuRun            := 'Zrëszenié';
    Result.No                 := 'Nié';
    Result.PSCode             := 'PapajScriptowi kòd';
    Result.Result             := 'Rezultat';
    Result.RunInt             := 'Zrësz';
    Result.RunExt             := 'Zrësz bùtnówò';
    Result.RunScriptInt       := 'Zrësz skripta';
    Result.RunScriptExt       := 'Zrësz skripta w bùtnowim òknié';
    Result.SampleCaption      := 'Wësłów PS';
    Result.SampleCountIt      := 'Rëchuj!';
    Result.SampleHint         := 'Nôpisze wësłów PS rozdzélony spacëjama, np. "2 3 +" lub "20 4 / 5 +"';
    Result.Save               := 'Zôpisze';
    Result.Submit             := 'Zacwierdze';
    Result.WindowMainName     := 'Kalkùlatór OPN – interpreter jãzëka PapajScript';
    Result.WindowScanName     := 'Wczëtôj wësłów';
    Result.WrongPS            := 'Félni wësłów PS';
    Result.Yes                := 'Jo';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langKashubianNew() : LangMap;
begin
    Result.AreYouSureQuit     := 'Jes të pevni/ô, že të chceš aplikacijã zamknąc';
    Result.AreYouSureContSave := 'Jes të pewny/ô, że të chcész jisc dalji bez zôpisënkù lopka';
    Result.AreYouSureQuitSave := 'Jes të pevni/ô, že të chceš aplikacijã zamknąc bez zapisënka lopka';
    Result.AutoClearTerminal  := 'Vëčišči terminala przed zrešenjem skripta';
    Result.DarkMode           := 'Cemni trib';
    Result.Error              := 'Fela';
    Result.ErrorLoadFile      := 'Fela przi včëtônku lopka';
    Result.Expression         := 'Vësłóv';
    Result.Load               := 'Včëtôj';
    Result.MenuApplication    := 'Aplikacijô';
    Result.MenuLanguage       := 'Jãzik';
    Result.MenuLoadFile       := 'Včëtôj skripta z lopka';
    Result.MenuNewFile        := 'Novi lopk';
    Result.MenuSaveFile       := 'Zapiše skripta do lopka';
    Result.MenuClose          := 'Zamkni';
    Result.MenuView           := 'Vëzdrjatk';
    Result.MenuRun            := 'Zrëšenje';
    Result.No                 := 'Nje';
    Result.PSCode             := 'PapajScriptovi kod';
    Result.Result             := 'Rezultat';
    Result.RunInt             := 'Zrëš';
    Result.RunExt             := 'Zrëš butnovo';
    Result.RunScriptInt       := 'Zrëš skripta';
    Result.RunScriptExt       := 'Zrëš skripta v butnovim oknje';
    Result.SampleCaption      := 'Vësłóv PS';
    Result.SampleCountIt      := 'Rëchuj!';
    Result.SampleHint         := 'Napiše vësłóv PS rozdzeloni spacijama, np. "2 3 +" lub "20 4 / 5 +"';
    Result.Save               := 'Zapiše';
    Result.Submit             := 'Zacvjerdze';
    Result.WindowMainName     := 'Kalkulator OPN – interpreter jãzika PapajScript';
    Result.WindowScanName     := 'Včëtôj vësłóv';
    Result.WrongPS            := 'Felni ësłóv PS';
    Result.Yes                := 'Jo';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langKashubianCyrilic() : LangMap;
begin
    Result.AreYouSureQuit     := 'Јес тё певни/ô, же тё чцеш апљикација̃ замкну̃ц';
    Result.AreYouSureContSave := 'Јес тё певни/ô, же тё чцеш јисц даљи без записёнка љопка';
    Result.AreYouSureQuitSave := 'Јес тё певни/ô, же тё чцеш апљикација̃ замкну̃ц без записёнка љопка';
    Result.AutoClearTerminal  := 'Вёчишчи терминала прјед зрешењем скрипта';
    Result.DarkMode           := 'Цемни триб';
    Result.Error              := 'Феља';
    Result.ErrorLoadFile      := 'Фела прји вчётôнку љопка';
    Result.Expression         := 'Вёсло́в';
    Result.Load               := 'Вчётôј';
    Result.MenuApplication    := 'Апљикацијô';
    Result.MenuLanguage       := 'Ја̃зик';
    Result.MenuLoadFile       := 'Вчётôј скрипт з љопка';
    Result.MenuNewFile        := 'Нови љопк';
    Result.MenuSaveFile       := 'Запише скрипт до љопка';
    Result.MenuClose          := 'Замкни';
    Result.MenuView           := 'Вёздрјатк';
    Result.MenuRun            := 'Зрёшење';
    Result.No                 := 'Ње';
    Result.PSCode             := 'ПапајСкриптови код';
    Result.Result             := 'Резултат';
    Result.RunInt             := 'Зрёш';
    Result.RunExt             := 'Зрёш бутново';
    Result.RunScriptInt       := 'Зрёш скрипта';
    Result.RunScriptExt       := 'Зрёш скрипта в бутновим окње';
    Result.SampleCaption      := 'Вёсло́в ПС';
    Result.SampleCountIt      := 'Рёхуј!';
    Result.SampleHint         := 'Напише вёсло́в ПС розѕељони спацијама, нп. "2 3 +" љуб "20 4 / 5 +"';
    Result.Save               := 'Запише';
    Result.Submit             := 'Зацвјерѕе';
    Result.WindowMainName     := 'Каљкуљатор ОПН – интерпретер ја̃зика PapajScript';
    Result.WindowScanName     := 'Вчётôј вёсло́в';
    Result.WrongPS            := 'Фељни вёсло́в ПС';
    Result.Yes                := 'Јо';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langAfrikaans() : LangMap;
begin
    Result.AreYouSureQuit     := 'Is jy seker dat jy wil die program toemaak';
    Result.AreYouSureContSave := 'Is jy seker dat jy wil voortgaan sonder om lêer te stoor?';
    Result.AreYouSureQuitSave := 'Is jy seker dat jy wil die program toemaak sonder om lêer te stoor?';
    Result.AutoClearTerminal  := 'Maak terminaal skoon voordat ''n skrip uitgevoer word';
    Result.DarkMode           := 'Donker tema';
    Result.Error              := 'Fout';
    Result.ErrorLoadFile      := 'Kon nie ''n lêer laai nie';
    Result.Expression         := 'Uitdrukking';
    Result.Load               := 'Laai';
    Result.MenuApplication    := 'Toepassing';
    Result.MenuLanguage       := 'Taal';
    Result.MenuLoadFile       := 'Laai skrip vanaf lêer';
    Result.MenuNewFile        := 'Nuut lêer';
    Result.MenuSaveFile       := 'Stoor skrip in lêer';
    Result.MenuClose          := 'Maak toe';
    Result.MenuView           := 'Weergawe';
    Result.MenuRun            := 'Uitvoer';
    Result.No                 := 'Nee';
    Result.PSCode             := 'PapajScript-kode';
    Result.Result             := 'Resultaat';
    Result.RunInt             := 'Voer uit';
    Result.RunExt             := 'Voer extern uit';
    Result.RunScriptInt       := 'Voer skrip uit';
    Result.RunScriptExt       := 'Voer skrip in ''n externe venster uit';
    Result.SampleCaption      := 'PS-uitdrukking';
    Result.SampleCountIt      := 'Tel dit!';
    Result.SampleHint         := 'Tik ''n PS-uitdrukking geskei deur spasies, bv. "2 3 +" of "20 4 / 5 +"';
    Result.Save               := 'Stoor';
    Result.Submit             := 'Dien in';
    Result.WindowMainName     := 'OPN-sakrekenaar – PapajScript';
    Result.WindowScanName     := 'Skandeer ''n uitdrukking';
    Result.WrongPS            := 'Verkeerde PS-uitdrukking';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langItalian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Sei sicuro di voler chiudere l''applicazione';
    Result.AreYouSureContSave := 'Sei sicuro di voler continuare senza salvare il file';
    Result.AreYouSureQuitSave := 'Sei sicuro di voler chiudere l''applicazione senza salvare il file';
    Result.AutoClearTerminal  := 'Cancella terminale prima di eseguire uno script';
    Result.DarkMode           := 'Tema scuro';
    Result.Error              := 'Errore';
    Result.ErrorLoadFile      := 'Errore durante il caricamento di un file';
    Result.Expression         := 'Espressione';
    Result.Load               := 'Carica';
    Result.MenuApplication    := 'Applicazione';
    Result.MenuLanguage       := 'Lingua';
    Result.MenuLoadFile       := 'Carica script da file';
    Result.MenuNewFile        := 'New file';
    Result.MenuSaveFile       := 'Salva script su file';
    Result.MenuClose          := 'Chiudi';
    Result.MenuView           := 'Visualizza';
    Result.MenuRun            := 'Esegui';
    Result.No                 := 'No';
    Result.PSCode             := 'Codice PapajScript';
    Result.Result             := 'Risultato';
    Result.RunInt             := 'Esegui';
    Result.RunExt             := 'Esegui esternamente';
    Result.RunScriptInt       := 'Esegui script';
    Result.RunScriptExt       := 'Esegui script in una finestra esterna';
    Result.SampleCaption      := 'Espressione PS';
    Result.SampleCountIt      := 'Contiamo!';
    Result.SampleHint         := 'Digita un''espressione PS delimitata da spazi, ad es. "2 3 +" o "20 4 / 5 +"';
    Result.Save               := 'Salva';
    Result.Submit             := 'Invia';
    Result.WindowMainName     := 'Calcolatrice NPI – PapajScript';
    Result.WindowScanName     := 'Scansiona un''espressione';
    Result.WrongPS            := 'Espressione PS errata';
    Result.Yes                := 'Sì';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langRussian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Вы уверены, что хотите закрыть приложение';
    Result.AreYouSureContSave := 'Вы уверены, что хотите продолжить, не сохраняя файл';
    Result.AreYouSureQuitSave := 'Вы уверены, что хотите закрыть приложение без сохранения файла';
    Result.AutoClearTerminal  := 'Очистите терминал перед запуском скрипта';
    Result.DarkMode           := 'Темная тема';
    Result.Error              := 'Ошибка';
    Result.ErrorLoadFile      := 'Ошибка при загрузке файла';
    Result.Expression         := 'Выражение';
    Result.Load               := 'Нагрузка';
    Result.MenuApplication    := 'Приложение';
    Result.MenuLanguage       := 'Язык';
    Result.MenuLoadFile       := 'Загрузите скрипт из файла';
    Result.MenuNewFile        := 'Новый файл';
    Result.MenuSaveFile       := 'Сохраните скрипт в файл';
    Result.MenuClose          := 'Закройте';
    Result.MenuView           := 'Вид';
    Result.MenuRun            := 'Запуск';
    Result.No                 := 'Нет';
    Result.PSCode             := 'Код PapajScript';
    Result.Result             := 'Результат';
    Result.RunInt             := 'Запустите';
    Result.RunExt             := 'Запустите в терминале';
    Result.RunScriptInt       := 'Запустите скрипт';
    Result.RunScriptExt       := 'Запустите скрипт во внешнем окне';
    Result.SampleCaption      := 'PS Выражение';
    Result.SampleCountIt      := 'Поехали!';
    Result.SampleHint         := 'Введите выражение PS, разделенное пробелами, например «2 3 +» или «20 4/5 +»';
    Result.Save               := 'Сохранение';
    Result.Submit             := 'Отправьте';
    Result.WindowMainName     := 'Калькулятор ОПЗ - интерпретатор PapajScript';
    Result.WindowScanName     := 'Просканируйте выражение';
    Result.WrongPS            := 'Неправильное выражение PS';
    Result.Yes                := 'Да';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langMacedonian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Дали си сигурен/а дека сакаш да ја затвориш апликацијата';
    Result.AreYouSureContSave := 'Дали си сигурен/а дека сакаш да продолжиш без да ја зачуваш датотеката';
    Result.AreYouSureQuitSave := 'Дали си сигурен/а дека сакаш да ја затвориш апликацијата без да ја зачуваш датотеката';
    Result.AutoClearTerminal  := 'Исчисти го терминалот пред да извршите скрипта';
    Result.DarkMode           := 'Темна тема';
    Result.Error              := 'Грешка';
    Result.ErrorLoadFile      := 'Грешка при вчитување датотека';
    Result.Expression         := 'Израз';
    Result.Load               := 'Вчитај';
    Result.MenuApplication    := 'Апликација';
    Result.MenuLanguage       := 'Јазик';
    Result.MenuLoadFile       := 'Вчитај скрипта од датотека';
    Result.MenuNewFile        := 'Нова датотека';
    Result.MenuSaveFile       := 'Зачувај скрипта во датотека';
    Result.MenuClose          := 'Затвори';
    Result.MenuView           := 'Поглед';
    Result.MenuRun            := 'Извршување';
    Result.No                 := 'Не';
    Result.PSCode             := 'Код на PapajScript';
    Result.Result             := 'Резултат';
    Result.RunInt             := 'Стартувај';
    Result.RunExt             := 'Стартувај во терминал';
    Result.RunScriptInt       := 'Стартувај скрипта';
    Result.RunScriptExt       := 'Стартувај скрипта во надворешен прозорец';
    Result.SampleCaption      := 'Израз на PS';
    Result.SampleCountIt      := 'Ајде да го преброиме!';
    Result.SampleHint         := 'Напиши PS израз ограничен со празни места, на пр. „2 3 +“ или „20 4 / 5 +“';
    Result.Save               := 'Зачувај';
    Result.Submit             := 'Поднеси';
    Result.WindowMainName     := 'Калкулатор RPN – преведувач на PapajScript';
    Result.WindowScanName     := 'Скенирај израз';
    Result.WrongPS            := 'Погрешен израз на PS';
    Result.Yes                := 'Да';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langNorwegian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Er du sikker på at du vil lukke programmet';
    Result.AreYouSureContSave := 'Er du sikker på at du vil fortsette uten å lagre filen';
    Result.AreYouSureQuitSave := 'Er du sikker på at du vil lukke programmet uten å lagre filen';
    Result.AutoClearTerminal  := 'Tøm terminalen før du kjører et skript';
    Result.DarkMode           := 'Mørkt tema';
    Result.Error              := 'Feil';
    Result.ErrorLoadFile      := 'Feil ved lasting av en fil';
    Result.Expression         := 'Uttrykk';
    Result.Load               := 'Last';
    Result.MenuApplication    := 'Applikasjon';
    Result.MenuLanguage       := 'Språk';
    Result.MenuLoadFile       := 'Last skript fra fil';
    Result.MenuNewFile        := 'Ny fil';
    Result.MenuSaveFile       := 'Lagre skript til fil';
    Result.MenuClose          := 'Lukk';
    Result.MenuView           := 'Utsikt';
    Result.MenuRun            := 'Kjør';
    Result.No                 := 'Nei';
    Result.PSCode             := 'PapajScript-kode';
    Result.Result             := 'Resultat';
    Result.RunInt             := 'Kjør';
    Result.RunExt             := 'Kjør i terminal';
    Result.RunScriptInt       := 'Kjør skript';
    Result.RunScriptExt       := 'Kjør skript i et eksternt vindu';
    Result.SampleCaption      := 'PS uttrykk';
    Result.SampleCountIt      := 'La oss telle det!';
    Result.SampleHint         := 'Skriv inn et PS-uttrykk avgrenset med mellomrom, f.eks. "2 3 +" eller "20 4 / 5 +"';
    Result.Save               := 'Lagre';
    Result.Submit             := 'Sende inn';
    Result.WindowMainName     := 'RPN-kalkulator – PapajScript-tolker';
    Result.WindowScanName     := 'Skann et uttrykk';
    Result.WrongPS            := 'Feil PS-uttrykk';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langNorwegian2() : LangMap;
begin
    Result.AreYouSureQuit     := 'Er du sikker på at du vil lukke programmet';
    Result.AreYouSureContSave := 'Er du sikker på at du vil fortsette utan å lagre filen';
    Result.AreYouSureQuitSave := 'Er du sikker på at du vil lukke programmet utan å lagre filen';
    Result.AutoClearTerminal  := 'Tøm terminalen før du kjører eit skript';
    Result.DarkMode           := 'Mørkt tema';
    Result.Error              := 'Feil';
    Result.ErrorLoadFile      := 'Feil ved lasting av ein fil';
    Result.Expression         := 'Uttrykk';
    Result.Load               := 'Last';
    Result.MenuApplication    := 'Applikasjon';
    Result.MenuLanguage       := 'Språk';
    Result.MenuLoadFile       := 'Last skript frå fil';
    Result.MenuNewFile        := 'Ny fil';
    Result.MenuSaveFile       := 'Lagre skript til fil';
    Result.MenuClose          := 'Lukk';
    Result.MenuView           := 'Utsikt';
    Result.MenuRun            := 'Kjør';
    Result.No                 := 'Nei';
    Result.PSCode             := 'PapajScript-kode';
    Result.Result             := 'Resultat';
    Result.RunInt             := 'Kjør';
    Result.RunExt             := 'Kjør i terminal';
    Result.RunScriptInt       := 'Kjør skript';
    Result.RunScriptExt       := 'Kjør skript i eit eksternt vindu';
    Result.SampleCaption      := 'PS uttrykk';
    Result.SampleCountIt      := 'La oss telle det!';
    Result.SampleHint         := 'Skriv inn et PS-uttrykk avgrenset med mellomrom, f.eks. "2 3 +" eller "20 4 / 5 +"';
    Result.Save               := 'Lagre';
    Result.Submit             := 'Sende inn';
    Result.WindowMainName     := 'RPN-kalkulator – PapajScript-tolker';
    Result.WindowScanName     := 'Skann eit uttrykk';
    Result.WrongPS            := 'Feil PS-uttrykk';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langSwedish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Är du säker på att du vill stänga applikationen';
    Result.AreYouSureContSave := 'Är du säker på att du vill fortsätta utan att spara filen';
    Result.AreYouSureQuitSave := 'Är du säker på att du vill stänga programmet utan att spara filen';
    Result.AutoClearTerminal  := 'Tøm terminalen før du kjører et skript';
    Result.DarkMode           := 'Mörkt tema';
    Result.Error              := 'Fel';
    Result.ErrorLoadFile      := 'Fel vid laddning av en fil';
    Result.Expression         := 'Uttryck';
    Result.Load               := 'Ladda';
    Result.MenuApplication    := 'Applikation';
    Result.MenuLanguage       := 'Språk';
    Result.MenuLoadFile       := 'Ladda skript från fil';
    Result.MenuNewFile        := 'Ny fil';
    Result.MenuSaveFile       := 'Spara skriptet till filen';
    Result.MenuClose          := 'Stänga';
    Result.MenuView           := 'Utsikt';
    Result.MenuRun            := 'Kör';
    Result.No                 := 'Nej';
    Result.PSCode             := 'PapajScript-kod';
    Result.Result             := 'Resultat';
    Result.RunInt             := 'Kör';
    Result.RunExt             := 'Kör i terminal';
    Result.RunScriptInt       := 'Kör skript';
    Result.RunScriptExt       := 'Kör skript i ett externt fönster';
    Result.SampleCaption      := 'PS-uttryck';
    Result.SampleCountIt      := 'Låt oss räkna det!';
    Result.SampleHint         := 'Skriv ett PS-uttryck avgränsat av mellanslag, t.ex. "2 3 +" eller "20 4 / 5 +"';
    Result.Save               := 'Spara';
    Result.Submit             := 'Skicka in';
    Result.WindowMainName     := 'RPN-kalkylator – PapajScript-tolkare';
    Result.WindowScanName     := 'Skanna ett uttryck';
    Result.WrongPS            := 'Fel PS-uttryck';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;


// language setting mechanism

{$IFDEF MSWINDOWS}
function GetLocaleInformation(Flag: integer): string;
var
    pcLCA: array[0..20] of char;
begin
    if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
    begin
        pcLCA[0] := #0;
    end;
    Result := pcLCA;
end;

{$ENDIF}

function GetLocaleLanguage: string;
begin
    {$IFDEF MSWINDOWS}
        Result := GetLocaleInformation(LOCALE_SENGLANGUAGE);
    {$ELSE}
        Result := SysUtils.GetEnvironmentVariable('LANG');
    {$ENDIF}
end;   

function DetermineLanguage() : Language;
begin
    // https://docs.moodle.org/dev/Table_of_locales
    case (GetLocaleLanguage) of
        'af_ZA.UTF-8' : Result := L_AFR;
        'af_ZA.utf8' : Result := L_AFR;
        'Afrikaans_South Africa.1252' : Result := L_AFR;
        'Afrikaans' : Result := L_AFR;
        'csb.UTF-8' : Result := L_CSB;
        'csb_PL.UTF-8' : Result := L_CSB;
        'csb.utf8' : Result := L_CSB;
        'csb_PL.utf8' : Result := L_CSB;
        'da_DK.UTF-8' : Result := L_DEN;
        'da_DK.utf8' : Result := L_DEN;
        'Danish_Denmark.1252' : Result := L_DEN;
        'Danish' : Result := L_DEN;
        'en.UTF-8' : Result := L_ENG;
        'en_AG.UTF-8' : Result := L_ENG;
        'en_AU.UTF-8' : Result := L_ENG;
        'en_BW.UTF-8' : Result := L_ENG;
        'en_CA.UTF-8' : Result := L_ENG;
        'en_DK.UTF-8' : Result := L_ENG;
        'en_GB.UTF-8' : Result := L_ENG;
        'en_HK.UTF-8' : Result := L_ENG;
        'en_IE.UTF-8' : Result := L_ENG;
        'en_IL.UTF-8' : Result := L_ENG;
        'en_IN.UTF-8' : Result := L_ENG;
        'en_NG.UTF-8' : Result := L_ENG;
        'en_NZ.UTF-8' : Result := L_ENG;
        'en_PH.UTF-8' : Result := L_ENG;
        'en_SG.UTF-8' : Result := L_ENG;
        'en_US.UTF-8' : Result := L_ENG;
        'en_ZA.UTF-8' : Result := L_ENG;
        'en_ZM.UTF-8' : Result := L_ENG;
        'en_ZW.UTF-8' : Result := L_ENG;
        'en_AG.utf8' : Result := L_ENG;
        'en_AU.utf8' : Result := L_ENG;
        'en_BW.utf8' : Result := L_ENG;
        'en_CA.utf8' : Result := L_ENG;
        'en_DK.utf8' : Result := L_ENG;
        'en_GB.utf8' : Result := L_ENG;
        'en_HK.utf8' : Result := L_ENG;
        'en_IE.utf8' : Result := L_ENG;
        'en_IL.utf8' : Result := L_ENG;
        'en_IN.utf8' : Result := L_ENG;
        'en_NG.utf8' : Result := L_ENG;
        'en_NZ.utf8' : Result := L_ENG;
        'en_PH.utf8' : Result := L_ENG;
        'en_SG.utf8' : Result := L_ENG;
        'en_US.utf8' : Result := L_ENG;
        'en_ZA.utf8' : Result := L_ENG;
        'en_ZM.utf8' : Result := L_ENG;
        'en_ZW.utf8' : Result := L_ENG;
        'en.utf8' : Result := L_ENG;
        'English_Australia.1252' : Result := L_ENG;
        'English' : Result := L_ENG;
        'French' : Result := L_FRA;
        'French (Canada)' : Result := L_FRA;
        'fr.UTF-8' : Result := L_FRA;
        'fr_FR.UTF-8' : Result := L_FRA;
        'fr_BE.UTF-8' : Result := L_FRA;
        'fr_CH.UTF-8' : Result := L_FRA;
        'fr_LU.UTF-8' : Result := L_FRA;
        'fr_CA.UTF-8' : Result := L_FRA;
        'fr.utf8' : Result := L_FRA;
        'fr_FR.utf8' : Result := L_FRA;
        'fr_BE.utf8' : Result := L_FRA;
        'fr_CH.utf8' : Result := L_FRA;
        'fr_LU.utf8' : Result := L_FRA;
        'fr_CA.utf8' : Result := L_FRA;
        'French_France.1252' : Result := L_FRA;
        'de_DE.UTF-8' : Result := L_GER;
        'de_DE.utf8' : Result := L_GER;
        'German_Germany.1252' : Result := L_GER;
        'German' : Result := L_GER;
        'he_IL.utf8' : Result := L_HBR;
        'he_IL.UTF-8' : Result := L_HBR;
        'Hebrew_Israel.1255' : Result := L_HBR;
        'Hebrew' : Result := L_HBR;
        'it_IT.UTF-8' : Result := L_ITA;
        'it_IT.utf8' : Result := L_ITA;
        'Italian_Italy.1252' : Result := L_ITA;
        'Italian' : Result := L_ITA;
        'mk_MK.UTF-8' : Result := L_MKD;
        'mk_MK.utf8' : Result := L_MKD;
        'Macedonian_Macedonia.1251' : Result := L_MKD;
        'Macedonian' : Result := L_MKD;
        'nl_NL.UTF-8' : Result := L_NED;
        'nl_NL.utf8' : Result := L_NED;
        'Dutch_Netherlands.1252' : Result := L_NED;
        'Dutch' : Result := L_NED;
        'no_NO.UTF-8' : Result := L_NOR;
        'no_NO.utf8' : Result := L_NOR;
        'Norwegian_Norway.1252' : Result := L_NOR;
        'Norwegian' : Result := L_NOR;
        'nn_NO.UTF-8' : Result := L_NOR2;
        'nn_NO.utf8' : Result := L_NOR2;
        'Norwegian-Nynorsk_Norway.1252' : Result := L_NOR2;
        'Norwegian-Nynorsk' : Result := L_NOR2;
        'Norwegian (Nynorsk)' : Result := L_NOR2;
        'Nynorsk' : Result := L_NOR2;
        'pl.UTF-8' : Result := L_POL;
        'pl_PL.UTF-8' : Result := L_POL;
        'pl.utf8' : Result := L_POL;
        'pl_PL.utf8' : Result := L_POL;
        'Polish_Poland.1250' : Result := L_POL;
        'Polish' : Result := L_POL;    
        'ru_RU.UTF-8' : Result := L_RUS;
        'ru_RU.utf8' : Result := L_RUS;
        'Russian_Russia.1251' : Result := L_RUS;
        'Russian' : Result := L_RUS;  
        'sv_SE.UTF-8' : Result := L_SWE;
        'sv_SE.utf8' : Result := L_SWE;
        'Swedish_Sweden.1252' : Result := L_SWE;
        'Swedish' : Result := L_SWE;  
        else Result := L_ENG;
    end;
end;

function GetLocale(lang : Language) : LangMap;
begin
    case lang of
        L_AFR  : Result := langAfrikaans();
        L_CSB  : Result := langKashubian();
        L_CSB2 : Result := langKashubianNew();
        L_CSB3 : Result := langKashubianCyrilic();
        L_DEN  : Result := langDanish();
        L_ENG  : Result := langEnglish();
        L_FRA  : Result := langFrench();
        L_GER  : Result := langGerman();
        L_HBR  : Result := langHebrew();
        L_ITA  : Result := langItalian();
        L_MKD  : Result := langMacedonian();
        L_NED  : Result := langDutch();
        L_NOR  : Result := langNorwegian();
        L_NOR2 : Result := langNorwegian2();
        L_POL  : Result := langPolish();
        L_RUS  : Result := langRussian();
        L_SWE  : Result := langSwedish();
        else Result := langEnglish();
     end;
end;

procedure ApplyLocaleMain(lang : LangMap);
begin
    Unit1.Form1.MenuAutoClear.Caption := lang.AutoClearTerminal;
    Unit1.Form1.MenuDarkMode.Caption := lang.DarkMode;
    Unit1.Form1.MenuItem1.Caption := lang.MenuApplication;
    Unit1.Form1.MenuItem2.Caption := lang.MenuLanguage;
    Unit1.Form1.MenuLoad.Caption := lang.MenuLoadFile;
    Unit1.Form1.MenuNewFile.Caption := lang.MenuNewFile;
    Unit1.Form1.MenuSave.Caption := lang.MenuSaveFile;
    Unit1.Form1.MenuQuit.Caption := lang.MenuClose;
    Unit1.Form1.MenuView.Caption := lang.MenuView;
    Unit1.Form1.MenuRunScript.Caption := lang.MenuRun;
    Unit1.Form1.Label2.Caption := lang.PSCode;
    Unit1.Form1.Edit2.TextHint := lang.Result;
    Unit1.Form1.Button2.Caption := lang.RunInt;
    Unit1.Form1.ButtonTerminal.Caption := lang.RunExt;
    Unit1.Form1.MenuRunHere.Caption := lang.RunScriptInt;
    Unit1.Form1.MenuRunExternal.Caption := lang.RunScriptExt;
    Unit1.Form1.Label1.Caption := lang.SampleCaption;
    Unit1.Form1.Button1.Caption := lang.SampleCountIt;
    Unit1.Form1.Edit1.TextHint := lang.SampleHint;
    Unit1.Form1.Caption := lang.WindowMainName;
    Unit1.Form1.OpenDialog1.Title := lang.MenuLoadFile;
    Unit1.Form1.SaveDialog1.Title := lang.MenuSaveFile;
    if (lang.isRightToLeft) then
    begin
        Unit1.Form1.BiDiMode := bdRightToLeft;
        Unit1.Form1.Label1.BiDiMode := bdRightToLeft;
        Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
    end else begin
        Unit1.Form1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
    end;
end;

procedure ApplyLocaleScan(lang : LangMap);
begin
    Unit4.Form2.Button1.Caption := lang.Submit;
    Unit4.Form2.Label1.Caption := lang.Expression;
    Unit4.Form2.Caption := lang.WindowScanName;
    if (lang.isRightToLeft) then
    begin
        Unit4.Form2.BiDiMode := bdLeftToRight;
        Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
        Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
    end else begin
        Unit4.Form2.BiDiMode := bdLeftToRight;
        Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
        Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
    end;
end;

end.
