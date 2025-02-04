unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type Language = (
    L_AFR,
    L_CRO,
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
    L_POR,
    L_POR2,
    L_RUS,
    L_SLO,
    L_SRB,
    L_SRB2,
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
    Cut                : String;
    Copy               : String;
    Paste              : String;
    Duplicate          : String;
    Remove             : String;
    Move               : String;
    Edit               : String;
    Search             : String;
    Find               : String;
    Replace            : String;
    Settings           : String;
    About              : String;
    SelectAll          : String;
    Print              : String;
    NoAppFound         : String;
    AddAppDir          : String;
    AddAppDirPath      : String;
    PauseAfterExec     : String;
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
procedure ApplyLocaleAbout(lang : LangMap);
procedure ApplyLocale(lang : LangMap);
implementation
uses 
{$IFDEF MSWINDOWS}
Windows,
{$ENDIF}
Unit1, Unit4, Unit6;

// NEW LANG ENGINE

// translation

//    Result.Cut                := '';
//    Result.Copy               := '';
//    Result.Paste              := '';
//    Result.Duplicate          := '';
//    Result.Remove             := '';
//    Result.Move               := '';
//    Result.Edit               := '';
//    Result.Search             := '';
//    Result.Replace            := '';
//    Result.Settings           := '';
//    Result.About              := '';
//    Result.SelectAll          := '';
//    Result.Print              := '';
//    NoAppFound         := '';
//    AddAppDir          := '';
//    AddAppDirPath      := '';
//    PauseAfterExec     := '';

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
    Result.WindowMainName     := 'Papaj GUI – PapajScript';
    Result.WindowScanName     := 'Scan en expression';
    Result.WrongPS            := 'Forkert PS-udtryk';
    Result.Yes                := 'Ja';
    Result.Cut                := 'Klip';
    Result.Copy               := 'Kopiér';
    Result.Paste              := 'Indsæt';
    Result.Duplicate          := 'Dupliker';
    Result.Remove             := 'Fjern';
    Result.Move               := 'Flyt';
    Result.Edit               := 'Rediger';
    Result.Search             := 'Søg';
    Result.Find               := 'Find';
    Result.Replace            := 'Erstat';
    Result.Settings           := 'Indstillinger';
    Result.About              := 'Om';
    Result.SelectAll          := 'Vælg alt';
    Result.Print              := 'Udskriv';
    Result.NoAppFound         := 'Ingen Papaj konsolapp blev fundet.';
    Result.AddAppDir          := 'Tilføj Papaj-konsolappen til den mappe, hvor GUI-appen er placeret.';
    Result.AddAppDirPath      := 'Tilføj Papaj-konsolappen til den mappe, hvor GUI-appen er placeret, eller til din $PATH.';
    Result.PauseAfterExec     := 'Sæt terminalen på pause efter scriptudførelse';
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
    Result.WindowMainName     := 'Papaj GUI – PapajScript interpreter';
    Result.WindowScanName     := 'Scan an expression';
    Result.WrongPS            := 'Wrong PS expression';
    Result.Yes                := 'Yes';
    Result.Cut                := 'Cut';
    Result.Copy               := 'Copy';
    Result.Paste              := 'Paste';
    Result.Duplicate          := 'Duplicate';
    Result.Remove             := 'Remove';
    Result.Move               := 'Move';
    Result.Edit               := 'Edit';
    Result.Search             := 'Search';
    Result.Find               := 'Find';
    Result.Replace            := 'Replace';
    Result.Settings           := 'Settings';
    Result.About              := 'About';
    Result.SelectAll          := 'Select All';
    Result.Print              := 'Print';
    Result.NoAppFound         := 'No Papaj console app found.';
    Result.AddAppDir          := 'Add Papaj console app to directory where the GUI app is located.';
    Result.AddAppDirPath      := 'Add Papaj console app to directory where the GUI app is located or your $PATH.';
    Result.PauseAfterExec     := 'Pause the terminal after script execution';
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
    Result.WindowMainName     := 'Papaj GUI – Interpréteur du PapajScript';
    Result.WindowScanName     := 'Mettez une expression';
    Result.WrongPS            := 'Expression PS incorrecte';
    Result.Yes                := 'Oui';
    Result.Cut                := 'Couper';
    Result.Copy               := 'Copier';
    Result.Paste              := 'Coller';
    Result.Duplicate          := 'Dupliquer';
    Result.Remove             := 'Supprimer';
    Result.Move               := 'Déplacer';
    Result.Edit               := 'Éditer';
    Result.Search             := 'Chercher';
    Result.Find               := 'Trouver';
    Result.Replace            := 'Remplacer';
    Result.Settings           := 'Réglages';
    Result.About              := 'À propos';
    Result.SelectAll          := 'Tout sélectionner';
    Result.Print              := 'Imprimer';
    Result.NoAppFound         := 'Aucune application de console de calculatrice NPI trouvée.';
    Result.AddAppDir          := 'Ajoutez l''application de console Papaj au répertoire où se trouve l''application GUI.';
    Result.AddAppDirPath      := 'Ajoutez l''application de console Papaj au répertoire où se trouve l''application GUI ou à votre $PATH.';
    Result.PauseAfterExec     := 'Mettre le terminal en pause après l''exécution du script';
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
    Result.Cut                := 'Ausschneiden';
    Result.Copy               := 'Kopieren';
    Result.Paste              := 'Einsetzen';
    Result.Duplicate          := 'Duplizieren';
    Result.Remove             := 'Löschen';
    Result.Move               := 'Verschieben';
    Result.Edit               := 'Bearbeiten';
    Result.Search             := 'Suchen';
    Result.Find               := 'Finden';
    Result.Replace            := 'Ersetzen';
    Result.Settings           := 'Einstellungen';
    Result.About              := 'Information';
    Result.SelectAll          := 'Alles auswählen';
    Result.Print              := 'Drucken';
    Result.NoAppFound         := 'Keine UPN-Rechner-Konsolen-App gefunden.';
    Result.AddAppDir          := 'Fügen Sie die UPN-Rechner-Konsolen-App dem Verzeichnis hinzu, in dem sich die GUI-App befindet.';
    Result.AddAppDirPath      := 'Fügen Sie die UPN-Rechner-Konsolen-App zum Verzeichnis hinzu, in dem sich die GUI-App befindet, oder zu Ihrem $PATH.';
    Result.PauseAfterExec     := 'Das Terminal nach der Skriptausführung anhalten';
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
    Result.Cut                := 'גזור';
    Result.Copy               := 'העתק';
    Result.Paste              := 'הדבק';
    Result.Duplicate          := 'כפול';
    Result.Remove             := 'הסר';
    Result.Move               := 'העבר';
    Result.Edit               := 'ערוך';
    Result.Search             := 'חפש';
    Result.Find               := 'מצא';
    Result.Replace            := 'החלף';
    Result.Settings           := 'הגדרות';
    Result.About              := 'אודות';
    Result.SelectAll          := 'בחר הכל';
    Result.Print              := 'הדפס';
    Result.NoAppFound         := 'אפליקציית קונסולה מחשבון RPN לא נמצאה.';
    Result.AddAppDir          := 'הוסף את אפליקציית קונסולה המחשבון RPN לספרייה שבה ממוקמת אפליקציית ה-GUI.';
    Result.AddAppDirPath      := 'הוסף את אפליקציית קונסולה המחשבון RPN לספרייה שבה ממוקמת אפליקציית ה-GUI או ל-$PATH שלך.';
    Result.PauseAfterExec     := 'השהה את הטרמינל לאחר ביצוע הסקריפט';
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
    Result.WindowMainName     := 'Papaj GUI – PapajScript';
    Result.WindowScanName     := 'Een uitdrukking scannen';
    Result.WrongPS            := 'Verkeerde PS-uitdrukking';
    Result.Yes                := 'Ja';
    Result.Cut                := 'Knippen';
    Result.Copy               := 'Kopiëren';
    Result.Paste              := 'Plakken';
    Result.Duplicate          := 'Dupliceren';
    Result.Remove             := 'Verwijderen';
    Result.Move               := 'Verplaatsen';
    Result.Edit               := 'Bewerken';
    Result.Search             := 'Zoeken';
    Result.Find               := 'Vinden';
    Result.Replace            := 'Vervangen';
    Result.Settings           := 'Instellingen';
    Result.About              := 'Over';
    Result.SelectAll          := 'Alles selecteren';
    Result.Print              := 'Afdrukken';
    Result.NoAppFound         := 'Papaj console-app niet gevonden.';
    Result.AddAppDir          := 'Voeg de Papaj-console-app toe aan de map waar de GUI-app zich bevindt.';
    Result.AddAppDirPath      := 'Voeg de Papaj-console-app toe aan de map waar de GUI-app zich bevindt of aan uw $PATH.';
    Result.PauseAfterExec     := 'Pauzeer de terminal na uitvoering van het script';
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
    Result.WindowMainName     := 'Papaj GUI – Interpreter PapajScript';
    Result.WindowScanName     := 'Wczytaj wyrażenie';
    Result.WrongPS            := 'Błędne wyrażenie PS';
    Result.Yes                := 'Tak';
    Result.Cut                := 'Wytnij';
    Result.Copy               := 'Kopiuj';
    Result.Paste              := 'Wklej';
    Result.Duplicate          := 'Duplikuj';
    Result.Remove             := 'Usuń';
    Result.Move               := 'Przenieś';
    Result.Edit               := 'Edytuj';
    Result.Search             := 'Szukaj';
    Result.Find               := 'Znajdź';
    Result.Replace            := 'Zamień';
    Result.Settings           := 'Ustawienia';
    Result.About              := 'O programie';
    Result.SelectAll          := 'Zaznacz wszystko';
    Result.Print              := 'Drukuj';
    Result.NoAppFound         := 'Nie znaleziono konsolowej aplikacji Papaj.';
    Result.AddAppDir          := 'Dodaj aplikację konsolową Papaj do folderu, w którym aplikacja GUI się znajduje.';
    Result.AddAppDirPath      := 'Dodaj aplikację konsolową Papaj do folderu, w którym aplikacja GUI się znajduje, lub do swojego $PATH.';
    Result.PauseAfterExec     := 'Zatrzymaj terminal po wykonaniu skryptu';
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
    Result.AutoClearTerminal  := 'Wëcziszczë terminala przed zrëszeniém skripta';
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
    Result.WindowMainName     := 'Papaj GUI – interpreter jãzëka PapajScript';
    Result.WindowScanName     := 'Wczëtôj wësłów';
    Result.WrongPS            := 'Félni wësłów PS';
    Result.Yes                := 'Jo';
    Result.Cut                := 'Wëtni';
    Result.Copy               := 'Kòpiwôj';
    Result.Paste              := 'Wkléji';
    Result.Duplicate          := 'Duplikòwôj';
    Result.Remove             := 'Rëmôj';
    Result.Move               := 'Przeniése';
    Result.Edit               := 'Edytuwôj';
    Result.Search             := 'Szëkôj';
    Result.Find               := 'Najdze';
    Result.Replace            := 'Zamiéni';
    Result.Settings           := 'Nastôwë';
    Result.About              := 'Ò aplikacëji';
    Result.SelectAll          := 'Wëbiérze wszëtkò';
    Result.Print              := 'Drëkùj';
    Result.NoAppFound         := 'Kònsolowô aplikacëjô Papaj nié je nalazłô.';
    Result.AddAppDir          := 'Dodôj kònsolową aplikacëjã Papaj do katalogù, gdze aplikacëjô GUI sã znajdôwô.';
    Result.AddAppDirPath      := 'Dodôj kònsolową aplikacëjã Papaj do katalogù, gdze aplikacëjô GUI sã znajdôwô, lub do swòjégò $PATH.';
    Result.PauseAfterExec     := 'Zatrzim terminal pò zrëszeniém skripta';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langKashubianNew() : LangMap;
begin
    Result.AreYouSureQuit     := 'Jes të pevni/ô, že të chceš aplikacijã zamknąc';
    Result.AreYouSureContSave := 'Jes të pevni/ô, že të chceš jic dalji bez zapisënka lopka';
    Result.AreYouSureQuitSave := 'Jes të pevni/ô, že të chceš aplikacijã zamknąc bez zapisënka lopka';
    Result.AutoClearTerminal  := 'Vëčišči terminala przed zrëšenjem skripta';
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
    Result.MenuView           := 'Vëzdrzatk';
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
    Result.WindowMainName     := 'Papaj GUI – interpreter jãzika PapajScript';
    Result.WindowScanName     := 'Včëtôj vësłóv';
    Result.WrongPS            := 'Felni vësłóv PS';
    Result.Yes                := 'Jo';
    Result.Cut                := 'Vëtni';
    Result.Copy               := 'Kopivôj';
    Result.Paste              := 'Vkleji';
    Result.Duplicate          := 'Duplikovôj';
    Result.Remove             := 'Rëmôj';
    Result.Move               := 'Przenjesi';
    Result.Edit               := 'Edituvôj';
    Result.Search             := 'Šëkôj';
    Result.Find               := 'Najdze';
    Result.Replace            := 'Zamjeni';
    Result.Settings           := 'Nastôvë';
    Result.About              := 'O aplikaciji';
    Result.SelectAll          := 'Vëbjerze všëtko';
    Result.Print              := 'Drëkuj';
    Result.NoAppFound         := 'Konsolovô aplikacijô Papaj nije nalazłô.';
    Result.AddAppDir          := 'Dodôj konsolovą aplikacijã Papaj do kataloga, dze aplikacijô GUI sã znajdôvô.';
    Result.AddAppDirPath      := 'Dodôj konsolową aplikacijã Papaj do kataloga, dze aplikacijô GUI sã znajdôvô, lub do svojego $PATH.';
    Result.PauseAfterExec     := 'Zatrzim terminala po zrëšenjem skripta';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langKashubianCyrilic() : LangMap;
begin
    Result.AreYouSureQuit     := 'Јес тё певни/ô, же тё чцеш апљикација̃ замкну̃ц';
    Result.AreYouSureContSave := 'Јес тё певни/ô, же тё чцеш јиц даљи без записёнка љопка';
    Result.AreYouSureQuitSave := 'Јес тё певни/ô, же тё чцеш апљикација̃ замкну̃ц без записёнка љопка';
    Result.AutoClearTerminal  := 'Вёчишчи терминала прјед зрёшењем скрипта';
    Result.DarkMode           := 'Цемни триб';
    Result.Error              := 'Феља';
    Result.ErrorLoadFile      := 'Феља прји вчётôнку љопка';
    Result.Expression         := 'Вёсло́в';
    Result.Load               := 'Вчётôј';
    Result.MenuApplication    := 'Апликацијô';
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
    Result.WindowMainName     := 'Калкулатор ОПН – интерпретер ја̃зика PapajScript';
    Result.WindowScanName     := 'Вчётôј вёсло́в';
    Result.WrongPS            := 'Фељни вёсло́в ПС';
    Result.Yes                := 'Јо';
    Result.Cut                := 'Вётни';
    Result.Copy               := 'Копивôј';
    Result.Paste              := 'Вклеји';
    Result.Duplicate          := 'Дупликовôј';
    Result.Remove             := 'Рёмôј';
    Result.Move               := 'Прзењеси';
    Result.Edit               := 'Едитувôј';
    Result.Search             := 'Шёкôј';
    Result.Find               := 'Најѕе';
    Result.Replace            := 'Замјени';
    Result.Settings           := 'Настôвё';
    Result.About              := 'О апликацији';
    Result.SelectAll          := 'Вёбјерје вшётко';
    Result.Print              := 'Дрёкуј';
    Result.NoAppFound         := 'Консоловô апликацијô ОПН Калкулатор није налазлô.';
    Result.AddAppDir          := 'Додôј консолову̃ апликација̃ ОПН Калкулатор до каталога, ѕе апликацијô GUI са̃ знајдôвô.';
    Result.AddAppDirPath      := 'Додôј консолову̃ апликација̃ ОПН Калкулатор до каталога, ѕе апликацијô GUI са̃ знајдôвô, љуб до својего $PATH.';
    Result.PauseAfterExec     := 'Затрјим терминала по зрёшењем скрипта';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
    // љ
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
    Result.Cut                := 'Knip';
    Result.Copy               := 'Kopiër';
    Result.Paste              := 'Plak';
    Result.Duplicate          := 'Dupliseer';
    Result.Remove             := 'Verwyder';
    Result.Move               := 'Skuif';
    Result.Edit               := 'Redigeer';
    Result.Search             := 'Soek';
    Result.Find               := 'Vind';
    Result.Replace            := 'Vervang';
    Result.Settings           := 'Instellings';
    Result.About              := 'Oor';
    Result.SelectAll          := 'Kies almal';
    Result.Print              := 'Druk';
    Result.NoAppFound         := 'OPN-sakrekenaar-konsole-toepassing nie gevind nie.';
    Result.AddAppDir          := 'Voeg OPN-sakrekenaar-konsole-toepassing by die gids waar die GUI-toepassing geleë is.';
    Result.AddAppDirPath      := 'Voeg OPN-sakrekenaar-konsole-toepassing by die gids waar die GUI-toepassing geleë is of by jou $PATH.';
    Result.PauseAfterExec     := 'Onderbreek die terminaal na die skripuitvoering';
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
    Result.Cut                := 'Taglia';
    Result.Copy               := 'Copia';
    Result.Paste              := 'Incolla';
    Result.Duplicate          := 'Duplica';
    Result.Remove             := 'Rimuovi';
    Result.Move               := 'Sposta';
    Result.Edit               := 'Modifica';
    Result.Search             := 'Ricerca';
    Result.Find               := 'Trova';
    Result.Replace            := 'Sostituisci';
    Result.Settings           := 'Impostazioni';
    Result.About              := 'Informazioni';
    Result.SelectAll          := 'Seleziona tutto';
    Result.Print              := 'Stampa';
    Result.NoAppFound         := 'App console Calcolatrice NPI non trovata.';
    Result.AddAppDir          := 'Aggiungi l''app della console Calcolatrice RPN alla directory in cui si trova l''app della GUI.';
    Result.AddAppDirPath      := 'Aggiungi l''app della console Calcolatrice RPN alla directory in cui si trova l''app della GUI o al tuo $PATH.';
    Result.PauseAfterExec     := 'Metti in pausa il terminale dopo l''esecuzione dello script';
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
    Result.WindowMainName     := 'Papaj GUI - интерпретатор PapajScript';
    Result.WindowScanName     := 'Просканируйте выражение';
    Result.WrongPS            := 'Неправильное выражение PS';
    Result.Yes                := 'Да';
    Result.Cut                := 'Вырезать';
    Result.Copy               := 'Копировать';
    Result.Paste              := 'Вставить';
    Result.Duplicate          := 'Дублировать';
    Result.Remove             := 'Удалить';
    Result.Move               := 'Переместить';
    Result.Edit               := 'Редактировать';
    Result.Search             := 'Искать';
    Result.Find               := 'Найти';
    Result.Replace            := 'Заменить';
    Result.Settings           := 'Настройки';
    Result.About              := 'О нас';
    Result.SelectAll          := 'Выбрать все';
    Result.Print              := 'Распечатать';
    Result.NoAppFound         := 'Консольное приложение Papaj не найдено.';
    Result.AddAppDir          := 'Добавьте консольное приложение Papaj в каталог, где находится приложение GUI.';
    Result.AddAppDirPath      := 'Добавьте консольное приложение Papaj в каталог, где находится приложение GUI, или в свой $PATH.';
    Result.PauseAfterExec     := 'Приостановить терминал после выполнения скрипта';
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
    Result.PSCode             := 'Код на ПапајСкрипт';
    Result.Result             := 'Резултат';
    Result.RunInt             := 'Стартувај';
    Result.RunExt             := 'Стартувај во терминал';
    Result.RunScriptInt       := 'Стартувај скрипта';
    Result.RunScriptExt       := 'Стартувај скрипта во надворешен прозорец';
    Result.SampleCaption      := 'Израз на ПС';
    Result.SampleCountIt      := 'Ајде да го преброиме!';
    Result.SampleHint         := 'Напиши ПС израз ограничен со празни места, на пр. „2 3 +“ или „20 4 / 5 +“';
    Result.Save               := 'Зачувај';
    Result.Submit             := 'Поднеси';
    Result.WindowMainName     := 'Papaj GUI – преведувач на PapajScript';
    Result.WindowScanName     := 'Скенирај израз';
    Result.WrongPS            := 'Погрешен израз на PS';
    Result.Yes                := 'Да';
    Result.Cut                := 'Исечи';
    Result.Copy               := 'Копирај';
    Result.Paste              := 'Залепи';
    Result.Duplicate          := 'Дуплирај';
    Result.Remove             := 'Отстрани';
    Result.Move               := 'Премести';
    Result.Edit               := 'Измени';
    Result.Search             := 'Барај';
    Result.Find               := 'Најди';
    Result.Replace            := 'Замени';
    Result.Settings           := 'Поставки';
    Result.About              := 'Информација';
    Result.SelectAll          := 'Селектирај се';
    Result.Print              := 'Испечатете';
    Result.NoAppFound         := 'Апликацијата за конзола Papaj не е пронајдена.';
    Result.AddAppDir          := 'Додај ја апликацијата за конзола Papaj во директориумот каде што се наоѓа апликацијата GUI.';
    Result.AddAppDirPath      := 'Додај ја апликацијата за конзола Papaj во директориумот каде што се наоѓа апликацијата GUI или во твојот $PATH.';
    Result.PauseAfterExec     := 'Паузирај го терминалот по извршувањето на скриптата';
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
    Result.WindowMainName     := 'Papaj GUI – PapajScript-tolker';
    Result.WindowScanName     := 'Skann et uttrykk';
    Result.WrongPS            := 'Feil PS-uttrykk';
    Result.Yes                := 'Ja';
    Result.Cut                := 'Klipp';
    Result.Copy               := 'Kopier';
    Result.Paste              := 'Lim';
    Result.Duplicate          := 'Dupliser';
    Result.Remove             := 'Fjern';
    Result.Move               := 'Flytt';
    Result.Edit               := 'Rediger';
    Result.Search             := 'Søk';
    Result.Find               := 'Finn';
    Result.Replace            := 'Erstatt';
    Result.Settings           := 'Innstillinger';
    Result.About              := 'Om';
    Result.SelectAll          := 'Velg alt';
    Result.Print              := 'Utskriv';
    Result.NoAppFound         := 'Papaj-konsollappen ble ikke funnet.';
    Result.AddAppDir          := 'Legg til Papaj-konsollappen i katalogen der GUI-appen er plassert.';
    Result.AddAppDirPath      := 'Legg til Papaj-konsollappen i katalogen der GUI-appen er plassert eller til $PATH-en din.';
    Result.PauseAfterExec     := 'Sett terminalen på pause etter skriptkjøring';
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
    Result.SampleHint         := 'Skriv inn eit PS-uttrykk avgrenset med mellomrom, f.eks. "2 3 +" eller "20 4 / 5 +"';
    Result.Save               := 'Lagre';
    Result.Submit             := 'Sende inn';
    Result.WindowMainName     := 'Papaj GUI – PapajScript-tolker';
    Result.WindowScanName     := 'Skann eit uttrykk';
    Result.WrongPS            := 'Feil PS-uttrykk';
    Result.Yes                := 'Ja';
    Result.Cut                := 'Klipp';
    Result.Copy               := 'Kopier';
    Result.Paste              := 'Lim';
    Result.Duplicate          := 'Dupliser';
    Result.Remove             := 'Fjern';
    Result.Move               := 'Flytt';
    Result.Edit               := 'Rediger';
    Result.Search             := 'Søk';
    Result.Find               := 'Finn';
    Result.Replace            := 'Erstatt';
    Result.Settings           := 'Innstillinger';
    Result.About              := 'Om';
    Result.SelectAll          := 'Velg alt';
    Result.Print              := 'Utskriv';
    Result.NoAppFound         := 'Papaj-konsollappen blei ikkje funnet.';
    Result.AddAppDir          := 'Legg til Papaj-konsollappen i katalogen der GUI-appen er plassert.';
    Result.AddAppDirPath      := 'Legg til Papaj-konsollappen i katalogen der GUI-appen er plassert eller til $PATH-en din.';
    Result.PauseAfterExec     := 'Sett terminalen på pause etter skriptkjøring';
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
    Result.WindowMainName     := 'Papaj GUI – PapajScript-tolkare';
    Result.WindowScanName     := 'Skanna ett uttryck';
    Result.WrongPS            := 'Fel PS-uttryck';
    Result.Yes                := 'Ja';
    Result.Cut                := 'Klipp ut';
    Result.Copy               := 'Kopiera';
    Result.Paste              := 'Klistra in';
    Result.Duplicate          := 'Duplicera';
    Result.Remove             := 'Ta bort';
    Result.Move               := 'Flytta';
    Result.Edit               := 'Redigera';
    Result.Search             := 'Sök';
    Result.Find               := 'Finna';
    Result.Replace            := 'Byt ut';
    Result.Settings           := 'Inställningar';
    Result.About              := 'Information';
    Result.SelectAll          := 'Välj allt';
    Result.Print              := 'Utskriv';
    Result.NoAppFound         := 'Papaj-konsolappen hittades inte.';
    Result.AddAppDir          := 'Lägg till Papaj-konsolappen i katalogen där GUI-appen finns.';
    Result.AddAppDirPath      := 'Lägg till Papaj-konsolappen i katalogen där GUI-appen finns eller till din $PATH.';
    Result.PauseAfterExec     := 'Pausa terminalen efter skriptkörning';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langSerbian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Да ли си сигуран/на да желиш да затвориш апликацију';
    Result.AreYouSureContSave := 'Да ли си сигуран/на да желиш да наставиш без сачувања датотеке';
    Result.AreYouSureQuitSave := 'Да ли си сигуран/на да желиш да затвориш апликацију без сачувања датотеке';
    Result.AutoClearTerminal  := 'Испразни терминал пре покретања скрипте';
    Result.DarkMode           := 'Мрачна тема';
    Result.Error              := 'Грешка';
    Result.ErrorLoadFile      := 'Грешка при учитавању датотеке';
    Result.Expression         := 'Израз';
    Result.Load               := 'Учитај';
    Result.MenuApplication    := 'Апликација';
    Result.MenuLanguage       := 'Језик';
    Result.MenuLoadFile       := 'Учитај скрипту из датотеке';
    Result.MenuNewFile        := 'Нова датотека';
    Result.MenuSaveFile       := 'Сачувајте скрипту у датотеку';
    Result.MenuClose          := 'Затвори';
    Result.MenuView           := 'Поглед';
    Result.MenuRun            := 'Покретање';
    Result.No                 := 'Не';
    Result.PSCode             := 'ПапајСкрипт код';
    Result.Result             := 'Резултат';
    Result.RunInt             := 'Покрени';
    Result.RunExt             := 'Покрени у терминалу';
    Result.RunScriptInt       := 'Покрени скрипту';
    Result.RunScriptExt       := 'Покрени скрипту у спољном прозору';
    Result.SampleCaption      := 'ПС Израз';
    Result.SampleCountIt      := 'Хајде да пребројимо!';
    Result.SampleHint         := 'Унеси ПС израз омеђен размацима, нпр. "2 3 +" или "20 4 / 5 +"';
    Result.Save               := 'Сачувај';
    Result.Submit             := 'Пошаљи';
    Result.WindowMainName     := 'ОПН калкулатор – тумач ПапајСкрипт';
    Result.WindowScanName     := 'Скенирај израз';
    Result.WrongPS            := 'Погрешан ПС израз';
    Result.Yes                := 'Да';
    Result.Cut                := 'Исеци';
    Result.Copy               := 'Копирај';
    Result.Paste              := 'Налепи';
    Result.Duplicate          := 'Дуплицирај';
    Result.Remove             := 'Уклони';
    Result.Move               := 'Помери';
    Result.Edit               := 'Измени';
    Result.Search             := 'Тражи';
    Result.Find               := 'Нађи';
    Result.Replace            := 'Замени';
    Result.Settings           := 'Подешавања';
    Result.About              := 'О апликацији';
    Result.SelectAll          := 'Изабери све';
    Result.Print              := 'Одштампај';
    Result.NoAppFound         := 'ОПН калкулатор конзолна апликација није пронађена.';
    Result.AddAppDir          := 'Додај конзолну апликацију ОПН калкулатора у директоријум где се налази GUI апликација.';
    Result.AddAppDirPath      := 'Додај конзолну апликацију ОПН калкулатора у директоријум где се налази GUI апликација или у свој $PATH.';
    Result.PauseAfterExec     := 'Паузирај терминал након извршења скрипте';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langSerbian2() : LangMap;
begin
    Result.AreYouSureQuit     := 'Da li si siguran/na da želiš da zatvoriš aplikaciju';
    Result.AreYouSureContSave := 'Da li si siguran/na da želiš da nastaviš bez sačuvanja datoteke';
    Result.AreYouSureQuitSave := 'Da li si siguran/na da želiš da zatvoriš aplikaciju bez sačuvanja datoteke';
    Result.AutoClearTerminal  := 'Isprazni terminal pre pokretanja skripte';
    Result.DarkMode           := 'Mračna tema';
    Result.Error              := 'Greška';
    Result.ErrorLoadFile      := 'Greška pri učitavanju datoteke';
    Result.Expression         := 'Izraz';
    Result.Load               := 'Učitaj';
    Result.MenuApplication    := 'Aplikacija';
    Result.MenuLanguage       := 'Jezik';
    Result.MenuLoadFile       := 'Učitaj skriptu iz datoteke';
    Result.MenuNewFile        := 'Nova datoteka';
    Result.MenuSaveFile       := 'Sačuvajte skriptu u datoteku';
    Result.MenuClose          := 'Zatvori';
    Result.MenuView           := 'Pogled';
    Result.MenuRun            := 'Pokretanje';
    Result.No                 := 'Ne';
    Result.PSCode             := 'PapajScript kod';
    Result.Result             := 'Rezultat';
    Result.RunInt             := 'Pokreni';
    Result.RunExt             := 'Pokreni u terminalu';
    Result.RunScriptInt       := 'Pokreni skriptu';
    Result.RunScriptExt       := 'Pokreni skriptu u spoljnom prozoru';
    Result.SampleCaption      := 'PS Izraz';
    Result.SampleCountIt      := 'Hajde da prebrojimo!';
    Result.SampleHint         := 'Unesi PS izraz omeđen razmacima, npr. "2 3 +" ili "20 4 / 5 +"';
    Result.Save               := 'Sačuvaj';
    Result.Submit             := 'Pošalji';
    Result.WindowMainName     := 'Papaj GUI – tumač PapajScript';
    Result.WindowScanName     := 'Skeniraj izraz';
    Result.WrongPS            := 'Pogrešan PS izraz';
    Result.Yes                := 'Da';
    Result.Cut                := 'Iseci';
    Result.Copy               := 'Kopiraj';
    Result.Paste              := 'Nalepi';
    Result.Duplicate          := 'Dupliciraj';
    Result.Remove             := 'Ukloni';
    Result.Move               := 'Pomeri';
    Result.Edit               := 'Izmeni';
    Result.Search             := 'Traži';
    Result.Find               := 'Nađi';
    Result.Replace            := 'Zameni';
    Result.Settings           := 'Podešavanja';
    Result.About              := 'O aplikaciji';
    Result.SelectAll          := 'Izaberi sve';
    Result.Print              := 'Odštampaj';
    Result.NoAppFound         := 'Papaj konzolna aplikacija nije pronađena.';
    Result.AddAppDir          := 'Dodaj konzolnu aplikaciju OPN kalkulatora u direktorijum gde se nalazi GUI aplikacija.';
    Result.AddAppDirPath      := 'Dodaj konzolnu aplikaciju OPN kalkulatora u direktorijum gde se nalazi GUI aplikacija ili u svoj $PATH.';
    Result.PauseAfterExec     := 'Pauziraj terminal nakon izvršenja skripte';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langCroatian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Jesi li siguran/na da želiš zatvoriti aplikaciju';
    Result.AreYouSureContSave := 'Jesi li siguran/na da želiš nastaviti bez spremanja datoteke';
    Result.AreYouSureQuitSave := 'Jesi li siguran/na da želiš zatvoriti aplikaciju bez spremanja datoteke';
    Result.AutoClearTerminal  := 'Isprazni terminal prije pokretanja skripte';
    Result.DarkMode           := 'Tamna tema';
    Result.Error              := 'Greška';
    Result.ErrorLoadFile      := 'Pogreška pri učitavanju datoteke';
    Result.Expression         := 'Izraz';
    Result.Load               := 'Učitaj';
    Result.MenuApplication    := 'Aplikacija';
    Result.MenuLanguage       := 'Jezik';
    Result.MenuLoadFile       := 'Učitaj skriptu iz datoteke';
    Result.MenuNewFile        := 'Nova datoteka';
    Result.MenuSaveFile       := 'Spremi skriptu u datoteku';
    Result.MenuClose          := 'Zatvori';
    Result.MenuView           := 'Pogled';
    Result.MenuRun            := 'Pokretanje';
    Result.No                 := 'Ne';
    Result.PSCode             := 'PapajScript kod';
    Result.Result             := 'Rezultat';
    Result.RunInt             := 'Pokreni';
    Result.RunExt             := 'Pokreni u terminalu';
    Result.RunScriptInt       := 'Pokreni skriptu';
    Result.RunScriptExt       := 'Pokreni skriptu u vanjskom prozoru';
    Result.SampleCaption      := 'PS izraz';
    Result.SampleCountIt      := 'Izbrojimo!';
    Result.SampleHint         := 'Upiši PS izraz omeđen razmacima, npr. "2 3 +" ili "20 4 / 5 +"';
    Result.Save               := 'Spremi';
    Result.Submit             := 'Pošalji';
    Result.WindowMainName     := 'OPN kalkulator – tumač PapajScript';
    Result.WindowScanName     := 'Skeniraj izraz';
    Result.WrongPS            := 'Pogrešan PS izraz';
    Result.Yes                := 'Da';
    Result.Cut                := 'Izreži';
    Result.Copy               := 'Kopiraj';
    Result.Paste              := 'Zalijepi';
    Result.Duplicate          := 'Dupliciraj';
    Result.Remove             := 'Ukloni';
    Result.Move               := 'Premjesti';
    Result.Edit               := 'Uredi';
    Result.Search             := 'Traži';
    Result.Find               := 'Nađi';
    Result.Replace            := 'Zamijeni';
    Result.Settings           := 'Postavke';
    Result.About              := 'O aplikaciji';
    Result.SelectAll          := 'Odaberi sve';
    Result.Print              := 'Ispiši';
    Result.NoAppFound         := 'Konzolna aplikacija OPN kalkulatora nije pronađena.';
    Result.AddAppDir          := 'Dodaj konzolnu aplikaciju OPN kalkulatora u direktorij gdje se nalazi GUI aplikacija.';
    Result.AddAppDirPath      := 'Dodaj konzolnu aplikaciju OPN kalkulatora u direktorij gdje se nalazi GUI aplikacija ili u svoj $PATH.';
    Result.PauseAfterExec     := 'Pauziraj terminal nakon izvršenja skripte';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langSlovene() : LangMap;
begin
    Result.AreYouSureQuit     := 'Ali si prepričan/na, da želiš zapreti aplikacijo';
    Result.AreYouSureContSave := 'Ali si prepričan/na, da želiš nadaljevati brez shranjevanja datoteke';
    Result.AreYouSureQuitSave := 'Ali si prepričan/na, da želiš zapreti aplikacijo brez shranjevanja datoteke';
    Result.AutoClearTerminal  := 'Pred zagonom skripta izprazni terminal';
    Result.DarkMode           := 'Temna tema';
    Result.Error              := 'Napaka';
    Result.ErrorLoadFile      := 'Napaka pri nalaganju datoteke';
    Result.Expression         := 'Izraz';
    Result.Load               := 'Naloži';
    Result.MenuApplication    := 'Aplikacija';
    Result.MenuLanguage       := 'Jezik';
    Result.MenuLoadFile       := 'Naloži skript iz datoteke';
    Result.MenuNewFile        := 'Nova datoteka';
    Result.MenuSaveFile       := 'Shrani skript v datoteko';
    Result.MenuClose          := 'Zapri';
    Result.MenuView           := 'Ogled';
    Result.MenuRun            := 'Zagon';
    Result.No                 := 'Ne';
    Result.PSCode             := 'Koda PapajScript';
    Result.Result             := 'Rezultat';
    Result.RunInt             := 'Zaženi';
    Result.RunExt             := 'Zaženi v terminalu';
    Result.RunScriptInt       := 'Zaženi skript';
    Result.RunScriptExt       := 'Zaženi skript v zunanjem oknu';
    Result.SampleCaption      := 'PS izraz';
    Result.SampleCountIt      := 'Preštejmo!';
    Result.SampleHint         := 'Vnesi izraz PS, razmejen s presledki, npr. "2 3 +" ali "20 4 / 5 +"';
    Result.Save               := 'Shrani';
    Result.Submit             := 'Pošlji';
    Result.WindowMainName     := 'Papaj GUI – tolmač PapajScript';
    Result.WindowScanName     := 'Skeniraj izraz';
    Result.WrongPS            := 'Napačen izraz PS';
    Result.Yes                := 'Da';
    Result.Cut                := 'Reži';
    Result.Copy               := 'Kopiraj';
    Result.Paste              := 'Prilepi';
    Result.Duplicate          := 'Podvoji';
    Result.Remove             := 'Odstrani';
    Result.Move               := 'Prestavi';
    Result.Edit               := 'Uredi';
    Result.Search             := 'Iskaj';
    Result.Find               := 'Najdi';
    Result.Replace            := 'Zamenjaj';
    Result.Settings           := 'Nastavitve';
    Result.About              := 'O aplikaciji';
    Result.SelectAll          := 'Izberi vse';
    Result.Print              := 'Natisni';
    Result.NoAppFound         := 'Konzolne aplikacije Papaj ni mogoče najti.';
    Result.AddAppDir          := 'Dodaj konzolno aplikacijo Papaj v imenik, kjer se nahaja aplikacija GUI.';
    Result.AddAppDirPath      := 'Dodaj konzolno aplikacijo Papaj v imenik, kjer se nahaja aplikacija GUI ali na tvoj $PATH.';
    Result.PauseAfterExec     := 'Začasno ustavi terminal po izvedbi skripta';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langPortuguese() : LangMap;
begin
    Result.AreYouSureQuit     := 'Tens certeza que desejas fechar o aplicativo';
    Result.AreYouSureContSave := 'Tens certeza que desejas continuar sem salvar o arquivo';
    Result.AreYouSureQuitSave := 'Tens certeza que desejas fechar o aplicativo sem salvar o arquivo';
    Result.AutoClearTerminal  := 'Esvazia o terminal antes de executar um script';
    Result.DarkMode           := 'Tema escuro';
    Result.Error              := 'Erro';
    Result.ErrorLoadFile      := 'Erro ao carregar um arquivo';
    Result.Expression         := 'Expressão';
    Result.Load               := 'Carregar';
    Result.MenuApplication    := 'Aplicativo';
    Result.MenuLanguage       := 'Língua';
    Result.MenuLoadFile       := 'Carregar script do arquivo';
    Result.MenuNewFile        := 'Novo arquivo';
    Result.MenuSaveFile       := 'Salvar script em arquivo';
    Result.MenuClose          := 'Fechar';
    Result.MenuView           := 'Vista';
    Result.MenuRun            := 'Execução';
    Result.No                 := 'Não';
    Result.PSCode             := 'Código PapajScript';
    Result.Result             := 'Resultado';
    Result.RunInt             := 'Executar';
    Result.RunExt             := 'Executar no terminal';
    Result.RunScriptInt       := 'Executar script';
    Result.RunScriptExt       := 'Executa o script em uma janela externa';
    Result.SampleCaption      := 'Expressão PS';
    Result.SampleCountIt      := 'Vamos contar!';
    Result.SampleHint         := 'Escreve uma expressão PS delimitada por espaços, por exemplo, "2 3 +" ou "20 4/5 +"';
    Result.Save               := 'Salvar';
    Result.Submit             := 'Submeter';
    Result.WindowMainName     := 'Papaj GUI - interpretador PapajScript';
    Result.WindowScanName     := 'Escanea uma expressão';
    Result.WrongPS            := 'Expressão de PS errada';
    Result.Yes                := 'Sim';
    Result.Cut                := 'Cortar';
    Result.Copy               := 'Cópiar';
    Result.Paste              := 'Colar';
    Result.Duplicate          := 'Duplicar';
    Result.Remove             := 'Remover';
    Result.Move               := 'Mover';
    Result.Edit               := 'Editar';
    Result.Search             := 'Procurar';
    Result.Find               := 'Encontrar';
    Result.Replace            := 'Substituir';
    Result.Settings           := 'Configurações';
    Result.About              := 'Sobre o aplicativo';
    Result.SelectAll          := 'Selecionar tudo';
    Result.Print              := 'Imprimir';
    Result.NoAppFound         := 'Aplicativo de console Papaj não encontrado.';
    Result.AddAppDir          := 'Adiciona o aplicativo de console Papaj ao diretório onde o aplicativo GUI está localizado.';
    Result.AddAppDirPath      := 'Adiciona o aplicativo de console Papaj ao diretório onde o aplicativo GUI está localizado ou ao teu $PATH.';
    Result.PauseAfterExec     := 'Pausar o terminal após a execução do script';
    Result.isRightToLeft      := false;
    Result.ExclamationMark    := '!';
    Result.QuestionMark       := '?';
    Result.ExclamationMark2   := '';
    Result.QuestionMark2      := '';
end;

function langPortuguese2() : LangMap;
begin
    Result.AreYouSureQuit     := 'Tem certeza que deseja fechar o aplicativo';
    Result.AreYouSureContSave := 'Tem certeza que deseja continuar sem salvar o arquivo';
    Result.AreYouSureQuitSave := 'Tem certeza que deseja fechar o aplicativo sem salvar o arquivo';
    Result.AutoClearTerminal  := 'Esvazie o terminal antes de executar um script';
    Result.DarkMode           := 'Tema escuro';
    Result.Error              := 'Erro';
    Result.ErrorLoadFile      := 'Erro ao carregar um arquivo';
    Result.Expression         := 'Expressão';
    Result.Load               := 'Carregar';
    Result.MenuApplication    := 'Aplicativo';
    Result.MenuLanguage       := 'Língua';
    Result.MenuLoadFile       := 'Carregar script do arquivo';
    Result.MenuNewFile        := 'Novo arquivo';
    Result.MenuSaveFile       := 'Salvar script em arquivo';
    Result.MenuClose          := 'Fechar';
    Result.MenuView           := 'Vista';
    Result.MenuRun            := 'Execução';
    Result.No                 := 'Não';
    Result.PSCode             := 'Código PapajScript';
    Result.Result             := 'Resultado';
    Result.RunInt             := 'Executar';
    Result.RunExt             := 'Executar no terminal';
    Result.RunScriptInt       := 'Executar script';
    Result.RunScriptExt       := 'Execute o script em uma janela externa';
    Result.SampleCaption      := 'Expressão PS';
    Result.SampleCountIt      := 'Vamos contar!';
    Result.SampleHint         := 'Escreva uma expressão PS delimitada por espaços, por exemplo, "2 3 +" ou "20 4/5 +"';
    Result.Save               := 'Salvar';
    Result.Submit             := 'Envie';
    Result.WindowMainName     := 'Papaj GUI - interpretador PapajScript';
    Result.WindowScanName     := 'Escaneie uma expressão';
    Result.WrongPS            := 'Expressão de PS errada';
    Result.Yes                := 'Sim';
    Result.Cut                := 'Cortar';
    Result.Copy               := 'Cópiar';
    Result.Paste              := 'Colar';
    Result.Duplicate          := 'Duplicar';
    Result.Remove             := 'Remover';
    Result.Move               := 'Mover';
    Result.Edit               := 'Editar';
    Result.Search             := 'Procurar';
    Result.Find               := 'Encontrar';
    Result.Replace            := 'Substituir';
    Result.Settings           := 'Configurações';
    Result.About              := 'Sobre o aplicativo';
    Result.SelectAll          := 'Selecionar tudo';
    Result.Print              := 'Imprimir';
    Result.NoAppFound         := 'Aplicativo de console Papaj não encontrado.';
    Result.AddAppDir          := 'Adicione o aplicativo de console Papaj ao diretório onde o aplicativo GUI está localizado.';
    Result.AddAppDirPath      := 'Adicione o aplicativo de console Papaj ao diretório onde o aplicativo GUI está localizado ou ao seu $PATH.';
    Result.PauseAfterExec     := 'Pausar o terminal após a execução do script';
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
        'af_ZA.UTF-8',
        'af_ZA.utf8',
        'Afrikaans_South Africa.1252',
        'Afrikaans',
        'Afrikaans (South Africa)',
        'Afrikaans (Namibia)' : Result := L_AFR;
        'hr_HR.UTF-8',
        'hr_HR.utf8',
        'Croatian_Croatia.1250',
        'Croatian' : Result := L_CRO;
        'csb.UTF-8',
        'csb_PL.UTF-8',
        'csb.utf8',
        'csb_PL.utf8' : Result := L_CSB;
        'da_DK.UTF-8',
        'da_DK.utf8',
        'Danish_Denmark.1252',
        'Danish' : Result := L_DEN;
        'en.UTF-8',
        'en_AG.UTF-8',
        'en_AU.UTF-8',
        'en_BW.UTF-8',
        'en_CA.UTF-8',
        'en_DK.UTF-8',
        'en_GB.UTF-8',
        'en_HK.UTF-8',
        'en_IE.UTF-8',
        'en_IL.UTF-8',
        'en_IN.UTF-8',
        'en_NG.UTF-8',
        'en_NZ.UTF-8',
        'en_PH.UTF-8',
        'en_SG.UTF-8',
        'en_US.UTF-8',
        'en_ZA.UTF-8',
        'en_ZM.UTF-8',
        'en_ZW.UTF-8',
        'en_AG.utf8',
        'en_AU.utf8',
        'en_BW.utf8',
        'en_CA.utf8',
        'en_DK.utf8',
        'en_GB.utf8',
        'en_HK.utf8',
        'en_IE.utf8',
        'en_IL.utf8',
        'en_IN.utf8',
        'en_NG.utf8',
        'en_NZ.utf8',
        'en_PH.utf8',
        'en_SG.utf8',
        'en_US.utf8',
        'en_ZA.utf8',
        'en_ZM.utf8',
        'en_ZW.utf8',
        'en.utf8',
        'English_Australia.1252',
        'English' : Result := L_ENG;
        'French',
        'French (Canada)',
        'fr.UTF-8',
        'fr_FR.UTF-8',
        'fr_BE.UTF-8',
        'fr_CH.UTF-8',
        'fr_LU.UTF-8',
        'fr_CA.UTF-8',
        'fr.utf8',
        'fr_FR.utf8',
        'fr_BE.utf8',
        'fr_CH.utf8',
        'fr_LU.utf8',
        'fr_CA.utf8',
        'French_France.1252' : Result := L_FRA;
        'de_DE.UTF-8',
        'de_AT.UTF-8',
        'de_CH.UTF-8',
        'de_DE.utf8',
        'de_AT.utf8',
        'de_CH.utf8',
        'German_Germany.1252',
        'German' : Result := L_GER;
        'he_IL.utf8',
        'he_IL.UTF-8',
        'Hebrew_Israel.1255',
        'Hebrew' : Result := L_HBR;
        'it_IT.UTF-8',
        'it_IT.utf8',
        'Italian_Italy.1252',
        'Italian' : Result := L_ITA;
        'mk_MK.UTF-8',
        'mk_MK.utf8',
        'Macedonian_Macedonia.1251',
        'Macedonian',
        'Macedonian (Macedonia)',
        'Macedonian (FYROM)',
        'Macedonian (North Macedonia)' : Result := L_MKD;
        'nl_NL.UTF-8',
        'nl_NL.utf8',
        'Dutch_Netherlands.1252',
        'Dutch' : Result := L_NED;
        'no_NO.UTF-8',
        'no_NO.utf8',
        'Norwegian_Norway.1252',
        'Norwegian' : Result := L_NOR;
        'nn_NO.UTF-8',
        'nn_NO.utf8',
        'Norwegian-Nynorsk_Norway.1252',
        'Norwegian-Nynorsk',
        'Norwegian (Nynorsk)',
        'Nynorsk' : Result := L_NOR2;
        'pl.UTF-8',
        'pl_PL.UTF-8',
        'pl.utf8',
        'pl_PL.utf8',
        'Polish_Poland.1250',
        'Polish',
        'Polish (Poland)' : Result := L_POL;  
        'pt_PT.UTF-8',
        'pt_PT.utf8',
        'Portuguese_Portugal.1252',
        'Portuguese',
        'Portuguese (Portugal)' : Result := L_POR;  
        'pt_BR.UTF-8',
        'pt_BR.utf8',
        'Portuguese_Brazil.1252',
        'Portuguese (Brazil)' : Result := L_POR2;  
        'ru_RU.UTF-8',
        'ru_RU.utf8',
        'Russian_Russia.1251',
        'Russian', 
        'Russian (Russia)' : Result := L_RUS; 
        'sl_SI.UTF-8',
        'sl_SI.utf8',
        'Slovenian_Slovenia.1250',
        'Slovenian',
        'Slovene' : Result := L_SLO; 
        'sr_CS.UTF-8',
        'sr_CS.utf8',
        'sr_RS.UTF-8',
        'sr_RS.utf8' : Result := L_SRB;
        'sr_CS.UTF-8@latin',
        'sr_CS.utf8@latin',
        'sr_RS.UTF-8@latin',
        'sr_RS.utf8@latin' : Result := L_SRB2;
        'Serbian_Serbia.1251',
        'Serbian',  
        'Serbian (Cyrilic)' : Result := L_SRB;  
        'Serbian (Latin)' : Result := L_SRB2; 
        'Serbian (Cyrilic, Serbia)' : Result := L_SRB;  
        'Serbian (Latin, Serbia)' : Result := L_SRB2;  
        'sv_SE.UTF-8',
        'sv_SE.utf8',
        'Swedish_Sweden.1252',
        'Swedish',  
        'Swedish (Sweden)',  
        'Swedish (Finland)' : Result := L_SWE;  
        else Result := L_ENG;
    end;
end;

function GetLocale(lang : Language) : LangMap;
begin
    case lang of
        L_AFR  : Result := langAfrikaans();
        L_CRO  : Result := langCroatian();
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
        L_POR  : Result := langPortuguese();
        L_POR2 : Result := langPortuguese2();
        L_RUS  : Result := langRussian();
        L_SLO  : Result := langSlovene();
        L_SRB  : Result := langSerbian();
        L_SRB2 : Result := langSerbian2();
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
    Unit1.Form1.MenuRunPause.Caption := lang.PauseAfterExec;
    Unit1.Form1.MenuAbout.Caption := lang.About;
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

procedure ApplyLocaleAbout(lang : LangMap);
begin
    Unit6.Form3.Caption := lang.About;
    Unit6.Form3.Button1.Caption := lang.MenuClose;
end;

procedure ApplyLocale(lang: LangMap);
begin
    ApplyLocaleMain(locale);
    ApplyLocaleScan(locale);
    ApplyLocaleAbout(locale);
end;

end.
