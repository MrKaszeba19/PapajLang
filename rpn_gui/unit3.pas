unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type Language = (
    L_CSB,
    L_CSB2,
    L_CSB3,
    L_DEN,
    L_ENG,
    L_FRA,
    L_GER,
	L_HBR,
    L_NED,
    L_POL
);

type LangMap = record
    AreYouSureQuit     : String;
    AreYouSureQuitSave : String;
    AutoClearTerminal  : String;
    DarkMode           : String;
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
    RunScriptInt       : String;
    RunScriptExt       : String;
    SampleCaption      : String;
    SampleCountIt      : String;
    SampleHint         : String;
    Save               : String;
    Submit             : String;
    WindowMainName     : String;
    WindowScanName     : String;
    Yes                : String;
    isRightToLeft      : Boolean;
end;

function DetermineLanguage() : Language;
function GetLocale(lang : Language) : LangMap;
//procedure ApplyLocale(lang : LangMap);
procedure ApplyLocaleMain(lang : LangMap);
procedure ApplyLocaleScan(lang : LangMap);

implementation
uses Unit1, Unit4;

// NEW LANG ENGINE

// translation

function langDanish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Er du sikker på, at du vil afslutte programmet?';
    Result.AreYouSureQuitSave := 'Er du sikker på, at du vil lukke filen uden at gemme?';
    Result.AutoClearTerminal  := 'Clear terminal before running a script';
    Result.DarkMode           := 'Mørkt tema';
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
    Result.RunScriptInt       := 'Kør scripten';
    Result.RunScriptExt       := 'Kør i et eksternt vindue';
    Result.SampleCaption      := 'PS-udtryk';
    Result.SampleCountIt      := 'Tæl det!';
    Result.SampleHint         := 'Indtast et PS-udtryk afgrænset af mellemrum, f.eks. "2 3 +" eller "20 4 / 5 +"';
    Result.Save               := 'Gem';
    Result.Submit             := 'Bekræft';
    Result.WindowMainName     := 'OPN-Lommeregner – PapajScript';
    Result.WindowScanName     := 'Scan en expression';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
end;

function langEnglish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Are you sure you want to close the application?';
    Result.AreYouSureQuitSave := 'Are you sure you want to close the application without saving the file?';
    Result.AutoClearTerminal  := 'Clear terminal before running a script';
    Result.DarkMode           := 'Dark mode';
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
    Result.RunScriptInt       := 'Run script';
    Result.RunScriptExt       := 'Run script in an external window';
    Result.SampleCaption      := 'PS Expression';
    Result.SampleCountIt      := 'Count it!';
    Result.SampleHint         := 'Type a PS expression delimited by spaces, e.g. "2 3 +" or "20 4 / 5 +"';
    Result.Save               := 'Save';
    Result.Submit             := 'Submit';
    Result.WindowMainName     := 'RPN Calculator – PapajScript';
    Result.WindowScanName     := 'Scan an expression';
    Result.Yes                := 'Yes';
    Result.isRightToLeft      := false;
end;

function langFrench() : LangMap;
begin
    Result.AreYouSureQuit     := 'Êtes-vous sûr de vouloir quitter ?';
    Result.AreYouSureQuitSave := 'Êtes-vous sûr de vouloir fermer le fichier sans l''enregistrer ?';
    Result.AutoClearTerminal  := 'Effacer le terminal avant d''exécuter un script';
    Result.DarkMode           := 'Thème sombre';
    Result.Expression         := 'Expression';
    Result.Load               := 'Charger';
    Result.MenuApplication    := 'Application';
    Result.MenuLanguage       := 'Langue';
    Result.MenuLoadFile       := 'Charger un script à partir d''un fichier';
    Result.MenuNewFile        := 'Nouveau fichier';
    Result.MenuSaveFile       := 'Enregistrer un script dans un fichier';
    Result.MenuClose          := 'Fermer';
    Result.MenuView           := 'Vue';
    Result.MenuRun            := 'Exécution';
    Result.No                 := 'Non';
    Result.PSCode             := 'PapajScript code';
    Result.Result             := 'Résultat';
    Result.RunScriptInt       := 'Exécuter le script';
    Result.RunScriptExt       := 'Exécuter le script dans une fenêtre externe';
    Result.SampleCaption      := 'PS Expression';
    Result.SampleCountIt      := 'Comptons !';
    Result.SampleHint         := 'Tapez une expression délimitée par des espaces, ex. "2 3 +" ou "20 4 / 5 +"';
    Result.Save               := 'Enregistrer';
    Result.Submit             := 'Soumettre';
    Result.WindowMainName     := 'La calculatrice NPI – Interpréteur du PapajScript';
    Result.WindowScanName     := 'Mettez une expression';
    Result.Yes                := 'Yes';
    Result.isRightToLeft      := false;
end;

function langGerman() : LangMap;
begin
    Result.AreYouSureQuit     := 'Möchten Sie die Anwendung wirklich schließen?';
    Result.AreYouSureQuitSave := 'Möchten Sie die Datei wirklich ohne Speichern schließen?';
    Result.AutoClearTerminal  := 'Bevor ein Skript ausgeführt wird, das Terminal leeren';
    Result.DarkMode           := 'Dunkles Thema';
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
    Result.RunScriptInt       := 'Skript ausführen';
    Result.RunScriptExt       := 'Skript in einem externen Fenster ausführen';
    Result.SampleCaption      := 'PS-Ausdruck';
    Result.SampleCountIt      := 'Zähle!';
    Result.SampleHint         := 'Geben Sie einen durch Leerzeichen getrennten PS-Ausdruck ein, e.g. "2 3 +" oder "20 4 / 5 +"';
    Result.Save               := 'Speichern';
    Result.Submit             := 'Senden';
    Result.WindowMainName     := 'UPN-Rechner – PapajScript';
    Result.WindowScanName     := 'Scanne einen Ausdruck hier';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
end;

function langHebrew() : LangMap;
begin
    Result.AreYouSureQuit     := 'האם את/ה בטוח/ה שברצונך לסגור את האפליקציה?';
    Result.AreYouSureQuitSave := 'האם את/ה בטוח/ה שברצונך לסגור את הקובץ מבלי לשמור?';
    Result.AutoClearTerminal  := 'נקה את הטרמינל לפני הפעלת סקריפט';
    Result.DarkMode           := 'ערכת נושא כהה';
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
    Result.RunScriptInt       := 'הפעל סקריפט';
    Result.RunScriptExt       := 'הפעל סקריפט בחלון חיצוני';
    Result.SampleCaption      := 'ביטוי PS';
    Result.SampleCountIt      := 'תחשיב!';
    Result.SampleHint         := 'הקלד ביטוי שמופרד במרווחים, לדוגמה "2 3 +" או "20 4 / 5 +", כאן.';
    Result.Save               := 'שמור';
    Result.Submit             := 'אישור';
    Result.WindowMainName     := 'מחשבון RPN – PapajScript';
    Result.WindowScanName     := 'העלה ביטוי';
    Result.Yes                := 'כן';
    Result.isRightToLeft      := true;
end;

function langDutch() : LangMap;
begin
    Result.AreYouSureQuit     := 'Weet u zeker dat u wilt afsluiten?';
    Result.AreYouSureQuitSave := 'Weet u zeker dat u het bestand wilt sluiten zonder op te slaan?';
    Result.AutoClearTerminal  := 'Terminal wissen voordat u een script uitvoert';
    Result.DarkMode           := 'Donker thema';
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
    Result.RunScriptInt       := 'Script uitvoeren';
    Result.RunScriptExt       := 'Script uitvoeren in een extern venster';
    Result.SampleCaption      := 'PS-uitdrukking';
    Result.SampleCountIt      := 'Bereken het!';
    Result.SampleHint         := 'Typ een PS-uitdrukking die wordt gescheiden door spaties, bijv. "2 3 +" of "20 4 / 5 +"';
    Result.Save               := 'Opslaan';
    Result.Submit             := 'Indienen';
    Result.WindowMainName     := 'OPN-calculator – PapajScript';
    Result.WindowScanName     := 'Een uitdrukking scannen';
    Result.Yes                := 'Ja';
    Result.isRightToLeft      := false;
end;


function langPolish() : LangMap;
begin
    Result.AreYouSureQuit     := 'Czy jesteś pewny/a, że chcesz zamknąć aplikację?';
    Result.AreYouSureQuitSave := 'Czy jesteś pewny/a, że chcesz zamknąć aplikację bez zapisywania pliku??';
    Result.AutoClearTerminal  := 'Wyczyść terminal przed uruchomieniem skryptu';
    Result.DarkMode           := 'Tryb ciemny';
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
    Result.RunScriptInt       := 'Uruchom skrypt';
    Result.RunScriptExt       := 'Uruchom skrypt w terminalu zewnętrznym';
    Result.SampleCaption      := 'Wyrażenie PS';
    Result.SampleCountIt      := 'Liczymy!';
    Result.SampleHint         := 'Napisz wyrażenie ONP rozdzielone spacjami, np. "2 3 +" or "20 4 / 5 +"';
    Result.Save               := 'Zapisz';
    Result.Submit             := 'Zatwierdź';
    Result.WindowMainName     := 'Kalkulator ONP – Interpreter PapajScript';
    Result.WindowScanName     := 'Wczytaj wyrażenie';
    Result.Yes                := 'Tak';
    Result.isRightToLeft      := false;
end;

function langKashubian() : LangMap;
begin
    Result.AreYouSureQuit     := 'Jes të pewny/ô, że të chcész aplikacëjã zamknąc?';
    Result.AreYouSureQuitSave := 'Jes të pewny/ô, że të chcész aplikacëjã zamknąc bez zôpisënkù lopka?';
    Result.AutoClearTerminal  := 'Wëcziszczë terminala przed zreszëniém skripta';
    Result.DarkMode           := 'Cemni trib';
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
    Result.RunScriptInt       := 'Zrësz skripta';
    Result.RunScriptExt       := 'Zrësz skripta w bùtnowim òknié';
    Result.SampleCaption      := 'Wësłów PS';
    Result.SampleCountIt      := 'Rëchuj!';
    Result.SampleHint         := 'Nôpisze wësłów OPN rozdzélony spacëjama, np. "2 3 +" lub "20 4 / 5 +"';
    Result.Save               := 'Zôpisze';
    Result.Submit             := 'Zacwierdze';
    Result.WindowMainName     := 'Kalkùlatór OPN – interpreter jãzëka PapajScript';
    Result.WindowScanName     := 'Wczëtôj wësłów';
    Result.Yes                := 'Jo';
    Result.isRightToLeft      := false;
end;

function langKashubianNew() : LangMap;
begin
    Result.AreYouSureQuit     := 'Jes të pevni/ô, že të chceš aplikacijã zamknąc?';
    Result.AreYouSureQuitSave := 'Jes të pevni/ô, že të chceš aplikacijã zamknąc bez zapisënka lopka?';
    Result.AutoClearTerminal  := 'Vëčišči terminala prjed zrešenjem skripta';
    Result.DarkMode           := 'Cemni trib';
    Result.Expression         := 'Vësłóv';
    Result.Load               := 'Včëtô';
    Result.MenuApplication    := 'Aplikacijô';
    Result.MenuLanguage       := 'Jãzik';
    Result.MenuLoadFile       := 'Včëtô skripta z lopka';
    Result.MenuNewFile        := 'Novi lopk';
    Result.MenuSaveFile       := 'Zapiše skripta do lopka';
    Result.MenuClose          := 'Zamkni';
    Result.MenuView           := 'Vëzdrjatk';
    Result.MenuRun            := 'Zrëšenje';
    Result.No                 := 'Nje';
    Result.PSCode             := 'PapajScriptovi kod';
    Result.Result             := 'Rezultat';
    Result.RunScriptInt       := 'Zrëš skripta';
    Result.RunScriptExt       := 'Zrëš skripta v butnovim oknje';
    Result.SampleCaption      := 'Vësłóv PS';
    Result.SampleCountIt      := 'Rëchuj!';
    Result.SampleHint         := 'Napiše vësłóv OPN rozdzeloni spacijama, np. "2 3 +" lub "20 4 / 5 +"';
    Result.Save               := 'Zapiše';
    Result.Submit             := 'Zacvjerdze';
    Result.WindowMainName     := 'Kalkulator OPN – interpreter jãzika PapajScript';
    Result.WindowScanName     := 'Včëtô vësłóv';
    Result.Yes                := 'Jo';
    Result.isRightToLeft      := false;
end;

function langKashubianCyrilic() : LangMap;
begin
    Result.AreYouSureQuit     := 'Јес тё певни/ô, же тё чцеш апљикацијã замкнąц?';
    Result.AreYouSureQuitSave := 'Јес тё певни/ô, же тё чцеш апљикацијã замкнąц без записёнка љопка?';
    Result.AutoClearTerminal  := 'Вёчишчи терминала прјед зрешењем скрипта';
    Result.DarkMode           := 'Цемни триб';
    Result.Expression         := 'Вёслóв';
    Result.Load               := 'Вчётô';
    Result.MenuApplication    := 'Апљикација';
    Result.MenuLanguage       := 'Јãзик';
    Result.MenuLoadFile       := 'Вчётô скрипт з љопка';
    Result.MenuNewFile        := 'Нови љопк';
    Result.MenuSaveFile       := 'Запише скрипт до љопка';
    Result.MenuClose          := 'Замкни';
    Result.MenuView           := 'Вёздрјатк';
    Result.MenuRun            := 'Зрёшење';
    Result.No                 := 'Ње';
    Result.PSCode             := 'ПапајСкриптови код';
    Result.Result             := 'Резултат';
    Result.RunScriptInt       := 'Зрёш скрипта';
    Result.RunScriptExt       := 'Зрёш скрипта в бутновим окње';
    Result.SampleCaption      := 'Вёслóв PS';
    Result.SampleCountIt      := 'Рёхуј!';
    Result.SampleHint         := 'Напише вёслóв ОПН розѕељони спацијама, нп. "2 3 +" љуб "20 4 / 5 +"';
    Result.Save               := 'Запише';
    Result.Submit             := 'Зацвјерѕе';
    Result.WindowMainName     := 'Каљкуљатор ОПН – интерпретер јãзика PapajScript';
    Result.WindowScanName     := 'Вчётô вёслóв';
    Result.Yes                := 'Јо';
    Result.isRightToLeft      := false;
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
        'csb.UTF-8' : Result := L_CSB;
        'csb_PL.UTF-8' : Result := L_CSB;
        'da_DK.UTF-8' : Result := L_DEN;
        'Danish_Denmark.1252' : Result := L_DEN;
        'Danish' : Result := L_DEN;
        'en.UTF-8' : Result := L_ENG;
        'en_GB.UTF-8' : Result := L_ENG;
        'en_US.UTF-8' : Result := L_ENG;
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
        'French_France.1252' : Result := L_FRA;
        'de_DE.UTF-8' : Result := L_GER;
        'German_Germany.1252' : Result := L_GER;
        'German' : Result := L_GER;
        'he_IL.utf8' : Result := L_HBR;
        'Hebrew_Israel.1255' : Result := L_HBR;
        'Hebrew' : Result := L_HBR;
        'nl_NL.UTF-8' : Result := L_NED;
        'Dutch_Netherlands.1252' : Result := L_NED;
        'Dutch' : Result := L_NED;
        'pl.UTF-8' : Result := L_POL;
        'pl_PL.UTF-8' : Result := L_POL;
        'Polish_Poland.1250' : Result := L_POL;
        'Polish' : Result := L_POL;    
        else Result := L_ENG;
    end;
end;

function GetLocale(lang : Language) : LangMap;
begin
    case lang of
          L_CSB  : Result := langKashubian();
          L_CSB2 : Result := langKashubianNew();
          L_CSB3 : Result := langKashubianCyrilic();
          L_DEN  : Result := langDanish();
          L_ENG  : Result := langEnglish();
          L_FRA  : Result := langFrench();
          L_GER  : Result := langGerman();
	      L_HBR  : Result := langHebrew();
          L_NED  : Result := langDutch();
          L_POL  : Result := langPolish();
          else Result := langEnglish();
     end;
end;

procedure ApplyLocale(lang : LangMap);
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
    Unit1.Form1.Button2.Caption := lang.RunScriptInt;
    Unit1.Form1.MenuRunHere.Caption := lang.RunScriptInt;
    Unit1.Form1.MenuRunExternal.Caption := lang.RunScriptExt;
    Unit1.Form1.Label1.Caption := lang.SampleCaption;
    Unit1.Form1.Button1.Caption := lang.SampleCountIt;
    Unit1.Form1.Edit1.TextHint := lang.SampleHint;
    Unit1.Form1.Caption := lang.WindowMainName;
    Unit1.Form1.OpenDialog1.Title := lang.MenuLoadFile;
    Unit1.Form1.SaveDialog1.Title := lang.MenuSaveFile;
    Unit4.Form2.Button1.Caption := lang.Submit;
    Unit4.Form2.Label1.Caption := lang.Expression;
    Unit4.Form2.Caption := lang.WindowScanName;
    if (lang.isRightToLeft) then
    begin
        Unit1.Form1.BiDiMode := bdRightToLeft;
        Unit1.Form1.Label1.BiDiMode := bdRightToLeft;
        Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
        Unit4.Form2.BiDiMode := bdLeftToRight;
        Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
        Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
    end else begin
        Unit1.Form1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Label1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Edit1.BiDiMode := bdLeftToRight;
        Unit1.Form1.Edit2.BiDiMode := bdLeftToRight;
        Unit4.Form2.BiDiMode := bdLeftToRight;
        Unit4.Form2.Label1.BiDiMode := bdLeftToRight;
        Unit4.Form2.Edit1.BiDiMode := bdLeftToRight;
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
    Unit1.Form1.Button2.Caption := lang.RunScriptInt;
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
