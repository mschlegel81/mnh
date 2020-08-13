UNIT mnh_splash;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,
  myGenerics,myStringUtil,
  mnh_constants,
  mnh_settings,
  ideLayoutUtil,
  packages,
  mnh_doc;

TYPE
  TSplashForm = class(TForm)
    buttonInitNormal: TButton;
    buttonInitPortable: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    ProgressBar: TProgressBar;
    PROCEDURE buttonInitNormalClick(Sender: TObject);
    PROCEDURE buttonInitPortableClick(Sender: TObject);
    PROCEDURE CheckBox1Change(Sender: TObject);
    PROCEDURE FormActivate(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormShow(Sender: TObject);
  private
    startupCall:boolean;
    PROCEDURE prepareDoc;
  public
    PROCEDURE showAbout;
  end;

PROCEDURE splashOnStartup;
PROCEDURE splashForAbout;

IMPLEMENTATION
//Uses for loading and applying settings
USES serializationUtil,
     editorMeta,
     customRunDialog,
     codeAssistance;
{$R *.lfm}
VAR splashForm:TSplashForm;

PROCEDURE splashOnStartup;
  begin
    splashForm:=TSplashForm.create(nil);
    splashForm.startupCall:=true;
    splashForm.position:=poScreenCenter;
    splashForm.ShowModal;
    splashForm.destroy;
  end;

PROCEDURE splashForAbout;
  begin
    splashForm:=TSplashForm.create(nil);
    splashForm.startupCall:=false;
    splashForm.position:=poOwnerFormCenter;
    splashForm.ShowModal;
    splashForm.destroy;
  end;

PROCEDURE TSplashForm.CheckBox1Change(Sender: TObject);
  begin
    ideSettings.doShowSplashScreen:=CheckBox1.checked;
  end;

PROCEDURE TSplashForm.FormActivate(Sender: TObject);
  VAR okayUpToHere:boolean;

  PROCEDURE loadStepDone(CONST ok:boolean);
    begin
      okayUpToHere:=okayUpToHere and ok;
      ProgressBar.position:=ProgressBar.position+1;
      Application.ProcessMessages;
    end;

  begin
    if {$ifdef Windows}(APP_STYLE<>APP_STYLE_BLANK) and {$endif} startupCall then begin
      ProgressBar.position:=0;
      ProgressBar.visible:=true;
      ProgressBar.caption:='Loading/applying settings';
      ideSettings.loadFromFile(ideSettingsFilename);
      CheckBox1.checked:=ideSettings.doShowSplashScreen;
      loadStepDone(workspace.loadFromFile(ideSettings.workspaceFilename));
      workspace.fileHistory.updateHistoryMenu;
      loadStepDone(runParameterHistory.loadFromFile(runParameterHistoryFileName));

      prepareDoc;
      if not(ideSettings.doShowSplashScreen) then close;
    end;
  end;

PROCEDURE TSplashForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    {$ifdef Windows}
    if (APP_STYLE=APP_STYLE_BLANK) then halt(17);
    {$endif}
  end;

PROCEDURE TSplashForm.prepareDoc;
  begin
    settings.fixLocations;
    {$ifdef Windows}
    buttonInitNormal  .enabled:=APP_STYLE=APP_STYLE_BLANK;
    buttonInitNormal  .visible:=APP_STYLE=APP_STYLE_BLANK;
    buttonInitPortable.enabled:=APP_STYLE=APP_STYLE_BLANK;
    buttonInitPortable.visible:=APP_STYLE=APP_STYLE_BLANK;
    {$endif}
    ProgressBar.visible:=true;
    ProgressBar.caption:='Initializing';
    ensureDefaultFiles(Application,ProgressBar,CODE_HASH<>htmlDocGeneratedForCodeHash,CODE_HASH<>htmlDocGeneratedForCodeHash);
    makeHtmlFromTemplate(Application,ProgressBar);
    ProgressBar.visible:=false;
  end;

PROCEDURE TSplashForm.showAbout;
  begin
    startupCall:=false;
    position:=poOwnerFormCenter;
    ShowModal;
  end;

PROCEDURE TSplashForm.buttonInitNormalClick(Sender: TObject);
  begin
    {$ifdef Windows}
    APP_STYLE:=APP_STYLE_NORMAL;
    sandbox^.runInstallScript;
    ProgressBar.visible:=true;
    prepareDoc;
    close;
    {$endif}
  end;

PROCEDURE TSplashForm.buttonInitPortableClick(Sender: TObject);
  begin
    {$ifdef Windows}
    APP_STYLE:=APP_STYLE_PORTABLE;
    sandbox^.runInstallScript;
    ProgressBar.visible:=true;
    prepareDoc;
    close;
    {$endif}
  end;

PROCEDURE TSplashForm.FormShow(Sender: TObject);
  VAR l:T_arrayOfString;
      i:longint;
  begin
    CheckBox1.checked:=ideSettings.doShowSplashScreen;
    l:=C_EMPTY_STRING_ARRAY;
    for i:=7 to length(LOGO)-1 do append(l,LOGO[i]);
    l[0]:=trim(l[0]);
    Label1.caption:=join(l,LineEnding);
    Label2.caption:='build '+intToStr(BUILD_NUMBER)+' ['+CODE_HASH+']';
    buttonInitNormal  .enabled:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
    buttonInitNormal  .visible:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
    buttonInitPortable.enabled:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
    buttonInitPortable.visible:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
  end;

end.

