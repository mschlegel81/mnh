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

  { TSplashForm }

  TSplashForm = class(TForm)
    buttonInitNormal: TButton;
    buttonInitPortable: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
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

FUNCTION splashOnStartup:boolean;
PROCEDURE splashForAbout;

IMPLEMENTATION
//Uses for loading and applying settings
USES serializationUtil,
     editorMeta,
     customRunDialog,
     codeAssistance;
{$R *.lfm}
VAR splashForm:TSplashForm;

FUNCTION splashOnStartup:boolean;
  begin
    result:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
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
  begin
    if {$ifdef Windows}(APP_STYLE<>APP_STYLE_BLANK) and {$endif} startupCall then begin
      Application.ProcessMessages;
      ideSettings.loadFromFile(ideSettingsFilename);
      CheckBox1.checked:=ideSettings.doShowSplashScreen;
      workspace.loadFromFile(ideSettings.workspaceFilename);
      workspace.fileHistory.updateHistoryMenu;
      runParameterHistory.loadFromFile(runParameterHistoryFileName);
      prepareDoc;
      if not(ideSettings.doShowSplashScreen) then begin
        Application.ProcessMessages;
        close;
      end;
    end;
  end;

PROCEDURE TSplashForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    {$ifdef Windows}
    if (APP_STYLE=APP_STYLE_BLANK) then halt(17);
    {$endif}
  end;

FUNCTION prepare_doc_thread(p:pointer):ptrint;
  begin
    ensureDefaultFiles(nil,CODE_HASH<>htmlDocGeneratedForCodeHash,CODE_HASH<>htmlDocGeneratedForCodeHash);
    makeHtmlFromTemplate;
    result:=1;
  end;

PROCEDURE TSplashForm.prepareDoc;
  begin
    {$ifdef Windows}
    buttonInitNormal  .enabled:=APP_STYLE=APP_STYLE_BLANK;
    buttonInitNormal  .visible:=APP_STYLE=APP_STYLE_BLANK;
    buttonInitPortable.enabled:=APP_STYLE=APP_STYLE_BLANK;
    buttonInitPortable.visible:=APP_STYLE=APP_STYLE_BLANK;
    {$endif}
    beginThread(@prepare_doc_thread);
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
    prepareDoc;
    close;
    {$endif}
  end;

PROCEDURE TSplashForm.buttonInitPortableClick(Sender: TObject);
  begin
    {$ifdef Windows}
    APP_STYLE:=APP_STYLE_PORTABLE;
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
    Label2.caption:='Version '+VERSION+', build '+intToStr(BUILD_NUMBER)+' ['+copy(CODE_HASH,0,8)+'...]';

    buttonInitNormal  .enabled:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
    buttonInitNormal  .visible:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
    buttonInitPortable.enabled:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
    buttonInitPortable.visible:={$ifdef Windows}APP_STYLE=APP_STYLE_BLANK{$else}false{$endif};
  end;

end.

