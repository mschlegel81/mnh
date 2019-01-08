UNIT mnh_splash;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls,
  myGenerics,myStringUtil,
  mnh_constants,
  mnh_settings,
  {$ifdef Windows} packages, {$endif}
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

VAR splashForm:TSplashForm;
PROCEDURE splashOnStartup;
IMPLEMENTATION
{$R *.lfm}

PROCEDURE splashOnStartup;
  begin
    splashForm:=TSplashForm.create(nil);
    splashForm.startupCall:=true;
    splashForm.position:=poScreenCenter;
    splashForm.ShowModal;
  end;

PROCEDURE TSplashForm.CheckBox1Change(Sender: TObject);
  begin
    settings.doShowSplashScreen:=CheckBox1.checked;
  end;

PROCEDURE TSplashForm.FormActivate(Sender: TObject);
  begin
    {$ifdef Windows}if APP_STYLE<>APP_STYLE_BLANK then{$endif} prepareDoc;
    if startupCall and not(settings.doShowSplashScreen) then close;
  end;

PROCEDURE TSplashForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    {$ifdef Windows}
    if APP_STYLE=APP_STYLE_BLANK then CloseAction:=caNone;
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
    ensureDemosAndPackages(Application,ProgressBar);
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
    prepareDoc;
    close;
    {$endif}
  end;

PROCEDURE TSplashForm.buttonInitPortableClick(Sender: TObject);
  begin
    {$ifdef Windows}
    APP_STYLE:=APP_STYLE_PORTABLE;
    sandbox^.runInstallScript;
    prepareDoc;
    close;
    {$endif}
  end;

PROCEDURE TSplashForm.FormShow(Sender: TObject);
  VAR l:T_arrayOfString;
      i:longint;
  begin
    CheckBox1.checked:=settings.doShowSplashScreen;
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

FINALIZATION
  if Assigned(splashForm) then FreeAndNil(splashForm);

end.

