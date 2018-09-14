UNIT mnh_splash;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  myGenerics,myStringUtil,
  mnh_constants,
  mnh_settings,
  mnh_doc;

TYPE
  TSplashForm = class(TForm)
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    PROCEDURE CheckBox1Change(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
  public
  end;

FUNCTION splashForm:TSplashForm;
PROCEDURE splashOnStartup;
FUNCTION isDocThreadRunning:boolean;
IMPLEMENTATION
VAR mySplashForm: TSplashForm=nil;
FUNCTION splashForm:TSplashForm;
  begin
    if not(Assigned(mySplashForm)) then mySplashForm:=TSplashForm.create(nil);
    result:=mySplashForm;
  end;

{$R *.lfm}

PROCEDURE splashOnStartup;
  begin
    if settings.doShowSplashScreen then splashForm.ShowModal;
  end;

PROCEDURE TSplashForm.CheckBox1Change(Sender: TObject);
  begin
    settings.doShowSplashScreen:=CheckBox1.checked;
  end;

VAR docThreadsRunning:longint=0;
FUNCTION isDocThreadRunning:boolean;
  begin
    result:=docThreadsRunning>0;
  end;

FUNCTION prepareDoc(p:pointer):ptrint;
  begin
    makeHtmlFromTemplate();
    result:=0;
    interlockedDecrement(docThreadsRunning);
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
    interLockedIncrement(docThreadsRunning);
    {$ifdef UNIX}
    prepareDoc(nil);
    {$else}
    beginThread(@prepareDoc);
    {$endif}
  end;

FINALIZATION
  if Assigned(mySplashForm) then FreeAndNil(mySplashForm);
  while (docThreadsRunning>0) do sleep(1);

end.

