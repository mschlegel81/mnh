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

  { TSplashForm }

  TSplashForm = class(TForm)
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Panel1: TPanel;
    PROCEDURE CheckBox1Change(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

FUNCTION splashForm:TSplashForm;
PROCEDURE splashOnStartup;
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
    if settings.value^.doShowSplashScreen then splashForm.ShowModal;
  end;

PROCEDURE TSplashForm.CheckBox1Change(Sender: TObject);
  begin
    settings.value^.doShowSplashScreen:=CheckBox1.Checked;
  end;

FUNCTION prepareDoc(p:pointer):ptrint;
  begin
    makeHtmlFromTemplate();
  end;

PROCEDURE TSplashForm.FormShow(Sender: TObject);
  VAR l:T_arrayOfString;
      i:longint;
  begin
    CheckBox1.Checked:=settings.value^.doShowSplashScreen;
    l:=C_EMPTY_STRING_ARRAY;
    for i:=7 to length(LOGO)-1 do append(l,LOGO[i]);
    l[0]:=trim(l[0]);
    Label1.caption:=join(l,LineEnding);
    beginThread(@prepareDoc);
  end;

FINALIZATION
  if Assigned(mySplashForm) then FreeAndNil(mySplashForm);
end.

