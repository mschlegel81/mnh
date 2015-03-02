unit mnh_gui_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ComCtrls, ColorBox, ButtonPanel, CheckLst, Grids,
  mnh_gui_settings, mnh_fileWrappers, mnh_tokens;

type

  { TMnhForm }

  TMnhForm = class(TForm)
    GroupBox1: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    Splitter1: TSplitter;
    StatusBar: TStatusBar;
    StringGrid1: TStringGrid;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    procedure FormCreate(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MnhForm: TMnhForm;
  inputWrapper: P_directInputWrapper;
  quitProgramPosted: boolean=false;
implementation

{$R *.lfm}

{ TMnhForm }
PROCEDURE TMnhForm.FormCreate(Sender: TObject);
  begin
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
  end;

FUNCTION evaluationThread(p:pointer):ptrint;
  VAR sleepTime:longint=1;
  begin

    if isReloadOfAllPackagesIndicated
    then begin
      reloadAllPackages;
      sleepTime:=1;
    end else if isReloadOfMainPackageIndicated
    then begin
      reloadMainPackage;
      sleepTime:=1;
    end else begin
      Sleep(sleepTime);
      if (sleepTime<500) then inc(sleepTime);
    end;

  end;

INITIALIZATION
  new(inputWrapper,create);
  inputWrapper^.logCheck;
  initMainPackage(inputWrapper);

end.

