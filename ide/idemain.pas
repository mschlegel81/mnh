UNIT ideMain;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, Forms, Controls, Dialogs, Menus, ExtCtrls,
  ComCtrls, StdCtrls, ideLayoutUtil, mnh_gui_settings,editorMeta;

TYPE

  { TIdeMainForm }

  TIdeMainForm = class(T_mnhContainerForm)
    MainMenu: TMainMenu;
    miNew: TMenuItem;
    miSettings: TMenuItem;
    smFile: TMenuItem;
    smShow: TMenuItem;

    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PageControl3: TPageControl;
    PageControl5: TPageControl;
    PageControl4: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    StatusBar1: TStatusBar;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDropFiles(Sender: TObject; CONST FileNames: array of string);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE miNewClick(Sender: TObject);
    PROCEDURE miSettingsClick(Sender: TObject);
    PROCEDURE PageControl1StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl2StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl3StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl5StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE PageControl4StartDock(Sender: TObject; VAR DragObject: TDragDockObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
  private
    splitterPositions:T_splitterPositions;
    FUNCTION startDock(CONST PageControl:TPageControl):TDragDockObject;
  public
    PROCEDURE attachNewForm(CONST form:T_mnhComponentForm); virtual;
    { public declarations }
  end;

VAR
  IdeMainForm: TIdeMainForm;

IMPLEMENTATION

{$R ideMain.lfm}

PROCEDURE TIdeMainForm.FormDropFiles(Sender: TObject; CONST FileNames: array of string);
  begin

  end;

PROCEDURE TIdeMainForm.FormCreate(Sender: TObject);
  begin
    splitterPositions[1]:=16384;
    splitterPositions[2]:=10000;
    splitterPositions[3]:=16384;
    splitterPositions[4]:=16384;
    ideLayoutUtil.mainForm:=self;
  end;

PROCEDURE TIdeMainForm.FormResize(Sender: TObject);
  begin
    PageControl1.width := width*splitterPositions[1] div 65535;
    PageControl2.height:=height*splitterPositions[2] div 65535;
    PageControl3.width := width*splitterPositions[3] div 65535;
    PageControl4.width := width*splitterPositions[4] div 65535;
  end;

PROCEDURE TIdeMainForm.miNewClick(Sender: TObject);
  begin
    attachNewForm(getNewEditor);
  end;

PROCEDURE TIdeMainForm.miSettingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
  end;

PROCEDURE TIdeMainForm.PageControl1StartDock(Sender: TObject; VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl1); end;
PROCEDURE TIdeMainForm.PageControl2StartDock(Sender: TObject; VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl2); end;
PROCEDURE TIdeMainForm.PageControl3StartDock(Sender: TObject; VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl3); end;
PROCEDURE TIdeMainForm.PageControl4StartDock(Sender: TObject; VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl4); end;
PROCEDURE TIdeMainForm.PageControl5StartDock(Sender: TObject; VAR DragObject: TDragDockObject); begin DragObject:=startDock(PageControl5); end;

PROCEDURE TIdeMainForm.Splitter1Moved(Sender: TObject);
  begin
    splitterPositions[1]:=PageControl1.width *65535 div  width;
    splitterPositions[2]:=PageControl2.height*65535 div height;
    splitterPositions[3]:=PageControl3.width *65535 div  width;
    splitterPositions[4]:=PageControl4.width *65535 div  width;
  end;

FUNCTION TIdeMainForm.startDock(CONST PageControl: TPageControl): TDragDockObject;
  VAR control:TControl;
      sheet:TTabSheet;
      newForm:T_mnhComponentForm;
      bounds:TRect;
  begin
    //Only handle pages with one control
    if PageControl.activePage.ControlCount<>1 then exit(nil);
    control:=PageControl.activePage.Controls[0];
    //If the sheet is a TForm return it directly
    if control.ClassType.InheritsFrom(T_mnhComponentForm.ClassType) then newForm:=T_mnhComponentForm(control)
    else begin
      ////Wrap the sheet contents in a new form
      //newForm:=TForm.create(Application);
      //bounds.topLeft    :=ClientToScreen(control.BoundsRect.topLeft);
      //bounds.bottomRight:=ClientToScreen(control.BoundsRect.bottomRight);
      //sheet:=PageControl.activePage;
      //with newForm do begin
      //  top     :=bounds.top;
      //  Left    :=bounds.Left;
      //  width   :=bounds.width;
      //  height  :=bounds.height;
      //  DragKind:= dkDock;
      //  DragMode:= dmAutomatic;
      //  caption :=sheet.caption;
      //  control.parent:=newForm;
      //  Show;
      //end;
      //FreeAndNil(sheet);
      raise Exception.create('Not an mnhComponent form!');
    end;
    result:=TDragDockObject.AutoCreate(newForm);
  end;

PROCEDURE TIdeMainForm.attachNewForm(CONST form: T_mnhComponentForm);
  VAR dockMeta:TDragDockObject=nil;
      componentParent:T_componentParent;
  begin
    componentParent:=lastDockLocationFor[form.getIdeComponentType];
    if componentParent in [cpPageControl1..cpPageControl5] then dockMeta:=TDragDockObject.create(form);
    case componentParent of
      cpPageControl1: PageControl1.DockDrop(dockMeta,0,0);
      cpPageControl2: PageControl2.DockDrop(dockMeta,0,0);
      cpPageControl3: PageControl3.DockDrop(dockMeta,0,0);
      cpPageControl4: PageControl4.DockDrop(dockMeta,0,0);
      cpPageControl5: PageControl5.DockDrop(dockMeta,0,0);
      else begin
        form.top :=top +(height-form.height) div 2;
        form.Left:=Left+(width -form.width ) div 2;
      end;
    end;
    if dockMeta<>nil then FreeAndNil(dockMeta);
  end;

end.

