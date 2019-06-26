TYPE
P_plotDockMeta=^T_plotDockMeta;
T_plotDockMeta=object(T_panelMeta)
  private
    relatedPlotAdapter:P_guiPlotSystem;
    plotDockedBefore:T_componentParent;
  public
  CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST plot:P_guiPlotSystem);
  PROCEDURE update; virtual;
  FUNCTION preferClientAlignment:boolean; virtual;
  PROCEDURE alignContents; virtual;
  DESTRUCTOR destroy; virtual;
  FUNCTION getName:string; virtual;
  PROCEDURE connect; virtual;
  PROCEDURE disconnect; virtual;
end;

CONSTRUCTOR T_plotDockMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation;
  VAR context: T_context; CONST plot:P_guiPlotSystem);
  begin
    inherited create(parent,def,location,context,[dmk_type]);
    relatedPlotAdapter:=plot;
  end;

PROCEDURE T_plotDockMeta.update; begin end;
FUNCTION T_plotDockMeta.preferClientAlignment:boolean; begin result:=true; end;

PROCEDURE T_plotDockMeta.alignContents;
  begin
  end;

DESTRUCTOR T_plotDockMeta.destroy;
  begin
    disconnect;
    inherited destroy;
  end;

FUNCTION T_plotDockMeta.getName:string;
  begin
    result:='T_plotDockMeta';
  end;

PROCEDURE T_plotDockMeta.connect;
  VAR plotForm:TplotForm;
  begin
    plotForm:=relatedPlotAdapter^.plotFormForConnecting(true);
    plotDockedBefore:=plotForm.myComponentParent;
    plotForm.myComponentParent:=cpNone;
    plotForm.ManualDock(TPanel(getControl));
    plotForm.Align:=alClient;
  end;

PROCEDURE T_plotDockMeta.disconnect;
  VAR control:TControl;
      newForm:T_mnhComponentForm;
      panel:TPanel;
  begin
    panel:=TPanel(getControl);
    if panel.ControlCount<>1 then exit;
    control:=panel.Controls[0];
    //If the sheet is a TForm return it directly
    if control.ClassType.InheritsFrom(T_mnhComponentForm.ClassType)
    then newForm:=T_mnhComponentForm(control)
    else exit;
    newForm.ManualDock(nil);
    newForm.myComponentParent:=plotDockedBefore;
    dockNewForm(newForm);
  end;

