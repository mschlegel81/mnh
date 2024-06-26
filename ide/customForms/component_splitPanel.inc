TYPE
P_splitPanelMeta=^T_splitPanelMeta;
T_splitPanelMeta=object(T_panelMeta)
  Left,Right:T_panelMeta;
  CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler);
  PROCEDURE update; virtual;
  FUNCTION preferClientAlignment: boolean; virtual;
  PROCEDURE alignContents; virtual;
  DESTRUCTOR destroy; virtual;
  PROCEDURE OnResize(Sender:TObject);
  FUNCTION getName:string; virtual;
end;

CONSTRUCTOR T_splitPanelMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral; CONST location: T_tokenLocation;
  VAR context: T_context; CONST recycler:P_literalRecycler);
  begin
    inherited create(parent,def,location,context,recycler,[dmk_type,dmk_left,dmk_right]);
    Left .create(@self,def,location,context,recycler,[dmk_type,dmk_left,dmk_right]); Left.winControl.Align:=alLeft;
    Right.create(@self,def,location,context,recycler,[dmk_type,dmk_left,dmk_right]); Right.winControl.Align:=alClient;
    getControl.OnResize:=@OnResize;
  end;

PROCEDURE T_splitPanelMeta.update; begin end;

FUNCTION T_splitPanelMeta.preferClientAlignment: boolean; begin result:=Left.preferClientAlignment or Right.preferClientAlignment; end;

PROCEDURE T_splitPanelMeta.alignContents;
  begin
    Left.alignContents;
    Right.alignContents;
  end;

DESTRUCTOR T_splitPanelMeta.destroy;
  begin
    Left .destroy;
    Right.destroy;
    inherited destroy;
  end;

PROCEDURE T_splitPanelMeta.OnResize(Sender: TObject);
  VAR leftW :longint=0;
      rightW:longint=0;
      totalW:longint=0;
      dummy :longint=0;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      if Left.preferClientAlignment then begin
        Left .getControl.AutoSize:=true;
        Left .preferredSize(leftW ,dummy);
        Right.preferredSize(rightW,dummy);
        totalW:=winControl.width;
        //leftWidth / totalW = leftW /(leftW+rightW)
        //leftWidth          = leftW /(leftW+rightW) * totalW
        Left .getControl.AutoSize:=false;
        Left .getControl.width:=round(leftW/(leftW+rightW)*totalW);
      end else begin
        Right.getControl.AutoSize:=true;
        Left .preferredSize(leftW ,dummy);
        Right.preferredSize(rightW,dummy);
        totalW:=winControl.width;
        Right .getControl.AutoSize:=false;
        Right .getControl.width:=round(rightW/(leftW+rightW)*totalW);
      end;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

FUNCTION T_splitPanelMeta.getName:string;
  begin
    result:='T_splitPanelMeta';
  end;
