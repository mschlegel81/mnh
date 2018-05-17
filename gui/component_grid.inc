TYPE
  P_gridMeta=^T_gridMeta;
  T_gridMeta=object(T_panelMeta)
    private
      ColCount:longint;
    public
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_threadContext);
    PROCEDURE add(CONST meta:P_guiElementMeta); virtual;
    PROCEDURE doneAdding;
    PROCEDURE update; virtual;
    PROCEDURE alignContents; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_gridMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                              CONST location: T_tokenLocation; VAR context: T_threadContext);
  VAR tmp:P_literal;
  begin
    inherited create(parent,def,location,context,[dmk_type,dmk_parts,dmk_colCount]);
    ColCount:=-1;
    tmp:=mapGet(def,key[dmk_colCount]);
    if (tmp<>nil) and (tmp^.literalType=lt_int) and (P_intLiteral(tmp)^.value.isBetween(1,100)) then ColCount:=P_intLiteral(tmp)^.value.toInt;
    if ColCount<0 then context.adapters^.raiseError('grids must have a specified colCount in [1..100]',location);
  end;

PROCEDURE T_gridMeta.add(CONST meta: P_guiElementMeta);
  VAR control:TControl;
      topNeighbor :P_guiElementMeta=nil;
      leftNeighbor:P_guiElementMeta=nil;
  begin
    if length(elements) mod ColCount<>0 then leftNeighbor:=elements[length(elements)-1];
    if length(elements)-ColCount>=0   then topNeighbor:=elements[length(elements)-ColCount];
    control:=meta^.getControl;
    control.Align:=alCustom;
    if leftNeighbor<>nil then begin
      control.AnchorToNeighbour(akLeft ,10,leftNeighbor^.getControl);
      control.AnchorParallel   (akBottom,0,leftNeighbor^.getControl);
    end else begin
      control.AnchorToNeighbour(akLeft,10,nil);
    end;
    if topNeighbor<>nil then begin
      control.AnchorToNeighbour(akTop  ,10,topNeighbor^.getControl);
      control.AnchorParallel   (akRight,0 ,topNeighbor^.getControl);
    end else begin
      control.AnchorToNeighbour(akTop,10,nil);
    end;
    setLength(elements,length(elements)+1);
    elements[length(elements)-1]:=meta;
    if length(elements) mod ColCount=0 then control.AnchorToNeighbour(akRight,0,nil);
    lastControl:=control;
  end;

PROCEDURE T_gridMeta.doneAdding;
  begin
    lastControl.AnchorToNeighbour(akBottom,0,nil);
    update;
  end;

PROCEDURE T_gridMeta.update;
  VAR k,rowIdx,colIdx:longint;
      maxLabelWidth:longint=0;
      l:TLabel;
      pw:longint=0;
      ph:longint=0;
      widths,heights:array of longint;
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    for colIdx:=0 to ColCount-1 do begin
      for k:=0 to length(elements)-1 do if k mod ColCount=colIdx then begin
        l:=elements[k]^.leftLabelOrNil;
        if (l<>nil) then begin
          l.AutoSize:=false;
          l.GetPreferredSize(pw,ph);
          if pw>maxLabelWidth then maxLabelWidth:=pw;
        end;
      end;
      if maxLabelWidth>0 then for k:=0 to length(elements)-1 do if k mod ColCount=colIdx then begin
        l:=elements[k]^.leftLabelOrNil;
        if (l<>nil) then l.width:=maxLabelWidth;
      end;
    end;
    setLength(widths ,0);
    SetLength(heights,0);
    for k:=0 to length(elements)-1 do begin
      colIdx:=k mod ColCount;
      rowIdx:=k div ColCount;
      elements[k]^.getControl.GetPreferredSize(pw,ph);
      if rowIdx>=length(heights) then begin
        SetLength(heights,rowIdx+1);
        heights[rowIdx]:=ph;
      end else if ph>heights[rowIdx] then heights[rowIdx]:=ph;
      if colIdx>=length(widths) then begin
        SetLength(widths,colIdx+1);
        widths[colIdx]:=pw;
      end else if pw>widths[colIdx] then widths[colIdx]:=pw;
    end;
    for k:=0 to length(elements)-1 do with elements[k]^.getControl do begin
      AutoSize:=false;
      height:=heights[k div ColCount];
      width :=widths [k mod ColCount];
    end;
    TPanel(winControl).AutoSize    :=true;
    leaveCriticalSection(elementCs);
  end;

PROCEDURE T_gridMeta.alignContents;
  begin
    update;
  end;

DESTRUCTOR T_gridMeta.destroy;
  begin
    inherited destroy;
    setLength(elements,0);
  end;

FUNCTION T_gridMeta.getName:string;
  begin
    result:='T_gridMeta holding '+intToStr(length(elements));
  end;

