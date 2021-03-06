TYPE
  P_gridMeta=^T_gridMeta;
  T_gridMeta=object(T_panelMeta)
    private
      colCount:longint;
    public
    CONSTRUCTOR create(CONST parent:P_panelMeta; CONST def:P_mapLiteral; CONST location:T_tokenLocation; VAR context:T_context; CONST recycler:P_literalRecycler);
    PROCEDURE add(CONST meta:P_guiElementMeta); virtual;
    PROCEDURE doneAdding;
    PROCEDURE update; virtual;
    PROCEDURE OnResize(Sender:TObject);
    PROCEDURE alignContents; virtual;
    FUNCTION preferClientAlignment:boolean; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION getName:string; virtual;
  end;

CONSTRUCTOR T_gridMeta.create(CONST parent: P_panelMeta; CONST def: P_mapLiteral;
                              CONST location: T_tokenLocation; VAR context: T_context; CONST recycler:P_literalRecycler);
  VAR tmp:P_literal;
  begin
    inherited create(parent,def,location,context,recycler,[dmk_type,dmk_parts,dmk_colCount]);
    colCount:=-1;
    tmp:=mapGet(def,key[dmk_colCount],recycler);
    if (tmp<>nil) and (tmp^.literalType in [lt_smallint,lt_bigint]) and (P_abstractIntLiteral(tmp)^.isBetween(1,100)) then colCount:=P_abstractIntLiteral(tmp)^.intValue;
    if colCount<0 then context.raiseError('grids must have a specified colCount in [1..100]',location);
    winControl.OnResize:=@OnResize;
    winControl.AutoSize:=true;
  end;

PROCEDURE T_gridMeta.add(CONST meta: P_guiElementMeta);
  VAR control:TControl;
      topNeighbor :P_guiElementMeta=nil;
      leftNeighbor:P_guiElementMeta=nil;
  begin
    if length(elements) mod colCount<>0 then leftNeighbor:=elements[length(elements)-1];
    if length(elements)-colCount>=0   then topNeighbor:=elements[length(elements)-colCount];
    control:=meta^.getControl;
    control.Align:=alCustom;
    if leftNeighbor<>nil then begin
      control.AnchorToNeighbour(akLeft ,10,leftNeighbor^.getControl);
      control.AnchorParallel   (akBottom,0,leftNeighbor^.getControl);
    end else begin
      control.AnchorToNeighbour(akLeft,10,nil);
    end;
    if length(elements) mod colCount=colCount-1 then control.AnchorToNeighbour(akRight,10,nil);

    if topNeighbor<>nil then begin
      control.AnchorToNeighbour(akTop  ,10,topNeighbor^.getControl);
      control.AnchorParallel   (akRight,0 ,topNeighbor^.getControl);
    end else begin
      control.AnchorToNeighbour(akTop,10,nil);
    end;
    setLength(elements,length(elements)+1);
    elements[length(elements)-1]:=meta;
    lastControl:=control;
  end;

PROCEDURE T_gridMeta.doneAdding;
  VAR i,i0:longint;
  begin
    i0:=(length(elements)-1) div colCount * colCount;
    for i:=i0 to length(elements)-1 do elements[i]^.getControl.AnchorToNeighbour(akBottom,0,nil);
    update;
  end;

PROCEDURE T_gridMeta.update;
  VAR k,rowIdx,colIdx:longint;
      maxLabelWidth:longint=0;
      l:TLabel;
      pw:longint=0;
      ph:longint=0;
      freeSpace,requiredSpace:longint;
      anyStretch:boolean;
      widths :array of record size:longint; stretch:boolean; end=();
      heights:array of record size:longint; stretch:boolean; end=();
  begin
    if tryEnterCriticalsection(elementCs)=0 then exit;
    try
      for colIdx:=0 to colCount-1 do begin
        for k:=0 to length(elements)-1 do if k mod colCount=colIdx then begin
          l:=elements[k]^.leftLabelOrNil;
          if (l<>nil) then begin
            l.AutoSize:=false;
            l.GetDefaultWidth;
            if pw>maxLabelWidth then maxLabelWidth:=pw;
          end;
        end;
        if maxLabelWidth>0 then for k:=0 to length(elements)-1 do if k mod colCount=colIdx then begin
          l:=elements[k]^.leftLabelOrNil;
          if (l<>nil) then l.width:=maxLabelWidth;
        end;
      end;
      setLength(widths ,0);
      setLength(heights,0);
      for k:=0 to length(elements)-1 do begin
        colIdx:=k mod colCount;
        rowIdx:=k div colCount;
        pw:=elements[k]^.getControl.GetDefaultWidth;
        ph:=elements[k]^.getControl.GetDefaultHeight;
        if rowIdx>=length(heights) then begin
          setLength(heights,rowIdx+1);
          heights[rowIdx].size   :=ph;
          heights[rowIdx].stretch:=elements[k]^.preferClientAlignment;
        end else begin
          if ph>heights[rowIdx].size then heights[rowIdx].size:=ph;
          heights[rowIdx].stretch:=heights[rowIdx].stretch or elements[k]^.preferClientAlignment;
        end;
        if colIdx>=length(widths) then begin
          setLength(widths,colIdx+1);
          widths[colIdx].size:=pw;
          widths[colIdx].stretch:=elements[k]^.preferClientAlignment;
        end else begin
          if pw>widths[colIdx].size then widths[colIdx].size:=pw;
          widths[colIdx].stretch:=widths[colIdx].stretch or elements[k]^.preferClientAlignment;
        end;
      end;

      freeSpace    :=winControl.ClientWidth;
      requiredSpace:=0;
      //if none stretch then all stretch
      anyStretch:=false;
      for k:=0 to length(widths)-1 do anyStretch:=anyStretch or widths[k].stretch;
      if not(anyStretch) then for k:=0 to length(widths)-1 do widths[k].stretch:=true;

      for k:=0 to length(widths)-1 do with widths[k] do if stretch then inc(requiredSpace,size) else dec(freeSpace,size);
      for k:=0 to length(widths)-1 do with widths[k] do if stretch then size:=round(size/requiredSpace*freeSpace);

      freeSpace    :=winControl.ClientHeight;
      requiredSpace:=0;

      for k:=0 to length(heights)-1 do with heights[k] do if stretch then inc(requiredSpace,size) else dec(freeSpace,size);
      for k:=0 to length(heights)-1 do with heights[k] do if stretch then size:=round(size/requiredSpace*freeSpace);

      for k:=0 to length(elements)-1 do try with elements[k]^.getControl do begin
        AutoSize:=false;
        height:=heights[k div colCount].size;
        width :=widths [k mod colCount].size;
      end except
        {$ifdef debugMode}
        writeln(stdErr,'ERROR UPDATING SIZES OF ELEMENT #',k,' ',elements[k]^.getName);
        {$endif}
      end;
      TPanel(winControl).AutoSize:=true;
    finally
      leaveCriticalSection(elementCs);
    end;
  end;

PROCEDURE T_gridMeta.OnResize(Sender:TObject);
  begin
    update;
  end;

PROCEDURE T_gridMeta.alignContents;
  begin
    update;
  end;

FUNCTION T_gridMeta.preferClientAlignment:boolean;
  begin
    result:=true;
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

