UNIT mnh_debuggingVar;
INTERFACE
USES sysutils,
     mnh_constants,
     mnh_basicTypes,
     mnh_litVar;
TYPE
  P_variableTreeEntry=^I_variableTreeEntry;
  T_variableTreeEntries=array of P_variableTreeEntry;
  I_variableTreeEntry=object
    FUNCTION canExpand:boolean; virtual; abstract;
    FUNCTION toString:string; virtual; abstract;
    FUNCTION getChildren:T_variableTreeEntries; virtual; abstract;
    DESTRUCTOR destroy; virtual; abstract;
  end;

  P_variableTreeEntryAnonymousValue=^T_variableTreeEntryAnonymousValue;
  T_variableTreeEntryAnonymousValue=object(I_variableTreeEntry)
    private
      value:P_literal;
      isKeyValuePair:boolean;
      preparedChildren:array of P_variableTreeEntryAnonymousValue;
    public
      CONSTRUCTOR create(CONST L:P_literal; CONST mapEntry:boolean);
      FUNCTION canExpand:boolean; virtual;
      FUNCTION toString:string; virtual;
      FUNCTION getChildren:T_variableTreeEntries; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_variableTreeEntryNamedValue=^T_variableTreeEntryNamedValue;
  T_variableTreeEntryNamedValue=object(T_variableTreeEntryAnonymousValue)
    private
      id      :T_idString;
    public
      CONSTRUCTOR create(CONST value_:P_literal; CONST id_:T_idString);
      FUNCTION toString:string; virtual;
      PROPERTY getIdOnly:T_idString read id;
  end;

  T_debuggerVariableCategory=(dvc_global,          //mutable of datastore from this package
                              dvc_local,
                              dvc_inline,
                              dvc_callParameter);

  P_variableTreeEntryCategoryNode=^T_variableTreeEntryCategoryNode;
  T_variableTreeEntryCategoryNode=object(I_variableTreeEntry)
    private
      category:T_debuggerVariableCategory;
      children:array of P_variableTreeEntryNamedValue;
    public
      CONSTRUCTOR create(CONST category_:T_debuggerVariableCategory);
      FUNCTION canExpand:boolean; virtual;
      FUNCTION toString:string; virtual;
      FUNCTION getChildren:T_variableTreeEntries; virtual;
      DESTRUCTOR destroy; virtual;

      PROCEDURE addEntry(CONST id:string; CONST value:P_literal; CONST retainExistent:boolean);
      FUNCTION findEntryForValue(CONST value:P_literal; CONST createIfMissing:boolean=false):P_variableTreeEntryNamedValue;
      PROCEDURE clear;
  end;

IMPLEMENTATION
CONSTRUCTOR T_variableTreeEntryCategoryNode.create(CONST category_: T_debuggerVariableCategory);
  begin
    category:=category_;
    setLength(children,0);
  end;

FUNCTION T_variableTreeEntryCategoryNode.canExpand: boolean;
  begin
    result:=length(children)>0;
  end;

FUNCTION T_variableTreeEntryCategoryNode.toString: string;
  CONST name:array[T_debuggerVariableCategory] of string=('global','local','inline','parameter');
  begin
    result:=name[category];
  end;

FUNCTION T_variableTreeEntryCategoryNode.getChildren: T_variableTreeEntries;
  VAR i:longint;
  begin
    setLength(result,length(children));
    for i:=0 to length(children)-1 do result[i]:=children[i];
  end;

PROCEDURE T_variableTreeEntryCategoryNode.clear;
  VAR i:longint;
  begin
    for i:=0 to length(children)-1 do dispose(children[i],destroy);
    setLength(children,0);
  end;

DESTRUCTOR T_variableTreeEntryCategoryNode.destroy;
  begin
    clear;
  end;

PROCEDURE T_variableTreeEntryCategoryNode.addEntry(CONST id: string; CONST value: P_literal; CONST retainExistent: boolean);
  VAR i,j:longint;
  begin
    if not(retainExistent) then begin
      j:=0;
      for i:=0 to length(children)-1 do if children[i]^.id=id
      then dispose(children[i],destroy)
      else begin
        children[j]:=children[i];
        inc(j);
      end;
      setLength(children,j);
    end;
    setLength(children,length(children)+1);
    new(children[length(children)-1],create(value,id));
  end;

FUNCTION T_variableTreeEntryCategoryNode.findEntryForValue(CONST value: P_literal; CONST createIfMissing:boolean=false): P_variableTreeEntryNamedValue;
  VAR n:P_variableTreeEntryNamedValue;
  begin
    for n in children do if value^.equals(n^.value) then exit(n);
    if createIfMissing then begin
      addEntry('$'+intToStr(length(children)+1)+ '$',value,true);
      exit(children[length(children)-1]);
    end;
    result:=nil;
  end;

CONSTRUCTOR T_variableTreeEntryNamedValue.create(CONST value_: P_literal; CONST id_: T_idString);
  begin
    inherited create(value_,false);
    id:=id_;
  end;

FUNCTION T_variableTreeEntryNamedValue.toString: string;
  begin
    result:=id+' = '+inherited toString;
  end;

CONSTRUCTOR T_variableTreeEntryAnonymousValue.create(CONST L:P_literal; CONST mapEntry:boolean);
  begin
    value:=L^.rereferenced;
    isKeyValuePair:=mapEntry and (L^.literalType in C_listTypes) and (P_listLiteral(L)^.size=2);
    setLength(preparedChildren,0);
  end;

FUNCTION T_variableTreeEntryAnonymousValue.canExpand: boolean;
  begin
    if value=nil then exit;
    if isKeyValuePair
    then result:=(length(P_listLiteral(value)^.value[0]^.toString(50))>=50) or
                 (length(P_listLiteral(value)^.value[1]^.toString(50))>=50)
    else result:=(value^.literalType in C_compoundTypes) and (length(value^.toString(100))>=100);
  end;

FUNCTION T_variableTreeEntryAnonymousValue.toString: string;
  begin
    if value=nil then exit(' ');
    if isKeyValuePair then begin
      result:=P_listLiteral(value)^.value[0]^.toString();
      if length(result)>=50
      then result:='key-value-pair'
      else result:=result+' => ';
      if length(P_listLiteral(value)^.value[1]^.toString(50))>=50
      then result:=result+P_listLiteral(value)^.value[1]^.typeString
      else result:=result+P_listLiteral(value)^.value[1]^.toString();
    end else if canExpand then result:=value^.typeString
    else                       result:=value^.toString();
  end;

FUNCTION T_variableTreeEntryAnonymousValue.getChildren: T_variableTreeEntries;
  VAR i:longint;
      iter:T_arrayOfLiteral;
  begin
    if (length(preparedChildren)=0) and (canExpand) then begin
      if isKeyValuePair then begin
        if length(P_listLiteral(value)^.value[0]^.toString())>=50 then begin
          setLength(preparedChildren,2);
          new(preparedChildren[0],create(P_listLiteral(value)^.value[0],false));
          new(preparedChildren[1],create(P_listLiteral(value)^.value[1],false));
        end else begin
          setLength(preparedChildren,1);
          new(preparedChildren[0],create(P_listLiteral(value)^.value[1],false));
        end;
      end else begin
        iter:=P_compoundLiteral(value)^.iteratableList;
        setLength(preparedChildren,length(iter));
        for i:=0 to length(iter)-1 do new(preparedChildren[i],create(iter[i],value^.literalType in C_mapTypes));
        disposeLiteral(iter);
      end;
    end;
    setLength(result,length(preparedChildren));
    for i:=0 to length(result)-1 do result[i]:=preparedChildren[i];
  end;

DESTRUCTOR T_variableTreeEntryAnonymousValue.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(preparedChildren)-1 do dispose(preparedChildren[i],destroy);
    setLength(preparedChildren,0);
    disposeLiteral(value);
  end;

end.
