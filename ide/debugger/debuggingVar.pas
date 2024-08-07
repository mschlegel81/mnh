UNIT debuggingVar;
INTERFACE
USES sysutils,
     mnh_constants,
     basicTypes,
     litVar,
     treeUtil;
TYPE
  P_variableTreeEntryAnonymousValue=^T_variableTreeEntryAnonymousValue;
  T_variableTreeEntryAnonymousValue=object(I_treeEntry)
    private
      value:P_literal;
      isKeyValuePair:boolean;
      preparedChildren:array of P_variableTreeEntryAnonymousValue;
    public
      CONSTRUCTOR create(CONST L:P_literal; CONST mapEntry:boolean);
      FUNCTION canExpand:boolean; virtual;
      FUNCTION toString:string; virtual;
      FUNCTION toStringForErrorTrace:string; virtual;
      FUNCTION getChildren:T_treeEntries; virtual;
      DESTRUCTOR destroy; virtual;
  end;

  P_variableTreeEntryNamedValue=^T_variableTreeEntryNamedValue;
  T_variableTreeEntryNamedValue=object(T_variableTreeEntryAnonymousValue)
    private
      id      :T_idString;
    public
      CONSTRUCTOR create(CONST value_:P_literal; CONST id_:T_idString);
      FUNCTION toString:string; virtual;
      FUNCTION toStringForErrorTrace:string; virtual;
      PROPERTY getIdOnly:T_idString read id;
      DESTRUCTOR destroy; virtual;
  end;

  T_debuggerVariableCategory=(dvc_global,          //mutable of datastore from this package
                              dvc_local,
                              dvc_inline,
                              dvc_callParameter);

  P_variableTreeEntryCategoryNode=^T_variableTreeEntryCategoryNode;
  T_variableTreeEntryCategoryNode=object(I_treeEntry)
    private
      category:T_debuggerVariableCategory;
      children:array of P_variableTreeEntryNamedValue;
    public
      CONSTRUCTOR create(CONST category_:T_debuggerVariableCategory);
      FUNCTION canExpand:boolean; virtual;
      FUNCTION toString:string; virtual;
      FUNCTION toStringForErrorTrace:string; virtual;
      FUNCTION getChildren:T_treeEntries; virtual;
      DESTRUCTOR destroy; virtual;

      PROCEDURE addEntry(CONST id:string; CONST value:P_literal; CONST retainExistent:boolean);
      FUNCTION findEntryForValue(CONST value:P_literal; CONST createIfMissing:boolean=false):P_variableTreeEntryNamedValue;
      PROCEDURE clear;
  end;

  F_fillCategoryNode=PROCEDURE(VAR cn:T_variableTreeEntryCategoryNode) of object;

FUNCTION newCallParametersNode(CONST L:P_listLiteral):P_variableTreeEntryCategoryNode;
IMPLEMENTATION
USES recyclers,myStringUtil,LazUTF8;
FUNCTION newCallParametersNode(CONST L:P_listLiteral):P_variableTreeEntryCategoryNode;
  VAR i:longint;
  begin
    new(result,create(dvc_callParameter));
    if L<>nil then for i:=0 to L^.size-1 do result^.addEntry('$'+intToStr(i),L^.value[i],true);
  end;

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

FUNCTION T_variableTreeEntryCategoryNode.toStringForErrorTrace:string;
  VAR child:P_variableTreeEntryNamedValue;
  begin
    if category<>dvc_callParameter then exit('');
    result:='';
    for child in children do result+=child^.toStringForErrorTrace+'; ';
  end;

FUNCTION T_variableTreeEntryCategoryNode.getChildren: T_treeEntries;
  VAR i:longint;
  begin
    initialize(result);
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

FUNCTION T_variableTreeEntryNamedValue.toStringForErrorTrace:string;
  begin
    result:=id+'='+inherited toStringForErrorTrace;
  end;

CONSTRUCTOR T_variableTreeEntryAnonymousValue.create(CONST L:P_literal; CONST mapEntry:boolean);
  begin
    value:=L^.rereferenced;
    isKeyValuePair:=mapEntry and (L^.literalType in C_listTypes) and (P_listLiteral(L)^.size=2);
    setLength(preparedChildren,0);
  end;

FUNCTION toStringForTree(CONST value:P_literal; CONST isKeyValuePair:boolean):string;
  VAR
    nonescapableFound: boolean;
  begin
    try
      if isKeyValuePair and (value^.literalType in C_listTypes) and (P_listLiteral(value)^.size=2) then begin
        result:=toStringForTree(P_listLiteral(value)^.value[0],false)+' => '+
                toStringForTree(P_listLiteral(value)^.value[1],false);
      end else if value^.literalType=lt_string then begin
        result:=P_stringLiteral(value)^.value;
        if length(result)>100 then begin
          if P_stringLiteral(value)^.getEncoding=se_utf8
          then result:=UTF8Copy(result,1,97)+'...'
          else result:=copy    (result,1,97)+'...';
        end;
        result:=escapeString(result,es_pickShortest,P_stringLiteral(value)^.getEncoding,nonescapableFound);
      end else result:=value^.toString(100);
    except
      result:='<error>';
    end;
  end;

FUNCTION T_variableTreeEntryAnonymousValue.canExpand: boolean;
  begin
    if value=nil then exit(false);
    if length(preparedChildren)>0 then exit(true);
    if isKeyValuePair
    then result:=length(toStringForTree(value,true))>50
    else result:=(value^.literalType in C_compoundTypes);
  end;

FUNCTION T_variableTreeEntryAnonymousValue.toString: string;
  begin
    if value=nil then exit(' ');
    if isKeyValuePair then begin
      result:=toStringForTree(value,true);
      if length(result)>50 then result:=toStringForTree(P_listLiteral(value)^.value[0],false)+' =>';
    end else if value^.literalType in C_compoundTypes then exit(value^.typeString)
    else result:=toStringForTree(value,isKeyValuePair);
  end;

FUNCTION T_variableTreeEntryAnonymousValue.toStringForErrorTrace:string;
  begin
    if value=nil
    then result:=''
    else result:=value^.toString(100);
  end;

FUNCTION T_variableTreeEntryAnonymousValue.getChildren: T_treeEntries;
  VAR i:longint;
      iter:T_arrayOfLiteral;
      recycler:P_recycler;
  begin
    if (length(preparedChildren)=0) and (canExpand) then begin
      recycler:=newRecycler;
      if isKeyValuePair then begin
        setLength(preparedChildren,1);
        new(preparedChildren[0],create(P_listLiteral(value)^.value[1],false));
      end else begin
        iter:=P_compoundLiteral(value)^.forcedIterableList(recycler);
        setLength(preparedChildren,length(iter));
        for i:=0 to length(iter)-1 do new(P_variableTreeEntryAnonymousValue(preparedChildren[i]),create(iter[i],value^.literalType in C_mapTypes));
        recycler^.disposeLiterals(iter);
      end;
      freeRecycler(recycler);
    end;
    initialize(result);
    setLength(result,length(preparedChildren));
    for i:=0 to length(result)-1 do result[i]:=preparedChildren[i];
  end;

DESTRUCTOR T_variableTreeEntryAnonymousValue.destroy;
  VAR i:longint;
      recycler:P_recycler;
  begin
    recycler:=newRecycler;
    for i:=0 to length(preparedChildren)-1 do dispose(preparedChildren[i],destroy);
    setLength(preparedChildren,0);
    recycler^.disposeLiteral(value);
    freeRecycler(recycler);
  end;

DESTRUCTOR T_variableTreeEntryNamedValue.destroy;
  begin
    id:='';
    inherited destroy;
  end;

end.
