UNIT editScripts;

{$mode objfpc}{$H+}

INTERFACE
USES Classes,
     myGenerics,
     mnh_constants,
     basicTypes,
     mnh_messages,
     out_adapters,
     recyclers,
     contexts,
     litVar,
     subrules,
     mnh_doc;

TYPE
T_scriptType     =(st_edit,st_insert,st_util);
P_scriptMeta=^T_scriptMeta;
T_scriptMetaArray=array of P_scriptMeta;
T_scriptMeta=object
  private
    name:string;
    editRule:P_subruleExpression;
    outputLanguage:string;
    createNewEditor:boolean;
    scriptType:T_scriptType;
  public
    CONSTRUCTOR create(CONST rule:P_subruleExpression; OUT isValid:boolean; CONST messages:P_messages);
    DESTRUCTOR destroy;
    PROPERTY getName:string read name;
end;

P_editScriptTask=^T_editScriptTask;
T_editScriptTask=object(T_payloadMessage)
  private
    script:P_scriptMeta;
    inputEditName:string;
    input,
    output:P_literal;
    outputLanguage:string;
    done  :boolean;
    succeeded:boolean;
  public
    CONSTRUCTOR create(CONST script_:P_scriptMeta; CONST inputEditFile:string; CONST input_:T_arrayOfString; CONST inputLang:string);
    CONSTRUCTOR createForNewEditor(CONST editLines:T_arrayOfString; CONST language:string='mnh');
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute(VAR globals:T_evaluationGlobals; VAR recycler:T_recycler);
    PROPERTY getOutput:P_literal read output;
    PROPERTY getOutputLanguage:string read outputLanguage;
    FUNCTION wantNewEditor:boolean;
    PROPERTY inputEditorPseudoName:string read inputEditName;
    FUNCTION wantOutput:boolean;
    FUNCTION wantInsert:boolean;
    PROPERTY successful:boolean read succeeded;
    FUNCTION withSuccessFlag(CONST trueWhenOk:boolean):P_editScriptTask;
end;

CONST
  C_scriptTypeMeta:array[T_scriptType] of record nameAttribute:string; validResultType:T_literalTypeSet; end=
  {st_edit} ((nameAttribute:'editScript'  ; validResultType:[lt_emptyList,lt_stringList]),
  {st_insert}(nameAttribute:'insertScript'; validResultType:[lt_string]),
  {st_util}  (nameAttribute:'utility';      validResultType:[lt_emptyList,lt_stringList,lt_void]));

FUNCTION utilityScriptFileName:string;
IMPLEMENTATION
USES
  sysutils,packages;
FUNCTION utilityScriptFileName:string;
  begin
    result:=configDir+'packages'+DirectorySeparator+'guiScripts.mnh';
    if not(fileExists(result)) then sandbox^.ensureDefaultFiles(nil,nil);
  end;

CONSTRUCTOR T_scriptMeta.create(CONST rule: P_subruleExpression; OUT isValid:boolean; CONST messages:P_messages);
  VAR t:T_scriptType;
  begin
    editRule:=rule;
    outputLanguage:=editRule^.metaData.getAttribute('language').value;
    createNewEditor:=editRule^.metaData.hasAttribute('newEdit');
    isValid:=false;
    for t in T_scriptType do if rule^.metaData.hasAttribute(C_scriptTypeMeta[t].nameAttribute) then begin
      scriptType:=t;
      name:=editRule^.metaData.getAttribute(C_scriptTypeMeta[t].nameAttribute).value;
      isValid:=true;
    end;
    if name='' then name:=editRule^.getId;
    if (scriptType=st_insert) and createNewEditor then begin
      isValid:=false;
      messages^.raiseSimpleError('Invalid attribute @newEdit for insert script',rule^.getLocation);
    end;
    if (scriptType=st_insert) and (outputLanguage<>'') then begin
      isValid:=false;
      messages^.raiseSimpleError('Invalid attribute @language for insert script',rule^.getLocation);
    end;
    if scriptType in [st_util,st_insert] then begin
      if not(rule^.acceptsSingleLiteral(lt_string)) then begin
        isValid:=false;
        messages^.raiseSimpleError('Scripts @'+C_scriptTypeMeta[scriptType].nameAttribute+' must accept a single string, the editor name',rule^.getLocation);
      end;
    end else begin
      if not(rule^.acceptsSingleLiteral(lt_stringList)) then begin
        isValid:=false;
        messages^.raiseSimpleError('Scripts @'+C_scriptTypeMeta[scriptType].nameAttribute+' must accept a single stringList, the editor contents',rule^.getLocation);
      end;
    end;
  end;

DESTRUCTOR T_scriptMeta.destroy;
  begin
  end;

CONSTRUCTOR T_editScriptTask.create(CONST script_:P_scriptMeta; CONST inputEditFile:string; CONST input_:T_arrayOfString; CONST inputLang:string);
  VAR s:string;
  begin
    inherited create(mt_guiEdit_done);
    script:=script_;
    inputEditName:=inputEditFile;
    if script^.scriptType=st_edit then begin
      input:=newListLiteral(length(input_));
      for s in input_ do P_listLiteral(input)^.appendString(s);
    end else input:=newStringLiteral(inputEditFile);
    output:=nil;
    outputLanguage:=script^.outputLanguage;
    if outputLanguage='' then begin
      if script^.scriptType=st_edit
      then outputLanguage:=inputLang
      else outputLanguage:='txt';
    end;
    done:=false;
  end;

CONSTRUCTOR T_editScriptTask.createForNewEditor(CONST editLines:T_arrayOfString; CONST language:string='mnh');
  VAR s:string;
  begin
    inherited create(mt_guiEdit_done);
    script:=nil;
    inputEditName:='';
    input:=nil;
    outputLanguage:=language;
    done:=true;
    succeeded:=true;
    output:=newListLiteral(length(editLines));
    for s in editLines do P_listLiteral(output)^.appendString(s);
    done:=false;
  end;

DESTRUCTOR T_editScriptTask.destroy;
  begin
    if output<>nil then disposeLiteral(output);
    inherited destroy;
  end;

PROCEDURE T_editScriptTask.execute(VAR globals:T_evaluationGlobals; VAR recycler:T_recycler);
  begin
    output:=script^.editRule^.evaluateToLiteral(script^.editRule^.getLocation,@globals.primaryContext,@recycler,input,nil).literal;
    disposeLiteral(input);
    if (output<>nil) and not(output^.literalType in C_scriptTypeMeta[script^.scriptType].validResultType) then begin
      globals.primaryContext.messages^.raiseSimpleError('Script failed due to invalid result type '+output^.typeString,script^.editRule^.getLocation);
      disposeLiteral(output);
    end;
    done:=true;
  end;

FUNCTION T_editScriptTask.wantNewEditor:boolean; begin result:=((script=nil) or script^.createNewEditor) and (output<>nil) and (output^.literalType=lt_stringList); end;
FUNCTION T_editScriptTask.wantOutput:boolean; begin result:=((output<>nil) and (output^.literalType=lt_stringList)) and ((script=nil) or (script^.scriptType=st_edit) or script^.createNewEditor); end;
FUNCTION T_editScriptTask.wantInsert:boolean; begin result:=((output<>nil) and (output^.literalType=lt_string)) and (script<>nil) and (script^.scriptType=st_insert); end;
FUNCTION T_editScriptTask.withSuccessFlag(CONST trueWhenOk:boolean):P_editScriptTask;
  begin
    succeeded:=trueWhenOk;
    result:=@self;
  end;

end.

