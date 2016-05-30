UNIT mnh_html;
INTERFACE
USES sysutils,mnh_constants,myStringUtil,mnh_litVar,mnh_out_adapters,mnh_tokLoc,FileUtil,mnh_tokens;
CONST HTML_FILE_START:ansistring= '<!doctype html> <html> <head> <meta http-equiv="refresh" content="%"/> '+
  '<meta charset="ANSI"> <style> body { padding-left: 1em; font-family: Georgia, "Times New Roman", Times, '+
  'serif; color: black; background-color: #EEEEEE} h1 { font-family: Helvetica, Geneva, Arial, SunSans-Regu'+
  'lar, sans-serif } code { font-family: Courier-New, Courier; white-space: pre } table { display: inline-t'+
  'able} .oben    { vertical-align:top} .red {color:#FF0000} .ruleHead  { vertical-align:top; background-co'+
  'lor: #DDDDDD} .identifier{color:#0000FF} .builtin{color:#0000FF;font-weight:bold} .modifier{color:#FF880'+
  '0;font-weight:bold} .stringLiteral{color:#008800} .literal{color:#FF0000} .operator{color:#000088;font-w'+
  'eight:bold} .comment{color:#666666;font-style:italic} .error{color:#FF0000; background-color:#FFFF00} </style> </head><body>';
  HTML_FILE_END='</body></html>';
TYPE
  P_htmlOutAdapter=^T_htmlOutAdapter;
  T_htmlOutAdapter=object(T_collectingOutAdapter)
    lastFileFlushTime:double;
    outputFileName:ansistring;
    lastWasEndOfEvaluation:boolean;
    CONSTRUCTOR create(CONST fileName:ansistring);
    DESTRUCTOR destroy; virtual;
    PROCEDURE append(CONST message: T_storedMessage); virtual;
    PROCEDURE flush;
  end;

  T_rawTokenizeCallback=FUNCTION(CONST inputString:ansistring):T_rawTokenArray;

VAR rawTokenizeCallback:T_rawTokenizeCallback;

FUNCTION addOutfile(VAR adapters:T_adapters; CONST fileName:ansistring):P_abstractOutAdapter;
FUNCTION span(CONST sc,txt:ansistring):ansistring;
FUNCTION imageTag(CONST fileName:ansistring):ansistring;
FUNCTION toHtmlCode(line:ansistring):ansistring;
FUNCTION escapeHtml(CONST line:ansistring):ansistring;

IMPLEMENTATION
FUNCTION addOutfile(VAR adapters:T_adapters; CONST fileName:ansistring):P_abstractOutAdapter;
  VAR htmlOutAdapter:P_htmlOutAdapter;
      fileOutAdapter:P_textFileOutAdapter;
  begin
    if uppercase(extractFileExt(fileName))='.HTML'
    then begin
      new(htmlOutAdapter,create(fileName));
      adapters.addOutAdapter(htmlOutAdapter,true);
      result:=htmlOutAdapter;
    end else begin
      new(fileOutAdapter,create(fileName));
      adapters.addOutAdapter(fileOutAdapter,true);
      result:=fileOutAdapter;
    end;
  end;

FUNCTION span(CONST sc,txt:ansistring):ansistring;
  begin
    result:='<span class="'+sc+'">'+txt+'</span>';
  end;

FUNCTION imageTag(CONST fileName:ansistring):ansistring;
  begin
    result:='<img src="'+fileName+'" alt="'+fileName+'">';
  end;

FUNCTION toHtmlCode(line:ansistring):ansistring;
  VAR raw:T_rawTokenArray;
      i:longint;
  begin
    result:='';
    raw:=rawTokenizeCallback(line);
    for i:=0 to length(raw)-1 do with raw[i] do begin
      case tokType of
        tt_literal, tt_aggregatorExpressionLiteral: result:=result+span('literal',txt);
        tt_intrinsicRule,tt_intrinsicRule_pon,
        tt_aggregatorConstructor, tt_each, tt_parallelEach, tt_forcedParallelEach, tt_when, tt_while, tt_try, tt_begin,  tt_end: result:=result+span('builtin',txt);
        tt_identifier, tt_parameterIdentifier, tt_localUserRule,
        tt_importedUserRule, tt_rulePutCacheValue,
        tt_identifier_pon,
        tt_localUserRule_pon,
        tt_importedUserRule_pon,
        tt_blockLocalVariable: result:=result+span('identifier',txt);
        tt_typeCheckScalar..tt_typeCheckKeyValueList: result:=result+span('builtin',txt);
        tt_modifier_private..tt_modifier_local: result:=result+span('modifier',txt);
        tt_comparatorEq..tt_cso_assignAppend: result:=result+span('operator',txt);
        tt_blank: begin
                    if (copy(trim(txt),1,2)='//') then result:=result+span('comment',txt) else result:=result+txt;
                  end;
        else result:=result+txt;
      end;
    end;
    result:=replaceOne(
            replaceOne(result,
                       '<span class="identifier">out</span><span class="operator">></span>','out>'),
                       '<span class="operator">in</span><span class="operator">></span>','in>');
  end;

FUNCTION escapeHtml(CONST line:ansistring):ansistring;
  VAR i0,i1,i:longint;
  begin
    result:='';
    i0:=1; i1:=0;
    for i:=1 to length(line) do begin
      case line[i] of
        '&': begin result:=result+copy(line,i0,i1-i0+1)+'&amp;'; i0:=i+1; i1:=i; end;
        '<': begin result:=result+copy(line,i0,i1-i0+1)+'&lt;' ; i0:=i+1; i1:=i; end;
        '>': begin result:=result+copy(line,i0,i1-i0+1)+'&gt;' ; i0:=i+1; i1:=i; end;
      else i1:=i;
      end;
    end;
    result:=result+copy(line,i0,i1-i0+1);
  end;

CONSTRUCTOR T_htmlOutAdapter.create(CONST fileName:ansistring);
  begin
    inherited create(at_htmlFile);
    outputFileName:=expandFileName(fileName);
    lastFileFlushTime:=now;
    lastWasEndOfEvaluation:=true;
    with outputBehaviour do begin
      doEchoDeclaration  :=true;
      doEchoInput        :=true;
      doShowExpressionOut:=true;
      doShowTimingInfo   :=true;
      minErrorLevel      :=2;
    end;
  end;

DESTRUCTOR T_htmlOutAdapter.destroy;
  begin flush; inherited destroy; end;

PROCEDURE T_htmlOutAdapter.append(CONST message: T_storedMessage);
  begin
    if (message.messageType<>mt_clearConsole) then inherited append(message);
    with storedMessages[length(storedMessages)-1] do if messageType in [mt_el3_stackTrace] then simpleMessage:=replaceAll(simpleMessage,#28,' ');
    if (message.messageType in [mt_endOfEvaluation, mt_clearConsole]) or (now-lastFileFlushTime>1/(24*60*60)) then flush;
  end;

PROCEDURE T_htmlOutAdapter.flush;
  VAR handle:text;
      i,j:longint;
  begin
    if length(storedMessages)=0 then exit;
    try
    assign(handle,outputFileName);
    if fileExists(outputFileName) then begin
      system.append(handle);
    end else begin
      rewrite(handle);
      writeln(handle,replaceOne(HTML_FILE_START,'%','10'),'<table>');
    end;
      for i:=0 to length(storedMessages)-1 do begin
        with storedMessages[i] do case messageType of
          mt_clearConsole: begin end;

          mt_printline:
          if length(multiMessage)<1 then writeln(handle,'<tr></tr>') else
          if length(multiMessage)=1 then write(handle,'<tr><td></td><td></td><td><code>',escapeHtml(multiMessage[0]),'</code></td></tr>')
          else begin
            writeln(handle,'<tr><td></td><td></td><td><code>',escapeHtml(multiMessage[0]));
            for j:=1 to length(multiMessage)-2 do writeln(handle,escapeHtml(multiMessage[j]));
            writeln(handle,escapeHtml(multiMessage[length(multiMessage)-1]),'</code></td></tr>');
          end;

          mt_endOfEvaluation: if not(lastWasEndOfEvaluation) then writeln(handle,'</table><div><hr></div><table>');

          mt_echo_input,mt_echo_output,mt_echo_declaration: writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td></td><td><code>',toHtmlCode(simpleMessage),'</code></td></tr>');
          {$ifdef fullVersion}
          mt_plotFileCreated: writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td></td><td>',
                                   imageTag(extractRelativePath(outputFileName,simpleMessage)),'</td></tr>');
          mt_plotCreatedWithDeferredDisplay,mt_plotCreatedWithInstantDisplay,mt_plotSettingsChanged: begin end;
          {$endif}
          else writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td>',ansistring(location),'</td><td><code>',simpleMessage,'</code></td></tr>');
        end;
        lastWasEndOfEvaluation:=storedMessages[i].messageType=mt_endOfEvaluation;
      end;
      close(handle);
    finally
      clearMessages;
      lastFileFlushTime:=now;
    end;
  end;
end.
