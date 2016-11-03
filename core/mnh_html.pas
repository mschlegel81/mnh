UNIT mnh_html;
INTERFACE
USES sysutils,mnh_constants,myStringUtil,mnh_litVar,mnh_out_adapters,mnh_basicTypes,FileUtil,mnh_tokens,base64;
CONST HTML_FILE_START:ansistring= '<!doctype html> <html> <head>'+
  '<meta charset="UTF8"> <style> body { padding-left: 1em; font-family: Georgia, "Times New Roman", Times, '+
  'serif; color: black; background-color: #EEEEEE} h1 { font-family: Helvetica, Geneva, Arial, SunSans-Regu'+
  'lar, sans-serif } code { font-family: Courier-New, Courier; white-space: pre } table { display: inline-t'+
  'able} .oben    { vertical-align:top} .red {color:#FF0000} .ruleHead  { vertical-align:top; background-co'+
  'lor: #DDDDDD} .identifier{color:#0000FF} .builtin{color:#0000FF;font-weight:bold} .modifier{color:#FF880'+
  '0;font-weight:bold} .stringLiteral{color:#008800} .literal{color:#FF0000} .operator{color:#000088;font-w'+
  'eight:bold} .comment{color:#666666;font-style:italic} .error{color:#FF0000; background-color:#FFFF00} </style> </head><body>';
  HTML_FILE_END='</body></html>';
TYPE
  P_htmlOutAdapter=^T_htmlOutAdapter;
  T_htmlOutAdapter=object(T_abstractFileOutAdapter)
    PROCEDURE flush(); virtual;
  public
    CONSTRUCTOR create(CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
    DESTRUCTOR destroy; virtual;
  end;

  T_rawTokenizeCallback=FUNCTION(CONST inputString:ansistring):T_rawTokenArray;

VAR rawTokenizeCallback:T_rawTokenizeCallback;

FUNCTION addOutfile(VAR adapters:T_adapters; CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_abstractOutAdapter;
FUNCTION span(CONST sc,txt:ansistring):ansistring;
FUNCTION imageTag(CONST fileName:ansistring):ansistring;
FUNCTION toHtmlCode(line:ansistring):ansistring;
FUNCTION escapeHtml(CONST line:ansistring):ansistring;

IMPLEMENTATION
FUNCTION addOutfile(VAR adapters:T_adapters; CONST fileNameAndOptions:ansistring; CONST appendMode:boolean=true):P_abstractOutAdapter;
  VAR htmlOutAdapter:P_htmlOutAdapter;
      fileOutAdapter:P_textFileOutAdapter;
      fileName:string;
      options:string='';
  begin
    if pos('(',fileNameAndOptions)>0 then begin
      options :=copy(fileNameAndOptions,  pos('(',fileNameAndOptions),length(fileNameAndOptions));
      fileName:=copy(fileNameAndOptions,1,pos('(',fileNameAndOptions)-1);
    end else fileName:=fileNameAndOptions;

    if uppercase(extractFileExt(fileName))='.HTML'
    then begin
      new(htmlOutAdapter,create(fileName,options,not(appendMode)));
      adapters.addOutAdapter(htmlOutAdapter,true);
      result:=htmlOutAdapter;
    end else begin
      new(fileOutAdapter,create(fileName,options,not(appendMode)));
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
        tt_intrinsicRule,
        tt_aggregatorConstructor, tt_each, tt_parallelEach, tt_while, tt_beginBlock,  tt_endBlock: result:=result+span('builtin',txt);
        tt_identifier, tt_parameterIdentifier, tt_localUserRule,
        tt_importedUserRule, tt_rulePutCacheValue,
        tt_blockLocalVariable: result:=result+span('identifier',txt);
        tt_typeCheckScalar..tt_typeCheckKeyValueList: result:=result+span('builtin',txt);
        tt_modifier_private..tt_modifier_local: result:=result+span('modifier',txt);
        tt_comparatorEq..tt_cso_assignAppend: result:=result+span('operator',txt);
        tt_blank: begin
                    if startsWith(trim(txt),COMMENT_PREFIX) then result:=result+span('comment',txt) else result:=result+txt;
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

CONSTRUCTOR T_htmlOutAdapter.create(CONST fileName:ansistring; CONST messageTypesToInclude_:T_messageTypeSet; CONST forceNewFile:boolean);
  begin
    inherited create(at_htmlFile,fileName,messageTypesToInclude_,forceNewFile);
  end;

DESTRUCTOR T_htmlOutAdapter.destroy;
  begin inherited destroy; end;

PROCEDURE T_htmlOutAdapter.flush;
  FUNCTION imageDataTag(CONST pngString:string):string;
    begin
      result:='<img alt="mnh plot" src="data:image/png;base64,'+EncodeStringBase64(pngString)+'"/>';
    end;

  FUNCTION imageLink(CONST fileName:string):string;
    begin
      result:='<a href="'+replaceAll(extractRelativePath(outputFileName,fileName),'\','/')+'">'+fileName+'</a>'
    end;

  VAR handle:text;
      i,j:longint;
  begin
    if length(storedMessages)=0 then exit;
    try
      enterCriticalSection(cs);
      assign(handle,outputFileName);
      if fileExists(outputFileName) and not(forceRewrite)
      then system.append(handle)
      else begin
        rewrite(handle);
        writeln(handle,replaceOne(HTML_FILE_START,'%','10'),'<table>');
      end;
      forceRewrite:=false;
      for i:=0 to length(storedMessages)-1 do begin
        with storedMessages[i] do case messageType of
          mt_printline:
            if length(multiMessage)<1 then writeln(handle,'<tr></tr>') else
            if length(multiMessage)=1 then write(handle,'<tr><td></td><td></td><td><code>',escapeHtml(multiMessage[0]),'</code></td></tr>')
            else begin
              writeln(handle,'<tr><td></td><td></td><td><code>',escapeHtml(multiMessage[0]));
              for j:=1 to length(multiMessage)-2 do writeln(handle,escapeHtml(multiMessage[j]));
              writeln(handle,escapeHtml(multiMessage[length(multiMessage)-1]),'</code></td></tr>');
            end;
          mt_endOfEvaluation:
            begin
              {$ifdef fullVersion}
              if length(multiMessage)=1 then writeln(handle,'<tr><td></td><td></td><td>',imageDataTag(multiMessage[0]),'</td></tr>');
              {$endif}
              writeln(handle,'</table><div><hr></div><table>');
            end;
          mt_echo_input,mt_echo_output,mt_echo_declaration:
            writeln(handle,'<tr><td>',C_messageTypeMeta[messageType].prefix,'</td><td></td><td><code>',toHtmlCode(simpleMessage),'</code></td></tr>');
          {$ifdef fullVersion}
          mt_plotCreatedWithInstantDisplay: writeln(handle,'<tr><td>',C_messageTypeMeta[messageType].prefix,'</td><td></td><td>',imageDataTag(multiMessage[0]),'</td></tr>');
          mt_plotFileCreated: writeln(handle,'<tr><td>',C_messageTypeMeta[messageType].prefix,'</td><td></td><td>',imageDataTag(multiMessage[0]),'<br>',imageLink(simpleMessage),'</td></tr>');
          {$endif}
          else writeln(handle,'<tr><td>',C_messageTypeMeta[messageType].prefix,'</td><td>',ansistring(location),'</td><td><code>',simpleMessage,'</code></td></tr>');
        end;
      end;
      clear;
      close(handle);
    finally
      leaveCriticalSection(cs);
    end;
  end;
end.
