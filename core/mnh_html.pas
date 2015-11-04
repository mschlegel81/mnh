UNIT mnh_html;
INTERFACE
USES sysutils,mnh_constants,myStringUtil,mnh_litVar,mnh_funcs,mnh_out_adapters,mnh_tokLoc,FileUtil;
CONST HTML_FILE_START:ansistring= '<!doctype html> <html> <head> <meta http-equiv="refresh" content="10"/> '+
  '<meta charset="ANSI"> <style> body { padding-left: 1em; font-family: Georgia, "Times New Roman", Times, '+
  'serif; color: black; background-color: #EEEEEE} h1 { font-family: Helvetica, Geneva, Arial, SunSans-Regu'+
  'lar, sans-serif } code { font-family: Courier-New, Courier; white-space: pre } table { display: inline-t'+
  'able} .oben    { vertical-align:top} .red {color:#FF0000} .ruleHead  { vertical-align:top; background-co'+
  'lor: #DDDDDD} .identifier{color:#0000FF} .builtin{color:#0000FF;font-weight:bold} .modifier{color:#FF880'+
  '0;font-weight:bold} .stringLiteral{color:#008800} .literal{color:#FF0000} .operator{color:#000088;font-w'+
  'eight:bold} .comment{color:#666666;font-style:italic} </style> </head><body><table>';

TYPE
  P_htmlOutAdapter=^T_htmlOutAdapter;
  T_htmlOutAdapter=object(T_collectingOutAdapter)
    lastFileFlushTime:double;
    outputFileName:ansistring;

    CONSTRUCTOR create(CONST fileName:ansistring);
    DESTRUCTOR destroy;
    PROCEDURE appendSingleMessage(CONST message: T_storedMessage); virtual;
  end;

FUNCTION toHtmlCode(line:ansistring):ansistring;
IMPLEMENTATION

FUNCTION toHtmlCode(line:ansistring):ansistring;
  VAR parsedLength:longint=0;
  FUNCTION span(CONST sc,txt:ansistring):ansistring;
    begin
      result:='<span class="'+sc+'">'+txt+'</span>';
    end;

  FUNCTION leadingIdLength(CONST allowQualified:boolean):longint;
    VAR i:longint;
    begin
      i:=1;
      while (i<length(line)) and (line[i+1] in ['a'..'z','A'..'Z','0'..'9','_',C_ID_QUALIFY_CHARACTER]) do begin
        inc(i);
        if line[i]=C_ID_QUALIFY_CHARACTER then begin
          if (i<length(line)) and (line[i+1]=C_ID_QUALIFY_CHARACTER) then begin
            exit(i-1);
          end else if not(allowQualified) then begin
            exit(length(line));
          end;
        end;
      end;
      result:=i;
    end;

  FUNCTION takeFromLine(CONST len:longint):ansistring;
    begin
      result:=copy(line,1,len);
      line:=copy(line,len+1,length(line)+len-1);
    end;

  PROCEDURE removeLeadingBlanks();
    VAR i:longint;
    begin
      i:=1;
      while (i<=length(line)) and (line[i] in [' ',C_lineBreakChar,C_tabChar,C_carriageReturnChar]) do inc(i);
      if i>1 then result:=result+takeFromLine(i-1);
    end;

  VAR id:ansistring;
      rwc:T_reservedWordClass;
  begin
    result:='';
    while length(line)>0 do begin
      parsedLength:=0;
      removeLeadingBlanks;
      if length(line)<1 then exit(result);
      case line[1] of
        '0'..'9': begin
          parseNumber(line,1,true,parsedLength);
          if parsedLength<=0 then begin result:=result+line; line:=''; end
                             else result:=result+span('literal', takeFromLine(parsedLength));
        end;
        '"','''': begin
          id:=unescapeString(line,1,parsedLength);
          if parsedLength=0 then begin result:=result+line; line:=''; end
          else result:=result+span('stringLiteral', takeFromLine(parsedLength));
        end;
        '$': result:=result+span('identifier', takeFromLine(leadingIdLength(false)));
        'a'..'z','A'..'Z':
          if copy(line,1,3)='in>' then result:=result+takeFromLine(3)
          else if copy(line,1,4)='out>' then result:=result+takeFromLine(4)
          else begin
            id:=takeFromLine(leadingIdLength(true));
            rwc:=isReservedWord(id);
            case rwc of
              rwc_specialConstruct  : result:=result+span('builtin',id);
              rwc_specialLiteral    : result:=result+span('literal',id);
              rwc_operator          : result:=result+span('operator',id);
              rwc_modifier          : result:=result+span('modifier',id);
              else begin
                if intrinsicRuleMap.containsKey(id) then result:=result+span('builtin',id)
                                                    else result:=result+span('identifier',id);
              end;
            end;
          end;
        ';',')','}','(','{',',',']','[': result:=result+takeFromLine(1);
        '@','^','?': result:=result+span('operator',takeFromLine(1));
        '|': if startsWith(line,'|=') then result:=result+span('operator',takeFromLine(2))
                                      else result:=result+span('operator',takeFromLine(1));
        '+': if startsWith(line,C_tokenString[tt_cso_assignPlus])
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '&': if startsWith(line,C_tokenString[tt_cso_assignStrConcat])
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '-': if startsWith(line,'->') or startsWith(line,C_tokenString[tt_cso_assignMinus])
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '*': if startsWith(line,'**') or startsWith(line,C_tokenString[tt_cso_assignMult])
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '>': if startsWith(line,'>=')
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '!': if startsWith(line,'!=')
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '=': if startsWith(line,'==')
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '<': if startsWith(line,'<>') or startsWith(line,'<=')
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '/': if startsWith(line,'//') then begin //comments
               parsedLength:=2;
               while (parsedLength<length(line)) and not(line[parsedLength+1] in [C_lineBreakChar,C_carriageReturnChar]) do inc(parsedLength);
               result:=result+span('comment',takeFromLine(parsedLength));
             end else if startsWith(line,C_tokenString[tt_cso_assignDiv])
             then result:=result+span('operator',takeFromLine(2))
             else result:=result+span('operator',takeFromLine(1));
        '%': if      startsWith(line,'%%%%') then result:=result+span('operator',takeFromLine(4))
             else if startsWith(line,'%%%') then  result:=result+span('operator',takeFromLine(3))
             else if startsWith(line,'%%') then   result:=result+span('operator',takeFromLine(2))
             else                                 result:=result+span('operator',takeFromLine(1));
        '.': if startsWith(line,C_tokenString[tt_optionalParameters]) then result:=result+span('operator',takeFromLine(3))
             else if startsWith(line,C_tokenString[tt_separatorCnt]) then result:=result+span('operator',takeFromLine(2))
             else result:=result+takeFromLine(length(line));
        ':': if startsWith(line,':=') then result:=result+span('operator',takeFromLine(2))
             else if (length(line)>=4) and (line[2] in ['b','e','i','l','n','s','r','k']) then begin
               id:=takeFromLine(leadingIdLength(true));
               if (id=C_tokenString[tt_typeCheckBoolList  ])
               or (id=C_tokenString[tt_typeCheckBoolean   ])
               or (id=C_tokenString[tt_typeCheckExpression])
               or (id=C_tokenString[tt_typeCheckIntList   ])
               or (id=C_tokenString[tt_typeCheckInt       ])
               or (id=C_tokenString[tt_typeCheckList      ])
               or (id=C_tokenString[tt_typeCheckNumList   ])
               or (id=C_tokenString[tt_typeCheckNumeric   ])
               or (id=C_tokenString[tt_typeCheckStringList])
               or (id=C_tokenString[tt_typeCheckScalar    ])
               or (id=C_tokenString[tt_typeCheckString    ])
               or (id=C_tokenString[tt_typeCheckRealList  ])
               or (id=C_tokenString[tt_typeCheckReal      ])
               or (id=C_tokenString[tt_typeCheckKeyValueList]) then result:=result+span('operator',id)
               else result:=result+span('operator',takeFromLine(1));
             end else result:=result+span('operator',takeFromLine(1));
        else begin
          result:=result+takeFromLine(length(line));
        end;
      end;
    end;
  end;

CONSTRUCTOR T_htmlOutAdapter.create(CONST fileName:ansistring);
  begin
    inherited create;
    outputFileName:=expandFileName(fileName);
    with outputBehaviour do begin
      doEchoDeclaration  :=true;
      doEchoInput        :=true;
      doShowExpressionOut:=true;
      doShowTimingInfo   :=true;
      minErrorLevel      :=2;
    end;
  end;

DESTRUCTOR T_htmlOutAdapter.destroy;
  begin inherited destroy; end;

PROCEDURE T_htmlOutAdapter.appendSingleMessage(CONST message: T_storedMessage);
  VAR handle:text;
      i,j:longint;
  begin
    if (message.messageType<>mt_clearConsole) then inherited appendSingleMessage(message);
    if (message.messageType in [mt_endOfEvaluation, mt_clearConsole]) or (now-lastFileFlushTime>1/(24*60*60)) then begin
      assign(handle,outputFileName);
      if fileExists(outputFileName) then begin
        append(handle);
      end else begin
        rewrite(handle);
        writeln(handle,HTML_FILE_START);
        writeln(handle,'<!--This is: ',outputFileName,'-->');
      end;
      for i:=0 to length(storedMessages)-1 do with storedMessages[i] do case messageType of
        mt_clearConsole: begin end;

        mt_printline:
        if length(multiMessage)<1 then writeln(handle,'<tr></tr>') else
        if length(multiMessage)=1 then write(handle,'<tr><td></td><td></td><td><code>',multiMessage[0],'</code></td></tr>')
        else begin
          writeln(handle,'<tr><td></td><td></td><td><code>',multiMessage[0]);
          for j:=1 to length(multiMessage)-2 do writeln(handle,multiMessage[j]);
          writeln(handle,multiMessage[length(multiMessage)-1],'</code></td></tr>');
        end;

        mt_endOfEvaluation: writeln(handle,'</table><div><hr></div><table>');

        mt_echo_input,mt_echo_output,mt_echo_declaration: writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td></td><td><code>',toHtmlCode(simpleMessage),'</code></td></tr>');
        mt_debug_step:                                    writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td>',ansistring(location),'</td><td><code>',toHtmlCode(simpleMessage),'</code></td></tr>');
        mt_imageCreated: writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td></td><td><img src="',
                                 extractRelativePath(outputFileName,simpleMessage),
                                 '" alt="',simpleMessage,'"></td></tr>');
        else writeln(handle,'<tr><td>',C_errorLevelTxt[messageType],'</td><td>',ansistring(location),'</td><td><code>',simpleMessage,'</code></td></tr>');
      end;
      clearMessages;
      close(handle);
      lastFileFlushTime:=now;
    end;
  end;



end.
