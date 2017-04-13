UNIT mnh_html;
INTERFACE
USES sysutils,
     myGenerics,myStringUtil,
     mnh_constants,mnh_basicTypes,
     mnh_litVar,mnh_out_adapters,FileUtil,mnh_tokens;
TYPE
  T_rawTokenizeCallback=FUNCTION(CONST inputString:ansistring):T_rawTokenArray;

VAR rawTokenizeCallback:T_rawTokenizeCallback;

FUNCTION span(CONST sc,txt:ansistring):ansistring;
FUNCTION imageTag(CONST fileName:ansistring):ansistring;
FUNCTION toHtmlCode(raw:T_rawTokenArray):ansistring;
FUNCTION toHtmlCode(line:ansistring):ansistring;
FUNCTION escapeHtml(CONST line:ansistring):ansistring;

IMPLEMENTATION
FUNCTION span(CONST sc,txt:ansistring):ansistring;
  begin
    if (sc='') or (txt='')
    then result:=txt
    else result:='<span class="'+sc+'">'+txt+'</span>';
  end;

FUNCTION imageTag(CONST fileName:ansistring):ansistring;
  begin
    result:='<img src="'+fileName+'" alt="'+fileName+'">';
  end;

FUNCTION toHtmlCode(raw:T_rawTokenArray):ansistring;
  VAR i:longint;
  begin
    result:='';
    for i:=0 to length(raw)-1 do with raw[i] do begin
      case tokType of
        tt_blank: if startsWith(trim(txt),COMMENT_PREFIX)
                  then result:=result+span('comment',txt)
                  else result:=result+txt;
        tt_literal: if (length(txt)>0) and (txt[1] in ['''','"','#'])
                    then result:=result+span('stringLiteral',txt)
                    else result:=result+span('literal'      ,txt);
        else result:=result+span(C_tokenInfo[tokType].defaultHtmlSpan,escapeHtml(txt));
      end;
    end;
    result:=replaceOne(
            replaceOne(result,
                       '<span class="identifier">out</span><span class="operator">></span>','out>'),
                       '<span class="operator">in</span><span class="operator">></span>','in>');
  end;

FUNCTION toHtmlCode(line:ansistring):ansistring;
  begin
    result:=toHtmlCode(rawTokenizeCallback(line));
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

end.
