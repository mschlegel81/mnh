UNIT mnh_html;
INTERFACE
USES sysutils,
     FileUtil,
     myStringUtil,
     mnh_constants,
     tokens;
TYPE
  T_rawTokenizeCallback=FUNCTION(CONST inputString:ansistring):T_rawTokenArray;

VAR rawTokenizeCallback:T_rawTokenizeCallback;

FUNCTION span(CONST sc,txt:ansistring):ansistring;
//FUNCTION imageTag(CONST fileName:ansistring):ansistring;
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
