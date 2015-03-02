UNIT mnh_out_adapters;
INTERFACE
USES mnh_stringutil, mnh_constants;
TYPE
  T_colorChannel=(cc_red,cc_green,cc_blue_cc_alpha);
  
  T_styleOption=(so_line,so_plus,so_cross,so_impulse,so_fill,so_rectangle);
  
  T_style=record
    title:string;
    styleOption:array [T_styleOption] of boolean;
    color:array[T_colorChannel] of byte;
  end;
  
  T_sampleRow=record
    style:T_style;
    sample:array of array [0..1] of double;  
  end;
  
  T_scaleStyle=(ss_linX_linY, ss_linX_logY,
                ss_logX_linY, ss_logX_logY,
                ss_autoscaleLinPreserveAspect,ss_autoscaleLinFill,ss_autoscaleLinNormalize,
                ss_autoscaleLogPreserveAspect,ss_autoscaleLogFill,ss_autoscaleLogNormalize);
                
  T_plot=object
    xAxisLabel,
    yAxisLabel:string;
    range:array['x'..'y',0..1] of double;
    tics,grid:array['x'..'y'] of boolean;
    scaleStyle:T_scaleStyle;
    line:array of T_sampleRow;
         
    CONSTRUCTOR createWithDefaults;
    DESTRUCTOR destroy;
    PROCEDURE addSampleRow(CONST sampleRow:T_sampleRow);
    PROCEDURE finalizeAutoscale(CONST displayWidth,displayHeight:double);
    PROCEDURE clear;
  end;
  
  T_writeCallback=PROCEDURE(CONST s:ansistring);
  T_plotCallback =PROCEDURE(VAR p:T_plot);
  
VAR inputDeclEcho,
    inputExprEcho,
    exprOut ,
    errorOut,
    printOut,
    tablePrintOut:T_writeCallback;
    plotOut:T_plotCallback;
    maxErrorLevel:T_errorLevel;

PROCEDURE writeDeclEcho(CONST s:ansistring);
PROCEDURE writeExprEcho(CONST s:ansistring);
PROCEDURE writeExprOut (CONST s:ansistring);
PROCEDURE writePrint   (CONST s:ansistring);
FUNCTION hasPlottingSupport:boolean;
PROCEDURE plot(VAR p:T_plot);      
PROCEDURE clearErrors;
PROCEDURE raiseError(CONST errorLevel:T_errorLevel; CONST errorMessage,errorLocation:ansistring);
FUNCTION errorLevel:T_errorLevel;

  
PROCEDURE plainConsoleOut(CONST s:ansistring);
PROCEDURE plainStdErrOut(CONST s:ansistring);

PROCEDURE printMemoryStatus;

IMPLEMENTATION
PROCEDURE writeDeclEcho(CONST s:ansistring); begin if inputDeclEcho<>nil then inputDeclEcho(s); end;  
PROCEDURE writeExprEcho(CONST s:ansistring); begin if inputExprEcho<>nil then inputExprEcho(s); end;
PROCEDURE writeExprOut (CONST s:ansistring); begin if exprOut<>nil       then exprOut(s); end;
PROCEDURE writePrint   (CONST s:ansistring); 
  VAR i:longint;
      tmp:ansistring;
  begin 
    if pos(C_tabChar,s)>0 then begin
      if tablePrintOut<>nil then tablePrintOut(s)
      else if printOut<>nil then writePrint(formatTabs(s));
    end else if printOut<>nil then begin
      tmp:=s;
      i:=pos(C_lineBreakChar,tmp);
      while i>0 do begin
        if (i>1) and (tmp[i-1]=C_carriaceReturnChar) 
        then printOut(copy(tmp,1,i-2))
        else printOut(copy(tmp,1,i-1));
        tmp:=copy(tmp,i+1,length(tmp));        
        i:=pos(C_lineBreakChar,tmp);
      end;
      printOut(tmp);
//       printOut(s);
    end;
  end;

PROCEDURE clearErrors;
  begin
    maxErrorLevel:=el0_allOkay;
  end;

PROCEDURE raiseError(CONST errorLevel:T_errorLevel; CONST errorMessage,errorLocation:ansistring);
  begin
    if errorLevel>maxErrorLevel then maxErrorLevel:=errorLevel;
    if errorOut<>nil then errorOut(C_errorLevelTxt[errorLevel]+errorMessage+'  @'+errorLocation);
  end;
  
FUNCTION errorLevel:T_errorLevel; 
  begin
    result:=maxErrorLevel;
  end;
  
FUNCTION hasPlottingSupport:boolean;
  begin
    result:=(plotOut<>nil);
    if not(result) then begin
    
    end;
  end;
  
PROCEDURE plot(VAR p:T_plot);
  begin
    if (plotOut<>nil) then plotOut(p)
    else begin
      if el3_evalError > maxErrorLevel then maxErrorLevel:=el3_evalError;
      if errorOut<>nil then errorOut(C_errorLevelTxt[el3_evalError]+' Plotting is not supported by the current adapter');
    end;
  end;

  
PROCEDURE plainConsoleOut(CONST s:ansistring);
  begin
    writeln(s);
  end;
 
PROCEDURE plainStdErrOut(CONST s:ansistring);
  begin
    writeln(stdErr,s);
  end;
  
CONSTRUCTOR T_plot.createWithDefaults;
  begin
    xAxisLabel:='x';
    yAxisLabel:='y';
    range['x',0]:=-1;
    range['x',1]:= 1;
    range['y',0]:=-1;
    range['y',1]:= 1;
    tics['x']:=true;
    tics['y']:=true;
    grid['x']:=false;
    grid['y']:=false;
    scaleStyle:=ss_linX_linY;
    clear;
  end;

DESTRUCTOR T_plot.destroy;
  begin clear; end;

PROCEDURE T_plot.addSampleRow(CONST sampleRow:T_sampleRow);
  begin
    setLength(line,length(line)+1);
    line[length(line)-1]:=sampleRow;
  end;
  
PROCEDURE T_plot.finalizeAutoscale(CONST displayWidth,displayHeight:double);
  begin
    {$WARNING unimplemented}
    //case scaleStyle of
    //  ss_autoscaleLinPreserveAspect
    //  ss_autoscaleLogPreserveAspect
    //  ss_autoscaleLinFill
    //  ss_autoscaleLinNormalize
    //  
    //  ss_autoscaleLogFill
    //  ss_autoscaleLogNormalize
    //
    //end;
  end;
  
PROCEDURE T_plot.clear;
  VAR i:longint;
  begin
    for i:=0 to length(line)-1 do with line[i] do setLength(sample,0);
    setLength(line,0);  
  end;
  
PROCEDURE printMemoryStatus;
  VAR mem:TMemoryManager;
      hs:THeapStatus;
      fhs:TFPCHeapStatus;

  begin
    GetMemoryManager(mem);
    hs:=mem.GetHeapStatus();
    fhs:=mem.GetFPCHeapStatus();
    writeln('free: ',hs.TotalFree,'; heap free=',fhs.CurrHeapFree,'; heap used=',fhs.CurrHeapUsed);    
  
  end;
  
INITIALIZATION
  inputDeclEcho:=nil; 
  inputExprEcho:=nil;
  exprOut      :=nil;
  errorOut     :=@plainStdErrOut;
  printOut     :=@plainConsoleOut;
  tablePrintOut:=nil;
  plotOut      :=nil;
  maxErrorLevel:=el0_allOkay;


  
end.