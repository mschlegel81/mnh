UNIT mnh_plots;
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
