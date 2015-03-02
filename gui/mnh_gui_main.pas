unit mnh_gui_main;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynCompletion, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, ComCtrls, Grids, PopupNotifier,
  SynHighlighterMnh, mnh_fileWrappers, mnh_gui_settings, mnh_tokloc,
  mnh_out_adapters, mnh_stringutil, mnh_evalThread, mnh_constants, myGenerics,
  types, LCLType,mnh_plotData,mnh_funcs,mnh_litvar,math;

type

  { TMnhForm }

  TMnhForm = class(TForm)
    ErrorMemo: TMemo;
    miAntiAliasingOff: TMenuItem;
    miAntiAliasing2: TMenuItem;
    miAntiAliasing3: TMenuItem;
    miAntiAliasing4: TMenuItem;
    miAntiAliasing5: TMenuItem;
    miAutoReset: TMenuItem;
    submenuPlotGrid: TMenuItem;
    MenuItem17: TMenuItem;
    miXTics: TMenuItem;
    miXGrid: TMenuItem;
    miXFinerGrid: TMenuItem;
    MenuItem21: TMenuItem;
    miYTics: TMenuItem;
    miYGrid: TMenuItem;
    miYFinerGrid: TMenuItem;
    submenuPlotScaling: TMenuItem;
    miPreserveAspect: TMenuItem;
    miAutoscaleX: TMenuItem;
    miAutoscaleY: TMenuItem;
    miLogscaleX: TMenuItem;
    miLogscaleY: TMenuItem;
    submenuPlotOptions: TMenuItem;
    miExportPlot: TMenuItem;
    miOpenPlot: TMenuItem;
    miSavePlot: TMenuItem;
    MenuItem9: TMenuItem;
    miFileHistory6: TMenuItem;
    miFileHistory7: TMenuItem;
    miFileHistory8: TMenuItem;
    miFileHistory9: TMenuItem;
    MenuItem2: TMenuItem;
    miFileHistory0: TMenuItem;
    miFileHistory1: TMenuItem;
    miFileHistory2: TMenuItem;
    miFileHistory3: TMenuItem;
    miFileHistory4: TMenuItem;
    miFileHistory5: TMenuItem;
    miOpenNpp: TMenuItem;
    miHelp: TMenuItem;
    miHaltEvalutaion: TMenuItem;
    miEvalModeDirect: TMenuItem;
    miEvaluateNow: TMenuItem;
    miEvalModeDirectOnKeypress: TMenuItem;
    miClear: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    OpenDialog: TOpenDialog;
    myhl:TSynMnhSyn;
    ErrorGroupBox: TGroupBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    submenuEditorAppearance: TMenuItem;
    miExpressionEcho: TMenuItem;
    miExpressionResult: TMenuItem;
    miDeclarationEcho: TMenuItem;
    miDecFontSize: TMenuItem;
    miIncFontSize: TMenuItem;
    mi_settings: TMenuItem;
    PageControl: TPageControl;
    plotImage: TImage;
    PopupNotifier1: TPopupNotifier;
    SaveDialog: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar: TStatusBar;
    InputEdit: TSynEdit;
    OutputEdit: TSynEdit;
    SynCompletion: TSynCompletion;
    EditorTabSheet: TTabSheet;
    PlotTabSheet: TTabSheet;
    UpdateTimeTimer: TTimer;
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE InputEditChange(Sender: TObject);
    PROCEDURE InputEditKeyDown(Sender: TObject; VAR Key: Word;
      Shift: TShiftState);
    PROCEDURE InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    PROCEDURE miClearClick(Sender: TObject);
    PROCEDURE miDecFontSizeClick(Sender: TObject);
    PROCEDURE miDeclarationEchoClick(Sender: TObject);
    PROCEDURE miEvalModeDirectClick(Sender: TObject);
    PROCEDURE miEvalModeDirectOnKeypressClick(Sender: TObject);
    PROCEDURE miEvaluateNowClick(Sender: TObject);
    procedure miExportPlotClick(Sender: TObject);
    PROCEDURE miExpressionEchoClick(Sender: TObject);
    PROCEDURE miExpressionResultClick(Sender: TObject);
    PROCEDURE miFileHistory0Click(Sender: TObject);
    PROCEDURE miFileHistory1Click(Sender: TObject);
    PROCEDURE miFileHistory2Click(Sender: TObject);
    PROCEDURE miFileHistory3Click(Sender: TObject);
    PROCEDURE miFileHistory4Click(Sender: TObject);
    PROCEDURE miFileHistory5Click(Sender: TObject);
    PROCEDURE miFileHistory6Click(Sender: TObject);
    PROCEDURE miFileHistory7Click(Sender: TObject);
    PROCEDURE miFileHistory8Click(Sender: TObject);
    PROCEDURE miFileHistory9Click(Sender: TObject);
    PROCEDURE miHaltEvalutaionClick(Sender: TObject);
    PROCEDURE miHelpClick(Sender: TObject);
    PROCEDURE miIncFontSizeClick(Sender: TObject);
    PROCEDURE miOpenClick(Sender: TObject);
    PROCEDURE miOpenNppClick(Sender: TObject);
    procedure miOpenPlotClick(Sender: TObject);
    PROCEDURE miSaveAsClick(Sender: TObject);
    PROCEDURE miSaveClick(Sender: TObject);
    procedure miSavePlotClick(Sender: TObject);
    PROCEDURE mi_settingsClick(Sender: TObject);
    PROCEDURE miAntialiasingOffClick(Sender: TObject);
    PROCEDURE miAutoResetClick(Sender: TObject);
    PROCEDURE miAutoscaleXClick(Sender: TObject);
    PROCEDURE miAutoscaleYClick(Sender: TObject);
    PROCEDURE miLogscaleXClick(Sender: TObject);
    PROCEDURE miLogscaleYClick(Sender: TObject);
    PROCEDURE miPreserveAspectClick(Sender: TObject);
    PROCEDURE miXFinerGridClick(Sender: TObject);
    PROCEDURE miXGridClick(Sender: TObject);
    PROCEDURE miXTicsClick(Sender: TObject);
    PROCEDURE miYFinerGridClick(Sender: TObject);
    PROCEDURE miYGridClick(Sender: TObject);
    PROCEDURE miYTicsClick(Sender: TObject);
    PROCEDURE OutputEditKeyDown(Sender: TObject; VAR Key: Word;
      Shift: TShiftState);
    PROCEDURE OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PageControlChange(Sender: TObject);
    PROCEDURE Splitter1Moved(Sender: TObject);
    PROCEDURE SynCompletionCodeCompletion(VAR Value: string;
      SourceValue: string; VAR SourceStart, SourceEnd: TPoint;
      KeyChar: TUTF8Char; Shift: TShiftState);
    PROCEDURE SynCompletionExecute(Sender: TObject);
    PROCEDURE SynCompletionSearchPosition(VAR APosition: integer);
    PROCEDURE UpdateTimeTimerTimer(Sender: TObject);

  private
    underCursor:T_tokenInfo;
    settingsHaveBeenProcessed:boolean;
    PROCEDURE processSettings;
    PROCEDURE processFileHistory;
    PROCEDURE flushThroughput;
    PROCEDURE positionHelpNotifier;
    PROCEDURE setUnderCursor(CONST lines:TStrings; CONST caret:TPoint);

    PROCEDURE doPlot();
    PROCEDURE pullPlotSettingsToGui();
    PROCEDURE pushSettingsToPlotContainer(CONST plotImmediately:boolean);
    PROCEDURE doConditionalPlotReset;

    { private declarations }
  public
    { public declarations }
  end;

VAR
  MnhForm: TMnhForm;

implementation
VAR errorThroughput:array of T_storedError;
    lastFormRepaint:double=0;
    repaintNecessary:boolean=false;
    output:T_listOfString;
    plotSubsystem:record
      rendering:boolean;
      state:(pss_neutral, pss_plotAfterCalculation, pss_plotOnShow);
    end;

{$R *.lfm}

PROCEDURE appendToOutputThroughput(CONST text:ansistring);
  begin
    output.add(text);
  end;

PROCEDURE writeDeclEcho(CONST s:ansistring);
  begin
    appendToOutputThroughput(C_DeclEchoHead+' '+s);
  end;

PROCEDURE writeExprEcho(CONST s:ansistring);
  begin
    appendToOutputThroughput(C_ExprEchoHead+' '+s);
  end;

PROCEDURE writeExprOut (CONST s:ansistring);
  begin
    appendToOutputThroughput(C_ExprOutHead+' '+s);
  end;

PROCEDURE writePrint   (CONST s:ansistring);
  begin
    appendToOutputThroughput(s);
  end;

PROCEDURE logError(CONST error:T_storedError);
  begin
    {$ifdef debugMode}
    mnh_out_adapters.plainStdErrOut(error);
    {$endif}
    if error.errorLevel<el2_warning then exit;
    SetLength(errorThroughput,length(errorThroughput)+1);
    errorThroughput[length(errorThroughput)-1]:=error;
    repaintNecessary:=true;
    //MnhForm.ErrorGroupBox.Visible:=true;
  end;

procedure TMnhForm.flushThroughput;
  VAR i:longint;
  begin
    OutputEdit.BeginUpdate();
    for i:=0 to length(errorThroughput)-1 do with errorThroughput[i] do
      ErrorMemo.Append(C_errorLevelTxt[errorLevel]+errorMessage+string(errorLocation) );
    setLength(errorThroughput,0);
    lastFormRepaint:=now;
    repaintNecessary:=false;
    OutputEdit.EndUpdate;
  end;

procedure TMnhForm.positionHelpNotifier;
  begin
    PopupNotifier1.ShowAtPos(left+Width-PopupNotifier1.vNotifierForm.Width,
                             ClientToScreen(Point(left,OutputEdit.Top)).y);
    InputEdit.SetFocus;
  end;

procedure TMnhForm.setUnderCursor(const lines: TStrings; const caret: TPoint);
  begin
    if (caret.y>0) and (caret.y<=lines.Count) then begin
      underCursor:=ad_getTokenInfo(lines[caret.y-1],caret.x+1);
      if (underCursor.tokenText<>'') and (underCursor.tokenText<>PopupNotifier1.Title) then begin
        PopupNotifier1.Title:=underCursor.tokenText;
        PopupNotifier1.Text:=replaceAll(underCursor.tokenExplanation,'#',C_lineBreakChar);
        miOpenNpp.Enabled:=underCursor.declaredInFile<>'';
        if miHelp.Checked and not(PopupNotifier1.Visible) then positionHelpNotifier;
      end;
    end;
  end;

procedure TMnhForm.doPlot;
  PROCEDURE drawGridAndRows(CONST target:TCanvas; CONST scalingFactor:longint);
    VAR rowId,i,x,y,yBaseLine,lastX,lastY:longint;
        symSize:double;
        lastWasValid,currentIsValid:boolean;
        sample:T_point;
        patternIdx:byte;
        rowColor:longint;

    PROCEDURE drawPatternRect(x0,y0,x1,y1:longint);
      VAR x,y,locY:longint;
      begin
        if x1<x0 then begin
          x:=x1; x1:=x0; x0:=x;
        end;
        for x:=x0 to x1 do begin
          locY:=round(y0+(y1-y0)*(x-x0)/(x1-x0));
          if locY>yBaseLine then begin
            for y:=yBaseLine to locY do
            if (x and 1)+2*(y and 1)=patternIdx then
              target.Pixels[x,y]:=rowColor;
          end else begin
            for y:=yBaseLine downto locY do
            if (x and 1)+2*(y and 1)=patternIdx then
              target.Pixels[x,y]:=rowColor;
          end;
        end;
      end;

    begin
      //Clear:------------------------------------------------------------------
      target.Brush.Style:=bsSolid;
      target.Brush.Color:=clWhite;
      target.Pen.Style  :=psClear;
      target.Pen.EndCap :=pecSquare;
      target.FillRect(0,0,target.Width-1,target.Height-1);
      target.Clear;
      //------------------------------------------------------------------:Clear
      //coordinate grid:========================================================
      target.Pen.Style:=psSolid;
      target.Pen.Width:=scalingFactor;
      //minor grid:-------------------------------------------------------------
      target.Pen.Color:=$DDDDDD;
      for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do if not(major) then begin
        y:=round(pos*scalingFactor);
        target.Line(0,y,activePlot.screenWidth*scalingFactor,y);
      end;
      for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do if not(major) then begin
        x:=round(pos*scalingFactor);
        target.Line(x,0,x,activePlot.screenHeight*scalingFactor);
      end;
      //-------------------------------------------------------------:minor grid
      //major grid:-------------------------------------------------------------
      target.Pen.Color:=$BBBBBB;
      for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do if major then begin
        y:=round(pos*scalingFactor);
        target.Line(0,y,activePlot.screenWidth*scalingFactor,y);
      end;
      for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do if major then begin
        x:=round(pos*scalingFactor);
        target.Line(x,0,x,activePlot.screenHeight*scalingFactor);
      end;
      //-------------------------------------------------------------:major grid
      //========================================================:coordinate grid
      //row data:===============================================================
      if activePlot.logscale['y'] then yBaseLine:=round(activePlot.realToScreen('y',1)*scalingFactor)
                                  else yBaseLine:=round(activePlot.realToScreen('y',0)*scalingFactor);
      if yBaseLine<0 then yBaseLine:=0 else if yBaseLine>=target.Height then yBaseLine:=target.Height-1;
      for rowId:=0 to length(activePlot.row)-1 do begin
        rowColor:=activePlot.row[rowId].style.getTColor;
        patternIdx:=rowId and 3;

        if activePlot.row[rowId].style.wantStraightLines then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(x,y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,lastY,x,y);
              end else target.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantLeftSteps then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(lastX,y);
                target.LineTo(    x,y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,y,x,y);
              end else target.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantRightSteps then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;
          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                target.LineTo(x,lastY);
                target.LineTo(x,    y);
                if activePlot.row[rowId].style.wantFill then drawPatternRect(lastX,lastY,x,lastY);
              end else target.MoveTo(x,y);
              lastX:=x;
              lastY:=y;
            end;
            lastWasValid:=currentIsValid;
          end;
        end else if activePlot.row[rowId].style.wantBars then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecRound;

          lastWasValid:=false;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              x:=round(sample[0]*scalingFactor);
              y:=round(sample[1]*scalingFactor);
              if lastWasValid then begin
                drawPatternRect(round(lastX*0.95+x*0.05),lastY,
                                round(lastX*0.05+x*0.95),lastY);
                target.Line(round(lastX*0.95+x*0.05),yBaseLine,round(lastX*0.95+x*0.05),lastY);
                target.Line(round(lastX*0.95+x*0.05),lastY    ,round(lastX*0.05+x*0.95),lastY);
                target.Line(round(lastX*0.05+x*0.95),yBaseLine,round(lastX*0.05+x*0.95),lastY);
              end;
              lastX:=x;
              lastY:=y;
              lastWasValid:=currentIsValid;
            end;
          end;

        end else if activePlot.row[rowId].style.wantBoxes then begin
          target.Pen.Style:=psClear;
          target.Brush.Style:=bsSolid;
          target.Brush.Color:=rowColor;
          lastWasValid:=false;
          i:=0;
          while i+1<length(activePlot.row[rowId].sample) do begin
            sample:=activePlot.row[rowId].sample[i];
            if activePlot.isSampleValid(sample) then begin
              sample:=activePlot.realToScreen(sample);
              lastX:=round(sample[0]*scalingFactor);
              lastY:=round(sample[1]*scalingFactor);
              sample:=activePlot.row[rowId].sample[i+1];
              if activePlot.isSampleValid(sample) then begin
                sample:=activePlot.realToScreen(sample);
                x:=round(sample[0]*scalingFactor);
                y:=round(sample[1]*scalingFactor);
                target.FillRect(lastX,lastY,x,y);
              end;
            end;
            inc(i,2);
          end;
        end;
        if activePlot.row[rowId].style.wantDot then begin
          target.Pen.Style:=psClear;
          target.Brush.Style:=bsSolid;
          target.Brush.Color:=rowColor;
          symSize:=activePlot.row[rowId].style.getSymbolWidth*scalingFactor;

          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              //target.Pixels[round(sample[0]*scalingFactor),round(sample[1]*scalingFactor)]:=rowColor;
              target.Ellipse(round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor-symSize),
                             round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantPlus then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          symSize:=activePlot.row[rowId].style.getSymbolWidth*scalingFactor;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Line(round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor),
                                    round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor));
              target.Line(round(sample[0]*scalingFactor),round(sample[1]*scalingFactor-symSize),
                                    round(sample[0]*scalingFactor),round(sample[1]*scalingFactor+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantCross then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          symSize:=activePlot.row[rowId].style.getSymbolRad*scalingFactor;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Line(round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor-symSize),
                                    round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor+symSize));
              target.Line(round(sample[0]*scalingFactor+symSize),round(sample[1]*scalingFactor-symSize),
                                    round(sample[0]*scalingFactor-symSize),round(sample[1]*scalingFactor+symSize));
            end;
          end;
        end;
        if activePlot.row[rowId].style.wantImpulses then begin
          target.Pen.Style:=psSolid;
          target.Pen.Color:=rowColor;
          target.Pen.Width:=activePlot.row[rowId].style.getIntLineWidth(scalingFactor);
          target.Pen.EndCap:=pecSquare;
          for i:=0 to length(activePlot.row[rowId].sample)-1 do begin
            sample:=activePlot.row[rowId].sample[i];
            currentIsValid:=activePlot.isSampleValid(sample);
            if currentIsValid then begin
              sample:=activePlot.realToScreen(sample);
              target.Line(round(sample[0]*scalingFactor),yBaseLine,
                                    round(sample[0]*scalingFactor),round(sample[1]*scalingFactor));
            end;
          end;
        end;
      end;
      //===============================================================:row data
    end;

  PROCEDURE scale(source : TImage;VAR dest:TImage;Factor : Real);
    VAR	ARect : TRect;
    	X,Y : Integer;
    begin
      X := Round(source.Width * Factor);
      Y := Round(source.Height * Factor);
      Arect := Rect(0,0,X,Y);
      dest.Canvas.StretchDraw(ARect,source.Picture.Bitmap);
    end;

  PROCEDURE drawCoordSys(CONST target:TCanvas);
    VAR i,x,y:longint;
    begin
      //coordinate system:======================================================
      //clear border:-----------------------------------------------------------
      target.Brush.Style:=bsSolid;
      target.Brush.Color:=clWhite;
      target.Pen.Style:=psClear;
      target.Pen.Width:=1;
      target.Pen.EndCap:=pecSquare;
      if activePlot.wantTics('y') then
        target.FillRect(0,0,activePlot.xOffset,PlotTabSheet.Height);
      if activePlot.wantTics('x') then
        target.FillRect(activePlot.xOffset,activePlot.yOffset,
                        PlotTabSheet.Width,PlotTabSheet.Height);
      //-----------------------------------------------------------:clear border
      //axis:-------------------------------------------------------------------
      target.Pen.Style:=psSolid;
      target.Pen.Color:=clBlack;
      target.Pen.Width:=1;
      if activePlot.wantTics('y') then
        target.Line(activePlot.xOffset    ,0                 ,
                              activePlot.xOffset,activePlot.yOffset);
      if activePlot.wantTics('x') then
        target.Line(activePlot.screenWidth,activePlot.yOffset,
                              activePlot.xOffset    ,activePlot.yOffset);
      //-------------------------------------------------------------------:axis
      //tics:-------------------------------------------------------------------
      if activePlot.wantTics('y') then begin
        for i:=0 to length(activePlot.tic['y'])-1 do with activePlot.tic['y'][i] do if major then begin
          y:=round(pos);
          target.Line(activePlot.xOffset-5,y,activePlot.xOffset,y);
          target.TextOut(activePlot.xOffset-5-target.TextWidth(txt),y-target.TextHeight(txt) shr 1,txt);
        end;
      end;
      if activePlot.wantTics('x') then begin
        for i:=0 to length(activePlot.tic['x'])-1 do with activePlot.tic['x'][i] do if major then begin
          x:=round(pos);
          target.Line(x,activePlot.yOffset+5,x,activePlot.yOffset);
          target.TextOut(x-target.TextWidth(txt) shr 1 ,activePlot.yOffset+5,txt);
        end;
      end;

      //-------------------------------------------------------------------:tics
      //======================================================:coordinate system
    end;

  VAR factor:longint;
      renderImage:TImage;
  begin
    plotSubsystem.state:=pss_neutral;
    PageControl.ActivePageIndex:=1;
    plotSubsystem.rendering:=true;
    //Prepare transformations:--------------------------------------------------
    activePlot.setScreenSize(PlotTabSheet.Width,PlotTabSheet.Height);
    repeat until not(
      activePlot.setTextSize(
        plotImage.Canvas.TextHeight(activePlot.longtestYTic),
        plotImage.Canvas.TextWidth (activePlot.longtestYTic)));
    //--------------------------------------------------:Prepare transformations
    if miAntialiasingOff.Checked then begin
      drawGridAndRows(plotImage.Canvas,1);
      drawCoordSys(plotImage.Canvas);
    end else begin
      if      miAntiAliasing5.Checked then factor:=5
      else if miAntiAliasing4.Checked then factor:=4
      else if miAntiAliasing3.Checked then factor:=3
      else factor:=2;
      renderImage:=TImage.Create(Self);
      renderImage.SetInitialBounds(0,0,PlotTabSheet.Width*factor,PlotTabSheet.Height*factor);
      drawGridAndRows(renderImage.Canvas,factor);
      scale(renderImage,plotImage,1/factor);
      renderImage.Free;
      drawCoordSys(plotImage.Canvas);
    end;
    plotSubsystem.rendering:=false;
  end;

procedure TMnhForm.pullPlotSettingsToGui;
begin
  miXTics.Checked         :=(activePlot.axisStyle['x'] and C_tics)=C_tics;
  miXGrid.Checked         :=(activePlot.axisStyle['x'] and C_grid)=C_grid;
  miXFinerGrid.Checked    :=(activePlot.axisStyle['x'] and C_finerGrid)=C_finerGrid;
  miYTics.Checked         :=(activePlot.axisStyle['y'] and C_tics)=C_tics;
  miYGrid.Checked         :=(activePlot.axisStyle['y'] and C_grid)=C_grid;
  miYFinerGrid.Checked    :=(activePlot.axisStyle['y'] and C_finerGrid)=C_finerGrid;
  miPreserveAspect.Checked:=activePlot.preserveAspect;
  miAutoscaleX.Checked    :=activePlot.autoscale['x'];
  miAutoscaleY.Checked    :=activePlot.autoscale['y'];
  miLogscaleX.Checked     :=activePlot.logscale['x'];
  miLogscaleY.Checked     :=activePlot.logscale['y'];
end;

procedure TMnhForm.pushSettingsToPlotContainer(const plotImmediately: boolean);
  VAR aidX,aidY:longint;
  begin
    aidX:=0;
    if miXTics.Checked      then aidX:=C_tics;;
    if miXGrid.Checked      then aidX:=aidX or C_grid;
    if miXFinerGrid.Checked then aidX:=aidX or C_finerGrid;
    aidY:=0;
    if miYTics.Checked      then aidY:=C_tics;;
    if miYGrid.Checked      then aidY:=aidY or C_grid;
    if miYFinerGrid.Checked then aidY:=aidY or C_finerGrid;
    activePlot.setAxisStyle(aidX,aidY);
    activePlot.setPreserveAspect(miPreserveAspect.Checked);
    activePlot.setLogscale(miLogscaleX.Checked,miLogscaleY.Checked);
    activePlot.setAutoscale(miAutoscaleX.Checked,miAutoscaleY.Checked);
    pullPlotSettingsToGui();
    if plotImmediately then begin
      if ad_evaluationRunning or plotSubsystem.rendering or (PageControl.ActivePageIndex<>1)
         then plotSubsystem.state:=pss_plotOnShow
         else doPlot();
    end else plotSubsystem.state:=pss_plotAfterCalculation;
  end;

procedure TMnhForm.doConditionalPlotReset;
  begin
    if miAutoReset.Checked then begin
      activePlot.setDefaults;
      pullPlotSettingsToGui();
    end;
  end;

PROCEDURE startOfEvaluationCallback;
  begin
    MnhForm.doConditionalPlotReset;
    setLength(errorThroughput,0);
    MnhForm.OutputEdit.Lines.Clear;
    MnhForm.ErrorMemo.Clear;
    //MnhForm.ErrorGroupBox.Visible:=false;
    repaintNecessary:=false;
    lastFormRepaint:=now;
  end;

{ TMnhForm }
procedure TMnhForm.FormCreate(Sender: TObject);
  begin
    myhl:=TSynMnhSyn.Create(nil);
    InputEdit.Highlighter:=myhl;
    OutputEdit.Highlighter:=myhl;
    settingsHaveBeenProcessed:=false;
    StatusBar.SimpleText:=
      'compiled on: '+{$I %DATE%}+
      ' at: '+{$I %TIME%}+
      ' with FPC'+{$I %FPCVERSION%}+
      ' for '+{$I %FPCTARGET%};
    OutputEdit.Lines.Append('       compiled on: '+{$I %DATE%});
    OutputEdit.Lines.Append('       at: '+{$I %TIME%});
    OutputEdit.Lines.Append('       with FPC'+{$I %FPCVERSION%});
    OutputEdit.Lines.Append('       for '+{$I %FPCTARGET%});
    mnh_out_adapters.errorOut:=@logError;
    mnh_evalThread.startOfEvaluationCallback:=@startOfEvaluationCallback;
    mnh_out_adapters.inputDeclEcho:=@writeDeclEcho;
    mnh_out_adapters.inputExprEcho:=@writeExprEcho;
    mnh_out_adapters.exprOut      :=@writeExprOut;
    mnh_out_adapters.printOut     :=@writePrint;
  end;

procedure TMnhForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    SettingsForm.setFileContents(InputEdit.Lines);
  end;

procedure TMnhForm.FormDestroy(Sender: TObject);
  begin
    mnh_out_adapters.errorOut:=@mnh_out_adapters.plainStdErrOut;
    myhl.Destroy;
    ad_killEvaluationLoopSoftly;
  end;

procedure TMnhForm.FormResize(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.mainForm.top   :=top;
      SettingsForm.mainForm.left  :=left;
      SettingsForm.mainForm.width :=width;
      SettingsForm.mainForm.height:=height;
      if ad_evaluationRunning or plotSubsystem.rendering or (PageControl.ActivePageIndex<>1)
        then plotSubsystem.state:=pss_plotOnShow
        else doPlot();
    end;
    if PopupNotifier1.Visible then positionHelpNotifier;
  end;

procedure TMnhForm.FormShow(Sender: TObject);
  begin
    DoubleBuffered:=true;
    if not(settingsHaveBeenProcessed) then begin
      processSettings;
      InputEdit.SetFocus;
    end;
    UpdateTimeTimer.Enabled:=true;
  end;

procedure TMnhForm.InputEditChange(Sender: TObject);
  begin
    if (miEvalModeDirectOnKeypress.Checked) and not(SynCompletion.IsActive) then begin
      ad_evaluate(InputEdit.Lines);
    end;
  end;

procedure TMnhForm.InputEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
    setUnderCursor(InputEdit.Lines,InputEdit.CaretXY);
  end;

procedure TMnhForm.InputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    setUnderCursor(InputEdit.Lines,InputEdit.PixelsToRowColumn(point));
  end;

procedure TMnhForm.miClearClick(Sender: TObject);
  begin
    ad_clearFile;
    InputEdit.ClearAll;
    if SettingsForm.setFileInEditor('') then processFileHistory;
  end;

procedure TMnhForm.miDecFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize-1;
      processSettings;
    end;
  end;

procedure TMnhForm.miDeclarationEchoClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miDeclarationEcho.Checked:=not(miDeclarationEcho.Checked);
      with SettingsForm.outputBehaviour do begin
        doEchoDeclaration:=miDeclarationEcho.Checked;
        if doEchoDeclaration then mnh_out_adapters.inputDeclEcho:=@writeDeclEcho
                             else mnh_out_adapters.inputDeclEcho:=nil;
      end;
    end;
  end;

procedure TMnhForm.miEvalModeDirectClick(Sender: TObject);
  begin
    if miEvalModeDirect.Checked then exit;
    miEvalModeDirect.Checked:=true;
  end;

procedure TMnhForm.miEvalModeDirectOnKeypressClick(Sender: TObject);
  begin
    if miEvalModeDirectOnKeypress.Checked then exit;
    miEvalModeDirectOnKeypress.Checked:=true;
  end;

procedure TMnhForm.miEvaluateNowClick(Sender: TObject);
  begin
    ad_evaluate(InputEdit.Lines);
  end;

procedure TMnhForm.miExportPlotClick(Sender: TObject);
  VAR storeImage:TImage;
      rect:TRect;
  begin
    SaveDialog.Filter:='Portable network graphics (PNG)|*.png';
    if SaveDialog.Execute then begin
      storeImage:=TImage.Create(Self);
      storeImage.SetInitialBounds(0,0,PlotTabSheet.Width,PlotTabSheet.Height);
      rect.Top:=0;
      rect.Left:=0;
      rect.Right:=PlotTabSheet.Width;
      rect.Bottom:=PlotTabSheet.Height;
      storeImage.Canvas.CopyRect(rect,plotImage.Canvas,rect);
      SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,'.png');
      storeImage.Picture.PNG.SaveToFile(SaveDialog.FileName);
      storeImage.Free;
    end;
  end;

procedure TMnhForm.miExpressionEchoClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miExpressionEcho.Checked:=not(miExpressionEcho.Checked);
      with SettingsForm.outputBehaviour do begin
        doEchoInput:=miExpressionEcho.Checked;
        if doEchoInput then mnh_out_adapters.inputExprEcho:=@writeExprEcho
                       else mnh_out_adapters.inputExprEcho:=nil;
      end;
    end;
  end;

procedure TMnhForm.miExpressionResultClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      miExpressionResult.Checked:=not(miExpressionResult.Checked);
      with SettingsForm.outputBehaviour do begin
        doShowExpressionOut:=miExpressionResult.Checked;
        if doShowExpressionOut then mnh_out_adapters.exprOut:=@writeExprOut
                               else mnh_out_adapters.exprOut:=nil;
      end;
    end;
  end;

procedure TMnhForm.miFileHistory0Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[0])
      then begin
        ad_setFile(SettingsForm.fileHistory[0],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[0]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory1Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[1])
      then begin
        ad_setFile(SettingsForm.fileHistory[1],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[1]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory2Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[2])
      then begin
        ad_setFile(SettingsForm.fileHistory[2],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[2]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory3Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[3])
      then begin
        ad_setFile(SettingsForm.fileHistory[3],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[3]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory4Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[4])
      then begin
        ad_setFile(SettingsForm.fileHistory[4],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[4]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory5Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[5])
      then begin
        ad_setFile(SettingsForm.fileHistory[5],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[5]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory6Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[6])
      then begin
        ad_setFile(SettingsForm.fileHistory[6],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[6]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory7Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[7])
      then begin
        ad_setFile(SettingsForm.fileHistory[7],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[7]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory8Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[8])
      then begin
        ad_setFile(SettingsForm.fileHistory[8],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[8]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miFileHistory9Click(Sender: TObject);
  begin
    if FileExists(SettingsForm.fileHistory[9])
      then begin
        ad_setFile(SettingsForm.fileHistory[9],InputEdit.Lines);
        if SettingsForm.setFileInEditor(SettingsForm.fileHistory[9]) then processFileHistory;
      end;
  end;

procedure TMnhForm.miHaltEvalutaionClick(Sender: TObject);
  begin
    ad_haltEvaluation;
  end;

procedure TMnhForm.miHelpClick(Sender: TObject);
begin
  miHelp.Checked:=not(miHelp.Checked);
  if not(miHelp.Checked) then PopupNotifier1.Visible:=false
                         else if underCursor.tokenText<>'' then positionHelpNotifier;
end;

procedure TMnhForm.miIncFontSizeClick(Sender: TObject);
  begin
    if settingsHaveBeenProcessed then begin
      SettingsForm.fontSize:=SettingsForm.fontSize+1;
      processSettings;
    end;
  end;

procedure TMnhForm.miOpenClick(Sender: TObject);
  begin
    OpenDialog.Title:='Open file';
    if OpenDialog.Execute and FileExists(OpenDialog.FileName)
    then begin
      ad_setFile(OpenDialog.FileName,InputEdit.Lines);
      if SettingsForm.setFileInEditor(OpenDialog.FileName) then processFileHistory;
    end;
  end;

procedure TMnhForm.miOpenNppClick(Sender: TObject);
begin
  if underCursor.declaredInFile<>'' then
    SettingsForm.canOpenFile(underCursor.declaredInFile,underCursor.declaredInLine);
end;

procedure TMnhForm.miOpenPlotClick(Sender: TObject);
begin
  OpenDialog.Filter:='MNH-Plot|*.mnh_plot';
  if OpenDialog.Execute then begin
    OpenDialog.FileName:=ChangeFileExt(OpenDialog.FileName,'.mnh_plot');;
    if FileExistsUTF8(OpenDialog.FileName) then begin
      if not(activePlot.loadFromFile(OpenDialog.FileName))
      then activePlot.setDefaults
      else begin
        if ad_evaluationRunning or plotSubsystem.rendering
           then plotSubsystem.state:=pss_plotAfterCalculation
           else doPlot();
      end;
    end;
  end;
end;

procedure TMnhForm.miSaveAsClick(Sender: TObject);
  begin
    if SaveDialog.Execute then begin
      MnhForm.InputEdit.Lines.SaveToFile(SaveDialog.FileName);
      ad_setFile(SaveDialog.FileName,InputEdit.Lines);
      if SettingsForm.setFileInEditor(SaveDialog.FileName) then processFileHistory;
      SettingsForm.saveSettings;
    end;
  end;

procedure TMnhForm.miSaveClick(Sender: TObject);
  begin
    if ad_currentFile='' then miSaveAsClick(Sender)
    else begin
      MnhForm.InputEdit.Lines.SaveToFile(ad_currentFile);
      if SettingsForm.setFileInEditor(ad_currentFile) then processFileHistory;
      SettingsForm.saveSettings;
    end;
  end;

procedure TMnhForm.miSavePlotClick(Sender: TObject);
  begin
    SaveDialog.Filter:='MNH-Plot|*.mnh_plot';
    if SaveDialog.Execute then begin
      SaveDialog.FileName:=ChangeFileExt(SaveDialog.FileName,'.mnh_plot');
      activePlot.saveToFile(SaveDialog.FileName);
    end;
  end;

procedure TMnhForm.mi_settingsClick(Sender: TObject);
  begin
    SettingsForm.ShowModal;
    processSettings;
  end;

procedure TMnhForm.OutputEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  begin
    setUnderCursor(OutputEdit.Lines,OutputEdit.CaretXY);
  end;


procedure TMnhForm.OutputEditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  VAR point:TPoint;
  begin
    point.x:=x;
    point.y:=y;
    setUnderCursor(OutputEdit.Lines,OutputEdit.PixelsToRowColumn(point));
  end;

procedure TMnhForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePageIndex=0 then begin
    submenuEditorAppearance.Visible:=true;
    submenuPlotGrid.Visible:=false;
    submenuPlotOptions.Visible:=false;
    submenuPlotScaling.Visible:=false;
  end else begin
    submenuEditorAppearance.Visible:=false;
    submenuPlotGrid.Visible:=true;
    submenuPlotOptions.Visible:=true;
    submenuPlotScaling.Visible:=true;
    if plotSubsystem.state=pss_plotOnShow then doPlot();
  end;
end;

procedure TMnhForm.Splitter1Moved(Sender: TObject);
  begin
    if PopupNotifier1.Visible then positionHelpNotifier;
  end;

procedure TMnhForm.SynCompletionCodeCompletion(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
  begin
    if (pos('.',value)>0) then begin
      if pos(sourceValue,value)<>1 then begin
        value:=copy(value,pos('.',value)+1,length(value));
      end;
    end;
  end;

procedure TMnhForm.SynCompletionExecute(Sender: TObject);
  VAR i:longint;
      s:string;
  begin
    SynCompletion.ItemList.Clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to completionList.size-1 do
      if (s='') or (pos(s,completionList[i])=1) then SynCompletion.ItemList.Add(completionList[i]);
  end;

procedure TMnhForm.SynCompletionSearchPosition(var APosition: integer);
  VAR i:longint;
      s:string;
  begin
    SynCompletion.ItemList.Clear;
    s:=SynCompletion.CurrentString;
    for i:=0 to completionList.size-1 do
      if pos(s,completionList[i])=1 then SynCompletion.ItemList.Add(completionList[i]);
    if SynCompletion.ItemList.Count>0 then APosition:=0 else APosition:=-1;
  end;

procedure TMnhForm.UpdateTimeTimerTimer(Sender: TObject);
  CONST MIN_INTERVALL=50;
        MAX_INTERVALL=1000;
        REPAINT_INTERVAL_IN_SECONDS=1;
  VAR aid:string;
      flag:boolean;
      L:array of AnsiString;
      i:longint;
  begin
    //Form caption:-------------------------------------------------------------
    aid:='MNH5 '+ad_currentFile;
    if aid<>Caption then begin Caption:=aid; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    //-------------------------------------------------------------:Form caption
    //Halt/Run enabled states:--------------------------------------------------
    flag:=ad_evaluationRunning;
    if flag<>miHaltEvalutaion.Enabled then begin miHaltEvalutaion.Enabled:=flag; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    flag:=not(flag);
    if flag<>miEvaluateNow.Enabled then begin miEvaluateNow.Enabled:=flag; UpdateTimeTimer.Interval:=MIN_INTERVALL; end;
    //--------------------------------------------------:Halt/Run enabled states
    //progress time:------------------------------------------------------------
    flag:=ad_evaluationRunning;
    if flag then aid:='Evaluating: '+formatFloat('0.000',(now-startOfEvaluation.value )*(24*60*60))+'s'
    else aid:=endOfEvaluationText.value;
    if StatusBar.SimpleText<>aid then begin
      StatusBar.SimpleText:=aid;
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
    end;
    //------------------------------------------------------------:progress time
    //file state:---------------------------------------------------------------
    if ad_needReload and not(flag) then begin
      InputEdit.BeginUpdate();
      ad_doReload(InputEdit.Lines);
      InputEdit.EndUpdate;
      UpdateTimeTimer.Interval:=MIN_INTERVALL;
      OutputEdit.ClearAll;
    end;
    //---------------------------------------------------------------:file state
    if output.size>0 then begin
      L:=output.elementArray;
      output.clear;
      for i:=0 to length(L)-1 do OutputEdit.Lines.Append(L[i]);
      repaintNecessary:=true;
    end;

    repaintNecessary:=repaintNecessary or (UpdateTimeTimer.Interval=MIN_INTERVALL);
    if UpdateTimeTimer.Interval<MAX_INTERVALL then UpdateTimeTimer.Interval:=UpdateTimeTimer.Interval+1;
    if ((now-lastFormRepaint)*24*60*60>REPAINT_INTERVAL_IN_SECONDS) and repaintNecessary then begin
      lastFormRepaint:=now;
      flushThroughput;
      repaint;
      if ((plotSubsystem.state=pss_plotAfterCalculation) or
          (plotSubsystem.state=pss_plotOnShow) and (PageControl.ActivePageIndex=1)) and
         not(ad_evaluationRunning) and
         not(plotSubsystem.rendering) then begin
        doPlot();
      end;
    end;
  end;

procedure TMnhForm.processSettings;
  begin
    if not(settingsHaveBeenProcessed) then begin
      InputEdit.BeginUpdate();
      SettingsForm.getFileContents(InputEdit.Lines);
      InputEdit.EndUpdate();
    end;

    InputEdit.Font.name:=SettingsForm.getEditorFontName;
    InputEdit.Font.Size:=SettingsForm.fontSize;
    if SettingsForm.AntialiasCheckbox.Checked
    then InputEdit.Font.Quality:=fqCleartypeNatural
    else InputEdit.Font.Quality:=fqNonAntialiased;

    OutputEdit.Font     :=InputEdit.Font;
    //ErrorStringGrid.Font:=InputEdit.Font;

    top   :=SettingsForm.mainForm.top;
    left  :=SettingsForm.mainForm.left;
    width :=SettingsForm.mainForm.width;
    height:=SettingsForm.mainForm.height;

    with SettingsForm.outputBehaviour do begin
      miDeclarationEcho.Checked:=doEchoDeclaration;
      if doEchoDeclaration   then mnh_out_adapters.inputDeclEcho:=@writeDeclEcho
                             else mnh_out_adapters.inputDeclEcho:=nil;
      miExpressionEcho.Checked:=doEchoInput;
      if doEchoInput         then mnh_out_adapters.inputExprEcho:=@writeExprEcho
                             else mnh_out_adapters.inputExprEcho:=nil;
      miExpressionResult.Checked:=doShowExpressionOut;
      if doShowExpressionOut then mnh_out_adapters.exprOut:=@writeExprOut
                             else mnh_out_adapters.exprOut:=nil;
    end;
    if ad_currentFile<>SettingsForm.getFileInEditor then begin
      if SettingsForm.getFileInEditor=''
      then ad_clearFile
      else ad_setFile(SettingsForm.getFileInEditor,InputEdit.Lines);
    end;
    if not(settingsHaveBeenProcessed) then processFileHistory;

    settingsHaveBeenProcessed:=true;
  end;

procedure TMnhForm.processFileHistory;
  FUNCTION historyMenuItem(index:byte):TMenuItem;
    begin
      case index of
        0: result:=miFileHistory0;
        1: result:=miFileHistory1;
        2: result:=miFileHistory2;
        3: result:=miFileHistory3;
        4: result:=miFileHistory4;
        5: result:=miFileHistory5;
        6: result:=miFileHistory6;
        7: result:=miFileHistory7;
        8: result:=miFileHistory8;
        9: result:=miFileHistory9;
      end;
    end;
  VAR i:longint;
  begin
    for i:=0 to 9 do if SettingsForm.fileHistory[i]='' then begin
      historyMenuItem(i).Enabled:=false;
      historyMenuItem(i).Caption:=IntToStr(i)+': <no file>';
    end else begin
      historyMenuItem(i).Enabled:=true;
      historyMenuItem(i).Caption:=IntToStr(i)+': '+SettingsForm.fileHistory[i];
    end;
  end;

PROCEDURE TMnhForm.miAntialiasingOffClick(Sender: TObject);
  begin
    if ad_evaluationRunning or plotSubsystem.rendering or (PageControl.ActivePageIndex<>1)
       then plotSubsystem.state:=pss_plotOnShow
       else doPlot();
  end;

PROCEDURE TMnhForm.miAutoResetClick(Sender: TObject);
begin
  miAutoReset.Checked:=not(miAutoReset.Checked);
end;

PROCEDURE TMnhForm.miAutoscaleXClick(Sender: TObject);
begin
  miAutoscaleX.Checked:=not(miAutoscaleX.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miAutoscaleYClick(Sender: TObject);
begin
  miAutoscaleY.Checked:=not(miAutoscaleY.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miLogscaleXClick(Sender: TObject);
  begin
    miLogscaleX.Checked:=not(miLogscaleX.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miLogscaleYClick(Sender: TObject);
  begin
    miLogscaleY.Checked:=not(miLogscaleY.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miPreserveAspectClick(Sender: TObject);
  begin
    miPreserveAspect.Checked:=not(miPreserveAspect.Checked);
    pushSettingsToPlotContainer(true);
  end;

PROCEDURE TMnhForm.miXFinerGridClick(Sender: TObject);
begin
  miXFinerGrid.Checked:=not(miXFinerGrid.Checked);
  if miXFinerGrid.Checked then miXGrid.Checked:=true;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miXGridClick(Sender: TObject);
begin
  miXGrid.Checked:=not(miXGrid.Checked);
  if not(miXGrid.Checked) then miXFinerGrid.Checked:=false;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miXTicsClick(Sender: TObject);
begin
  miXTics.Checked:=not(miXTics.Checked);
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miYFinerGridClick(Sender: TObject);
begin
  miYFinerGrid.Checked:=not(miYFinerGrid.Checked);
  if miYFinerGrid.Checked then miYGrid.Checked:=true;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miYGridClick(Sender: TObject);
begin
  miYGrid.Checked:=not(miYGrid.Checked);
  if not(miYGrid.Checked) then miYFinerGrid.Checked:=false;
  pushSettingsToPlotContainer(true);
end;

PROCEDURE TMnhForm.miYTicsClick(Sender: TObject);
begin
  miYTics.Checked:=not(miYTics.Checked);
  pushSettingsToPlotContainer(true);
end;


FUNCTION addPlot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR options:ansistring='';
      sizeWithoutOptions:longint;
      rowId,i,iMax:longint;
      X,Y:P_listLiteral;

  begin
    if (params<>nil) and (params^.size>=1) then begin
      activePlot.setScreenSize(MnhForm.PlotTabSheet.Width,
                               MnhForm.PlotTabSheet.Height);
      if (params^.value(params^.size-1)^.literalType=lt_string) then begin
        options:=P_stringLiteral(params^.value(params^.size-1))^.value;
        sizeWithoutOptions:=params^.size-1;
      end else begin
        options:='';
        sizeWithoutOptions:=params^.size
      end;
      if (sizeWithoutOptions=1) and
         (params^.value(0)^.literalType=lt_list)
      then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        for i:=0 to X^.size-1 do begin
          if (X^.value(i)^.literalType in [lt_intList,lt_realList,lt_numList]) then begin
            Y:=P_listLiteral(X^.value(i));
            if Y^.size=2 then activePlot.row[rowId].addSample(fReal(Y^.value(0)),fReal(Y^.value(1)))
                         else activePlot.row[rowId].addSample(Nan,Nan);
          end else activePlot.row[rowId].addSample(Nan,Nan);
        end;
        plotSubsystem.state:=pss_plotAfterCalculation;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=1) and
         (params^.value(0)^.literalType in [lt_intList,lt_realList,lt_numList])
      then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        for i:=0 to X^.size-1 do activePlot.row[rowId].addSample(i,fReal(X^.value(i)));
        plotSubsystem.state:=pss_plotAfterCalculation;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=2) and
         (params^.value(0)^.literalType in [lt_intList,lt_realList,lt_numList]) and
         (params^.value(1)^.literalType in [lt_intList,lt_realList,lt_numList])
      then begin
        rowId:=activePlot.addRow(options);
        X:=P_listLiteral(params^.value(0));
        Y:=P_listLiteral(params^.value(1));
        iMax:=Min(X^.size,Y^.size);
        for i:=0 to iMax-1 do activePlot.row[rowId].addSample(fReal(X^.value(i)),fReal(Y^.value(i)));
        plotSubsystem.state:=pss_plotAfterCalculation;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=4) and
         (params^.value(0)^.literalType = lt_expression) and
         (params^.value(1)^.literalType in [lt_int,lt_real]) and
         (params^.value(2)^.literalType in [lt_int,lt_real]) and
         (params^.value(3)^.literalType = lt_int)
      then begin
        rowId:=activePlot.addRow(options);
        activePlot.row[rowId].setRules(
          nil,
          P_expressionLiteral(params^.value(0)),
                fReal(params^.value(1)),
                fReal(params^.value(2)),
          round(fReal(params^.value(3))));
        plotSubsystem.state:=pss_plotAfterCalculation;
        result:=newBoolLiteral(true);
      end else if (sizeWithoutOptions=5) and
         (params^.value(0)^.literalType = lt_expression) and
         (params^.value(1)^.literalType = lt_expression) and
         (params^.value(2)^.literalType in [lt_int,lt_real]) and
         (params^.value(3)^.literalType in [lt_int,lt_real]) and
         (params^.value(4)^.literalType = lt_int)
      then begin
        rowId:=activePlot.addRow(options);
        activePlot.row[rowId].setRules(
          P_expressionLiteral(params^.value(0)),
          P_expressionLiteral(params^.value(1)),
                fReal(params^.value(2)),
                fReal(params^.value(3)),
          round(fReal(params^.value(4))));
        plotSubsystem.state:=pss_plotAfterCalculation;
        result:=newBoolLiteral(true);
      end else result:=newErrorLiteralRaising('Functions plot and addPlot cannot be applied to parameter list'+params^.toParameterListString(true),tokenLocation);
    end else result:=newErrorLiteralRaising('Function plot and addPlot cannot be applied to empty parameter list',tokenLocation);
  end;

FUNCTION plot(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    activePlot.clear;
    result:=addPlot(params,tokenLocation,callDepth);
  end;

FUNCTION setAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_booleanList) and (P_listLiteral(params^.value(0))^.size=2) then begin
      activePlot.setAutoscale(P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
                              P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      MnhForm.pullPlotSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotAutoscale expects a list of 2 booleans as parameter.',tokenLocation);
  end;

FUNCTION getAutoscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=activePlot.getAutoscale;
  end;

FUNCTION setLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_booleanList) and (P_listLiteral(params^.value(0))^.size=2) then begin
      activePlot.setLogscale(P_boolLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
                             P_boolLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      MnhForm.pullPlotSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotLogscale expects a list of 2 booleans as parameter.',tokenLocation);
  end;

FUNCTION getLogscale(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=activePlot.getLogscale;
  end;

FUNCTION setPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  VAR x,y:P_literal;
      x0,y0,x1,y1:double;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_list) and (P_listLiteral(params^.value(0))^.size=2) then begin
      x:=P_listLiteral(params^.value(0))^.value(0);
      y:=P_listLiteral(params^.value(0))^.value(1);
      if (x^.literalType in [lt_intList,lt_realList,lt_numList]) and (P_listLiteral(x)^.size=2) and
         (y^.literalType in [lt_intList,lt_realList,lt_numList]) and (P_listLiteral(y)^.size=2) then begin
        x0:=fReal(P_listLiteral(x)^.value(0));
        x1:=fReal(P_listLiteral(x)^.value(1));
        y0:=fReal(P_listLiteral(y)^.value(0));
        y1:=fReal(P_listLiteral(y)^.value(1));
        if not(IsNan(x0)) and not(IsInfinite(x0)) and
           not(IsNan(x1)) and not(IsInfinite(x1)) and
           not(IsNan(y0)) and not(IsInfinite(y0)) and
           not(IsNan(y1)) and not(IsInfinite(y1)) then begin
          activePlot.setRange(x0,y0,x1,y1);
          MnhForm.pullPlotSettingsToGui();
          result:=newBoolLiteral(true);
        end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
      end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
    end else result:=newErrorLiteralRaising('Function setPlotRange expects a list of structure [[x0,x1],[y0,y1]] as parameter. Infinite and NaN values are forbidden.',tokenLocation);
  end;

FUNCTION getPlotRange(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=activePlot.getRange;
  end;

FUNCTION setAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_intList) and (P_listLiteral(params^.value(0))^.size=2) then begin
      activePlot.setAxisStyle(P_intLiteral(P_listLiteral(params^.value(0))^.value(0))^.value,
                              P_intLiteral(P_listLiteral(params^.value(0))^.value(1))^.value);
      MnhForm.pullPlotSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotAxisStyle expects a list of 2 integers as parameter.',tokenLocation);
  end;

FUNCTION getAxisStyle(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=activePlot.getAxisStyle;
  end;

FUNCTION setPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=nil;
    if (params<>nil) and (params^.size=1) and (params^.value(0)^.literalType=lt_boolean) then begin
      activePlot.setPreserveAspect(P_boolLiteral(params^.value(0))^.value);
      MnhForm.pullPlotSettingsToGui();
      result:=newBoolLiteral(true);
    end else result:=newErrorLiteralRaising('Function setPlotPreserveAspect expects a boolean as parameter.',tokenLocation);
  end;

FUNCTION getPreserveAspect(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    while plotSubsystem.rendering do sleep(1);
    result:=activePlot.getPreserveAspect;
  end;

FUNCTION true_impl(CONST params:P_listLiteral; CONST tokenLocation:T_tokenLocation; CONST callDepth:word):P_literal;
  begin
    result:=newBoolLiteral(true);
  end;


initialization
  mnh_funcs.registerRule('plotAvailable' ,@true_impl,'returns true (because plotting is available)');
  mnh_funcs.registerRule('plot',@plot,'plot(list,[options]); //plots flat numeric list or xy-list'+
  '#plot(xList,yList,[options]); //plots flat numeric list or xy-list'+
  '#plot(yExpression,t0,t1,samples,[options]); //plots yExpression versus t in [t0,t1]'+
  '#plot(xExpression,yExpression,t0,t1,samples,[options]); //plots yExpression versus xExpression for t in [t0,t1]');
  mnh_funcs.registerRule('addPlot',@addPlot,'addPlot(list,[options]); //adds plot flat numeric list or xy-list'+
  '#addPlot(xList,yList,[options]); //plots flat numeric list or xy-list'+
  '#addPlot(yExpression,t0,t1,samples,[options]); //plots yExpression versus t in [t0,t1]'+
  '#addPlot(xExpression,yExpression,t0,t1,samples,[options]); //plots yExpression versus xExpression for t in [t0,t1]');
  mnh_funcs.registerRule('setPlotAutoscale',@setAutoscale,'setPlotAutoscale([forX,forY]);#Sets autoscale per axis and returns true#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule('getPlotAutoscale',@getAutoscale,'getPlotAutoscale;#Returns the current autoscale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule('setPlotLogscale',@setLogscale,'setPlotLogscale([forX,forY]);#Sets log-scale per axis and returns true#Expects a tuple of two booleans as parameter.');
  mnh_funcs.registerRule('getPlotLogscale',@getLogscale,'getPlotLogscale;#Returns the current log-scale settings per axis as a tuple of two booleans.');
  mnh_funcs.registerRule('setPlotRange',@setPlotRange,'setPlotRange([[x0,x1],[y0,y1]]);#Sets the plot-range for the next plot and returns true.');
  mnh_funcs.registerRule('getPlotRange',@getPlotRange,'getPlotRange;#Returns the plot-range of the last plot as a nested list: [[x0,x1],[y0,y1]]');
  mnh_funcs.registerRule('setPlotAxisStyle',@setAxisStyle,'setPlotAxisStyle([sx,sy]);#Sets the axis style for the next plot and returns true.');
  mnh_funcs.registerRule('getPlotAxisStyle',@getAxisStyle,'getPlotAxisStyle([sx,sy]);#Returns the current axis-style as a tuple of two integers.');
  mnh_funcs.registerRule('setPlotPreserveAspect',@setPreserveAspect,'setPlotPreserveAspect(b:boolean);#Sets or un-sets preservation of aspect ratio for the next plot.');
  mnh_funcs.registerRule('getPlotPreserveAspect',@getPreserveAspect,'getPlotPreserveAspect;#Returns a boolean indicating whether the aspect ratio will be preserverd for the next plot');
  mnh_evalThread.initIntrinsicRuleList;

  plotSubsystem.state:=pss_neutral;
  plotSubsystem.rendering:=false;

  output.create;
  setLength(errorThroughput,0);

finalization
  output.destroy;
end.

