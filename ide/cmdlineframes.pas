unit cmdLineFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls;

type

  { TCmdLineParametersFrame }

  TCmdLineParametersFrame = class(TFrame)
    addOutFile: TButton;
    cbConsoleLikeLog: TCheckBox;
    cbConvertPrintToLog: TCheckBox;
    Label3: TLabel;
    logLocationLengthEdit: TEdit;
    formatPreviewMemo: TMemo;
    timeFormatEdit: TEdit;
    GroupBox6: TGroupBox;
    Label2: TLabel;
    outFileVerbosityEdit: TEdit;
    verbosityGroupBox: TGroupBox;
    logFilenameEdit: TEdit;
    GroupBox5: TGroupBox;
    rbOutputToFile: TRadioButton;
    rbOutputToStdout: TRadioButton;
    rbOutputToStderr: TRadioButton;
    removeOutFile: TButton;
    ComboBox1: TComboBox;
    consoleVerbosityEdit: TEdit;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    quietFlagCb: TCheckBox;
    silentFlagCb: TCheckBox;
    GroupBox3: TGroupBox;
    pauseFlagCb: TCheckBox;
    pauseOnErrorFlagCb: TCheckBox;
    guiFlagCb: TCheckBox;
    headlessFlagCb: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    PageControl1: TPageControl;
    lightVersionRb: TRadioButton;
    fullVersionRb: TRadioButton;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
  private

  public

  end;

implementation

{$R *.lfm}

end.

