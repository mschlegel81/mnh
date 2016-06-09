// MIT License
//
// Copyright (c) 2016 Martin Schlegel
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

{$ifdef windows}{$MAXSTACKSIZE 100000000}{$endif}
PROGRAM mnh_gui;

{$mode objfpc}{$H+}

USES {$IFDEF UNIX} cthreads, cmem,{$ENDIF}
  {$ifdef DEBUGMODE}heaptrc,{$endif}
  Interfaces, // this includes the LCL widgetset
  Forms, mnh_gui_settings, mnh_gui_main, closeDialog, askDialog,
  mnh_cmdLineInterpretation, mnh_funcs, mnh_funcs_list,
  mnh_funcs_math, mnh_funcs_mnh, mnh_funcs_regex, mnh_funcs_strings,
  mnh_funcs_system, mnh_litVar, mnh_packages, mnh_out_adapters,
  consoleAsk, mnh_constants, mnh_doc, mnh_html,
  SynHighlighterMnh, mnh_evalThread, mySys,
  mnh_plotData,mnh_plotFuncs, mnh_plotForm, newCentralPackageDialog, mnh_tables;

{$R *.res}

begin
  parseCmdLine;
  hideConsole;

  mnh_gui_main.lateInitialization;
  Application.title:='MNH5 - GUI';
  RequireDerivedFormResource := true;
  Application.initialize;

  Application.CreateForm(TMnhForm, MnhForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TcloseDialogForm, closeDialogForm);
  Application.CreateForm(TaskForm, askForm);
  Application.CreateForm(TplotForm, plotForm);
  Application.CreateForm(TnewCentralPackageForm, newCentralPackageForm);
  Application.CreateForm(TtableForm, tableForm);
  Application.run;
  showConsole;
end.
