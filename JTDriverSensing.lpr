program JTDriverSensing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, lazcontrols, JTDriverSensingMain,
  SerialUSBSelection, NameSetting, SIXControlUnit, Fitting, PumpControlUnit,
  AboutForm, ceFontFrame, ceAxisDlg, ceBrushFrame, ceLegendDlg, cePenFrame,
  cePointerFrame, ceSeriesDlg, ceShapeBrushPenMarginsFrame, ceTitleFootDlg,
  ceUtils, Calibration;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Title:='JT Driver Sensing';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFitForm, FitForm);
  Application.CreateForm(TNameSettingF, NameSettingF);
  Application.CreateForm(TSerialUSBSelectionF, SerialUSBSelectionF);
  Application.CreateForm(TAboutFormF, AboutFormF);
  Application.CreateForm(TCalibrationF, CalibrationF);
  Application.Run;
end.

