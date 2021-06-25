unit calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, TAChartListbox, TALegend, TACustomSeries,
  // custom forms
  JTDriverSensingMain;

type

  { TCalibrationF }

 TCalibrationF = class(TForm)
  CalibOKBB: TBitBtn;
  CalibCancelBB: TBitBtn;
  SIXCHCLB: TChartListbox;
  ComboBox1: TComboBox;
  FloatSpinEdit1: TFloatSpinEdit;
  Label1: TLabel;
  procedure SIXCHCLBAddSeries(ASender{%H-}: TChartListbox;
   ASeries: TCustomChartSeries; AItems{%H-}: TChartLegendItems; var ASkip: Boolean);
  procedure SIXCHCLBItemClick(ASender{%H-}: TObject; AIndex{%H-}: Integer);
 private

 public

 end;

var
  CalibrationF: TCalibrationF;

implementation

{$R *.lfm}

{ TCalibrationF }

procedure TCalibrationF.SIXCHCLBAddSeries(ASender: TChartListbox;
  ASeries: TCustomChartSeries; AItems: TChartLegendItems; var ASkip: Boolean);
var
 SenderEnd : string;
begin
 // don't add the selection line series
 ASkip:= (ASeries = MainForm.LeftLine) or (ASeries = MainForm.RightLine)
         or (ASeries = MainForm.TopLine) or (ASeries = MainForm.BottomLine);
 if ASkip then
  exit; // because their names are too short for the following string operations
 // don't show the result series
 // ASeries.Name is in the form 'SIXCh6xxx' so check if xxx = 'Results'
 SenderEnd:= RightStr(ASeries.Name, 7);
 if SenderEnd = 'Results' then
  ASkip:= true;
 // don't show the temperature series
 // ASeries.Name is then 'SIXTempValues'
 SenderEnd:= Copy(ASeries.Name, 4, 4);
 if SenderEnd = 'Temp' then
  ASkip:= true;
end;

procedure TCalibrationF.SIXCHCLBItemClick(ASender: TObject; AIndex: Integer);
begin
 //CalcStats;
end;


end.

