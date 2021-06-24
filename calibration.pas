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
begin
 // don't add the selection line series
 ASkip:= (ASeries = MainForm.LeftLine) or (ASeries = MainForm.RightLine)
         or (ASeries = MainForm.TopLine) or (ASeries = MainForm.BottomLine);
end;

procedure TCalibrationF.SIXCHCLBItemClick(ASender: TObject; AIndex: Integer
  );
begin
 //CalcStats;
end;


end.

