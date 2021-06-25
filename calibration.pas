unit calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, ExtCtrls, TAChartListbox, TALegend, TACustomSeries, Math,
  // custom forms
  JTDriverSensingMain;

type

  { TCalibrationF }

 TCalibrationF = class(TForm)
  CalibOKBB: TBitBtn;
  CalibCancelBB: TBitBtn;
  ConcentrationGB: TGroupBox;
  MeanValueLE: TLabeledEdit;
  AvailableChannelsGB: TGroupBox;
  SIXCHCLB: TChartListbox;
  UnitCB: TComboBox;
  ValueFSE: TFloatSpinEdit;
  procedure FormShow(Sender: TObject);
  procedure SIXCHCLBAddSeries(ASender{%H-}: TChartListbox;
   ASeries: TCustomChartSeries; AItems{%H-}: TChartLegendItems; var ASkip: Boolean);
  procedure SIXCHCLBItemClick(ASender{%H-}: TObject; AIndex{%H-}: Integer);
 private

 public
  procedure CalculateMean;

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
 // don't show non-active series
 if not ASeries.Active then
 begin
  ASkip:= true;
  exit;
 end;
 // don't add the selection line series
 if (ASeries = MainForm.LeftLine) or (ASeries = MainForm.RightLine)
    or (ASeries = MainForm.TopLine) or (ASeries = MainForm.BottomLine) then
 begin
  ASkip:= true;
  exit;
 end;
 // don't show the result series
 // ASeries.Name is in the form 'SIXCh6xxx' so check if xxx = 'Results'
 SenderEnd:= RightStr(ASeries.Name, 7);
 if SenderEnd = 'Results' then
 begin
  ASkip:= true;
  exit;
 end;
 // don't show the temperature series
 // ASeries.Name is then 'SIXTempValues'
 SenderEnd:= Copy(ASeries.Name, 4, 4);
 if SenderEnd = 'Temp' then
  ASkip:= true;
end;

procedure TCalibrationF.FormShow(Sender: TObject);
begin
 // recalculate in case a series is also preselected from a prior form call
 MeanValueLE.Text:= '';
 CalculateMean;
end;

procedure TCalibrationF.SIXCHCLBItemClick(ASender: TObject; AIndex: Integer);
begin
 MeanValueLE.Text:= '';
 CalculateMean;
end;

procedure TCalibrationF.CalculateMean;
var
 selectedSeries: TChartSeries;
 i, j, n: integer;
 x, y, yMean: double;
 x1, x2, y1, y2: double;
begin
 x1:= Min(MainForm.LeftLine.Position, MainForm.RightLine.Position);
 x2:= Max(MainForm.LeftLine.Position, MainForm.RightLine.Position);
 y1:= Min(MainForm.TopLine.Position, MainForm.BottomLine.Position);
 y2:= Max(MainForm.TopLine.Position, MainForm.BottomLine.Position);

 for i:= 0 to SIXCHCLB.SeriesCount-1 do
 begin
  if not SIXCHCLB.Selected[i] then
   continue;
  selectedSeries:= SIXCHCLB.Series[i] as TChartSeries;

  yMean:= 0.0;
  n:= 0;
  for j:= 0 to selectedSeries.Count-1 do
  begin
   x:= selectedSeries.XValue[j];
   y:= selectedSeries.YValue[j];
   // only calculate the values that are within the selection rectangle
   if (x >= x1) and (x <= x2)
      and (y >= y1) and (y <= y2) then
   begin
    yMean:= yMean + selectedSeries.YValue[j];
    inc(n);
   end;
  end;

  // catch the case that no data is within the selection
  if n = 0 then
  begin
   MeanValueLE.Text:= 'no data in selection';
   CalibOKBB.Enabled:= false;
   CalibOKBB.Hint:= 'No data available to' + LineEnding
                    + 'perform the calibration';
  end
  else
  begin
   yMean:= yMean / n;
   MeanValueLE.Text:= FloatToStr(RoundTo(yMean, -4));
   CalibOKBB.Enabled:= true;
   CalibOKBB.Hint:= '';
  end;

 end;

end;


end.

