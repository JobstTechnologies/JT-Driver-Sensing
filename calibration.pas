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
  GlucoseRB: TRadioButton;
  LactateRB: TRadioButton;
  SubstanceGB: TGroupBox;
  MeanValueLE: TLabeledEdit;
  AvailableChannelsGB: TGroupBox;
  SIXCHCLB: TChartListbox;
  UnitCB: TComboBox;
  ValueFSE: TFloatSpinEdit;
  procedure FormShow(Sender: TObject);
  procedure SIXCHCLBAddSeries(ASender{%H-}: TChartListbox;
   ASeries: TCustomChartSeries; AItems{%H-}: TChartLegendItems; var ASkip: Boolean);
  procedure SIXCHCLBItemClick(ASender{%H-}: TObject; AIndex{%H-}: Integer);
  procedure SubstanceGBClick(Sender: TObject);
 private

 public
  procedure CalculateMean;

 end;

var
  CalibrationF: TCalibrationF;
  calibChannel : integer = 0;
  calibGain : double = 0.0;

implementation

{$R *.lfm}

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

procedure TCalibrationF.SubstanceGBClick(Sender: TObject);
begin
 CalculateMean;
end;

procedure TCalibrationF.CalculateMean;
var
 selectedSeries : TChartSeries;
 i, j, n : integer;
 x, y, yMean, x1, x2, y1, y2, calibValue, molWeight : double;
begin

 // catch the case that no substance was selected
 if (not GlucoseRB.Checked) and (not LactateRB.Checked) then
 begin
  MeanValueLE.Text:= 'no substance selected';
  CalibOKBB.Enabled:= false;
  CalibOKBB.Hint:= 'Select a substance to' + LineEnding
                   + 'perform the calibration';
  exit;
 end;

 x1:= Min(MainForm.LeftLine.Position, MainForm.RightLine.Position);
 x2:= Max(MainForm.LeftLine.Position, MainForm.RightLine.Position);
 y1:= Min(MainForm.TopLine.Position, MainForm.BottomLine.Position);
 y2:= Max(MainForm.TopLine.Position, MainForm.BottomLine.Position);

 for i:= 0 to SIXCHCLB.SeriesCount-1 do
 begin
  if not SIXCHCLB.Selected[i] then
   continue;
  calibChannel:= i;
  selectedSeries:= SIXCHCLB.Series[i] as TChartSeries;

  // check if selected series fits to the substance
  if (copy(
       (MainForm.FindComponent('Channel' + IntToStr(i+1) + 'GB')
        as TGroupBox).Caption, 1, 7) <> 'Glucose')
   and
     (copy((MainForm.FindComponent('Channel' + IntToStr(i+1) + 'GB')
        as TGroupBox).Caption, 1, 7) <> 'glucose')
   and (GlucoseRB.Checked) then
  begin
   MeanValueLE.Text:= 'wrong substance';
   CalibOKBB.Enabled:= false;
   CalibOKBB.Hint:= 'Select a correct channel to' + LineEnding
                    + 'perform the calibration';
   exit;
  end;
  if (copy(
       (MainForm.FindComponent('Channel' + IntToStr(i+1) + 'GB')
        as TGroupBox).Caption, 1, 7) <> 'Lactate')
   and
     (copy((MainForm.FindComponent('Channel' + IntToStr(i+1) + 'GB')
        as TGroupBox).Caption, 1, 7) <> 'lactate')
   and (LactateRB.Checked) then
  begin
   MeanValueLE.Text:= 'wrong substance';
   CalibOKBB.Enabled:= false;
   CalibOKBB.Hint:= 'Select a correct channel to' + LineEnding
                    + 'perform the calibration';
   exit;
  end;

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
   exit;
  end;

  yMean:= yMean / n;
  MeanValueLE.Text:= FloatToStr(RoundTo(yMean, -4));
  CalibOKBB.Enabled:= true;
  CalibOKBB.Hint:= '';

  // output the mean as plain nA value
  if MainForm.RawCurrentCB.Checked then
  // calibMean:= yMean
  else
  begin
   // check units
   calibValue:= ValueFSE.Value; // mmol/l
   if LactateRB.Checked then
    molWeight:= 90.07 //in g/mol
   else
    molWeight:= 180.156;

   if UnitCB.ItemIndex = 1 then // g/l
    calibValue:= ValueFSE.Value / molWeight / 1000
   else if UnitCB.ItemIndex = 2 then // mg/dl
    calibValue:= ValueFSE.Value / molWeight / 1000 / 100;
   calibGain:= calibValue / yMean;
  end;
 end;

end;

end.
