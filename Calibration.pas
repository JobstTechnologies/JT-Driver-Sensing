unit Calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, ExtCtrls, TAChartListbox, TALegend, TACustomSeries, TAChartUtils,
  TASeries, Math,
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
  procedure ValueFSEChange(Sender: TObject);
 private

 public
  procedure CalculateMean;

 end;

var
  CalibrationF: TCalibrationF;
  calibChannelA : integer = 0;
  calibChannelB : integer = 0;
  calibFactorA : double = 0.0;
  calibFactorB : double = 0.0;

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

procedure TCalibrationF.ValueFSEChange(Sender: TObject);
begin
 // recalculate mean with the new calibration value
 CalculateMean;
end;

procedure TCalibrationF.CalculateMean;
var
 selectedSeries, selectedSeriesMean : TChartSeries;
 selectedSeriesA : TLineSeries;
 i, k, n, calibChannel : integer;
 x, y, yMean, x1, x2, y1, y2, calibValue, molWeight,
 yMeanA, yMeanB, yMeanMean  : double;
 selSeriesName : string;
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
 // since we have an axis transformation the positions are in respect
 // to the range 0-1 so we need to remap them
 y1:= MainForm.SIXCH.LeftAxis.GetTransform.GraphToAxis(y1);
 y2:= MainForm.SIXCH.LeftAxis.GetTransform.GraphToAxis(y2);

 for i:= 0 to SIXCHCLB.SeriesCount-1 do
 begin
  if not SIXCHCLB.Selected[i] then
   continue;

  selectedSeries:= SIXCHCLB.Series[i] as TChartSeries;
  selSeriesName:= selectedSeries.Name;

  // find what channel is selected
  // selSeriesName is in the form 'SIXChxValues' and we need the x
  // so get the 6th character of the name
  calibChannel:= StrToInt(Copy(selSeriesName, 6, 1));

  // initialize
  yMean:= 0.0;
  yMeanA:= 0.0;
  yMeanB:= 0.0;
  n:= 0;
  // silence compiler
  selectedSeriesMean:= SIXCHCLB.Series[i] as TChartSeries;
  selectedSeriesA:= MainForm.SIXCH.Series[0] as TLineSeries;

  // check if selected series fits to the substance
  if calibChannel < 7 then
  begin
   if ( (pos('Glu', (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'GB')
        as TGroupBox).Caption) = 0)
     and
      (pos('glu', (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'GB')
        as TGroupBox).Caption) = 0)
     and (GlucoseRB.Checked) )
    or
     ( (pos('Lac', (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'GB')
        as TGroupBox).Caption) = 0)
     and
      (pos('lac', (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'GB')
       as TGroupBox).Caption) = 0)
     and (LactateRB.Checked) ) then
   begin
    MeanValueLE.Text:= 'wrong substance';
    CalibOKBB.Enabled:= false;
    CalibOKBB.Hint:= 'Select a correct channel to' + LineEnding
                     + 'perform the calibration';
    exit;
   end;

   // calculate the mean
   for k:= 0 to selectedSeries.Count-1 do
   begin
    x:= selectedSeries.XValue[k];
    y:= selectedSeries.YValue[k];
    // only calculate the values that are within the selection rectangle
    if (x >= x1) and (x <= x2)
     and (y >= y1) and (y <= y2) then
    begin
     yMean:= yMean + y;
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
   yMeanMean:= yMean / n;

   calibChannelA:= calibChannel;
   calibChannelB:= 0;
  end

  // if an operation channel
  else
  begin
   if (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'CB')
       as TComboBox).Text = 'mean(#2, #5)' then
   begin
    if ( (pos('Glu' , (MainForm.FindComponent('Channel' + IntToStr(2) + 'GB')
        as TGroupBox).Caption) = 0)
      and
      (pos('glu', (MainForm.FindComponent('Channel' + IntToStr(2) + 'GB')
        as TGroupBox).Caption) = 0)
      and (GlucoseRB.Checked) )
     or
      ( (pos('Lac', (MainForm.FindComponent('Channel' + IntToStr(2) + 'GB')
         as TGroupBox).Caption) = 0)
      and
       (pos('lac', (MainForm.FindComponent('Channel' + IntToStr(2) + 'GB')
        as TGroupBox).Caption) = 0)
      and (LactateRB.Checked) ) then
    begin
     MeanValueLE.Text:= 'wrong substance';
     CalibOKBB.Enabled:= false;
     CalibOKBB.Hint:= 'Select a correct channel to' + LineEnding
                      + 'perform the calibration';
     exit;
    end
    else
    begin
     selectedSeriesMean:= SIXCHCLB.Series[i] as TChartSeries;
     selectedSeriesA:= MainForm.SIXCH.Series[2] as TLineSeries;
     calibChannelA:= 2;
     calibChannelB:= 5;
    end;
   end
   else if (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'CB')
       as TComboBox).Text = 'mean(#3, #6)' then
   begin
    if ( (pos('Glu', (MainForm.FindComponent('Channel' + IntToStr(3) + 'GB')
        as TGroupBox).Caption) = 0)
      and
       (pos('glu', (MainForm.FindComponent('Channel' + IntToStr(3) + 'GB')
        as TGroupBox).Caption) = 0)
      and (GlucoseRB.Checked) )
     or
      ( (pos('Lac', (MainForm.FindComponent('Channel' + IntToStr(3) + 'GB')
        as TGroupBox).Caption) = 0)
      and
       (pos('lac', (MainForm.FindComponent('Channel' + IntToStr(3) + 'GB')
        as TGroupBox).Caption) = 0)
      and (LactateRB.Checked) ) then
    begin
     MeanValueLE.Text:= 'wrong substance';
     CalibOKBB.Enabled:= false;
     CalibOKBB.Hint:= 'Select a correct channel to' + LineEnding
                      + 'perform the calibration';
     exit;
    end
    else
    begin
     selectedSeriesMean:= SIXCHCLB.Series[i] as TChartSeries;
     selectedSeriesA:= MainForm.SIXCH.Series[4] as TLineSeries;
     calibChannelA:= 3;
     calibChannelB:= 6;
    end;
   end
   else if (MainForm.FindComponent('Channel' + IntToStr(calibChannel) + 'CB')
       as TComboBox).Text = 'mean(#1, #4)' then
   begin
    if ( (pos('Glu', (MainForm.FindComponent('Channel' + IntToStr(1) + 'GB')
        as TGroupBox).Caption) = 0)
      and
       (pos('glu', (MainForm.FindComponent('Channel' + IntToStr(1) + 'GB')
        as TGroupBox).Caption) = 0)
      and (GlucoseRB.Checked) )
     or
      ( (pos('Lac', (MainForm.FindComponent('Channel' + IntToStr(1) + 'GB')
        as TGroupBox).Caption) = 0)
      and
       (pos('lac', (MainForm.FindComponent('Channel' + IntToStr(1) + 'GB')
        as TGroupBox).Caption) = 0)
      and (LactateRB.Checked) ) then
    begin
     MeanValueLE.Text:= 'wrong substance';
     CalibOKBB.Enabled:= false;
     CalibOKBB.Hint:= 'Select a correct channel to' + LineEnding
                      + 'perform the calibration';
     exit;
    end
    else
    begin
     selectedSeriesMean:= SIXCHCLB.Series[i] as TChartSeries;
     selectedSeriesA:= MainForm.SIXCH.Series[0] as TLineSeries;
     calibChannelA:= 1;
     calibChannelB:= 4;
    end;
   end; // end if 'mean(#1, #4)'

   // calculate now the mean of the two channels
   // we purposely use the mean channel because the user set the range
   // according to this and not a single channel
   for k:= 0 to selectedSeriesMean.Count-1 do
    begin
     x:= selectedSeriesMean.XValue[k];
     y:= selectedSeriesMean.YValue[k];
     // only calculate the values that are within the selection rectangle
     if (x >= x1) and (x <= x2)
      and (y >= y1) and (y <= y2) then
     begin
      yMean:= yMean + y;
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
    yMeanMean:= yMean / n;

    // now get the mean of the first channel
    yMean:= 0.0;
    n:= 0;
    for k:= 0 to selectedSeriesA.Count-1 do
    begin
     x:= selectedSeriesA.XValue[k];
     // only calculate the values that are within the x-range
     if (x >= x1) and (x <= x2) then
     begin
      yMean:= yMean + selectedSeriesA.YValue[k];
      inc(n);
     end;
    end;
    if n = 0 then
    begin
     MeanValueLE.Text:= 'no data in first channel';
     CalibOKBB.Enabled:= false;
     CalibOKBB.Hint:= 'No data available to' + LineEnding
                      + 'perform the calibration';
     exit;
    end;
    yMeanA:= yMean / n;

    // we can now calculate the mean of the second channel
    yMeanB:= 2*yMeanMean - YMeanA;

  end; //end else

  CalibOKBB.Enabled:= true;
  CalibOKBB.Hint:= '';
  MeanValueLE.Text:= FloatToStr(RoundTo(yMeanMean, -4));

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

  // output the factor(s) with which the current gain must be multiplied
  if calibChannel < 7 then
  begin
   calibFactorA:= calibValue / yMeanMean;
   calibFactorB:= 0.0;
  end
  else
  begin
   calibFactorA:= calibValue / yMeanA;
   calibFactorB:= calibValue / yMeanB;
  end;

 end;

end;

end.
