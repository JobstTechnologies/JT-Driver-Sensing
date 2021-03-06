unit ceTitleFootFrame;

{$mode ObjFPC}{$H+}
{.$DEFINE WYSIWYG_TITLE}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls,
  TATextElements, TAGraph,
  ceFontFrame, ceShapeBrushPenMarginsFrame;

type

  { TChartTitleFootFrame }

  TChartTitleFootFrame = class(TFrame)
    cbShow: TCheckBox;
    gbFont: TGroupBox;
    gbShapeBrushPenMargins: TGroupBox;
    leText: TLabeledEdit;
    TextPanel: TPanel;
    ParamsPanel: TPanel;
    rgAlignment: TRadioGroup;
    procedure cbShowChange(Sender: TObject);
    procedure leTextChange(Sender: TObject);
    procedure rgAlignmentClick(Sender: TObject);
  private
    FTitle: TChartTitle;
    FFontFrame: TChartFontFrame;
    FShapeBrushPenMarginsFrame: TChartShapeBrushPenMarginsFrame;
    procedure ChangedHandler(Sender: TObject);
    function GetAlignment: TAlignment;
    procedure SetAlignment(AValue: TAlignment);
    procedure ShapeChangedHandler(AShape: TChartLabelShape);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
    function GetChart: TChart;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Prepare(ATitle: TChartTitle);
  end;

implementation

{$R *.lfm}

uses
  ceUtils;

constructor TChartTitleFootFrame.Create(AOwner: TComponent);
begin
  inherited;

  FFontFrame := TChartFontFrame.Create(self);
  FFontFrame.Parent := gbFont;
  FFontFrame.Align := alClient;
  FFontFrame.BorderSpacing.Left := 8;
  FFontFrame.BorderSpacing.Right := 8;
  FFontFrame.OnChange := @ChangedHandler;
  gbFont.AutoSize := true;

  FShapeBrushPenMarginsFrame := TChartShapeBrushPenMarginsFrame.Create(self);
  FShapeBrushPenMarginsFrame.Parent := gbShapeBrushPenMargins;
  FShapeBrushPenMarginsFrame.Align := alClient;
  FShapeBrushPenMarginsFrame.BorderSpacing.Left := 4;
  FShapeBrushPenMarginsFrame.BorderSpacing.Right := 4;
  FShapeBrushPenMarginsFrame.BorderSpacing.Bottom := 4;
  FShapeBrushPenMarginsFrame.OnChange := @ChangedHandler;
  FShapeBrushPenMarginsFrame.OnShapeChange := @ShapeChangedHandler;
  FShapeBrushPenMarginsFrame.AutoSize := true;
  gbShapeBrushPenMargins.AutoSize := true;

  BoldHeaders(Self);

  ParamsPanel.AutoSize := true;
end;

procedure TChartTitleFootFrame.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredHeight := TextPanel.Constraints.MinHeight
    + ParamsPanel.Height + ParamsPanel.BorderSpacing.Top
    + ParamsPanel.BorderSpacing.Bottom;

  PreferredWidth := gbFont.Width +
    gbShapeBrushPenMargins.Width + gbShapeBrushPenMargins.BorderSpacing.Left;
end;


procedure TChartTitleFootFrame.cbShowChange(Sender: TObject);
begin
  FTitle.Visible := cbShow.Checked;
  leText.Visible := cbShow.Checked;
  rgAlignment.Visible := cbShow.Checked;
  gbShapeBrushPenMargins.Visible := cbShow.Checked;
  gbFont.Visible := cbShow.Checked;
end;

procedure TChartTitleFootFrame.ChangedHandler(Sender: TObject);
begin
  GetChart.Invalidate;
  {$IFDEF WYSIWYG_TITLE}
  mmoText.Font.Assign(FTitle.Font);
  mmoText.Color := FTitle.Brush.Color;
  {$ENDIF}
end;

function TChartTitleFootFrame.GetAlignment: TAlignment;
const
  ALIGNMENTS: array[0..2] of TAlignment = (taLeftJustify, taCenter, taRightJustify);
begin
  Result := ALIGNMENTS[rgAlignment.ItemIndex];
end;

function TChartTitleFootFrame.GetChart: TChart;
begin
  Result := FTitle.GetOwner as TChart;
end;

procedure TChartTitleFootFrame.leTextChange(Sender: TObject);
begin
  // we must assure that the title is not empty because of the
  // elements we have on top of the chart
  // thus set a space when the user emptied it
  if leText.Text = '' then
    leText.Text := ' ';
  FTitle.Text[0] := leText.Text;
end;

procedure TChartTitleFootFrame.rgAlignmentClick(Sender: TObject);
begin
  FTitle.Alignment := GetAlignment;
end;

procedure TChartTitleFootFrame.Prepare(ATitle: TChartTitle);
begin
  FTitle := ATitle;

  cbShow.Checked := ATitle.Visible;
  leText.Text := ATitle.Text[0];
  {$IFDEF WYSIWYG_TITLE}
  mmoText.Font.Assign(ATitle.Font);
  mmoText.Font.Orientation := 0;
  {$ENDIF}

  SetAlignment(ATitle.Alignment);

  FFontFrame.Prepare(ATitle.Font, false);
  FShapeBrushPenMarginsFrame.Prepare(ATitle.Shape, ATitle.Brush, ATitle.Frame, ATitle.Margins);
end;

procedure TChartTitleFootFrame.SetAlignment(AValue: TAlignment);
const
  ALIGNMENTS: array[TAlignment] of Integer = (0, 2, 1);
begin
  rgAlignment.ItemIndex := ALIGNMENTS[AValue];
end;

procedure TChartTitleFootFrame.ShapeChangedHandler(AShape: TChartLabelShape);
begin
  FTitle.Shape := AShape;
end;

end.

