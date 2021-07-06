unit SerialUSBSelection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TSerialUSBSelectionF }

  TSerialUSBSelectionF = class(TForm)
    OKButtonB: TButton;
    CancelButtonB: TButton;
    SerialUSBPortCB: TComboBox;
    SerialUSBPortL: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CancelButtonBClick(Sender: TObject);
    procedure OKButtonBClick(Sender: TObject);
  private

  public

  end;

var
  SerialUSBSelectionF: TSerialUSBSelectionF;
  COMPort : string;

implementation

{$R *.lfm}

procedure TSerialUSBSelectionF.FormCreate(Sender: TObject);
begin
 ActiveControl:= SerialUSBPortCB;
end;

procedure TSerialUSBSelectionF.OKButtonBClick(Sender: TObject);
begin
 COMPort:= SerialUSBPortCB.Text;
 SerialUSBSelectionF.Close;
end;

procedure TSerialUSBSelectionF.CancelButtonBClick(Sender: TObject);
begin
 COMPort:= 'Ignore';
 SerialUSBSelectionF.Close;
end;

end.
