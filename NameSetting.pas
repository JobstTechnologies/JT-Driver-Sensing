unit NameSetting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  LCLType;

type

  TNameSettingF = class(TForm)
    NameL: TLabel;
    OKButtonBB: TBitBtn;
    CancelButtonBB: TBitBtn;
    NameE: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure NameEKeyPress(Sender: TObject; var Key: char);
  private

  public

  end;

var
  NameSettingF: TNameSettingF;

implementation

{$R *.lfm}

procedure TNameSettingF.FormCreate(Sender: TObject);
begin
 ActiveControl:= NameE;
end;

procedure TNameSettingF.NameEKeyPress(Sender: TObject; var Key: char);
begin
 // if we got a return key, treat it as if the user pressed the OK button
 if key = Char(VK_RETURN) then
  OKButtonBB.Click;
end;

end.

