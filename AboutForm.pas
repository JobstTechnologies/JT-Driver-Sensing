unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  LCLIntf;

type

  { TAboutFormF }

  TAboutFormF = class(TForm)
    OKBB: TBitBtn;
    ReadmeLinkL: TLabel;
    NameL: TLabel;
    DescriptionTextST: TStaticText;
    VersionNumberL: TLabel;
    UsageTextST: TStaticText;
    SourceCodeTextL: TLabel;
    GitHubLinkL: TLabel;
    procedure GitHubLinkLClick(Sender: TObject);
    procedure ReadmeLinkLClick(Sender: TObject);
  private

  public

  end;

var
  AboutFormF: TAboutFormF;

implementation

{$R *.lfm}

{ TAboutFormF }

procedure TAboutFormF.ReadmeLinkLClick(Sender: TObject);
begin
 OpenURL('https://github.com/JobstTechnologies/JT-Driver-Sensing#readme');
end;

procedure TAboutFormF.GitHubLinkLClick(Sender: TObject);
begin
 OpenURL('https://github.com/JobstTechnologies/JT-Driver-Sensing');
end;

end.

