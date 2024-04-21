unit uconfigstyles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustyles,
  BCPanel, BCButton, BGRABitmap, BGRABitmapTypes;

type

  { TfrmConfigStyles }

  TfrmConfigStyles = class(TForm)
    bcbDefaultDark: TBCButton;
    bcbDefaultLight: TBCButton;
    bcpTop: TBCPanel;
    bcpClient: TBCPanel;
    cbAccent: TColorButton;
    cbBorder2: TColorButton;
    cbText1: TColorButton;
    cbBorder1: TColorButton;
    cbBackground1: TColorButton;
    cbBackground2: TColorButton;
    cbText2: TColorButton;
    lblAccent: TLabel;
    lblBorder2: TLabel;
    lblText1: TLabel;
    lblBorder1: TLabel;
    lblBackground1: TLabel;
    lblBackground2: TLabel;
    lblText2: TLabel;
    procedure bcbDefaultDarkClick(Sender: TObject);
    procedure bcbDefaultLightClick(Sender: TObject);
    procedure cbAccentColorChanged(Sender: TObject);
    procedure cbBackground1ColorChanged(Sender: TObject);
    procedure cbBackground2ColorChanged(Sender: TObject);
    procedure cbBorder1ColorChanged(Sender: TObject);
    procedure cbBorder2ColorChanged(Sender: TObject);
    procedure cbText1ColorChanged(Sender: TObject);
    procedure cbText2ColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  frmConfigStyles: TfrmConfigStyles;

implementation

{$R *.lfm}

{ TfrmConfigStyles }

procedure TfrmConfigStyles.FormCreate(Sender: TObject);
begin

end;

procedure TfrmConfigStyles.cbBackground1ColorChanged(Sender: TObject);
begin
  BACKGROUND_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbAccentColorChanged(Sender: TObject);
begin
  ACCENT_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.bcbDefaultDarkClick(Sender: TObject);
begin
  defaultColors;
  cbAccent.ButtonColor := ACCENT_COLOR;
  cbBackground1.ButtonColor := BACKGROUND_COLOR;
  cbBackground2.ButtonColor := BACKGROUND_DARK_COLOR;
  cbBorder1.ButtonColor := BORDER_COLOR;
  cbBorder2.ButtonColor := BORDER_DARK_COLOR;
  cbText1.ButtonColor := TEXT_COLOR;
  cbText2.ButtonColor := TEXT_DARK_COLOR;
end;

procedure TfrmConfigStyles.bcbDefaultLightClick(Sender: TObject);
begin
  defaultLightColors;
  cbAccent.ButtonColor := ACCENT_COLOR;
  cbBackground1.ButtonColor := BACKGROUND_COLOR;
  cbBackground2.ButtonColor := BACKGROUND_DARK_COLOR;
  cbBorder1.ButtonColor := BORDER_COLOR;
  cbBorder2.ButtonColor := BORDER_DARK_COLOR;
  cbText1.ButtonColor := TEXT_COLOR;
  cbText2.ButtonColor := TEXT_DARK_COLOR;
end;

procedure TfrmConfigStyles.cbBackground2ColorChanged(Sender: TObject);
begin
  BACKGROUND_DARK_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbBorder1ColorChanged(Sender: TObject);
begin
  BORDER_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbBorder2ColorChanged(Sender: TObject);
begin
  BORDER_DARK_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbText1ColorChanged(Sender: TObject);
begin
  TEXT_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbText2ColorChanged(Sender: TObject);
begin
  TEXT_DARK_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.FormShow(Sender: TObject);
begin
  styleForm(Self);
end;

end.
