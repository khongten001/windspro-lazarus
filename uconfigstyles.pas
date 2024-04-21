unit uconfigstyles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ustyles,
  BCPanel, BCButton, BGRABitmap, BGRABitmapTypes;

type

  { TfrmConfigStyles }

  TfrmConfigStyles = class(TForm)
    bcbDefault: TBCButton;
    bcpTop: TBCPanel;
    bcpClient: TBCPanel;
    cbAccent: TColorButton;
    cbText: TColorButton;
    cbBorder: TColorButton;
    cbBackground1: TColorButton;
    cbBackground2: TColorButton;
    lblAccent: TLabel;
    lblText: TLabel;
    lblBorder: TLabel;
    lblBackground1: TLabel;
    lblBackground2: TLabel;
    procedure bcbDefaultClick(Sender: TObject);
    procedure cbAccentColorChanged(Sender: TObject);
    procedure cbBackground1ColorChanged(Sender: TObject);
    procedure cbBackground2ColorChanged(Sender: TObject);
    procedure cbBorderColorChanged(Sender: TObject);
    procedure cbTextColorChanged(Sender: TObject);
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

procedure TfrmConfigStyles.bcbDefaultClick(Sender: TObject);
begin
  defaultColors;
  cbAccent.ButtonColor := ACCENT_COLOR;
  cbBackground1.ButtonColor := BACKGROUND_COLOR;
  cbBackground2.ButtonColor := BACKGROUND_DARK_COLOR;
  cbBorder.ButtonColor := BORDER_COLOR;
  cbText.ButtonColor := TEXT_COLOR;
end;

procedure TfrmConfigStyles.cbBackground2ColorChanged(Sender: TObject);
begin
  BACKGROUND_DARK_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbBorderColorChanged(Sender: TObject);
begin
  BORDER_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.cbTextColorChanged(Sender: TObject);
begin
  TEXT_COLOR := TColorButton(Sender).ButtonColor;
  styleForm(Self);
end;

procedure TfrmConfigStyles.FormShow(Sender: TObject);
begin
  styleForm(Self);
end;

end.
