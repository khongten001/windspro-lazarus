unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ustyles, BCPanel, BCMaterialEdit, BGRAVirtualScreen, BGRABitmap, BCTypes,
  BCListBox, BCButton, BGRABitmapTypes, LCLType, Generics.Collections,
  uprograms,
  {$ifdef DEBUG}
  LazLoggerBase
  {$else}
  LazLoggerDummy
  {$endif}, Types;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bcbOpen: TBCButton;
    bcbClose: TBCButton;
    bcmeSearch: TBCMaterialEdit;
    bcpFile: TBCPanel;
    bcpTop: TBCPanel;
    bcpLeft: TBCPanel;
    odOpen: TOpenDialog;
    vsBackgroundImage: TBGRAVirtualScreen;
    lbPrograms: TListBox;
    procedure bcbCloseClick(Sender: TObject);
    procedure bcbOpenClick(Sender: TObject);
    procedure bcmeSearchChange(Sender: TObject);
    procedure vsBackgroundImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbProgramsDblClick(Sender: TObject);
    procedure lbProgramsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    backgroundImage: TBGRABitmap;
    procedure SearchAndFill(aSearch: string);
    procedure SearchKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public

  end;

resourcestring
  SEARCH = 'SEARCH';

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  styleForm(Self);
  bcmeSearch.Align := alTop;
  bcmeSearch.Title.Caption := SEARCH;
  bcmeSearch.Edit.OnKeyDown := @SearchKeyDown;
  backgroundImage := TBGRABitmap.Create;
  backgroundImage.LoadFromResource('BACKGROUND');
  SearchAndFill('');
  if ParamCount > 0 then
  begin
    bcpFile.Caption := ParamStr(1);
    bcpFile.Visible := True;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  backgroundImage.Free;
end;

procedure TfrmMain.lbProgramsDblClick(Sender: TObject);
begin
  TProgram(lbPrograms.Items.Objects[lbPrograms.ItemIndex]).Run(bcpFile.Caption);
end;

procedure TfrmMain.lbProgramsDrawItem(Control: TWinControl; Index: integer;
  ARect: TRect; State: TOwnerDrawState);
var
  bmp: TBGRABitmap;
  textRect: TRect;
begin
  if odSelected in State then
    bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, ACCENT_COLOR)
  else
    bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, BACKGROUND_COLOR);
  bmp.DrawHorizLine(0, bmp.Height - 1, bmp.Width, BORDER_COLOR);
  textRect := ARect;
  textRect.Left := TListBox(Control).ItemHeight;
  bmp.PutImage((TListBox(Control).ItemHeight - 32) div 2, (TListBox(Control).ItemHeight - 32) div 2, TProgram(TListBox(Control).Items.Objects[Index]).icon, dmDrawWithTransparency);
  bmp.Draw(TListBox(Control).Canvas, ARect.Left, ARect.Top, True);
  bmp.Free;
  TListBox(Control).Canvas.Font.Color := TEXT_COLOR;
  TListBox(Control).Canvas.TextRect(textRect, textRect.Left, textRect.Top +
    (TListBox(Control).ItemHeight - TListBox(Control).Canvas.GetTextHeight(
    TListBox(Control).Items[Index])) div 2,
    TListBox(Control).Items[Index]);
end;

procedure TfrmMain.SearchAndFill(aSearch: string);
var
  contains: boolean;
  i, j: integer;
  arr: TStringArray;
begin
  lbPrograms.Clear;
  arr := aSearch.ToUpper.Split(' ', TStringSplitOptions.ExcludeEmpty);

  for j := 0 to programs.Count - 1 do
  begin
    contains := True;
    for i := 0 to Length(arr) - 1 do
    begin
      if programs[j].Name.Contains(arr[i]) then
        continue
      else
        contains := False;
    end;
    if (contains) then
    begin
      lbPrograms.AddItem(programs[j].Name, programs[j]);
    end;
  end;

  if (lbPrograms.Count > 0) then
    lbPrograms.ItemIndex := 0;
end;

procedure TfrmMain.SearchKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
  begin
    if (lbPrograms.ItemIndex + 1 < lbPrograms.Count) then
    begin
      lbPrograms.ItemIndex := lbPrograms.ItemIndex + 1;
    end;
    Key := 0;
  end;
  if (Key = VK_UP) then
  begin
    if (lbPrograms.ItemIndex - 1 >= 0) then
    begin
      lbPrograms.ItemIndex := lbPrograms.ItemIndex - 1;
    end;
    Key := 0;
  end;
  if (Key = VK_RETURN) then
  begin
    if (lbPrograms.ItemIndex >= 0) and (lbPrograms.ItemIndex < lbPrograms.Count) then
      lbProgramsDblClick(nil);
  end;
end;

procedure TfrmMain.vsBackgroundImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.StretchPutImageProportionally(Rect(0, 0, Bitmap.Width, Bitmap.Height),
    taCenter, tlCenter, backgroundImage, dmSet, 255, True);
end;

procedure TfrmMain.bcmeSearchChange(Sender: TObject);
begin
  debugln(bcmeSearch.Edit.Text);
  SearchAndFill(bcmeSearch.Edit.Text);
end;

procedure TfrmMain.bcbOpenClick(Sender: TObject);
begin
  if (odOpen.Execute) then
  begin
    bcpFile.Caption := odOpen.FileName;
    bcpFile.Visible := True;
    SearchAndFill(bcmeSearch.Edit.Text);
  end;
end;

procedure TfrmMain.bcbCloseClick(Sender: TObject);
begin
  bcpFile.Caption := '';
  bcpFile.Visible := False;
  SearchAndFill(bcmeSearch.Edit.Text);
end;

end.
