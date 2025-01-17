unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ustyles, BCPanel, BCMaterialEdit, BGRAVirtualScreen, BGRABitmap,
  BCListBox, BCButton, BGRABitmapTypes, LCLType, JSONPropStorage, FileUtil,
  uprograms, uconfigstyles,
  {$ifdef DEBUG}
  LazLoggerBase
  {$else}
  LazLoggerDummy
  {$endif}, Types, PropertyStorage;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bcbColors: TBCButton;
    bcbOpen: TBCButton;
    bcbClose: TBCButton;
    bcbBackground: TBCButton;
    bcmeSearch: TBCMaterialEdit;
    bcmeSearchFiles: TBCMaterialEdit;
    bcpSearch: TBCPanel;
    bvpSearchFiles: TBCPanel;
    bcpFilesList: TBCPanel;
    bcpProgramsList: TBCPanel;
    bcpFiles: TBCPanel;
    bcpFile: TBCPanel;
    bcpTop: TBCPanel;
    bcpLeft: TBCPanel;
    JSONPropStorage1: TJSONPropStorage;
    lbFiles: TListBox;
    odOpen: TOpenDialog;
    sdOpen: TSelectDirectoryDialog;
    vsBackgroundImage: TBGRAVirtualScreen;
    lbPrograms: TListBox;
    procedure bcbBackgroundClick(Sender: TObject);
    procedure bcbCloseClick(Sender: TObject);
    procedure bcbColorsClick(Sender: TObject);
    procedure bcbOpenClick(Sender: TObject);
    procedure bcmeSearchChange(Sender: TObject);
    procedure bcmeSearchFilesChange(Sender: TObject);
    procedure JSONPropStorage1StoredValues0Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues0Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues1Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues1Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues2Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues2Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues3Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues3Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues4Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues4Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues5Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues5Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues6Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues6Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues7Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues7Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues8Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure JSONPropStorage1StoredValues8Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure lbFilesDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure lbFilesSelectionChange(Sender: TObject; User: boolean);
    procedure vsBackgroundImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbProgramsDblClick(Sender: TObject);
    procedure lbProgramsDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    backgroundImageFileName: string;
    backgroundImage: TBGRABitmap;
    files: TStringList;
    procedure FillFiles();
    procedure SearchAndFillFiles(aSearch: string; aSelectDefault: boolean = True);
    procedure SearchAndFill(aSearch: string);
    procedure SearchFilesKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
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
  files := TStringList.Create;
  bcmeSearch.Align := alTop;
  bcmeSearch.Title.Caption := SEARCH;
  bcmeSearch.Edit.OnKeyDown := @SearchKeyDown;
  bcmeSearchFiles.Align := alTop;
  bcmeSearchFiles.Title.Caption := SEARCH;
  bcmeSearchFiles.Edit.OnKeyDown := @SearchFilesKeyDown;
  backgroundImageFileName := '';
  backgroundImage := TBGRABitmap.Create;
  backgroundImage.LoadFromResource('BACKGROUND');
  ForceDirectories(GetAppConfigDir(False));
  JSONPropStorage1.JSONFileName := GetAppConfigFile(False);
  JSONPropStorage1.Restore;
  if ParamCount > 0 then
  begin
    bcpFile.Caption := ParamStr(1);
    bcpFile.Visible := True;
  end;
  SearchAndFill('');
  styleForm(Self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  files.Free;
  backgroundImage.Free;
  JSONPropStorage1.Save;
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
  try
    if odSelected in State then
      bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, ACCENT_COLOR)
    else
      bmp := TBGRABitmap.Create(ARect.Width, ARect.Height, BACKGROUND_COLOR);
    try
      bmp.DrawHorizLine(0, bmp.Height - 1, bmp.Width, BORDER_COLOR);
      textRect := ARect;
      textRect.Left := TListBox(Control).ItemHeight;
      bmp.PutImage((TListBox(Control).ItemHeight - 32) div 2,
        (TListBox(Control).ItemHeight - 32) div 2,
        TProgram(TListBox(Control).Items.Objects[Index]).bitmap, dmDrawWithTransparency);
      bmp.Draw(TListBox(Control).Canvas, ARect.Left, ARect.Top, True);
      TListBox(Control).Canvas.Font.Color := TEXT_COLOR;
      TListBox(Control).Canvas.TextRect(textRect, textRect.Left,
        textRect.Top + (TListBox(Control).ItemHeight -
        TListBox(Control).Canvas.GetTextHeight(TListBox(Control).Items[Index])) div 2,
        UpperCase(TListBox(Control).Items[Index]));
    finally
      bmp.Free;
    end;
  except
    on e: Exception do
    begin

    end;
  end;
end;

procedure TfrmMain.FillFiles();
begin
  files.Clear;
  if bcpFiles.Caption <> '' then
    FindAllFiles(files, bcpFiles.Caption, '*.*', True);
end;

procedure TfrmMain.SearchAndFillFiles(aSearch: string; aSelectDefault: boolean);
var
  contains: boolean;
  i, j: integer;
  arr: TStringArray;
begin
  lbFiles.Clear;
  aSearch := bcmeSearchFiles.Edit.Text;
  arr := aSearch.ToUpper.Split(' ', TStringSplitOptions.ExcludeEmpty);

  for j := 0 to files.Count - 1 do
  begin
    contains := True;
    for i := 0 to Length(arr) - 1 do
    begin
      if files[j].ToUpper.Contains(arr[i]) then
        continue
      else
        contains := False;
    end;
    if (contains) then
    begin
      lbFiles.AddItem(files[j], nil);
    end;
  end;

  if aSelectDefault and (lbFiles.Count > 0) then
    lbFiles.ItemIndex := 0;
end;

procedure TfrmMain.SearchAndFill(aSearch: string);
var
  ext: string;
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
      if programs[j].Name.ToUpper.Contains(arr[i]) or
        programs[j].category.ToUpper.Contains(arr[i]) then
        continue
      else
        contains := False;
    end;

    if contains and (bcpFile.Caption <> '') then
    begin
      ext := ExtractFileExt(bcpFile.Caption);
      ext := '(' + ext.ToLower.Replace('.', '', [rfReplaceAll]) + ')';
      contains := programs[j].extensions.Contains(ext);
    end;

    if (contains) then
    begin
      lbPrograms.AddItem(programs[j].Name, programs[j]);
    end;
  end;

  if (lbPrograms.Count > 0) then
    lbPrograms.ItemIndex := 0;
end;

procedure TfrmMain.SearchFilesKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = VK_DOWN) then
  begin
    if (lbFiles.ItemIndex + 1 < lbFiles.Count) then
    begin
      lbFiles.ItemIndex := lbFiles.ItemIndex + 1;
    end;
    Key := 0;
  end;
  if (Key = VK_UP) then
  begin
    if (lbFiles.ItemIndex - 1 >= 0) then
    begin
      lbFiles.ItemIndex := lbFiles.ItemIndex - 1;
    end;
    Key := 0;
  end;
  if (Key = VK_RETURN) then
  begin
    bcmeSearch.Edit.SetFocus;
  end;
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

procedure TfrmMain.bcmeSearchFilesChange(Sender: TObject);
begin
  SearchAndFillFiles(bcmeSearchFiles.Edit.Text);
end;

procedure TfrmMain.JSONPropStorage1StoredValues0Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  bcpFiles.Caption := Value;
  FillFiles;
  SearchAndFillFiles(bcmeSearchFiles.Edit.Text, False);
end;

procedure TfrmMain.JSONPropStorage1StoredValues0Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := bcpFiles.Caption;
end;

procedure TfrmMain.JSONPropStorage1StoredValues1Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  backgroundImageFileName := Value;
  if FileExists(backgroundImageFileName) then
  begin
    backgroundImage.LoadFromFile(backgroundImageFileName);
    vsBackgroundImage.DiscardBitmap;
  end;
end;

procedure TfrmMain.JSONPropStorage1StoredValues1Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := backgroundImageFileName;
end;

procedure TfrmMain.JSONPropStorage1StoredValues2Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    BACKGROUND_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues2Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := BACKGROUND_COLOR.ToString;
end;

procedure TfrmMain.JSONPropStorage1StoredValues3Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    BACKGROUND_DARK_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues3Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := BACKGROUND_DARK_COLOR.ToString;
end;

procedure TfrmMain.JSONPropStorage1StoredValues4Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    ACCENT_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues4Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := ACCENT_COLOR.ToString;
end;

procedure TfrmMain.JSONPropStorage1StoredValues5Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    BORDER_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues5Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := BORDER_COLOR.ToString;
end;

procedure TfrmMain.JSONPropStorage1StoredValues6Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    TEXT_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues6Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := TEXT_COLOR.ToString;
end;

procedure TfrmMain.JSONPropStorage1StoredValues7Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    BORDER_DARK_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues7Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := BORDER_DARK_COLOR.ToString;
end;

procedure TfrmMain.JSONPropStorage1StoredValues8Restore(Sender: TStoredValue;
  var Value: TStoredType);
begin
  if Value <> '' then
    TEXT_DARK_COLOR.FromString(Value);
end;

procedure TfrmMain.JSONPropStorage1StoredValues8Save(Sender: TStoredValue;
  var Value: TStoredType);
begin
  Value := TEXT_DARK_COLOR.ToString;
end;

procedure TfrmMain.lbFilesDrawItem(Control: TWinControl; Index: integer;
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
  bmp.Draw(TListBox(Control).Canvas, ARect.Left, ARect.Top, True);
  bmp.Free;
  TListBox(Control).Canvas.Font.Color := TEXT_COLOR;
  textRect := ARect;
  textRect.Left := Scale96ToForm(8);
  TListBox(Control).Canvas.TextRect(textRect, textRect.Left, textRect.Top +
    (TListBox(Control).ItemHeight - TListBox(Control).Canvas.GetTextHeight(
    TListBox(Control).Items[Index])) div 2,
    ExtractFileName(TListBox(Control).Items[Index]));
end;

procedure TfrmMain.lbFilesSelectionChange(Sender: TObject; User: boolean);
begin
  bcpFile.Caption := TListBox(Sender).Items[TListBox(Sender).ItemIndex];
  bcpFile.Visible := True;
  SearchAndFill(bcmeSearch.Edit.Text);
end;

procedure TfrmMain.bcbOpenClick(Sender: TObject);
begin
  if (sdOpen.Execute) then
  begin
    bcpFiles.Caption := sdOpen.FileName;
    FillFiles;
    SearchAndFillFiles(bcmeSearchFiles.Edit.Text);
  end;
end;

procedure TfrmMain.bcbCloseClick(Sender: TObject);
begin
  bcpFile.Caption := '';
  bcpFile.Visible := False;
  SearchAndFill(bcmeSearch.Edit.Text);
end;

procedure TfrmMain.bcbColorsClick(Sender: TObject);
begin
  uconfigstyles.frmConfigStyles.cbAccent.ButtonColor := ACCENT_COLOR;
  uconfigstyles.frmConfigStyles.cbBackground1.ButtonColor := BACKGROUND_COLOR;
  uconfigstyles.frmConfigStyles.cbBackground2.ButtonColor := BACKGROUND_DARK_COLOR;
  uconfigstyles.frmConfigStyles.cbText1.ButtonColor := TEXT_COLOR;
  uconfigstyles.frmConfigStyles.cbText2.ButtonColor := TEXT_DARK_COLOR;
  uconfigstyles.frmConfigStyles.cbBorder1.ButtonColor := BORDER_COLOR;
  uconfigstyles.frmConfigStyles.cbBorder2.ButtonColor := BORDER_DARK_COLOR;
  uconfigstyles.frmConfigStyles.ShowModal;
  styleForm(Self);
end;

procedure TfrmMain.bcbBackgroundClick(Sender: TObject);
begin
  if (odOpen.Execute) then
  begin
    backgroundImageFileName := odOpen.FileName;
    backgroundImage.LoadFromFile(backgroundImageFileName);
    vsBackgroundImage.DiscardBitmap;
  end;
end;

end.
