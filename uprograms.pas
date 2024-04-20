unit uprograms;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Process, Forms, BGRABitmap, BGRABitmapTypes,
  {$ifdef DEBUG}
  LazLoggerBase
  {$else}
  LazLoggerDummy
  {$endif};

type

  { TProgram }

  TProgram = class(TObject)
  public
    Name: string;
    path: string;
    icon: TBGRABitmap;
    extensions: string;
    category: string;
    constructor Create(aName: string; aPath: string; aIcon: string;
      aExtensions: string; aCategory: string);
    destructor Destroy; override;
    procedure Run(aFile: string);
  end;

  TProgramList = specialize TObjectList<TProgram>;

var
  programs: TProgramList;

implementation

{ TProgram }

constructor TProgram.Create(aName: string; aPath: string; aIcon: string;
  aExtensions: string; aCategory: string);
begin
  Name := aName;
  path := aPath;
  if FileExists(aIcon) then
    icon := TBGRABitmap.Create(aIcon)
  else
    icon := TBGRABitmap.Create(32, 32, BGRAPixelTransparent);
  extensions := aExtensions;
  category := aCategory;
end;

destructor TProgram.Destroy;
begin
  icon.Free;
  inherited Destroy;
end;

procedure TProgram.Run(aFile: string);
var
  outStr: string;
begin
  Application.Minimize;
  RunCommandIndir(ExtractFilePath(path), path, aFile, outStr, [],
    TShowWindowOptions.swoShowNormal);
  Application.Restore;
end;

procedure AddProgram(aName: string; aPath: string; aIcon: string);
var
  p, i: string;
begin
  p := ExtractFilePath(ParamStr(0)) + 'emu' + PathDelim + aPath;
  i := ExtractFilePath(ParamStr(0)) + 'resources\app\icons\' + aIcon;
  if FileExists(p) then
  begin
    debugln(p);
    programs.Add(TProgram.Create(aName, p, i, '', ''));
  end;
end;

initialization
  programs := TProgramList.Create(True);
  AddProgram('CITRA', 'citra\citra-qt.exe', 'citra.png');
  AddProgram('DESMUME', 'desmume\desmume.exe', 'desmume.png');
  AddProgram('MYZOOM', 'no$gba\myzoom.exe', 'nocashgba.png');
  AddProgram('NGZOOM', 'no$gba\ngzoom.exe', 'nocashgba.png');
  AddProgram('NO$GBA 2.6A', 'no$gba\no$gba.exe', 'nocashgba.png');
  AddProgram('NO$GBA2X', 'no$gba\no$gba2x.exe', 'nocashgba.png');
  AddProgram('RYUJINX', 'ryujinx\ryujinx.exe', 'ryujinx.png');

finalization
  programs.Free;

end.
