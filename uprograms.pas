unit uprograms;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Process, Forms,
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
    constructor Create(aName: string; aPath: string);
    procedure Run(aFile: string);
  end;

  TProgramList = specialize TObjectList<TProgram>;

var
  programs: TProgramList;

implementation

{ TProgram }

constructor TProgram.Create(aName: string; aPath: string);
begin
  Name := aName;
  path := ExtractFilePath(ParamStr(0)) + 'emu' + PathDelim + aPath;
  debugln(path);
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

procedure AddProgram(aName: string; aPath: string);
begin
  programs.Add(TProgram.Create(aName, aPath));
end;

initialization
  programs := TProgramList.Create(True);
  AddProgram('DESMUME', 'desmume\desmume.exe');
  AddProgram('MYZOOM', 'no$gba\myzoom.exe');
  AddProgram('NGZOOM', 'no$gba\ngzoom.exe');
  AddProgram('NO$GBA 2.6A', 'no$gba\no$gba.exe');
  AddProgram('NO$GBA2X', 'no$gba\no$gba2x.exe');

finalization
  programs.Free;

end.
