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
    path: string;
    Name: string;
    category: string;
    version: string;
    executable: string;
    folder: string;
    icon: string;
    extensions: string;
    bitmap: TBGRABitmap;
    constructor Create;
    destructor Destroy; override;
    procedure Run(aFile: string);
    function GetSelf: TProgram;
  end;

  TProgramList = specialize TObjectList<TProgram>;

var
  programs: TProgramList;

implementation

const
  Atari5200 = '(a52)';
  Atari7800 = '(a78)';
  AtariJaguar = '(j64)';
  AtariLynx = '(lnx)';
  BandaiWonderSwanColor = '(wsc)';
  BandaiWonderSwan = '(ws)';
  ColecoVision = '(col)';
  Commodore64PP = '(g64)(nib)';
  Commodore64Tapes = '(tap)';
  Commodore64 = '(102)(bin)(crt)(u2)(u3)(u4)(u5)(ua2)(ub3)';
  CommodorePlus4 = '(bin)(u24)(u25)(u4)';
  CommodoreVIC20 = '(20)(40)(60)(70)(80)(a0)(b0)(bin)(ub3)(ud7)(ue1)';
  MSX2 = '(rom)';
  MSX = '(rom)';
  NECPCEngine = '(pce)';
  NECSuperGrafx = '(pce)(sgx)';
  NintendoFDS = '(bin)(fds)';
  NintendoGBA = '(agb)(bin)(elf)(gba)(mb)';
  NintendoGBC = '(cgb)(gbc)';
  NintendoGB = '(gb)(gmb)(sgb)';
  Nintendo3DS = '(3ds)(3dsx)(cci)(cxi)(csu)';
  Nintendo64 = '(n64)(v64)(z64)(rom)(jap)(pal)(usa)';
  NintendoDS = '(dsi)(nds)(pme)(srl)';
  NintendoNES = '(nes)(nsf)(unf)(bin)';
  NintendoGameCube = '(iso)';
  NintendoWii = '(iso)';
  NintendoPokemonMini = '(min)';
  NintendoSNES = '(bin)(smc)(fig)(sfc)(swc)';
  NintendoVirtualBoy = '(vb)(vboy)';
  Sega32X = '(32x)(bin)';
  SegaDreamcast = '(iso)';
  SegaGameGear = '(gg)(sms)';
  SegaMasterSystem = '(sms)(sg)(sc)(mv)';
  SegaMegaDrive = '(md)(smd)(gen)';
  SegaPICO = '(md)';
  SegaSaturn = '(iso)';
  SegaSG1000 = '(sc)(sg)';
  NeoGeoPocketColor = '(ngc)(ngp)';
  NeoGeoPocket = '(ngp)';
  SonyPlayStation2 = '(iso)';
  SonyPlayStation3 = '(iso)';
  //SonyPlayStation4 = '(iso)';
  SonyPlayStationPortable = '(iso)';
  SonyPlayStation = '(iso)(bin)';

var
  i: integer;

  { TProgram }

function TProgram.GetSelf: TProgram;
begin
  Result := Self;
end;

constructor TProgram.Create();
begin
  bitmap := TBGRABitmap.Create(32, 32);
end;

destructor TProgram.Destroy;
begin
  bitmap.Free;
  inherited Destroy;
end;

procedure TProgram.Run(aFile: string);
var
  p: TProcess;
begin
  p := DefaultTProcess.Create(nil);
  p.Executable := path;
  p.CurrentDirectory := ExtractFilePath(path);
  if aFile <> '' then
    p.Parameters.Add(aFile);
  try
    p.Execute;
  finally
    p.Free;
  end;
end;

initialization
  programs := TProgramList.Create(True);
  with TProgram.Create do
  begin
    Name := 'DeSmuME';
    category := 'Nintendo DS (NDS)';
    version := '2020.01.21';
    executable := 'desmume.exe';
    folder := 'desmume';
    icon := 'desmume.png';
    extensions := '(dsi)(nds)(pme)(srl)';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'MyZoom';
    category := 'Nintendo DS (NDS)';
    version := '1.8.36';
    executable := 'myzoom.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'NGZoom';
    category := 'Nintendo DS (NDS)';
    version := '1.0';
    executable := 'ngzoom.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'No$gba 2.6a';
    category := 'Nintendo DS (NDS), Game Boy Advance (GBA)';
    version := '2.6a';
    executable := 'no$gba.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '(dsi)(nds)(pme)(srl)(agb)(bin)(elf)(gba)(mb)';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'No$gba2X';
    category := 'Nintendo DS (NDS)';
    version := '1.0';
    executable := 'no$gba2x.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'No$gba';
    category := 'Nintendo DS (NDS), Game Boy Advance (GBA)';
    version := '3.0';
    executable := 'no$gba.exe';
    folder := 'no$gba_';
    icon := 'nocashgba.png';
    extensions := '(dsi)(nds)(pme)(srl)(agb)(bin)(elf)(gba)(mb)';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'No$Mooz';
    category := 'Nintendo DS (NDS)';
    version := '1.0';
    executable := 'no$mooz.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'No$Zoomer';
    category := 'Nintendo DS (NDS), Game Boy Advance (GBA)';
    version := '2.3.0.2';
    executable := 'no$zoomer.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '(dsi)(nds)(pme)(srl)(agb)(bin)(elf)(gba)(mb)';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'NOZ';
    category := 'Nintendo DS (NDS)';
    version := '2.3';
    executable := 'noz.exe';
    folder := 'no$gba';
    icon := 'nocashgba.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'VBA Link';
    category := 'Game Boy (GB), Game Boy Color (GBC), Game Boy Advance (GBA)';
    version := '1.8.0';
    executable := 'vbalink.exe';
    folder := 'vbalink';
    icon := 'vbam.png';
    extensions := '(gb)(gmb)(sgb)(cgb)(gbc)(agb)(bin)(elf)(gba)(mb)';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'VBA-M';
    category := 'Game Boy (GB), Game Boy Color (GBC), Game Boy Advance (GBA)';
    version := '2.0';
    executable := 'visualboyadvance-m.exe';
    folder := 'vbam';
    icon := 'vbam.png';
    extensions := '(gb)(gmb)(sgb)(cgb)(gbc)(agb)(bin)(elf)(gba)(mb)';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Atari800Win PLus';
    category := 'Atari 5200';
    version := '4.1';
    executable := 'Atari800Win.exe';
    folder := 'Atari800Win_PLus_41';
    icon := 'atari800winplus.png';
    extensions := Atari5200;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'BizHawk';
    category := 'Multiplatform, Nintendo, Sega, Atari';
    version := '2019.04.09';
    executable := 'EmuHawk.exe';
    folder := 'BizHawk_153';
    icon := 'bizhawk.png';
    extensions := SegaMegaDrive + Sega32X + NintendoNES + NintendoSNES +
      NintendoGB + NintendoGBC + NintendoGBA;

    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'blueMSX';
    category := 'Multiplatform, Coleco, Sega, MSX';
    version := '2.8.3';
    executable := 'blueMSX.exe';
    folder := 'blueMSX_282';
    icon := 'bluemsx.png';
    extensions := ColecoVision + MSX + MSX2 + SegaSG1000;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'CCS64';
    category := 'Commodore 64 (C64)';
    version := '3.9.2';
    executable := 'CCS64.exe';
    folder := 'CCS64_39';
    icon := 'ccs64.png';
    extensions := Commodore64 + Commodore64PP + Commodore64Tapes;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Cemu';
    category := 'Nintendo Wii U (Wii U)';
    version := '1.16.1';
    executable := 'Cemu.exe';
    folder := 'cemu';
    icon := 'cemu.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Cxbx Reloaded';
    category := 'Microsoft XBOX';
    version := '2020.01.02';
    executable := 'Cxbx.exe';
    folder := 'Cxbx';
    icon := 'cxbx.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Decaf';
    category := 'Nintendo Wii U (Wii U)';
    version := '2019.09.28';
    executable := 'decaf-qt.exe';
    folder := 'decaf';
    icon := 'decaf.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Demul';
    category := 'Sega Dreamcast (DC)';
    version := '0.7a';
    executable := 'demul.exe';
    folder := 'Demul_0582';
    icon := 'demul.png';
    extensions := SegaDreamcast;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'DeSmuME X432R 32-bit';
    category := 'Nintendo DS (NDS)';
    version := '2015.04.19';
    executable := 'DeSmuME_X432R_x86.exe';
    folder := 'DeSmuME_x432R';
    icon := 'desmume.png';
    extensions := NintendoDS;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'DeSmuME X432R 64-bit';
    category := 'Nintendo DS (NDS)';
    version := '2015.04.19';
    executable := 'DeSmuME_X432R_x64.exe';
    folder := 'DeSmuME_x432R';
    icon := 'desmume.png';
    extensions := NintendoDS;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Dolphin (Ishiiruka) 32-bit';
    category := 'Nintendo Wii (Wii), Nintendo GameCube (GC)';
    version := '2015.02.10';
    executable := 'Dolphin.exe';
    folder := 'IshiirukaDolphin';
    icon := 'dolphinishiiruka.png';
    extensions := NintendoGameCube + NintendoWii;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Dolphin (Ishiiruka) 64-bit';
    category := 'Nintendo Wii (Wii), Nintendo GameCube (GC)';
    version := '2019.12.21';
    executable := 'Dolphin.exe';
    folder := 'IshiirukaDolphin64';
    icon := 'dolphinishiiruka.png';
    extensions := NintendoGameCube + NintendoWii;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Dolphin 32-bit';
    category := 'Nintendo Wii (Wii), Nintendo GameCube (GC)';
    version := '4.0.2';
    executable := 'Dolphin.exe';
    folder := 'Dolphin_402_32-bit';
    icon := 'dolphin.png';
    extensions := NintendoGameCube + NintendoWii;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Dolphin 64-bit';
    category := 'Nintendo Wii (Wii), Nintendo GameCube (GC)';
    version := '5.0-11428';
    executable := 'Dolphin.exe';
    folder := 'Dolphin_402_64-bit';
    icon := 'dolphin.png';
    extensions := NintendoGameCube + NintendoWii;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'ePSXe';
    category := 'Sony PlayStation (PSX)';
    version := '2.0.5';
    executable := 'ePSXe.exe';
    folder := 'ePSXe_190';
    icon := 'epsxe.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'FCEUX';
    category := 'Nintendo (NES)';
    version := '2020.01.04';
    executable := 'fceux64.exe';
    folder := 'FCEUX';
    icon := 'fceux.png';
    extensions := NintendoNES + NintendoFDS;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Final Burn Alpha rr';
    category := 'Multiplatform';
    version := '0.0.7';
    executable := 'fba.exe';
    folder := 'Final_Burn_Alpha_rr_007';
    icon := 'finalburnalpha.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Gens GS';
    category := 'Sega Mega Drive (SMD), Sega 32X (32X)';
    version := 'r7';
    executable := 'gens.exe';
    folder := 'Gens_gs_r7';
    icon := 'gensgs.png';
    extensions := Sega32X + SegaMegaDrive;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Gens rr';
    category := 'Sega Mega Drive (SMD), Sega 32X (32X)';
    version := '11 r352';
    executable := 'Gens.exe';
    folder := 'Gens_rr_11_r341';
    icon := 'gens.png';
    extensions := Sega32X + SegaMegaDrive;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Handy';
    category := 'Atari Lynx';
    version := '0.95';
    executable := 'handy.exe';
    folder := 'Handy_095';
    icon := 'handy.png';
    extensions := AtariLynx;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Hourglass';
    category := 'Win32';
    version := 'r81';
    executable := 'hourglass.exe';
    folder := 'Hourglass_r81';
    icon := 'hourglass.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'JPC-rr';
    category := 'DOS';
    version := 'r11.7';
    executable := 'start-jpcrr.bat';
    folder := 'JPC_rr_r117';
    icon := 'jpcrr.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Kega Fusion';
    category := 'Sega Mega Drive (SMD), Sega 32X (32X)';
    version := '3.64';
    executable := 'Fusion.exe';
    folder := 'Kega_Fusion_364';
    icon := 'kegafusion.png';
    extensions := Sega32X + SegaGameGear + SegaMasterSystem +
      SegaMegaDrive + SegaPICO + SegaSG1000;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'lsnes rr2';
    category := 'Super Nintendo (SNES)';
    version := 'beta 23';
    executable := 'lsnes-bsnes.exe';
    folder := 'lsnes_rr2_beta15';
    icon := 'default.png';
    extensions := NintendoSNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Mednafen (MedGui)';
    category := 'Multiplatform, Atari, Nintendo, Sega, Sony, SNK, Bandai, NEC';
    version := '0.9.36.5';
    executable := 'MedGui.exe';
    folder := 'mednafen_09321_wip';
    icon := 'medgui.png';
    extensions := AtariLynx + BandaiWonderSwan + NintendoGB +
      NintendoGBC + NintendoGBA + NintendoNES + NintendoFDS +
      NintendoVirtualBoy + NECPCEngine + NECSuperGrafx + SegaGameGear +
      SegaMegaDrive + SegaMasterSystem + SonyPlayStation + NeoGeoPocket +
      NeoGeoPocketColor;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Mednafen-rr';
    category := 'Multiplatform, Atari, Nintendo, Sega, Sony, SNK, Bandai, NEC';
    version := '1.1';
    executable := 'Mednafen-Front.exe';
    folder := 'Mednafen_rr_11';
    icon := 'mednafen.png';
    extensions := AtariLynx + BandaiWonderSwan + NintendoGB +
      NintendoGBC + NintendoGBA + NintendoNES + NintendoFDS +
      NintendoVirtualBoy + NECPCEngine + NECSuperGrafx + SegaGameGear +
      SegaMegaDrive + SegaMasterSystem + SonyPlayStation + NeoGeoPocket +
      NeoGeoPocketColor;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Mupen64-rr';
    category := 'Nintendo 64 (N64)';
    version := '4.6.2';
    executable := 'mupen64.exe';
    folder := 'Mupen64_rr_462';
    icon := 'mupen64.png';
    extensions := Nintendo64;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Mupen64Plus';
    category := 'Nintendo 64 (N64)';
    version := '2.5';
    executable := 'mupen64plus-gui.exe';
    folder := 'Mupen64Plus_20';
    icon := 'mupen64plus.png';
    extensions := Nintendo64;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'no$2k6';
    category := 'Atari 2600 (A2600)';
    version := '1.1';
    executable := 'NO$2K6.exe';
    folder := 'no$2k6_11';
    icon := 'nocash2k6.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'no$c64';
    category := 'Commodore 64 (C64)';
    version := '1.1';
    executable := 'NO$C64.exe';
    folder := 'no$c64_11';
    icon := 'nocashc64.png';
    extensions := Commodore64 + Commodore64Tapes + Commodore64PP;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'no$msx';
    category := 'MSX';
    version := '1.5';
    executable := 'NO$MSX.EXE';
    folder := 'no$msx_15';
    icon := 'nocashmsx.png';
    extensions := MSX + MSX2;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'no$nes';
    category := 'Nintendo (NES)';
    version := '1.1';
    executable := 'NO$NES.EXE';
    folder := 'no$nes_11';
    icon := 'nocashnes.png';
    extensions := NintendoNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'no$psx';
    category := 'Sony PlayStation (PSX)';
    version := '1.9';
    executable := 'NO$PSX.EXE';
    folder := 'no$psx_18';
    icon := 'nocashpsx.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'no$sns';
    category := 'Super Nintendo (SNES)';
    version := '1.5';
    executable := 'NO$SNS.EXE';
    folder := 'no$sns_15';
    icon := 'nocashsnes.png';
    extensions := NintendoSNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'nullDC';
    category := 'Sega Dreamcast (DC)';
    version := 'r150';
    executable := 'nullDC_Win32_Release.exe';
    folder := 'nullDC_150';
    icon := 'nulldc.png';
    extensions := SegaDreamcast;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Ootake';
    category := 'NEC';
    version := '2.80';
    executable := 'Ootake.exe';
    folder := 'Ootake_275';
    icon := 'ootake.png';
    extensions := NECPCEngine + NECSuperGrafx;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Open MSX 32-bit';
    category := 'MSX';
    version := '0.14.0';
    executable := 'openmsx.exe';
    folder := 'Open_MSX_0100_32-bit';
    icon := 'openmsx.png';
    extensions := MSX;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Open MSX 64-bit';
    category := 'MSX';
    version := '0.14.0';
    executable := 'openmsx.exe';
    folder := 'Open_MSX_0100_64-bit';
    icon := 'openmsx.png';
    extensions := MSX;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Oswan';
    category := 'Bandai';
    version := '1.7.3';
    executable := 'Oswan.exe';
    folder := 'Oswan_173';
    icon := 'oswan.png';
    extensions := BandaiWonderSwan + BandaiWonderSwanColor;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PCSX-R';
    category := 'Sony PlayStation (PSX)';
    version := '2018.04.28';
    executable := 'pcsxr-pgxp.exe';
    folder := 'PCSXR_1993';
    icon := 'pcsxr.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PCSX2';
    category := 'Sony PlayStation 2 (PS2)';
    version := 'v1.5.0-dev-3341-g80e3e00f9';
    executable := 'pcsx2-qt.exe';
    folder := 'PCSX2_121';
    icon := 'pcsx2.png';
    extensions := SonyPlayStation2;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Play!';
    category := 'Sony PlayStation 2 (PS2)';
    version := '2018.06.03';
    executable := 'Play.exe';
    folder := 'Play_32';
    icon := 'play.png';
    extensions := SonyPlayStation2;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PokeMini';
    category := 'Pokémon Mini';
    version := '0.60';
    executable := 'PokeMini.exe';
    folder := 'PokeMini_054';
    icon := 'pokemini.png';
    extensions := NintendoPokemonMini;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PPSSPP 32-bit';
    category := 'Sony PlayStation Portable (PSP)';
    version := 'v1.9.3-171-g347433910';
    executable := 'PPSSPPWindows.exe';
    folder := 'PPSSPP';
    icon := 'ppsspp.png';
    extensions := SonyPlayStationPortable;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PPSSPP 64-bit';
    category := 'Sony PlayStation Portable (PSP)';
    version := 'v1.9.3-171-g347433910';
    executable := 'PPSSPPWindows64.exe';
    folder := 'PPSSPP';
    icon := 'ppsspp.png';
    extensions := SonyPlayStationPortable;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Project64';
    category := 'Nintendo 64 (N64)';
    version := 'v2.4.0-1100-g617db96';
    executable := 'Project64.exe';
    folder := 'Project64_21';
    icon := 'project64.png';
    extensions := Nintendo64;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Project64K7E';
    category := 'Nintendo 64 (N64)';
    version := '1.3.1';
    executable := 'Project64K7E.exe';
    folder := 'Project64K7E_131';
    icon := 'project64k7e.png';
    extensions := Nintendo64;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'ProSystem';
    category := 'Atari 7800 (A7800)';
    version := '1.3';
    executable := 'ProSystem.exe';
    folder := 'ProSystem_13';
    icon := 'prosystem.png';
    extensions := Atari7800;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'pSX fin';
    category := 'Sony PlayStation (PSX)';
    version := '1.13';
    executable := 'psxfin.exe';
    folder := 'pSX_1_13';
    icon := 'psxfin.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PSXjin';
    category := 'Sony PlayStation (PSX)';
    version := '2.0.2';
    executable := 'psxjin.exe';
    folder := 'PSXjin_202';
    icon := 'psxjin.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Red Dragon';
    category := 'Nintendo Virtual Boy (VB)';
    version := '0.38';
    executable := 'rdragon.exe';
    folder := 'Red_Dragon_038';
    icon := 'reddragon.png';
    extensions := NintendoVirtualBoy;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'rpcs3';
    category := 'Sony PlayStation 3 (PS3)';
    version := '0.0.8-9326';
    executable := 'rpcs3.exe';
    folder := 'rpcs3';
    icon := 'rpcs3.png';
    extensions := SonyPlayStation3;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Snes9x 32-bit';
    category := 'Super Nintendo (SNES)';
    version := '2019.04.11';
    executable := 'snes9x.exe';
    folder := 'Snes9x_153';
    icon := 'snes9x.png';
    extensions := NintendoSNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Snes9x 64-bit';
    category := 'Super Nintendo (SNES)';
    version := '2019.04.11';
    executable := 'snes9x-x64.exe';
    folder := 'Snes9x_153_(64-bit)';
    icon := 'snes9x.png';
    extensions := NintendoSNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'SSF';
    category := 'Sega Saturn';
    version := '2018.01.07';
    executable := 'SSF.exe';
    folder := 'SSF_012_beta_R4';
    icon := 'default.png';
    extensions := SegaSaturn;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'VBA-rr';
    category := 'Nintendo Game Boy (GB) / Color (GBC) / Advance (GBA)';
    version := 'v24m r480';
    executable := 'vba-v24m-svn480.exe';
    folder := 'VBA-rr_v24m_r480';
    icon := 'vbarr.png';
    extensions := NintendoGB + NintendoGBC + NintendoGBA;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'VBjin';
    category := 'Nintendo Virtual Boy (VB)';
    version := 'r61';
    executable := 'vbjin.exe';
    folder := 'VBjin_r61';
    icon := 'vbjin.png';
    extensions := NintendoVirtualBoy;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Virtual Jaguar';
    category := 'Atari Jaguar';
    version := '2.1.2';
    executable := 'virtualjaguar.exe';
    folder := 'Virtual_Jaguar_210';
    icon := 'virtualjaguar.png';
    extensions := AtariJaguar;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'VirtuaNES';
    category := 'Nintendo (NES)';
    version := '0.9.7e';
    executable := 'VirtuaNES.exe';
    folder := 'VirtuaNES_097e';
    icon := 'virtuanes.png';
    extensions := NintendoNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE C128';
    category := 'Commodore 128 (C128)';
    version := '3.2';
    executable := 'x128.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := Commodore64 + Commodore64PP + Commodore64Tapes;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE C64';
    category := 'Commodore 64 (C64)';
    version := '3.2';
    executable := 'x64.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := Commodore64 + Commodore64PP + Commodore64Tapes;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE C64 DTV';
    category := 'Commodore 64 Direct-to-TV (C64)';
    version := '3.2';
    executable := 'x64dtv.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := Commodore64 + Commodore64PP + Commodore64Tapes;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE CBM-II';
    category := 'Commodore CBM-II (CBM-II)';
    version := '3.2';
    executable := 'xcbm2.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE PET';
    category := 'Commodore PET (PET)';
    version := '3.2';
    executable := 'xpet.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE PLUS4';
    category := 'Commodore PLUS4 (PLUS4)';
    version := '3.2';
    executable := 'xplus4.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := CommodorePlus4;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'WinVICE VIC20';
    category := 'Commodore VIC20 (VIC20)';
    version := '3.2';
    executable := 'xvic.exe';
    folder := 'WinVICE_22';
    icon := 'winvice.png';
    extensions := CommodoreVIC20;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Xenia';
    category := 'Microsoft XBOX 360';
    version := '2019.12.05';
    executable := 'xenia.exe';
    folder := 'xenia';
    icon := 'xenia.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Xenia D3D';
    category := 'Microsoft XBOX 360';
    version := '2018.12.29';
    executable := 'xenia.exe';
    folder := 'xeniad3d';
    icon := 'xenia.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'ZSNES';
    category := 'Super Nintendo (SNES)';
    version := '1.5.1';
    executable := 'zsnesw.exe';
    folder := 'ZSNES_151';
    icon := 'zsnes.png';
    extensions := NintendoSNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PCSX Redux';
    category := 'Sony PlayStation (PSX)';
    version := '2022/03/30';
    executable := 'pcsx-redux.exe';
    folder := 'PCSXRedux';
    icon := 'pcsxredux.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'mGBA';
    category := 'Game Boy Advance (GBA)';
    version := '2022/03/31';
    executable := 'mGBA.exe';
    folder := 'mGBA';
    icon := 'mgba.png';
    extensions := NintendoGBA;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Flycast';
    category := 'Sega Dreamcast (DC)';
    version := '2022/04/05';
    executable := 'flycast.exe';
    folder := 'Flycast';
    icon := 'Flycast.png';
    extensions := SegaDreamcast;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Ares';
    category := 'Multiplatform, Nintendo, Sega';
    version := '2022/05/05';
    executable := 'ares.exe';
    folder := 'Ares';
    icon := 'ares.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'Nestopia';
    category := 'Nintendo';
    version := '2022/07/28';
    executable := 'nestopia.exe';
    folder := 'Nestopia';
    icon := 'nestopia.png';
    extensions := NintendoNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'melonDS';
    category := 'Nintendo DS (NDS)';
    version := '2022/08/11';
    executable := 'melonDS.exe';
    folder := 'melonds';
    icon := 'melonds.png';
    extensions := NintendoNES;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'duckstation';
    category := 'Sony PlayStation (PSX)';
    version := '2022/09/22';
    executable := 'duckstation-qt-x64-ReleaseLTCG.exe';
    folder := 'duckstation';
    icon := 'duckstation.png';
    extensions := SonyPlayStation;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'xemu';
    category := 'Microsoft XBOX';
    version := '2023/01/19';
    executable := 'xemu.exe';
    folder := 'xemu';
    icon := 'xemu.png';
    extensions := '';
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'PCSX2-clang';
    category := 'Sony PlayStation 2 (PS2)';
    version := 'v1.5.0-dev-3341-g80e3e00f9';
    executable := 'pcsx2-qtx64-avx2-clang.exe';
    folder := 'PCSX2_121';
    icon := 'pcsx2.png';
    extensions := SonyPlayStation2;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'lime 3DS';
    category := 'Nintendo 3DS (3DS)';
    version := '24-4-02';
    executable := 'lime-qt.exe';
    folder := 'lime3ds';
    icon := 'lime3ds.png';
    extensions := Nintendo3DS;
    programs.Add(GetSelf);
  end;
  with TProgram.Create do
  begin
    Name := 'suyu';
    category := 'Nintendo Switch';
    version := '1.0';
    executable := 'suyu.exe';
    folder := 'suyu';
    icon := 'suyu.png';
    extensions := '';
    programs.Add(GetSelf);
  end;

  for i := 0 to programs.Count - 1 do
  begin
    programs[i].path := ExtractFilePath(ParamStr(0)) + 'emu\' +
      programs[i].folder + PathDelim + programs[i].executable;
    programs[i].icon := ExtractFilePath(ParamStr(0)) + 'resources\app\icons\' +
      programs[i].icon;
    programs[i].bitmap.LoadFromFile(programs[i].icon);
  end;

  for i := programs.Count - 1 downto 0 do
  begin
    if not FileExists(programs[i].path) then
    begin
      programs.Remove(programs[i]);
    end;
  end;

finalization
  programs.Free;

end.
