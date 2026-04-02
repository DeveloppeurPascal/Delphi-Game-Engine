(* C2PP
  ***************************************************************************

  Delphi Game Engine
  Copyright (c) 2021-2026 Patrick PREMARTIN

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.

  ***************************************************************************

  Delphi Game Engine contains libraries and components to use in VCL or
  FireMonkey game (or classic) projects.

  If you want to play sounds or musics, use game controllers, pilot your
  user interface with the keyboard or a game controller, it's the good place.

  ***************************************************************************

  Author(s) :
  Patrick PREMARTIN

  Site :
  https://delphigameengine.developpeur-pascal.fr

  Project site :
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine

  ***************************************************************************
  File last update : 2025-05-25T17:36:33.882+02:00
  Signature : 7b2470cd56e0c9aaf47301b5a5a6dac69d9b8335
  ***************************************************************************
*)

unit fMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  Gamolf.FMX.HelpBar;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DGEFMXHelpBar1: TDGEFMXHelpBar;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGKenneyInputKeys,
  USVGKenneyInputXbox;

procedure TForm2.Button1Click(Sender: TObject);
begin
  DGEFMXHelpBar1.CloseHelpBar;
  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.Center;
  DGEFMXHelpBar1.VertAlign := TDGEFMXHelpBarVertAlign.Bottom;
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardArrowDown,
    TSVGKenneyInputXbox.XboxDpadRoundDown, 'down');
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  DGEFMXHelpBar1.CloseHelpBar;
  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.Left;
  DGEFMXHelpBar1.VertAlign := TDGEFMXHelpBarVertAlign.Top;
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardArrowDown,
    TSVGKenneyInputXbox.XboxDpadRoundDown, 'down');
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardEscape,
    TSVGKenneyInputXbox.XboxButtonColorX, 'quit');
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardSpace,
    TSVGKenneyInputXbox.XboxButtonColorA, 'go');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  DGEFMXHelpBar1.CloseHelpBar;
  DGEFMXHelpBar1.OpenHelpBar;
  DGEFMXHelpBar1.HorzAlign := TDGEFMXHelpBarHorzAlign.right;
  DGEFMXHelpBar1.VertAlign := TDGEFMXHelpBarVertAlign.Top;
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardSpace,
    TSVGKenneyInputXbox.XboxButtonColorA);
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardEscape,
    TSVGKenneyInputXbox.XboxButtonColorX, 'quit');
  DGEFMXHelpBar1.AddItem(TSVGKenneyInputKeys.KeyboardSpace,
    TSVGKenneyInputXbox.XboxButtonColorA, 'go');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  DGEFMXHelpBar1.IconGamepadBitmapListIndex := TOlfSVGBitmapList.AddAList;
  TOlfSVGBitmapList.AddItem(DGEFMXHelpBar1.IconGamepadBitmapListIndex,
    SVGKenneyInputXbox);

  DGEFMXHelpBar1.IconKeyBitmapListIndex := TOlfSVGBitmapList.AddAList;
  TOlfSVGBitmapList.AddItem(DGEFMXHelpBar1.IconKeyBitmapListIndex,
    SVGKenneyInputKeys);
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
