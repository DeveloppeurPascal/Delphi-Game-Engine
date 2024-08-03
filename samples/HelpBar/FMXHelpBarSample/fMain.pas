/// <summary>
/// ***************************************************************************
///
/// Delphi Game Engine
///
/// Copyright 2021-2024 Patrick Prémartin under AGPL 3.0 license.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
/// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
/// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
/// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
/// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
/// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
/// DEALINGS IN THE SOFTWARE.
///
/// ***************************************************************************
///
/// Delphi Game Engine contains libraries and components to use in VCL or
/// FireMonkey game (or classic) projects.
///
/// If you want to play sounds or musics, use game controllers, pilot your
/// user interface with the keyboard or a game controller, it's the good place.
///
/// ***************************************************************************
/// File last update : 31/07/2024 15:43:06
/// Signature : e5d7c3771e04cba7e52120d0d26c8d6d9e5d5b70
/// ***************************************************************************
/// </summary>

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
