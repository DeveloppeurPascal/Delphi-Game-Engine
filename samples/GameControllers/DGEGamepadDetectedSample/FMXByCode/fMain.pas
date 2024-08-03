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
/// File last update : 30/07/2024 14:23:12
/// Signature : 5cd3f7aeafd6d30f8ffa5d529f137d889b7a7c96
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
  FMX.Effects,
  FMX.Layouts,
  FMX.Objects,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.EditBox,
  FMX.NumberBox,
  Gamolf.RTL.Joystick,
  FMX.StdCtrls,
  FMX.Ani;

type
  TForm3 = class(TForm)
    lGamepadDetectedContent: TLayout;
    Layout2: TLayout;
    lGamepadDetectedBackground: TLayout;
    edtWidth: TNumberBox;
    edtHeight: TNumberBox;
    edtMarginTop: TNumberBox;
    edtMarginRight: TNumberBox;
    edtMarginBottom: TNumberBox;
    edtMarginLeft: TNumberBox;
    cbPosition: TComboBox;
    cbDirection: TComboBox;
    GamepadDetectedGamepadManager: TDGEGamepadManager;
    GridPanelLayout1: TGridPanelLayout;
    btnSimulateDetected: TButton;
    btnSimulateLost: TButton;
    Rectangle1: TRectangle;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure GamepadDetectedGamepadManagerGamepadLost(const GamepadID
      : Integer);
    procedure GamepadDetectedGamepadManagerNewGamepadDetected(const GamepadID
      : Integer);
    procedure btnSimulateDetectedClick(Sender: TObject);
    procedure btnSimulateLostClick(Sender: TObject);
    procedure edtWidthChange(Sender: TObject);
    procedure edtHeightChange(Sender: TObject);
    procedure edtMarginTopChange(Sender: TObject);
    procedure edtMarginRightChange(Sender: TObject);
    procedure edtMarginBottomChange(Sender: TObject);
    procedure edtMarginLeftChange(Sender: TObject);
    procedure cbDirectionChange(Sender: TObject);
    procedure cbPositionChange(Sender: TObject);
  private
  protected
    procedure RefreshContentLayout;
    procedure AddGamepadImage(const Detected: boolean);
    procedure FinishedAnimation(Sender: TObject);
    procedure Init;
  public
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

uses
  Olf.Skia.SVGToBitmap,
  USVGKenneyGamepad;

procedure TForm3.AddGamepadImage(const Detected: boolean);
var
  img: TImage;
  shadow: TShadowEffect;
  Ani: TFloatAnimation;
begin
  img := TImage.create(self);
  img.parent := lGamepadDetectedContent;
  RefreshContentLayout;
  if Detected then
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(tag,
      TSVGKenneyGamepad.ControllerGeneric + TSVGKenneyGamepad.tag,
      round(edtWidth.Value), round(edtHeight.Value), Image1.Bitmap.BitmapScale))
  else
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(tag,
      TSVGKenneyGamepad.ControllerDisconnected + TSVGKenneyGamepad.tag,
      round(edtWidth.Value), round(edtHeight.Value),
      Image1.Bitmap.BitmapScale));

  shadow := TShadowEffect.create(img);
  shadow.parent := img;

  Ani := TFloatAnimation.create(img);
  Ani.parent := img;
  Ani.Duration := 5;
  Ani.Interpolation := TInterpolationType.Bounce;
  Ani.OnFinish := FinishedAnimation;
  Ani.PropertyName := 'Opacity';
  Ani.StartValue := 1;
  Ani.StopValue := 0;
  Ani.start;
end;

procedure TForm3.btnSimulateDetectedClick(Sender: TObject);
begin
  GamepadDetectedGamepadManagerNewGamepadDetected(0);
end;

procedure TForm3.GamepadDetectedGamepadManagerGamepadLost(const GamepadID
  : Integer);
begin
  AddGamepadImage(false);
end;

procedure TForm3.GamepadDetectedGamepadManagerNewGamepadDetected(const GamepadID
  : Integer);
begin
  AddGamepadImage(true);
end;

procedure TForm3.Init;
begin
  tag := TOlfSVGBitmapList.AddAList;
  TSVGKenneyGamepad.tag := TOlfSVGBitmapList.AddItem(tag, SVGKenneyGamepad);
end;

procedure TForm3.RefreshContentLayout;
var
  img: TImage;
  i: Integer;
begin
  case cbDirection.ItemIndex of
    0: // horizontal
      if cbPosition.ItemIndex in [0, 1] then // TopLeft, TopRight
        lGamepadDetectedContent.align := talignlayout.top
      else
        lGamepadDetectedContent.align := talignlayout.bottom;
    1: // vertical
      if cbPosition.ItemIndex in [0, 2] then // TopLeft, BottomLeft
        lGamepadDetectedContent.align := talignlayout.left
      else
        lGamepadDetectedContent.align := talignlayout.right;
  end;

  if cbDirection.ItemIndex = 1 then // vertical
    lGamepadDetectedContent.Width := edtWidth.Value;

  if cbDirection.ItemIndex = 0 then // horizontal
    lGamepadDetectedContent.Height := edtHeight.Value;

  for i := 0 to lGamepadDetectedContent.ChildrenCount - 1 do
    if lGamepadDetectedContent.Children[i] is TImage then
      case cbDirection.ItemIndex of
        0: // horizontal
          begin
            if cbPosition.ItemIndex in [0, 2] then // TopLeft, BottomLeft
              (lGamepadDetectedContent.Children[i] as TImage).align :=
                talignlayout.left
            else
              (lGamepadDetectedContent.Children[i] as TImage).align :=
                talignlayout.right;
            (lGamepadDetectedContent.Children[i] as TImage).Width :=
              edtWidth.Value;
          end;
        1: // vertical
          begin
            if cbPosition.ItemIndex in [0, 1] then // TopLeft, TopRight
              (lGamepadDetectedContent.Children[i] as TImage).align :=
                talignlayout.top
            else
              (lGamepadDetectedContent.Children[i] as TImage).align :=
                talignlayout.bottom;
            (lGamepadDetectedContent.Children[i] as TImage).Height :=
              edtHeight.Value;
          end;
      end;
end;

procedure TForm3.btnSimulateLostClick(Sender: TObject);
begin
  GamepadDetectedGamepadManagerGamepadLost(0);
end;

procedure TForm3.cbDirectionChange(Sender: TObject);
begin
  RefreshContentLayout;
end;

procedure TForm3.cbPositionChange(Sender: TObject);
begin
  RefreshContentLayout;
end;

procedure TForm3.edtHeightChange(Sender: TObject);
begin
  if (edtHeight.Value > 16) then
    RefreshContentLayout;
end;

procedure TForm3.edtMarginBottomChange(Sender: TObject);
begin
  lGamepadDetectedBackground.margins.bottom := edtMarginBottom.Value;
end;

procedure TForm3.edtMarginLeftChange(Sender: TObject);
begin
  lGamepadDetectedBackground.margins.left := edtMarginLeft.Value;
end;

procedure TForm3.edtMarginRightChange(Sender: TObject);
begin
  lGamepadDetectedBackground.margins.right := edtMarginRight.Value;
end;

procedure TForm3.edtMarginTopChange(Sender: TObject);
begin
  lGamepadDetectedBackground.margins.top := edtMarginTop.Value;
end;

procedure TForm3.edtWidthChange(Sender: TObject);
begin
  if (edtWidth.Value > 16) then
    RefreshContentLayout;
end;

procedure TForm3.FinishedAnimation(Sender: TObject);
var
  o: TFMXObject;
begin
  if Sender is TFloatAnimation then
  begin
    o := (Sender as TFloatAnimation).parent;
    tthread.forcequeue(nil,
      procedure
      begin
        o.free;
      end);
  end;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  edtWidth.Value := 128;
  edtHeight.Value := 64;
  edtMarginTop.Value := 10;
  edtMarginRight.Value := 10;
  edtMarginBottom.Value := 10;
  edtMarginLeft.Value := 10;
  cbPosition.ItemIndex := 1;
  cbDirection.ItemIndex := 1;

  Init;
end;

initialization

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}

end.
