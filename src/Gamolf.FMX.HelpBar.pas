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
/// File last update : 31/07/2024 15:41:30
/// Signature : 3031185b29c24cf8d1db313e1e44526821463321
/// ***************************************************************************
/// </summary>

unit Gamolf.FMX.HelpBar;

interface

uses
  FMX.Graphics,
  FMX.Objects,
  FMX.Types,
  FMX.Layouts,
  System.Classes;

type
{$SCOPEDENUMS ON}
  TDGEFMXHelpBarHorzAlign = (Left, Center, Right);
  TDGEFMXHelpBarVertAlign = (Top, Bottom);

  /// <summary>
  /// Help bar component
  /// </summary>

{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TDGEFMXHelpBar = class(TFMXObject)
  private
    FTextSettings: TTextSettings;
    FBackgroundFill: TBrush;
    FHorzAlign: TDGEFMXHelpBarHorzAlign;
    FBackgroundStroke: TStrokeBrush;
    FIconMarginRight: single;
    FBackgroundOpacity: single;
    FTextMarginRight: single;
    FVertAlign: TDGEFMXHelpBarVertAlign;
    FMargins: TBounds;
    FGamepadMode: boolean;
    FBackgroundXRadius: single;
    FBackgroundYRadius: single;
    FIconGamepadBitmapListIndex: word;
    FIconKeyBitmapListIndex: word;
    FBackgroundPadding: TBounds;
    FHeight: single;
    FRefreshInProgress: boolean;
    procedure SetBackgroundFill(const Value: TBrush);
    procedure SetBackgroundOpacity(const Value: single);
    procedure SetBackgroundStroke(const Value: TStrokeBrush);
    procedure SetHorzAlign(const Value: TDGEFMXHelpBarHorzAlign);
    procedure SetIconMarginRight(const Value: single);
    procedure SetTextMarginRight(const Value: single);
    procedure SetTextSettings(const Value: TTextSettings);
    procedure SetVertAlign(const Value: TDGEFMXHelpBarVertAlign);
    procedure SetMargins(const Value: TBounds);
    procedure SetGamepadMode(const Value: boolean);
    procedure SetBackgroundXRadius(const Value: single);
    procedure SetBackgroundYRadius(const Value: single);
    procedure SetIconGamepadBitmapListIndex(const Value: word);
    procedure SetIconKeyBitmapListIndex(const Value: word);
    procedure SetBackgroundPadding(const Value: TBounds);
    procedure SetHeight(const Value: single);
    procedure FormResize(Sender: TObject);
  protected
    lGlobal: TLayout;
    lHelpBar: TLayout;
    lContent: TLayout;
    rBackground: TRectangle;
    imgImage: TImage;
    procedure GamepadLost(const GamepadID: integer);
    procedure GamepadDetected(const GamepadID: integer);
    function GetNewX(const WithRightMargin: boolean = true): single;
  public
    /// <summary>
    ///
    /// </summary>
    property GamepadMode: boolean read FGamepadMode write SetGamepadMode;
    /// <summary>
    ///
    /// </summary>
    property TextSettings: TTextSettings read FTextSettings
      write SetTextSettings;
    /// <summary>
    ///
    /// </summary>
    constructor Create(AOwner: TComponent); override;
    /// <summary>
    ///
    /// </summary>
    procedure AfterConstruction; override;
    /// <summary>
    ///
    /// </summary>
    destructor Destroy; override;
    /// <summary>
    ///
    /// </summary>
    procedure RefreshHelpBar;
    /// <summary>
    /// Clear the help bar content and show it
    /// </summary>
    procedure OpenHelpBar;
    /// <summary>
    /// Hide the help bar
    /// </summary>
    procedure CloseHelpBar;
    /// <summary>
    /// Add an image to the Help Bar
    /// </summary>
    procedure AddItem(const SVGKeyIndex, SVGGamepadIndex: integer); overload;
    /// <summary>
    /// Add an image and text to the Help Bar
    /// </summary>
    procedure AddItem(const SVGKeyIndex, SVGGamepadIndex: integer;
      const Text: string); overload;
    /// <summary>
    /// Empty the Help bar content layout
    /// </summary>
    procedure Clear;
  published
    // Icons properties
    /// <summary>
    ///
    /// </summary>
    property IconMarginRight: single read FIconMarginRight
      write SetIconMarginRight;
    /// <summary>
    ///
    /// </summary>
    property IconKeyBitmapListIndex: word read FIconKeyBitmapListIndex
      write SetIconKeyBitmapListIndex default 0;
    /// <summary>
    ///
    /// </summary>
    property IconGamepadBitmapListIndex: word read FIconGamepadBitmapListIndex
      write SetIconGamepadBitmapListIndex default 0;
    // Texts properties
    /// <summary>
    ///
    /// </summary>
    property TextMarginRight: single read FTextMarginRight
      write SetTextMarginRight;
    // Background properties
    /// <summary>
    ///
    /// </summary>
    property BackgroundFill: TBrush read FBackgroundFill
      write SetBackgroundFill;
    /// <summary>
    ///
    /// </summary>
    property BackgroundOpacity: single read FBackgroundOpacity
      write SetBackgroundOpacity;
    /// <summary>
    ///
    /// </summary>
    property BackgroundXRadius: single read FBackgroundXRadius
      write SetBackgroundXRadius;
    /// <summary>
    ///
    /// </summary>
    property BackgroundYRadius: single read FBackgroundYRadius
      write SetBackgroundYRadius;
    /// <summary>
    ///
    /// </summary>
    property BackgroundPadding: TBounds read FBackgroundPadding
      write SetBackgroundPadding;
    // Background stroke properties
    /// <summary>
    ///
    /// </summary>
    property BackgroundStroke: TStrokeBrush read FBackgroundStroke
      write SetBackgroundStroke;
    // Help bar properties
    /// <summary>
    ///
    /// </summary>
    property HorzAlign: TDGEFMXHelpBarHorzAlign read FHorzAlign
      write SetHorzAlign default TDGEFMXHelpBarHorzAlign.Center;
    /// <summary>
    ///
    /// </summary>
    property VertAlign: TDGEFMXHelpBarVertAlign read FVertAlign
      write SetVertAlign default TDGEFMXHelpBarVertAlign.Bottom;
    /// <summary>
    ///
    /// </summary>
    property Margins: TBounds read FMargins write SetMargins;
    /// <summary>
    ///
    /// </summary>
    property Height: single read FHeight write SetHeight;
  end;

procedure Register;

implementation

uses
{$IFNDEF IDE}
  Gamolf.RTL.Joystick,
  Olf.Skia.SVGToBitmap,
{$ENDIF}
  FMX.Controls,
  FMX.Forms,
  System.Types,
  System.UITypes,
  System.SysUtils;

procedure Register;
begin
  RegisterComponents('Gamolf', [TDGEFMXHelpBar]);
end;

{ TDGEFMXHelpBar }

procedure TDGEFMXHelpBar.AddItem(const SVGKeyIndex, SVGGamepadIndex: integer);
begin
  AddItem(SVGKeyIndex, SVGGamepadIndex, '');
end;

procedure TDGEFMXHelpBar.AddItem(const SVGKeyIndex, SVGGamepadIndex: integer;
  const Text: string);
{$IFNDEF IDE}
var
  img: TImage;
  txt: ttext;
{$ENDIF}
begin
{$IFNDEF IDE}
  if lContent.Height < 1 then
    RefreshHelpBar;

  // Icon for Keyboard
  img := TImage.Create(self);
  img.Parent := lContent;
  img.HitTest := false;
  img.TabStop := false;
  img.Height := lContent.Height;
  img.Width := img.Height;
  img.Bitmap.assign(TOlfSVGBitmapList.Bitmap(FIconKeyBitmapListIndex,
    SVGKeyIndex, round(img.Width), round(img.Height),
    imgImage.Bitmap.BitmapScale));
  img.tag := 1;
  img.WrapMode := TImageWrapMode.Fit;

  // Icon for Gamepad
  img := TImage.Create(self);
  img.Parent := lContent;
  img.HitTest := false;
  img.TabStop := false;
  img.Height := lContent.Height;
  img.Width := img.Height;
  img.Bitmap.assign(TOlfSVGBitmapList.Bitmap(FIconGamepadBitmapListIndex,
    SVGGamepadIndex, round(img.Width), round(img.Height),
    imgImage.Bitmap.BitmapScale));
  img.tag := 2;
  img.WrapMode := TImageWrapMode.Fit;

  // Text
  if not Text.isempty then
  begin
    txt := ttext.Create(self);
    txt.Parent := lContent;
    txt.HitTest := false;
    txt.TabStop := false;
    txt.TextSettings.assign(FTextSettings);
    txt.AutoSize := true;
    txt.Text := Text;
  end;

  RefreshHelpBar;
{$ENDIF}
end;

procedure TDGEFMXHelpBar.AfterConstruction;
{$IFNDEF IDE}
  function GetForm(c: TComponent): TCommonCustomForm;
  begin
    if not assigned(c) then
      result := application.mainform
    else if c is TCommonCustomForm then
      result := c as TCommonCustomForm
    else if c is tcontrol then
      result := GetForm((c as tcontrol).Parent)
    else
      result := GetForm(c.Owner);
  end;

var
  GamepadManager: TDGEGamepadManager;
{$ENDIF}
begin
  inherited;
{$IFNDEF IDE}
  GamepadManager := TDGEGamepadManager.Create(self);
  GamepadManager.SynchronizedEvents := true;
  GamepadManager.OnNewGamepadDetected := GamepadDetected;
  GamepadManager.OnGamepadLost := GamepadLost;

  imgImage.Parent := GetForm(Owner);
  imgImage.HitTest := false;
  imgImage.TabStop := false;
  imgImage.Visible := false;

  lGlobal.Parent := imgImage.Parent;
  lGlobal.HitTest := false;
  lGlobal.TabStop := false;
  lGlobal.Align := TAlignLayout.Contents;

  lHelpBar.Parent := lGlobal;
  lHelpBar.HitTest := false;
  lHelpBar.TabStop := false;
  lHelpBar.OnResize := FormResize;

  rBackground.Parent := lHelpBar;
  rBackground.HitTest := false;
  rBackground.TabStop := false;
  rBackground.position.y := 0;

  lContent.Parent := lHelpBar;
  lContent.HitTest := false;
  lContent.TabStop := false;
  lContent.position.y := 0;
  lContent.BringToFront;

  CloseHelpBar;
{$ENDIF}
end;

procedure TDGEFMXHelpBar.Clear;
begin
{$IFNDEF IDE}
  while (lContent.ChildrenCount > 0) do
    lContent.Children[0].free;

  RefreshHelpBar;
{$ENDIF}
end;

procedure TDGEFMXHelpBar.CloseHelpBar;
begin
{$IFNDEF IDE}
  lHelpBar.Visible := false;
{$ENDIF}
end;

constructor TDGEFMXHelpBar.Create(AOwner: TComponent);
begin
  inherited;
  FRefreshInProgress := false;

  FTextSettings := TTextSettings.Create(self);
  FTextSettings.HorzAlign := TTextAlign.leading;
  FTextSettings.VertAlign := TTextAlign.Center;
  FTextSettings.Font.Style := [TFontStyle.fsBold];

  FBackgroundFill := TBrush.Create(tbrushkind.Solid, talphacolors.White);
  FBackgroundFill.Color := talphacolors.White;

  FHorzAlign := TDGEFMXHelpBarHorzAlign.Center;

  FBackgroundStroke := TStrokeBrush.Create(tbrushkind.None, talphacolors.White);
  FBackgroundStroke.Kind := tbrushkind.None;

  FIconMarginRight := 5;
  FBackgroundOpacity := 0.6;
  FTextMarginRight := 20;
  FVertAlign := TDGEFMXHelpBarVertAlign.Bottom;
  FMargins := TBounds.Create(trectf.Create(10, 20, 10, 20));
  FGamepadMode := false;
  FBackgroundXRadius := 15;
  FBackgroundYRadius := 10;
  FIconGamepadBitmapListIndex := 0;
  FIconKeyBitmapListIndex := 0;

  FBackgroundPadding := TBounds.Create(trectf.Create(15, 10, 15, 10));

  FHeight := 50;

  imgImage := TImage.Create(self);
  lGlobal := TLayout.Create(self);
  lHelpBar := TLayout.Create(self);
  rBackground := TRectangle.Create(self);
  lContent := TLayout.Create(self);
end;

destructor TDGEFMXHelpBar.Destroy;
begin
  FTextSettings.free;
  FBackgroundFill.free;
  FBackgroundStroke.free;
  FMargins.free;
  FBackgroundPadding.free;
  inherited;
end;

procedure TDGEFMXHelpBar.FormResize(Sender: TObject);
begin
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.GamepadDetected(const GamepadID: integer);
begin
  GamepadMode := true;
end;

procedure TDGEFMXHelpBar.GamepadLost(const GamepadID: integer);
begin
{$IFNDEF IDE}
  GamepadMode := (TGamepadDevicesManager.Current.ConnectedGamepadCount > 0);
{$ENDIF}
end;

function TDGEFMXHelpBar.GetNewX(const WithRightMargin: boolean): single;
var
  i: integer;
  Ctrl: tcontrol;
  Marge: single;
begin
  result := 0;
  Marge := 0;
  for i := 0 to lContent.ChildrenCount - 1 do
    if (lContent.Children[i] is tcontrol) then
    begin
      Ctrl := lContent.Children[i] as tcontrol;
      if (result <= Ctrl.position.x) then
      begin
        result := Ctrl.position.x + Ctrl.Margins.Left + Ctrl.Width +
          Ctrl.Margins.Right;
        if Ctrl is TImage then
          Marge := FIconMarginRight
        else if Ctrl is ttext then
          Marge := FTextMarginRight
        else
          raise exception.Create
            ('Can''t calculate a margin between help bar elements.');
      end;
    end;
  if (result > 0) and WithRightMargin then
    result := result + Marge;
end;

procedure TDGEFMXHelpBar.OpenHelpBar;
begin
{$IFNDEF IDE}
  Clear;
  lHelpBar.Visible := true;
  lGlobal.BringToFront;
{$ENDIF}
end;

procedure TDGEFMXHelpBar.RefreshHelpBar;
{$IFNDEF IDE}
var
  i: integer;
  NewPosX, NewMargin: single;
  img: TImage;
  txt: ttext;
{$ENDIF}
begin
{$IFNDEF IDE}
  if csLoading in ComponentState then // loading the DFM
    exit;

  if FRefreshInProgress then // procédure non réentrante (ni récursive !)
    exit;

  FRefreshInProgress := true;
  try

    case FVertAlign of
      TDGEFMXHelpBarVertAlign.Top:
        lHelpBar.Align := TAlignLayout.Top;
      TDGEFMXHelpBarVertAlign.Bottom:
        lHelpBar.Align := TAlignLayout.Bottom;
    end;
    lHelpBar.Height := FHeight;
    lHelpBar.Margins.assign(Margins);

    rBackground.fill.assign(FBackgroundFill);
    rBackground.Stroke.assign(FBackgroundStroke);
    rBackground.Padding.assign(FBackgroundPadding);
    rBackground.Opacity := FBackgroundOpacity;
    rBackground.XRadius := FBackgroundXRadius;
    rBackground.YRadius := FBackgroundYRadius;
    rBackground.Height := lHelpBar.Height;

    lContent.position.y := rBackground.position.y + rBackground.Padding.Top;
    lContent.Height := rBackground.Height - FBackgroundPadding.Top -
      FBackgroundPadding.Bottom;

    NewPosX := 0;
    NewMargin := 0;
    for i := 0 to lContent.ChildrenCount - 1 do
    begin
      if lContent.Children[i] is TImage then
      begin
        img := lContent.Children[i] as TImage;
        case img.tag of
          1:
            img.Visible := not FGamepadMode;
          2:
            img.Visible := FGamepadMode;
        else
          raise exception.Create('Can''t define icon visibility.');
        end;
        if img.Visible then
        begin
          img.Height := lContent.Height;
          img.Width := img.Height;
          img.position.x := NewPosX + NewMargin;
          img.position.y := 0;
          NewPosX := img.position.x + img.Width;
          NewMargin := FIconMarginRight;
        end;
      end
      else if lContent.Children[i] is ttext then
      begin
        txt := lContent.Children[i] as ttext;
        txt.TextSettings.assign(FTextSettings);
        txt.Height := lContent.Height;
        txt.position.x := NewPosX + NewMargin;
        case TextSettings.VertAlign of
          TTextAlign.leading:
            txt.position.y := 0;
          TTextAlign.Center:
            txt.position.y := (lContent.Height - txt.Height) / 2;
          TTextAlign.Trailing:
            txt.position.y := lContent.Height - txt.Height;
        else
          raise exception.Create('Unknown vertical text alignment.');
        end;
        NewPosX := txt.position.x + txt.Width;
        NewMargin := FTextMarginRight;
      end
      else
        raise exception.Create('Unknow content type in the help bar.');
    end;
    lContent.Width := NewPosX;

    rBackground.Width := lContent.Width + FBackgroundPadding.Left +
      FBackgroundPadding.Right;
    case FHorzAlign of
      TDGEFMXHelpBarHorzAlign.Left:
        rBackground.position.x := 0;
      TDGEFMXHelpBarHorzAlign.Center:
        rBackground.position.x := (lHelpBar.Width - rBackground.Width) / 2;
      TDGEFMXHelpBarHorzAlign.Right:
        rBackground.position.x := lHelpBar.Width - rBackground.Width;
    end;

    lContent.position.x := rBackground.position.x + rBackground.Padding.Left;
  finally
    FRefreshInProgress := false;
  end;
{$ENDIF}
end;

procedure TDGEFMXHelpBar.SetBackgroundFill(const Value: TBrush);
begin
  FBackgroundFill := Value;
  RefreshHelpBar; // not called for Fill properties changes
end;

procedure TDGEFMXHelpBar.SetBackgroundOpacity(const Value: single);
begin
  FBackgroundOpacity := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetBackgroundPadding(const Value: TBounds);
begin
  FBackgroundPadding := Value;
  RefreshHelpBar; // not called for padding properties changes
end;

procedure TDGEFMXHelpBar.SetBackgroundStroke(const Value: TStrokeBrush);
begin
  FBackgroundStroke := Value;
  RefreshHelpBar; // not called for a change in TStrokeBrush properties
end;

procedure TDGEFMXHelpBar.SetBackgroundXRadius(const Value: single);
begin
  FBackgroundXRadius := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetBackgroundYRadius(const Value: single);
begin
  FBackgroundYRadius := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetGamepadMode(const Value: boolean);
begin
  FGamepadMode := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetHeight(const Value: single);
begin
  FHeight := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetHorzAlign(const Value: TDGEFMXHelpBarHorzAlign);
begin
  FHorzAlign := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetIconGamepadBitmapListIndex(const Value: word);
begin
  FIconGamepadBitmapListIndex := Value;
end;

procedure TDGEFMXHelpBar.SetIconKeyBitmapListIndex(const Value: word);
begin
  FIconKeyBitmapListIndex := Value;
end;

procedure TDGEFMXHelpBar.SetIconMarginRight(const Value: single);
begin
  FIconMarginRight := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetMargins(const Value: TBounds);
begin
  FMargins := Value;
  RefreshHelpBar; // not called for margins properties changes
end;

procedure TDGEFMXHelpBar.SetTextMarginRight(const Value: single);
begin
  FTextMarginRight := Value;
  RefreshHelpBar;
end;

procedure TDGEFMXHelpBar.SetTextSettings(const Value: TTextSettings);
begin
  FTextSettings := Value;
  RefreshHelpBar; // not called for text settings properties changes
end;

procedure TDGEFMXHelpBar.SetVertAlign(const Value: TDGEFMXHelpBarVertAlign);
begin
  FVertAlign := Value;
  RefreshHelpBar;
end;

end.
