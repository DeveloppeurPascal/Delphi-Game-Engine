﻿/// <summary>
/// ***************************************************************************
///
/// Delphi Game Engine
///
/// Copyright 2021-2025 Patrick Prémartin under AGPL 3.0 license.
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
///
/// Author(s) :
/// Patrick PREMARTIN
///
/// Site :
/// https://delphigameengine.developpeur-pascal.fr
///
/// Project site :
/// https://github.com/DeveloppeurPascal/Delphi-Game-Engine
///
/// ***************************************************************************
/// File last update : 2025-01-14T16:57:50.000+01:00
/// Signature : 2e51eda9c5ef778a3f696d7d7fffd0a11709da72
/// ***************************************************************************
/// </summary>

unit Gamolf.RTL.GamepadDetected;

interface

{$IF Defined(FRAMEWORK_VCL) or Defined(FRAMEWORK_FMX)}
// For real projects, with the component code and the visual actions
{$DEFINE FULLCODE}
{$ELSE}
// For other projects and in the IDE, only the code of the non visual component
{$UNDEF FULLCODE}
{$ENDIF}

uses
{$IFDEF FRAMEWORK_FMX}
  FMX.Objects,
  FMX.Layouts,
{$ENDIF}
  System.Classes;

type
{$SCOPEDENUMS ON}
  TDGEGamepadDetectedPosition = (TopLeft, TopRight, BottomLeft, BottomRight);
  TDGEGamepadDetectedDirection = (Horizontal, Vertical);

{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TDGEGamepadDetected = class(TComponent)
  private
    FEnabled: boolean;
    FTagBool: boolean;
    FTagFloat: single;
    FTagString: string;
    FTagObject: TObject;
    FWidth: integer;
    FHeight: integer;
    FDirection: TDGEGamepadDetectedDirection;
    FPosition: TDGEGamepadDetectedPosition;
    FMarginBottom: integer;
    FMarginTop: integer;
    FMarginLeft: integer;
    FMarginRight: integer;
    FDuration: single;
    procedure SetEnabled(const Value: boolean);
    procedure SetTagBool(const Value: boolean);
    procedure SetTagFloat(const Value: single);
    procedure SetTagObject(const Value: TObject);
    procedure SetTagString(const Value: string);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetDirection(const Value: TDGEGamepadDetectedDirection);
    procedure SetPosition(const Value: TDGEGamepadDetectedPosition);
    procedure SetMarginBottom(const Value: integer);
    procedure SetMarginLeft(const Value: integer);
    procedure SetMarginRight(const Value: integer);
    procedure SetMarginTop(const Value: integer);
    procedure SetDuration(const Value: single);
  protected
{$IFDEF FRAMEWORK_FMX}
    lBackground: TLayout;
    lContent: TLayout;
    imgImage: TImage;
{$ENDIF}
{$IFDEF FULLCODE}
    procedure GamepadLost(const GamepadID: integer);
    procedure GamepadDetected(const GamepadID: integer);
    procedure FinishedAnimation(Sender: TObject);
    procedure RefreshContentLayout;
    procedure AddGamepadImage(const Detected: boolean);
{$ENDIF}
  public
    /// <summary>
    /// TagObject property "in case of" not used in this class
    /// </summary>
    property TagObject: TObject read FTagObject write SetTagObject default nil;
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    /// <summary>
    /// Use Test() method to simulate a game controller detected or lost event
    /// </summary>
    procedure Test(const OnOff: boolean = true);
  published
    property Enabled: boolean read FEnabled write SetEnabled default true;
    /// <summary>
    /// Width of the GamepadOnOff displayed picture
    /// </summary>
    property Width: integer read FWidth write SetWidth default 64;
    /// <summary>
    /// Height of the GamepadOnOff displayed picture
    /// </summary>
    property Height: integer read FHeight write SetHeight default 64;
    /// <summary>
    /// Margin between the game controller images and bottom of the form.
    /// </summary>
    property MarginBottom: integer read FMarginBottom write SetMarginBottom
      default 10;
    /// <summary>
    /// Margin between the game controller images and left of the form.
    /// </summary>
    property MarginLeft: integer read FMarginLeft write SetMarginLeft
      default 10;
    /// <summary>
    /// Margin between the game controller images and right of the form.
    /// </summary>
    property MarginRight: integer read FMarginRight write SetMarginRight
      default 10;
    /// <summary>
    /// Margin between the game controller images and top of the form.
    /// </summary>
    property MarginTop: integer read FMarginTop write SetMarginTop default 10;
    /// <summary>
    /// Where you want to have a game controller image when one is detected or lost
    /// </summary>
    property Position: TDGEGamepadDetectedPosition read FPosition
      write SetPosition default TDGEGamepadDetectedPosition.TopRight;
    /// <summary>
    /// Used when more than one game controller event (detected/lost) is visible
    /// </summary>
    /// <remarks>
    /// Only used in FireMonkey projects.
    /// </remarks>
    property Direction: TDGEGamepadDetectedDirection read FDirection
      write SetDirection default TDGEGamepadDetectedDirection.Vertical;
    /// <summary>
    /// Display duration in seconds
    /// </summary>
    property Duration: single read FDuration write SetDuration;
    /// <summary>
    /// Tag property "in case of" not used in this class
    /// </summary>
    property Tag;
    /// <summary>
    /// TagBool property "in case of" not used in this class
    /// </summary>
    property TagBool: boolean read FTagBool write SetTagBool default false;
    /// <summary>
    /// TagFloat property "in case of" not used in this class
    /// </summary>
    property TagFloat: single read FTagFloat write SetTagFloat;
    /// <summary>
    /// TagString property "in case of" not used in this class
    /// </summary>
    property TagString: string read FTagString write SetTagString;
  end;

procedure Register;

implementation

uses
{$IFDEF FRAMEWORK_FMX}
  FMX.Forms,
  FMX.Effects,
  FMX.Ani,
  FMX.Types,
  FMX.Controls,
{$ENDIF}
{$IFDEF FRAMEWORK_VCL}
  VCL.Forms,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.Graphics,
{$ENDIF}
{$IFDEF FULLCODE}
  Gamolf.RTL.Joystick,
  Olf.Skia.SVGToBitmap,
{$ENDIF}
  System.SysUtils;

procedure Register;
begin
  RegisterComponents('Gamolf', [TDGEGamepadDetected]);
end;

{ TDGEGamepadDetected }

procedure TDGEGamepadDetected.AfterConstruction;
{$IFDEF FRAMEWORK_FMX}
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
{$ENDIF}
{$IFDEF FULLCODE}

var
  GamepadManager: TDGEGamepadManager;
{$ENDIF}
begin
  inherited;
{$IFDEF FULLCODE}
  GamepadManager := TDGEGamepadManager.Create(self);
  GamepadManager.SynchronizedEvents := true;
  GamepadManager.OnNewGamepadDetected := GamepadDetected;
  GamepadManager.OnGamepadLost := GamepadLost;
{$ENDIF}
{$IFDEF FRAMEWORK_FMX}
  imgImage := TImage.Create(self);
  imgImage.Parent := GetForm(Owner);
  imgImage.HitTest := false;
  imgImage.Visible := false;

  lBackground := TLayout.Create(self);
  lBackground.Parent := imgImage.Parent;
  lBackground.HitTest := false;
  lBackground.Align := talignlayout.Contents;

  lContent := TLayout.Create(self);
  lContent.Parent := lBackground;
  lContent.HitTest := false;

  RefreshContentLayout;
{$ENDIF}
end;

constructor TDGEGamepadDetected.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
  FTagBool := false;
  FTagFloat := 0;
  FTagString := '';
  FTagObject := nil;
  FWidth := 64;
  FHeight := 64;
  FPosition := TDGEGamepadDetectedPosition.TopRight;
  FDirection := TDGEGamepadDetectedDirection.Vertical;
  FMarginBottom := 10;
  FMarginTop := 10;
  FMarginLeft := 10;
  FMarginRight := 10;
  FDuration := 5;
{$IFDEF FRAMEWORK_FMX}
  lBackground := nil;
  lContent := nil;
  imgImage := nil;
{$ENDIF}
end;

procedure TDGEGamepadDetected.SetDirection(const Value
  : TDGEGamepadDetectedDirection);
begin
  FDirection := Value;
end;

procedure TDGEGamepadDetected.SetDuration(const Value: single);
begin
  FDuration := Value;
end;

procedure TDGEGamepadDetected.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TDGEGamepadDetected.SetHeight(const Value: integer);
begin
  FHeight := Value;
end;

procedure TDGEGamepadDetected.SetMarginBottom(const Value: integer);
begin
  FMarginBottom := Value;
end;

procedure TDGEGamepadDetected.SetMarginLeft(const Value: integer);
begin
  FMarginLeft := Value;
end;

procedure TDGEGamepadDetected.SetMarginRight(const Value: integer);
begin
  FMarginRight := Value;
end;

procedure TDGEGamepadDetected.SetMarginTop(const Value: integer);
begin
  FMarginTop := Value;
end;

procedure TDGEGamepadDetected.SetPosition(const Value
  : TDGEGamepadDetectedPosition);
begin
  FPosition := Value;
end;

procedure TDGEGamepadDetected.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

procedure TDGEGamepadDetected.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

procedure TDGEGamepadDetected.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TDGEGamepadDetected.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

procedure TDGEGamepadDetected.SetWidth(const Value: integer);
begin
  FWidth := Value;
end;

procedure TDGEGamepadDetected.Test(const OnOff: boolean);
begin
{$IFDEF FULLCODE}
  AddGamepadImage(OnOff);
{$ENDIF}
end;

{$IFDEF FULLCODE}
{$REGION 'Default SVG declaration'}
// /////////////////////////////////////////////////////////////////////////////
// Default SVG from Kenney's "Input Prompts" assets pack.
//
// License CC0
//
// Assets pack : https://www.kenney.nl/assets/input-prompts
// Author website : https://www.kenney.nl/
// /////////////////////////////////////////////////////////////////////////////

// ****************************************
// File generator : SVG Folder to Delphi Unit (1.0)
// Website : https://svgfolder2delphiunit.olfsoftware.fr/
// Generation date : 30/07/2024 12:17:18
//
// Don't do any change on this file.
// They will be erased by next generation !
// ****************************************

const
  CSVGControllerDisconnected = 0;
  CSVGControllerGeneric = 1;

type
{$SCOPEDENUMS ON}
  TSVGKenneyGamepadIndex = (ControllerDisconnected = CSVGControllerDisconnected,
    ControllerGeneric = CSVGControllerGeneric);

  TSVGKenneyGamepad = class
  private
  class var
    FTag: integer;
    FTagBool: boolean;
    FTagFloat: single;
    FTagObject: TObject;
    FTagString: string;
    class procedure SetTag(const Value: integer); static;
    class procedure SetTagBool(const Value: boolean); static;
    class procedure SetTagFloat(const Value: single); static;
    class procedure SetTagObject(const Value: TObject); static;
    class procedure SetTagString(const Value: string); static;
  public const
    ControllerDisconnected = CSVGControllerDisconnected;
    ControllerGeneric = CSVGControllerGeneric;
    class property Tag: integer read FTag write SetTag;
    class property TagBool: boolean read FTagBool write SetTagBool;
    class property TagFloat: single read FTagFloat write SetTagFloat;
    class property TagObject: TObject read FTagObject write SetTagObject;
    class property TagString: string read FTagString write SetTagString;
    class function SVG(const Index: integer): string; overload;
    class function SVG(const Index: TSVGKenneyGamepadIndex): string; overload;
    class function Count: integer;
    class constructor Create;
  end;

var
  SVGKenneyGamepad: array of String;

  { TSVGKenneyGamepad }

class constructor TSVGKenneyGamepad.Create;
begin
  inherited;
  FTag := 0;
  FTagBool := false;
  FTagFloat := 0;
  FTagObject := nil;
  FTagString := '';
end;

class procedure TSVGKenneyGamepad.SetTag(const Value: integer);
begin
  FTag := Value;
end;

class procedure TSVGKenneyGamepad.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

class procedure TSVGKenneyGamepad.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

class procedure TSVGKenneyGamepad.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

class procedure TSVGKenneyGamepad.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

class function TSVGKenneyGamepad.SVG(const Index: integer): string;
begin
  if (index < Count) then
    result := SVGKenneyGamepad[index]
  else
    raise Exception.Create('SVG not found. Index out of range.');
end;

class function TSVGKenneyGamepad.SVG(const Index
  : TSVGKenneyGamepadIndex): string;
begin
  result := SVG(ord(index));
end;

class function TSVGKenneyGamepad.Count: integer;
begin
  result := length(SVGKenneyGamepad);
end;
{$ENDREGION}

procedure TDGEGamepadDetected.GamepadLost(const GamepadID: integer);
begin
  AddGamepadImage(false);
end;

procedure TDGEGamepadDetected.GamepadDetected(const GamepadID: integer);
begin
  AddGamepadImage(true);
end;

procedure TDGEGamepadDetected.FinishedAnimation(Sender: TObject);
{$IFDEF FRAMEWORK_FMX}
var
  o: TComponent;
{$ENDIF}
begin
{$IFDEF FRAMEWORK_FMX}
  if Sender is TFloatAnimation then
  begin
    o := (Sender as TFloatAnimation).Parent;
    tthread.forcequeue(nil,
      procedure
      begin
        o.free;
      end);
  end;
{$ENDIF}
end;

procedure TDGEGamepadDetected.RefreshContentLayout;
{$IFDEF FRAMEWORK_FMX}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF FRAMEWORK_FMX}
  case FDirection of
    TDGEGamepadDetectedDirection.Horizontal: // horizontal
      if FPosition in [TDGEGamepadDetectedPosition.TopLeft,
        TDGEGamepadDetectedPosition.TopRight] then // TopLeft, TopRight
        lContent.Align := talignlayout.top
      else
        lContent.Align := talignlayout.bottom;
    TDGEGamepadDetectedDirection.Vertical: // vertical
      if FPosition in [TDGEGamepadDetectedPosition.TopLeft,
        TDGEGamepadDetectedPosition.BottomLeft] then // TopLeft, BottomLeft
        lContent.Align := talignlayout.left
      else
        lContent.Align := talignlayout.right;
  end;

  if FDirection = TDGEGamepadDetectedDirection.Vertical then // vertical
    lContent.Width := FWidth
  else // horizontal
    lContent.Height := FHeight;

  for i := 0 to lContent.ChildrenCount - 1 do
    if lContent.Children[i] is TImage then
      case FDirection of
        TDGEGamepadDetectedDirection.Horizontal:
          begin
            if FPosition in [TDGEGamepadDetectedPosition.TopLeft,
              TDGEGamepadDetectedPosition.BottomLeft] then
              (lContent.Children[i] as TImage).Align := talignlayout.left
            else
              (lContent.Children[i] as TImage).Align := talignlayout.right;
            (lContent.Children[i] as TImage).Width := FWidth;
          end;
      else
        if FPosition in [TDGEGamepadDetectedPosition.TopLeft,
          TDGEGamepadDetectedPosition.TopRight] then
          (lContent.Children[i] as TImage).Align := talignlayout.top
        else
          (lContent.Children[i] as TImage).Align := talignlayout.bottom;
        (lContent.Children[i] as TImage).Height := FHeight;
end;
{$ENDIF}
end;

procedure TDGEGamepadDetected.AddGamepadImage(const Detected: boolean);
{$IFDEF FRAMEWORK_VCL}
  function GetForm(c: TComponent): TCustomForm;
  begin
    if not assigned(c) then
      result := application.mainform
    else if c is TCustomForm then
      result := c as TCustomForm
    else if c is tcontrol then
      result := GetForm((c as tcontrol).Parent)
    else
      result := GetForm(c.Owner);
  end;
{$ENDIF}

var
  img: TImage;
{$IFDEF FRAMEWORK_FMX}
  shadow: TShadowEffect;
  Ani: TFloatAnimation;
{$ENDIF}
begin
  img := TImage.Create(self);
{$IFDEF FRAMEWORK_VCL}
  img.Parent := GetForm(self);
  img.Align := alnone;
  img.Width := FWidth;
  img.Height := FHeight;
  case FPosition of
    TDGEGamepadDetectedPosition.TopLeft:
      begin
        img.left := FMarginLeft;
        img.top := FMarginTop;
        img.Anchors := [akTop, akLeft];
      end;
    TDGEGamepadDetectedPosition.TopRight:
      begin
        img.left := img.Parent.ClientWidth - FMarginRight - img.Width;
        img.top := FMarginTop;
        img.Anchors := [akTop, akRight];
      end;
    TDGEGamepadDetectedPosition.BottomRight:
      begin
        img.left := img.Parent.ClientWidth - FMarginRight - img.Width;
        img.top := img.Parent.ClientHeight - FMarginBottom - img.Height;
        img.Anchors := [akBottom, akRight];
      end;
    TDGEGamepadDetectedPosition.BottomLeft:
      begin
        img.left := FMarginLeft;
        img.top := img.Parent.ClientHeight - FMarginBottom - img.Height;
        img.Anchors := [akBottom, akLeft];
      end;
  end;
  if Detected then
    img.picture.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(TSVGKenneyGamepad.Tag,
      TSVGKenneyGamepad.ControllerGeneric, round(FWidth), round(FHeight)))
  else
    img.picture.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(TSVGKenneyGamepad.Tag,
      TSVGKenneyGamepad.ControllerDisconnected, round(FWidth), round(FHeight)));
  img.Visible := true;
  tthread.CreateAnonymousThread(
    procedure
    var
      Timer: integer;
    begin
      Timer := round(FDuration * 1000);
      while (not tthread.CheckTerminated) and (Timer > 0) do
      begin
        sleep(100);
        Timer := Timer - 100;
      end;
      if not tthread.CheckTerminated then
        tthread.queue(nil,
          procedure
          begin
            try
              img.free;
            except
            end;
          end);
    end).start;
{$ENDIF}
{$IFDEF FRAMEWORK_FMX}
  img.Parent := lContent;

  RefreshContentLayout;

  if Detected then
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(TSVGKenneyGamepad.Tag,
      TSVGKenneyGamepad.ControllerGeneric, round(FWidth), round(FHeight),
      imgImage.Bitmap.BitmapScale))
  else
    img.Bitmap.Assign(TOlfSVGBitmapList.Bitmap(TSVGKenneyGamepad.Tag,
      TSVGKenneyGamepad.ControllerDisconnected, round(FWidth), round(FHeight),
      imgImage.Bitmap.BitmapScale));

  shadow := TShadowEffect.Create(img);
  shadow.Parent := img;

  Ani := TFloatAnimation.Create(img);
  Ani.Parent := img;
  Ani.Duration := FDuration;
  Ani.Interpolation := TInterpolationType.Bounce;
  Ani.OnFinish := FinishedAnimation;
  Ani.PropertyName := 'Opacity';
  Ani.StartValue := 1;
  Ani.StopValue := 0;
  Ani.start;

  lBackground.BringToFront;
{$ENDIF}
end;

{$ENDIF}

initialization

{$IFDEF FULLCODE}
{$REGION 'Default SVG initialization'}
{$IF CompilerVersion >= 36}
// Delphi 12 Athens

SetLength(SVGKenneyGamepad, 2);
{$TEXTBLOCK NATIVE XML}
// TODO : remove the INC file when the code formater will be compatible with multilines string
{$I 'Gamolf.RTL.GamepadDetected.inc'}
{$ELSE}
  SetLength(SVGKenneyGamepad, 0);
{$MESSAGE WARN 'no default SVG images available before Delphi 12 Athens'}
{$ENDIF}
TSVGKenneyGamepad.Tag := TOlfSVGBitmapList.AddAList;
TOlfSVGBitmapList.AddItem(TSVGKenneyGamepad.Tag, SVGKenneyGamepad);
{$ENDREGION}
{$ENDIF}

end.
