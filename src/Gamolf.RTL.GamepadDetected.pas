unit Gamolf.RTL.GamepadDetected;

interface

uses
  System.Classes;

type
{$SCOPEDENUMS ON}
  TDGEGamepadDetectedPosition = (TopLeft, TopRight, BottomLeft, BottomRight);
  TDGEGamepadDetectedDirection = (Horizontal, Vertical);

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
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Enabled: boolean read FEnabled write SetEnabled default true;
    /// <summary>
    /// Width of the GamepadOnOff displayed picture
    /// </summary>
    property Width: integer read FWidth write SetWidth;
    /// <summary>
    /// Height of the GamepadOnOff displayed picture
    /// </summary>
    property Height: integer read FHeight write SetHeight;
    property MarginBottom: integer read FMarginBottom write SetMarginBottom
      default 10;
    property MarginLeft: integer read FMarginLeft write SetMarginLeft
      default 10;
    property MarginRight: integer read FMarginRight write SetMarginRight
      default 10;
    property MarginTop: integer read FMarginTop write SetMarginTop default 10;
    property Position: TDGEGamepadDetectedPosition read FPosition
      write SetPosition default TDGEGamepadDetectedPosition.TopRight;
    property Direction: TDGEGamepadDetectedDirection read FDirection
      write SetDirection default TDGEGamepadDetectedDirection.Vertical;
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
    /// TagObject property "in case of" not used in this class
    /// </summary>
    property TagObject: TObject read FTagObject write SetTagObject default nil;
    /// <summary>
    /// TagString property "in case of" not used in this class
    /// </summary>
    property TagString: string read FTagString write SetTagString;
  end;

procedure Register;

implementation

uses
  FMX.Types;
// TODO : FMX.Types à retirer lors de l'ajout de la prise en charge de la VCL

procedure Register;
begin
  RegisterComponents('Gamolf', [TDGEGamepadDetected]);
end;

{ TDGEGamepadDetected }

constructor TDGEGamepadDetected.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
  FTagBool := false;
  FTagFloat := 0;
  FTagString := '';
  FTagObject := nil;
  FWidth := 128;
  FHeight := 64;
  FPosition := TDGEGamepadDetectedPosition.TopRight;
  FDirection := TDGEGamepadDetectedDirection.Vertical;
  FMarginBottom := 10;
  FMarginTop := 10;
  FMarginLeft := 10;
  FMarginRight := 10;
end;

procedure TDGEGamepadDetected.SetDirection(const Value
  : TDGEGamepadDetectedDirection);
begin
  FDirection := Value;
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

initialization

// TODO : retirer la restriction au groupe TFMXObject lors de l'ajout de la prise en charge de la VCL
StartClassGroup(TFMXObject);
ActivateClassGroup(TFMXObject);
GroupDescendentsWith(TDGEGamepadDetected, TFMXObject);

end.
