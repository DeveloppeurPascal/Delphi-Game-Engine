unit Gamolf.RTL.Joystick;

interface

uses System.SysUtils;

type
  /// <summary>
  /// Base class for all Joystick exceptions
  /// </summary>
  EJoystickServiceException = class(exception)
  end;

  /// <summary>
  /// Raised if the controller is not available
  /// </summary>
  EJoystickUnpluggedException = class(EJoystickServiceException)

  end;

  /// <summary>
  /// Type for Joystick ID
  /// </summary>
  TJoystickID = byte;

  /// <summary>
  /// Type for Button ID
  /// </summary>
  TButtonID = byte;

  /// <summary>
  /// Joystick/gamepad controller datas
  /// </summary>
  TJoystickInfo = record
    /// <summary>
    /// Values for each axes managed by the joystick
    /// 0 => X, 1 => Y, 2 => Z
    /// 3 => R, 4 => U, 5 => V
    /// Values between -1 and 1.
    /// -1 means left, 0 means center, 1 means right
    /// can be around the real value and not egal the values, try round()
    /// </summary>
    Axes: array of single;
    /// <summary>
    /// List of all available buttons on this controller.
    /// Value is true if the button is pressed, false if not
    /// </summary>
    Buttons: array of boolean;
    /// <summary>
    /// List of pressed buttons for this controller
    /// </summary>
    PressedButtons: array of TButtonID;
    /// <summary>
    /// DPad value between 0 (top) and 359,
    /// for center, the value is higher than 359
    /// </summary>
    DPad: word;
  end;

{$SCOPEDENUMS on}

  /// <summary>
  /// DPad standard values (from 0 to 365° and 65535 when not value is selected)
  /// </summary>
  TJoystickDPad = (Top = 0, TopRight = 45, RightTop = 45, Right = 90,
    BottomRight = 135, RightBottom = 135, Bottom = 180, BottomLeft = 225,
    LeftBottom = 225, Left = 270, LeftTop = 315, TopLeft = 315, Center = 65535);

  /// <summary>
  /// Platform service to access to joystick/gamepad controllers on a computer
  /// </summary>
  IGamolfJoystickService = interface(IInterface)
    ['{74BA65B4-B468-41E2-A5FF-1FB92A79E9F4}']
    /// <summary>
    /// Return the number of joysticks managed by the system
    /// </summary>
    function Count: byte;
    /// <summary>
    /// Return "true" if the JoystickID controller is connected and available
    /// </summary>
    function isConnected(JoystickID: TJoystickID): boolean;
    /// <summary>
    /// Return a TJoystick for the JoystickID controller
    /// </summary>
    procedure getInfo(JoystickID: TJoystickID; var Joystick: TJoystickInfo);
    /// <summary>
    /// Check if button "ButtonID" from controller "JoystickID" is pressed or not
    /// </summary>
    function isPressed(JoystickID: TJoystickID; ButtonID: TButtonID): boolean;
    /// <summary>
    /// Return X,Y axes values for JoystickID controller
    /// </summary>
    procedure getXY(JoystickID: TJoystickID; var x, y: single);
    /// <summary>
    /// Return X axes values for JoystickID controller
    /// </summary>
    function getX(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return Y axes values for JoystickID controller
    /// </summary>
    function getY(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return Z axes values for JoystickID controller
    /// </summary>
    function getZ(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return the DPad value between (0-359° or 65535)
    /// Compare it to Top, TopRight/RightTop, Right, BottomRight/RightBottom, Bottom, BottomLeft/LeftBottom, Left, LeftTop/TopLeft, Center values from TJoystickDPad enumeration
    /// </summary>
    function getDPad(JoystickID: TJoystickID): word;
    /// <summary>
    /// Check is the DPad / POV is in a standard position for a JoystickID or in general
    /// </summary>
    function isDPad(JoystickID: TJoystickID; JoystickDPad: TJoystickDPad)
      : boolean; overload;
    function isDPad(JoystickID: TJoystickID;
      JoystickDPads: array of TJoystickDPad): boolean; overload;
    function isDPad(DPad: word; JoystickDPad: TJoystickDPad): boolean; overload;
    function isDPad(DPad: word; JoystickDPads: array of TJoystickDPad)
      : boolean; overload;
    /// <summary>
    /// Get orientation (like DPad) from (x,y) axis
    /// </summary>
    function getDPadFromXY(x, y: single): TJoystickDPad;
    /// <summary>
    /// Get the values for (x,y) axis from a DPad/POV orientation
    /// </summary>
    procedure getXYFromDPad(DPad: word; var x, y: single);
  end;

  /// <summary>
  /// Platform service to access to joystick/gamepad controllers on a computer
  /// </summary>
  TGamolfCustomJoystickService = class(TInterfacedObject,
    IGamolfJoystickService)
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary>
    /// Return the number of joysticks managed by the system
    /// </summary>
    function Count: byte; virtual; abstract;
    /// <summary>
    /// Return "true" if the JoystickID controller is connected and available
    /// </summary>
    function isConnected(JoystickID: TJoystickID): boolean; virtual; abstract;
    /// <summary>
    /// Return a TJoystick for the JoystickID controller
    /// </summary>
    procedure getInfo(JoystickID: TJoystickID; var Joystick: TJoystickInfo);
      virtual; abstract;
    /// <summary>
    /// Check if button "ButtonID" from controller "JoystickID" is pressed or not
    /// </summary>
    function isPressed(JoystickID: TJoystickID; ButtonID: TButtonID): boolean;
    /// <summary>
    /// Return X,Y axes values for JoystickID controller
    /// </summary>
    procedure getXY(JoystickID: TJoystickID; var x, y: single);
    /// <summary>
    /// Return X axes values for JoystickID controller
    /// </summary>
    function getX(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return Y axes values for JoystickID controller
    /// </summary>
    function getY(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return Z axes values for JoystickID controller
    /// </summary>
    function getZ(JoystickID: TJoystickID): single;
    /// <summary>
    /// Return the DPad value between (0-359° or 65535)
    /// Compare it to Top, TopRight/RightTop, Right, BottomRight/RightBottom, Bottom, BottomLeft/LeftBottom, Left, LeftTop/TopLeft, Center values from TJoystickDPad enumeration
    /// </summary>
    function getDPad(JoystickID: TJoystickID): word;
    /// <summary>
    /// Check is the DPad / POV is in a standard position for a JoystickID or in general
    /// </summary>
    function isDPad(JoystickID: TJoystickID; JoystickDPad: TJoystickDPad)
      : boolean; overload;
    function isDPad(JoystickID: TJoystickID;
      JoystickDPads: array of TJoystickDPad): boolean; overload;
    function isDPad(DPad: word; JoystickDPad: TJoystickDPad): boolean; overload;
    function isDPad(DPad: word; JoystickDPads: array of TJoystickDPad)
      : boolean; overload;
    /// <summary>
    /// Get orientation (like DPad) from (x,y) axis
    /// </summary>
    function getDPadFromXY(x, y: single): TJoystickDPad;
    /// <summary>
    /// Get the values for (x,y) axis from a DPad/POV orientation
    /// </summary>
    procedure getXYFromDPad(DPad: word; var x, y: single);
  end;

implementation

{ TGamolfCustomJoystickService }

constructor TGamolfCustomJoystickService.Create;
begin
  //
end;

destructor TGamolfCustomJoystickService.Destroy;
begin
  //
  inherited;
end;

function TGamolfCustomJoystickService.getDPad(JoystickID: TJoystickID): word;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    result := Joystick.DPad;
end;

function TGamolfCustomJoystickService.getDPadFromXY(x, y: single)
  : TJoystickDPad;
var
  rx, ry: integer;
begin
  // from -1..1 as decimal to (-1, 0, 1)
  rx := round(x);
  ry := round(y);
  if (rx = 0) and (ry = -1) then
    result := TJoystickDPad.Top
  else if (rx = 1) and (ry = -1) then
    result := TJoystickDPad.TopRight
  else if (rx = 1) and (ry = 0) then
    result := TJoystickDPad.Right
  else if (rx = 1) and (ry = 1) then
    result := TJoystickDPad.BottomRight
  else if (rx = 0) and (ry = 1) then
    result := TJoystickDPad.Bottom
  else if (rx = -1) and (ry = 1) then
    result := TJoystickDPad.BottomLeft
  else if (rx = -1) and (ry = 0) then
    result := TJoystickDPad.Left
  else if (rx = -1) and (ry = -1) then
    result := TJoystickDPad.TopLeft
  else
    result := TJoystickDPad.Center;
end;

function TGamolfCustomJoystickService.getX(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    result := Joystick.Axes[0];
end;

procedure TGamolfCustomJoystickService.getXY(JoystickID: TJoystickID;
  var x, y: single);
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    x := Joystick.Axes[0];
  if (length(Joystick.Axes) > 1) then
    y := Joystick.Axes[1];
end;

procedure TGamolfCustomJoystickService.getXYFromDPad(DPad: word;
  var x, y: single);
begin
  if (isDPad(DPad, [TJoystickDPad.Left, TJoystickDPad.TopLeft,
    TJoystickDPad.BottomLeft])) then
    x := -1
  else if (isDPad(DPad, [TJoystickDPad.Right, TJoystickDPad.TopRight,
    TJoystickDPad.BottomRight])) then
    x := 1
  else
    x := 0;
  if (isDPad(DPad, [TJoystickDPad.TopLeft, TJoystickDPad.Top,
    TJoystickDPad.TopRight])) then
    y := -1
  else if (isDPad(DPad, [TJoystickDPad.BottomLeft, TJoystickDPad.Bottom,
    TJoystickDPad.BottomRight])) then
    y := 1
  else
    y := 0;
end;

function TGamolfCustomJoystickService.getY(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 1) then
    result := Joystick.Axes[1];
end;

function TGamolfCustomJoystickService.getZ(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 2) then
    result := Joystick.Axes[2];
end;

function TGamolfCustomJoystickService.isDPad(JoystickID: TJoystickID;
  JoystickDPad: TJoystickDPad): boolean;
begin
  result := isDPad(JoystickID, [JoystickDPad]);
end;

function TGamolfCustomJoystickService.isDPad(JoystickID: TJoystickID;
  JoystickDPads: array of TJoystickDPad): boolean;
var
  DPad: word;
  i: integer;
begin
  DPad := getDPad(JoystickID);
  result := isDPad(DPad, JoystickDPads);
end;

function TGamolfCustomJoystickService.isPressed(JoystickID: TJoystickID;
  ButtonID: TButtonID): boolean;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  result := (ButtonID >= 0) and (ButtonID < length(Joystick.Buttons)) and
    Joystick.Buttons[ButtonID];
end;

function TGamolfCustomJoystickService.isDPad(DPad: word;
  JoystickDPads: array of TJoystickDPad): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to length(JoystickDPads) - 1 do
    result := result or (DPad = ord(JoystickDPads[i]));
end;

function TGamolfCustomJoystickService.isDPad(DPad: word;
  JoystickDPad: TJoystickDPad): boolean;
begin
  result := isDPad(DPad, [JoystickDPad]);
end;

end.
