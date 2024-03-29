unit Gamolf.RTL.Joystick;

{
  Acc�s aux contr�leurs de jeu et � leurs informations.

  Logiciel open source distribu� sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Pr�martin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

uses System.SysUtils;

type
{$SCOPEDENUMS on}
  /// <summary>
  /// ID for game controllers buttons (when it's know by the API)
  /// </summary>
  TJoystickButtons = (A, B, X, Y, Home, Options, Menu, LeftShoulder,
    RightShoulder, LeftTrigger, RightTrigger, LeftThumbStick, RightThumbStick);

  /// <summary>
  /// DPad standard values (from 0 to 365� and 65535 when not value is selected)
  /// </summary>
  TJoystickDPad = (Top = 0, TopRight = 45, RightTop = 45, Right = 90,
    BottomRight = 135, RightBottom = 135, Bottom = 180, BottomLeft = 225,
    LeftBottom = 225, Left = 270, LeftTop = 315, TopLeft = 315, Center = 65535);

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
    /// 0 => X, 1 => Y,
    /// 2 => Z, 3 => R,
    /// 4 => U, 5 => V
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
    /// <summary>
    /// Check if a button is pressed by it's name (for compatible platforms)
    /// </summary>
    function isPressed(Button: TJoystickButtons): boolean;
    /// <summary>
    /// Used by platforms compatibles to set pressed buttons
    /// </summary>
    procedure setPressed(Button: TJoystickButtons; isPressed: boolean);
    /// <summary>
    /// Initialize the buttons list depending on TJoystickButtons list
    /// </summary>
    procedure initButtonsToJoystickButtons;
  end;

  /// <summary>
  /// Signature for a callback procedure used to get game controller infos
  /// </summary>
  TJoystickInfosCallbackProc = reference to procedure(JoystickID: TJoystickID;
    var JoystickInfo: TJoystickInfo; hadError: boolean);
  /// <summary>
  /// Signature for a callback method used to get game controller infos
  /// </summary>
  TJoystickInfosCallbackEvent = procedure(JoystickID: TJoystickID;
    var JoystickInfo: TJoystickInfo; hadError: boolean) of object;

  /// <summary>
  /// Signature for a callback procedure used to get connected game controller infos
  /// </summary>
  TJoystickInfosConnectedCallbackProc = reference to procedure
    (JoystickID: TJoystickID; var JoystickInfo: TJoystickInfo);
  /// <summary>
  /// Signature for a callback method used to get connected game controller infos
  /// </summary>
  TJoystickInfosConnectedCallbackEvent = procedure(JoystickID: TJoystickID;
    var JoystickInfo: TJoystickInfo) of object;

  /// <summary>
  /// Signature for a callback procedure used to signal an error for the game controller JoystickID
  /// </summary>
  TJoystickErrorCallbackProc = reference to procedure(JoystickID: TJoystickID);
  /// <summary>
  /// Signature for a callback method used to signal an error for the game controller JoystickID
  /// </summary>
  TJoystickErrorCallbackEvent = procedure(JoystickID: TJoystickID) of object;

  /// <summary>
  /// Platform service to access to joystick/gamepad controllers on a computer
  /// </summary>
  IGamolfJoystickService = interface(IInterface)
    ['{74BA65B4-B468-41E2-A5FF-1FB92A79E9F4}']
    /// <summary>
    /// Scan for available devices (if some are already used, their ID could change)
    /// </summary>
    procedure StartDiscovery;
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
    procedure getXY(JoystickID: TJoystickID; var X, Y: single);
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
    /// Return the DPad value between (0-359� or 65535)
    /// Compare it to Top, TopRight/RightTop, Right, BottomRight/RightBottom, Bottom, BottomLeft/LeftBottom, Left, LeftTop/TopLeft, Center values from TJoystickDPad enumeration
    /// </summary>
    function getDPad(JoystickID: TJoystickID;
      FromXYWhenNoDPadAvailable: boolean = false): word;
    /// <summary>
    /// Check if the game controller has a DPad/POV button
    /// </summary>
    function hasDPad(JoystickID: TJoystickID): boolean;
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
    function getDPadFromXY(X, Y: single): word;
    /// <summary>
    /// Get the values for (x,y) axis from a DPad/POV orientation
    /// </summary>
    procedure getXYFromDPad(DPad: word; var X, Y: single);
    /// <summary>
    /// Loop on all game controllers and call the procedure with infos for each one
    /// </summary>
    procedure ForEach(var JoystickInfo: TJoystickInfo;
      CallbackProc: TJoystickInfosCallbackProc); overload;
    /// <summary>
    /// Loop on all game controllers and call the method with infos for each one
    /// </summary>
    procedure ForEach(var JoystickInfo: TJoystickInfo;
      CallbackEvent: TJoystickInfosCallbackEvent); overload;
    /// <summary>
    /// Loop on all game controllers and call the procedure with infos for each connected device
    /// </summary>
    procedure ForEachConnectedDevice(var JoystickInfo: TJoystickInfo;
      CallbackProc: TJoystickInfosConnectedCallbackProc;
      ErrorCallbackProc: TJoystickErrorCallbackProc = nil); overload;
    /// <summary>
    /// Loop on all game controllers and call the method with infos for each connected device
    /// </summary>
    procedure ForEachConnectedDevice(var JoystickInfo: TJoystickInfo;
      CallbackEvent: TJoystickInfosConnectedCallbackEvent;
      ErrorCallbackEvent: TJoystickErrorCallbackEvent = nil); overload;
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
    /// Reset Joystick structure
    /// </summary>
    procedure initJoystick(var Joystick: TJoystickInfo); virtual;
    /// <summary>
    /// Scan for availale devices (if some are already used, their ID could change)
    /// </summary>
    procedure StartDiscovery; virtual; abstract;
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
    procedure getXY(JoystickID: TJoystickID; var X, Y: single);
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
    /// Return the DPad value between (0-359� or 65535)
    /// Compare it to Top, TopRight/RightTop, Right, BottomRight/RightBottom, Bottom, BottomLeft/LeftBottom, Left, LeftTop/TopLeft, Center values from TJoystickDPad enumeration
    /// </summary>
    function getDPad(JoystickID: TJoystickID;
      FromXYWhenNoDPadAvailable: boolean = false): word;
    /// <summary>
    /// Check if the game controller has a DPad/POV button
    /// </summary>
    function hasDPad(JoystickID: TJoystickID): boolean; virtual; abstract;
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
    function getDPadFromXY(X, Y: single): word;
    /// <summary>
    /// Get the values for (x,y) axis from a DPad/POV orientation
    /// </summary>
    procedure getXYFromDPad(DPad: word; var X, Y: single);
    /// <summary>
    /// Loop on all game controllers and call the procedure with infos for each one
    /// </summary>
    procedure ForEach(var JoystickInfo: TJoystickInfo;
      CallbackProc: TJoystickInfosCallbackProc); overload;
    /// <summary>
    /// Loop on all game controllers and call the method with infos for each one
    /// </summary>
    procedure ForEach(var JoystickInfo: TJoystickInfo;
      CallbackEvent: TJoystickInfosCallbackEvent); overload;
    /// <summary>
    /// Loop on all game controllers and call the procedure with infos for each connected device
    /// </summary>
    procedure ForEachConnectedDevice(var JoystickInfo: TJoystickInfo;
      CallbackProc: TJoystickInfosConnectedCallbackProc;
      ErrorCallbackProc: TJoystickErrorCallbackProc = nil); overload;
    /// <summary>
    /// Loop on all game controllers and call the method with infos for each connected device
    /// </summary>
    procedure ForEachConnectedDevice(var JoystickInfo: TJoystickInfo;
      CallbackEvent: TJoystickInfosConnectedCallbackEvent;
      ErrorCallbackEvent: TJoystickErrorCallbackEvent = nil); overload;
    /// <summary>
    /// Override this function and return true for platforms where buttons place are known.
    /// By default, it's false;
    /// </summary>
    function hasJoystickButtonsAPI: boolean; virtual;
  end;

implementation

{ TGamolfCustomJoystickService }

constructor TGamolfCustomJoystickService.Create;
begin
  StartDiscovery;
end;

destructor TGamolfCustomJoystickService.Destroy;
begin
  //
  inherited;
end;

procedure TGamolfCustomJoystickService.ForEach(var JoystickInfo: TJoystickInfo;
  CallbackProc: TJoystickInfosCallbackProc);
var
  i: integer;
  hadError: boolean;
begin
  for i := 0 to Count - 1 do
  begin
    try
      getInfo(i, JoystickInfo);
      hadError := false;
    except
      hadError := true;
      initJoystick(JoystickInfo);
    end;
    if assigned(CallbackProc) then
      CallbackProc(i, JoystickInfo, hadError);
  end;
end;

procedure TGamolfCustomJoystickService.ForEach(var JoystickInfo: TJoystickInfo;
  CallbackEvent: TJoystickInfosCallbackEvent);
begin
  ForEach(JoystickInfo,
    procedure(JoystickID: TJoystickID; var JoystickInfo: TJoystickInfo;
      hadError: boolean)
    begin
      CallbackEvent(JoystickID, JoystickInfo, hadError);
    end);
end;

procedure TGamolfCustomJoystickService.ForEachConnectedDevice(var JoystickInfo
  : TJoystickInfo; CallbackProc: TJoystickInfosConnectedCallbackProc;
ErrorCallbackProc: TJoystickErrorCallbackProc);
var
  i: integer;
  hadError: boolean;
begin
  for i := 0 to Count - 1 do
    if (isConnected(i)) then
    begin
      try
        getInfo(i, JoystickInfo);
        hadError := false;
      except
        hadError := true;
      end;
      if (not hadError) and assigned(CallbackProc) then
        CallbackProc(i, JoystickInfo)
      else if (hadError) and assigned(ErrorCallbackProc) then
        ErrorCallbackProc(i);
    end;
end;

procedure TGamolfCustomJoystickService.ForEachConnectedDevice(var JoystickInfo
  : TJoystickInfo; CallbackEvent: TJoystickInfosConnectedCallbackEvent;
ErrorCallbackEvent: TJoystickErrorCallbackEvent);
begin
  ForEachConnectedDevice(JoystickInfo,
    procedure(JoystickID: TJoystickID; var JoystickInfo: TJoystickInfo)
    begin
      CallbackEvent(JoystickID, JoystickInfo);
    end,
    procedure(JoystickID: TJoystickID)
    begin
      if assigned(ErrorCallbackEvent) then
        ErrorCallbackEvent(JoystickID);
    end);
end;

function TGamolfCustomJoystickService.getDPad(JoystickID: TJoystickID;
FromXYWhenNoDPadAvailable: boolean): word;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if hasDPad(JoystickID) then
    result := Joystick.DPad
  else if FromXYWhenNoDPadAvailable then
    result := getDPadFromXY(Joystick.Axes[0], Joystick.Axes[1])
  else
    result := ord(TJoystickDPad.Center);
end;

function TGamolfCustomJoystickService.getDPadFromXY(X, Y: single): word;
var
  rx, ry: integer;
begin
  // from -1..1 as decimal to (-1, 0, 1)
  rx := round(X);
  ry := round(Y);
  if (rx = 0) and (ry = -1) then
    result := ord(TJoystickDPad.Top)
  else if (rx = 1) and (ry = -1) then
    result := ord(TJoystickDPad.TopRight)
  else if (rx = 1) and (ry = 0) then
    result := ord(TJoystickDPad.Right)
  else if (rx = 1) and (ry = 1) then
    result := ord(TJoystickDPad.BottomRight)
  else if (rx = 0) and (ry = 1) then
    result := ord(TJoystickDPad.Bottom)
  else if (rx = -1) and (ry = 1) then
    result := ord(TJoystickDPad.BottomLeft)
  else if (rx = -1) and (ry = 0) then
    result := ord(TJoystickDPad.Left)
  else if (rx = -1) and (ry = -1) then
    result := ord(TJoystickDPad.TopLeft)
  else
    result := ord(TJoystickDPad.Center);
end;

function TGamolfCustomJoystickService.getX(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    result := Joystick.Axes[0]
  else
    result := 0;
end;

procedure TGamolfCustomJoystickService.getXY(JoystickID: TJoystickID;
var X, Y: single);
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 0) then
    X := Joystick.Axes[0];
  if (length(Joystick.Axes) > 1) then
    Y := Joystick.Axes[1];
end;

procedure TGamolfCustomJoystickService.getXYFromDPad(DPad: word;
var X, Y: single);
begin
  if (isDPad(DPad, [TJoystickDPad.Left, TJoystickDPad.TopLeft,
    TJoystickDPad.BottomLeft])) then
    X := -1
  else if (isDPad(DPad, [TJoystickDPad.Right, TJoystickDPad.TopRight,
    TJoystickDPad.BottomRight])) then
    X := 1
  else
    X := 0;
  if (isDPad(DPad, [TJoystickDPad.TopLeft, TJoystickDPad.Top,
    TJoystickDPad.TopRight])) then
    Y := -1
  else if (isDPad(DPad, [TJoystickDPad.BottomLeft, TJoystickDPad.Bottom,
    TJoystickDPad.BottomRight])) then
    Y := 1
  else
    Y := 0;
end;

function TGamolfCustomJoystickService.getY(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 1) then
    result := Joystick.Axes[1]
  else
    result := 0;
end;

function TGamolfCustomJoystickService.getZ(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) > 2) then
    result := Joystick.Axes[2]
  else
    result := 0;
end;

function TGamolfCustomJoystickService.hasJoystickButtonsAPI: boolean;
begin
  result := false;
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

procedure TGamolfCustomJoystickService.initJoystick(var Joystick
  : TJoystickInfo);
var
  i: integer;
begin
  for i := 0 to length(Joystick.Axes) - 1 do
    Joystick.Axes[i] := 0;
  if hasJoystickButtonsAPI then
    Joystick.initButtonsToJoystickButtons
  else
  begin
    for i := 0 to length(Joystick.Buttons) - 1 do
      Joystick.Buttons[i] := false;
    setlength(Joystick.PressedButtons, 0);
  end;
  Joystick.DPad := ord(TJoystickDPad.Center);
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

{ TJoystickInfo }

procedure TJoystickInfo.initButtonsToJoystickButtons;
const
  CNBButtons = ord(high(TJoystickButtons)) + 1;
var
  i: integer;
begin
  if (length(Buttons) <> CNBButtons) then
    setlength(Buttons, CNBButtons);
  for i := 0 to CNBButtons - 1 do
    Buttons[i] := false;
  setlength(PressedButtons, 0);
end;

function TJoystickInfo.isPressed(Button: TJoystickButtons): boolean;
var
  idx: integer;
begin
  idx := ord(Button);
  if (idx >= 0) and (idx < length(Buttons)) then
    result := Buttons[idx]
  else
    result := false;
end;

procedure TJoystickInfo.setPressed(Button: TJoystickButtons;
isPressed: boolean);
var
  idx: integer;
  i: integer;
  found: boolean;
begin
  idx := ord(Button);
  if (idx >= 0) and (idx < length(Buttons)) and (Buttons[idx] <> isPressed) then
  begin
    Buttons[idx] := isPressed;
    if isPressed then
    begin
      setlength(PressedButtons, length(PressedButtons) + 1);
      PressedButtons[length(PressedButtons) - 1] := idx;
    end
    else
    begin
      i := 0;
      found := false;
      while (i < length(PressedButtons)) do
      begin
        if (PressedButtons[i] = idx) then
          found := true
        else if found then
          PressedButtons[i - 1] := PressedButtons[i];
      end;
      if found then
        setlength(PressedButtons, length(PressedButtons) - 1);
    end;
  end;
end;

end.
