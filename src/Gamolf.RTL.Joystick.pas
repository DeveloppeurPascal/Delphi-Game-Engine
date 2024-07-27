unit Gamolf.RTL.Joystick;

{
  Accès aux contrôleurs de jeu et à leurs informations.

  Logiciel open source distribué sous licence AGPL.
  Open source software distributed under the AGPL license

  Copyright Patrick Prémartin

  Find the original source code on
  https://github.com/DeveloppeurPascal/Delphi-Game-Engine
}
interface

uses
  System.Generics.Collections,
  System.Classes,
  System.SysUtils;

type
{$SCOPEDENUMS on}
  TGamepadDevice = class;
  TGamepadDeviceDict = class;
  TGamepad = class;
  TGamepadList = class;
  TGamepadManager = class;
  TGamepadManagerList = class;

  /// <summary>
  /// ID for game controllers buttons (when it's know by the API)
  /// </summary>
  TJoystickButtons = (A, B, X, Y, Home, Options, Menu, LeftShoulder,
    RightShoulder, LeftThumbStick, RightThumbStick, LeftTrigger, RightTrigger);
  TJoystickButtonsSet = set of TJoystickButtons;

  /// <summary>
  /// DPad standard values (from 0 to 365° and 65535 when not value is selected)
  /// </summary>
  TJoystickDPad = (Top = 0, TopRight = 45, RightTop = 45, Right = 90,
    BottomRight = 135, RightBottom = 135, Bottom = 180, BottomLeft = 225,
    LeftBottom = 225, Left = 270, LeftTop = 315, TopLeft = 315, Center = 65535);

  TJoystickAxes = (LeftStickX = 0, X = 0, LeftStickY = 1, Y = 1,
    RightStickX = 2, U = 2, RightStickY = 3, R = 3, LeftTrigger = 4, Z = 4,
    RightTrigger = 5, V = 5);
  TJoystickAxesSet = set of TJoystickAxes;

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

  TOnNewGamepadDetected = procedure(const GamepadID: integer) of object;
  TOnGamepadLost = procedure(const GamepadID: integer) of object;
  TOnGamepadButtonUp = procedure(const GamepadID: integer;
    const Button: TJoystickButtons) of object;
  TOnGamepadButtonDown = procedure(const GamepadID: integer;
    const Button: TJoystickButtons) of object;
  TOnGamepadAxesChange = procedure(const GamepadID: integer;
    const Axe: TJoystickAxes; const Value: single) of object;
  TOnGamepadDirectionPadChange = procedure(const GamepadID: integer;
    const Value: TJoystickDPad) of object;

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
    /// Return the DPad value between (0-359° or 65535)
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
    /// <summary>
    /// Returns true for platforms where buttons place are known and mapped correctly to TJoystickButtons enumeration.
    /// If false, you can use buttons ID to check if they are pressed or not.
    /// </summary>
    function hasJoystickButtonsAPI: boolean;
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
    /// Return the DPad value between (0-359° or 65535)
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
    /// <remarks>
    /// If "true" use TJoystickButtons enumeration to check if a button is pressed.
    /// If "false" use buttons ID to check if a button is pressed.
    /// </remarks>
    function hasJoystickButtonsAPI: boolean; virtual;
  end;

  /// <summary>
  /// Gamepad manager class, to use as a singleton.
  /// </summary>
  TGamepadDevicesManager = class(TInterfacedObject)
  private
    class var FGamepadManager: TGamepadDevicesManager;

  var
    /// <summary>
    /// List of gamepad datas
    /// </summary>
    FGamepads: TGamepadDeviceDict;
    /// <summary>
    /// List of TGamepadManager components
    /// </summary>
    FManagers: TGamepadManagerList;
    /// <summary>
    /// Access to the joystick interface
    /// </summary>
    FGamolfJoystickService: IGamolfJoystickService;
    /// <summary>
    /// Activate or stop the gamepad infos check loop
    /// </summary>
    FEnabled: boolean;
    /// <summary>
    /// To know if the thread loop is running
    /// </summary>
    FLoopIsRunning: boolean;
    /// <summary>
    /// To check the thread status and terminate it if needed
    /// </summary>
    FLoopThread: TThread;
    FOnNewGamepadDetected: TOnNewGamepadDetected;
    FOnGamepadLost: TOnGamepadLost;
    FOnGamepadDirectionPadChange: TOnGamepadDirectionPadChange;
    FOnGamepadAxesChange: TOnGamepadAxesChange;
    FOnGamepadButtonDown: TOnGamepadButtonDown;
    FOnGamepadButtonUp: TOnGamepadButtonUp;
    FSynchronizedEvents: boolean;
    FTagBool: boolean;
    FTagFloat: single;
    FTagString: string;
    FTagObject: TObject;
    FTag: integer;
    procedure SetTag(const Value: integer);
    procedure SetTagBool(const Value: boolean);
    procedure SetTagFloat(const Value: single);
    procedure SetTagObject(const Value: TObject);
    procedure SetTagString(const Value: string);
    procedure SetSynchronizedEvents(const Value: boolean);
    procedure SetOnGamepadAxesChange(const Value: TOnGamepadAxesChange);
    procedure SetOnGamepadButtonDown(const Value: TOnGamepadButtonDown);
    procedure SetOnGamepadButtonUp(const Value: TOnGamepadButtonUp);
    procedure SetOnGamepadDirectionPadChange(const Value
      : TOnGamepadDirectionPadChange);
    procedure SetOnGamepadLost(const Value: TOnGamepadLost);
    procedure SetOnNewGamepadDetected(const Value: TOnNewGamepadDetected);
    function GetIsSupported: boolean;
    procedure SetEnabled(const Value: boolean);
  protected
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure RegisterGamePadDevice(const Gamepad: TGamepadDevice);
    procedure UnRegisterGamePadDevice(const Gamepad: TGamepadDevice);
    procedure RegisterGamePadManager(const Manager: TGamepadManager);
    procedure UnRegisterGamePadManager(const Manager: TGamepadManager);
    procedure DoNewGamepadDetected(const AGamepadID: integer);
    procedure DoLostGamepad(const AGamepadID: integer);
    procedure DoGamepadDirectionPadChange(const AGamepadID: integer;
      const AValue: TJoystickDPad);
    procedure DoGamepadAxesChange(const AGamepadID: integer;
      const AAxe: TJoystickAxes; const AValue: single);
    procedure DoGamepadButtonDown(const AGamepadID: integer;
      const AButton: TJoystickButtons);
    procedure DoGamepadButtonUp(const AGamepadID: integer;
      const AButton: TJoystickButtons);
    function IsGamepadConnected(const GamepadID: integer): boolean;
  public
    /// <summary>
    /// Check if the gamepad API is available for this platform
    /// </summary>
    property IsSupported: boolean read GetIsSupported;
    /// <summary>
    /// Start or stop the gamepad infos check loop
    /// </summary>
    property Enabled: boolean read FEnabled write SetEnabled;
    /// <summary>
    /// Execute events in main thread or in the thread used by the gamepad manager
    /// </summary>
    property SynchronizedEvents: boolean read FSynchronizedEvents
      write SetSynchronizedEvents;
    /// <summary>
    /// Used when a new gamepad is detected (it should be connected but can be not).
    /// </summary>
    property OnNewGamepadDetected: TOnNewGamepadDetected
      read FOnNewGamepadDetected write SetOnNewGamepadDetected;
    /// <summary>
    /// To know if a connected gamepad is disconnected from the system or powered off.
    /// </summary>
    property OnGamepadLost: TOnGamepadLost read FOnGamepadLost
      write SetOnGamepadLost;
    /// <summary>
    /// Called when a gamepad button is up (unpressed)
    /// </summary>
    property OnGamepadButtonUp: TOnGamepadButtonUp read FOnGamepadButtonUp
      write SetOnGamepadButtonUp;
    /// <summary>
    /// Called when a gamepad button is down (pressed)
    /// </summary>
    property OnGamepadButtonDown: TOnGamepadButtonDown read FOnGamepadButtonDown
      write SetOnGamepadButtonDown;
    /// <summary>
    /// Called for each new value of a gamepad axe (X,Y or others)
    /// </summary>
    property OnGamepadAxesChange: TOnGamepadAxesChange read FOnGamepadAxesChange
      write SetOnGamepadAxesChange;
    /// <summary>
    /// Called for each direction change from a gamepad DPAD (if available on it)
    /// </summary>
    property OnGamepadDirectionPadChange: TOnGamepadDirectionPadChange
      read FOnGamepadDirectionPadChange write SetOnGamepadDirectionPadChange;
    class function Current: TGamepadDevicesManager;
{$IF CompilerVersion>33}
    class constructor Create;
{$ENDIF}
{$IF CompilerVersion>33}
    class destructor Destroy;
{$ENDIF}
    /// <summary>
    /// Return the gamepad data class
    /// </summary>
    function GetGamepad(const AID: integer): TGamepadDevice;
    /// <summary>
    /// Count the detected gamepads number (detected or declared in the OS depending on the platform)
    /// </summary>
    function GamepadCount: integer;
    /// <summary>
    /// Return the connected gamepads number
    /// </summary>
    function ConnectedGamepadCount: integer;
    /// <summary>
    /// Tag property "in case of" not used in this class
    /// </summary>
    property Tag: integer read FTag write SetTag;
    /// <summary>
    /// TagBool property "in case of" not used in this class
    /// </summary>
    property TagBool: boolean read FTagBool write SetTagBool;
    /// <summary>
    /// TagFloat property "in case of" not used in this class
    /// </summary>
    property TagFloat: single read FTagFloat write SetTagFloat;
    /// <summary>
    /// TagObject property "in case of" not used in this class
    /// </summary>
    property TagObject: TObject read FTagObject write SetTagObject;
    /// <summary>
    /// TagString property "in case of" not used in this class
    /// </summary>
    property TagString: string read FTagString write SetTagString;
  end;

  /// <summary>
  /// Gamepad manager component
  /// </summary>
{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TGamepadManager = class(TComponent)
  private
    FTagBool: boolean;
    FTagFloat: single;
    FTagString: string;
    FTagObject: TObject;
    FEnabled: boolean;
    FOnGamepadButtonUp: TOnGamepadButtonUp;
    FOnGamepadDirectionPadChange: TOnGamepadDirectionPadChange;
    FOnNewGamepadDetected: TOnNewGamepadDetected;
    FOnGamepadAxesChange: TOnGamepadAxesChange;
    FOnGamepadButtonDown: TOnGamepadButtonDown;
    FOnGamepadLost: TOnGamepadLost;
    FSynchronizedEvents: boolean;
    function GetIsSupported: boolean;
    procedure SetEnabled(const Value: boolean);
    procedure SetOnGamepadLost(const Value: TOnGamepadLost);
    procedure SetOnNewGamepadDetected(const Value: TOnNewGamepadDetected);
    procedure SetOnGamepadAxesChange(const Value: TOnGamepadAxesChange);
    procedure SetOnGamepadButtonDown(const Value: TOnGamepadButtonDown);
    procedure SetOnGamepadButtonUp(const Value: TOnGamepadButtonUp);
    procedure SetOnGamepadDirectionPadChange(const Value
      : TOnGamepadDirectionPadChange);
    procedure SetSynchronizedEvents(const Value: boolean);
    procedure SetTagBool(const Value: boolean);
    procedure SetTagFloat(const Value: single);
    procedure SetTagObject(const Value: TObject);
    procedure SetTagString(const Value: string);
  protected
  public
    property IsSupported: boolean read GetIsSupported;
    /// <summary>
    /// Return the gamepad data class
    /// </summary>
    function GetGamepad(const AID: integer): TGamepadDevice;
    /// <summary>
    /// Count the detected gamepads number (detected or declared in the OS depending on the platform)
    /// </summary>
    function GamepadCount: integer;
    /// <summary>
    /// Return the connected gamepads number
    /// </summary>
    function ConnectedGamepadCount: integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoNewGamepadDetected(const AGamepadID: integer);
    procedure DoLostGamepad(const AGamepadID: integer);
    procedure DoGamepadDirectionPadChange(const AGamepadID: integer;
      const AValue: TJoystickDPad);
    procedure DoGamepadAxesChange(const AGamepadID: integer;
      const AAxe: TJoystickAxes; const AValue: single);
    procedure DoGamepadButtonDown(const AGamepadID: integer;
      const AButton: TJoystickButtons);
    procedure DoGamepadButtonUp(const AGamepadID: integer;
      const AButton: TJoystickButtons);
  published
    /// <summary>
    /// Execute events in main thread or in the thread used by the gamepad manager
    /// </summary>
    property SynchronizedEvents: boolean read FSynchronizedEvents
      write SetSynchronizedEvents default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property OnNewGamepadDetected: TOnNewGamepadDetected
      read FOnNewGamepadDetected write SetOnNewGamepadDetected;
    property OnGamepadLost: TOnGamepadLost read FOnGamepadLost
      write SetOnGamepadLost;
    property OnButtonUp: TOnGamepadButtonUp read FOnGamepadButtonUp
      write SetOnGamepadButtonUp;
    property OnButtonDown: TOnGamepadButtonDown read FOnGamepadButtonDown
      write SetOnGamepadButtonDown;
    property OnAxesChange: TOnGamepadAxesChange read FOnGamepadAxesChange
      write SetOnGamepadAxesChange;
    property OnDirectionPadChange: TOnGamepadDirectionPadChange
      read FOnGamepadDirectionPadChange write SetOnGamepadDirectionPadChange;
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

  TGamepadManagerList = class(TList<TGamepadManager>)
  end;

  /// <summary>
  /// Gamepad class to access data from each gamepad detected on the system
  /// </summary>
  TGamepadDevice = class(TInterfacedObject)
  private
    /// <summary>
    /// List of gamepad components for this ID
    /// </summary>
    FGamepads: TGamepadList;
    FID: integer;
    FEnabled: boolean;
    FOnDirectionPadChange: TOnGamepadDirectionPadChange;
    FOnAxesChange: TOnGamepadAxesChange;
    FOnButtonDown: TOnGamepadButtonDown;
    FOnButtonUp: TOnGamepadButtonUp;
    FOnLost: TOnGamepadLost;
    FJoystickInfo: TJoystickInfo;
    FIsConnected: boolean;
    FhasDPAD: boolean;
    FSynchronizedEvents: boolean;
    FTagBool: boolean;
    FTagFloat: single;
    FTagString: string;
    FTagObject: TObject;
    FTag: integer;
    function GetIsSupported: boolean;
    procedure SetEnabled(const Value: boolean);
    procedure SetOnAxesChange(const Value: TOnGamepadAxesChange);
    procedure SetOnButtonDown(const Value: TOnGamepadButtonDown);
    procedure SetOnButtonUp(const Value: TOnGamepadButtonUp);
    procedure SetOnDirectionPadChange(const Value
      : TOnGamepadDirectionPadChange);
    procedure SetOnLost(const Value: TOnGamepadLost);
    procedure SetIsConnected(const Value: boolean);
    function GetIsConnected: boolean;
    function GetAxes(const AxeID: TJoystickAxes): single;
    function GetButtons(const ButtonID: TJoystickButtons): boolean;
    function getDPad: TJoystickDPad;
    procedure SetSynchronizedEvents(const Value: boolean);
    procedure SetTag(const Value: integer);
    procedure SetTagBool(const Value: boolean);
    procedure SetTagFloat(const Value: single);
    procedure SetTagObject(const Value: TObject);
    procedure SetTagString(const Value: string);
  protected
    procedure SetNewJoystickInfo(const NewJoystickInfo: TJoystickInfo);
    procedure DoAxeChanged(const AAxeID: integer);
    procedure DoButtonChanged(const AButtonID: integer);
    procedure DoDirectionPadChanged;
    procedure DoLost;
    procedure RegisterGamePadComponent(const Gamepad: TGamepad);
    procedure UnRegisterGamePadComponent(const Gamepad: TGamepad);
  public
    property ID: integer read FID;
    property IsSupported: boolean read GetIsSupported;
    property Enabled: boolean read FEnabled write SetEnabled;
    property isConnected: boolean read GetIsConnected write SetIsConnected;
    property hasDPad: boolean read FhasDPAD;
    property Buttons[const ButtonID: TJoystickButtons]: boolean read GetButtons;
    property Axes[const AxeID: TJoystickAxes]: single read GetAxes;
    property DPad: TJoystickDPad read getDPad;
    /// <summary>
    /// Execute events in main thread or in the thread used by the gamepad manager
    /// </summary>
    property SynchronizedEvents: boolean read FSynchronizedEvents
      write SetSynchronizedEvents;
    property OnGamepadButtonUp: TOnGamepadButtonUp read FOnButtonUp
      write SetOnButtonUp;
    property OnGamepadButtonDown: TOnGamepadButtonDown read FOnButtonDown
      write SetOnButtonDown;
    property OnGamepadAxesChange: TOnGamepadAxesChange read FOnAxesChange
      write SetOnAxesChange;
    property OnGamepadDirectionPadChange: TOnGamepadDirectionPadChange
      read FOnDirectionPadChange write SetOnDirectionPadChange;
    property OnGamepadLost: TOnGamepadLost read FOnLost write SetOnLost;
    constructor Create(const AID: integer);
    destructor Destroy; override;
    /// <summary>
    /// Tag property "in case of" not used in this class
    /// </summary>
    property Tag: integer read FTag write SetTag;
    /// <summary>
    /// TagBool property "in case of" not used in this class
    /// </summary>
    property TagBool: boolean read FTagBool write SetTagBool;
    /// <summary>
    /// TagFloat property "in case of" not used in this class
    /// </summary>
    property TagFloat: single read FTagFloat write SetTagFloat;
    /// <summary>
    /// TagObject property "in case of" not used in this class
    /// </summary>
    property TagObject: TObject read FTagObject write SetTagObject;
    /// <summary>
    /// TagString property "in case of" not used in this class
    /// </summary>
    property TagString: string read FTagString write SetTagString;
  end;

  TGamepadDeviceDict = class(TObjectDictionary<integer, TGamepadDevice>)
  end;

  /// <summary>
  /// Gamepad component
  /// </summary>
{$IF CompilerVersion >= 33.0}
  [ComponentPlatformsAttribute(pfidWindows or pfidOSX or pfidLinux or
    pfidAndroid or pfidiOS)]
{$ELSE}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidOSX64 or
    pidiOSSimulator or pidiOSDevice32 or pidiOSDevice64 or pidAndroid or
    pidAndroid64 or pidLinux64)]
{$ENDIF}

  TGamepad = class(TComponent)
  private
    FID: integer;
    FEnabled: boolean;
    FOnDirectionPadChange: TOnGamepadDirectionPadChange;
    FOnAxesChange: TOnGamepadAxesChange;
    FOnButtonDown: TOnGamepadButtonDown;
    FOnButtonUp: TOnGamepadButtonUp;
    FOnLost: TOnGamepadLost;
    FhasDPAD: boolean;
    FSynchronizedEvents: boolean;
    FTagBool: boolean;
    FTagFloat: single;
    FTagString: string;
    FTagObject: TObject;
    procedure SetID(const Value: integer);
    function GetIsSupported: boolean;
    procedure SetEnabled(const Value: boolean);
    procedure SetOnAxesChange(const Value: TOnGamepadAxesChange);
    procedure SetOnButtonDown(const Value: TOnGamepadButtonDown);
    procedure SetOnButtonUp(const Value: TOnGamepadButtonUp);
    procedure SetOnDirectionPadChange(const Value
      : TOnGamepadDirectionPadChange);
    procedure SetOnLost(const Value: TOnGamepadLost);
    function GetIsConnected: boolean;
    function GetAxes(const AxeID: TJoystickAxes): single;
    function GetButtons(const ButtonID: TJoystickButtons): boolean;
    function getDPad: TJoystickDPad;
    procedure SetSynchronizedEvents(const Value: boolean);
    procedure SetTagBool(const Value: boolean);
    procedure SetTagFloat(const Value: single);
    procedure SetTagObject(const Value: TObject);
    procedure SetTagString(const Value: string);
  protected
    function getGamepadData: TGamepadDevice;
    procedure DoAxeChanged(const AAxeID: integer);
    procedure DoButtonChanged(const AButtonID: integer);
    procedure DoDirectionPadChanged;
    procedure DoLost;
  public
    property IsSupported: boolean read GetIsSupported;
    property isConnected: boolean read GetIsConnected;
    property hasDPad: boolean read FhasDPAD;
    property Buttons[const ButtonID: TJoystickButtons]: boolean read GetButtons;
    property Axes[const AxeID: TJoystickAxes]: single read GetAxes;
    property DPad: TJoystickDPad read getDPad;
    constructor Create(AOwner: TComponent); override;
  published
    property ID: integer read FID write SetID default -1;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    /// <summary>
    /// Execute events in main thread or in the thread used by the gamepad manager
    /// </summary>
    property SynchronizedEvents: boolean read FSynchronizedEvents
      write SetSynchronizedEvents default false;
    property OnButtonUp: TOnGamepadButtonUp read FOnButtonUp
      write SetOnButtonUp;
    property OnButtonDown: TOnGamepadButtonDown read FOnButtonDown
      write SetOnButtonDown;
    property OnAxesChange: TOnGamepadAxesChange read FOnAxesChange
      write SetOnAxesChange;
    property OnDirectionPadChange: TOnGamepadDirectionPadChange
      read FOnDirectionPadChange write SetOnDirectionPadChange;
    property OnLost: TOnGamepadLost read FOnLost write SetOnLost;
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

  TGamepadList = class(TList<TGamepad>)
  end;

procedure Register;

implementation

{$IF Defined(FRAMEWORK_FMX)}

uses
{$IF Defined(DEBUG)}
  FMX.Types,
{$ENDIF}
  FMX.Platform,
  Gamolf.FMX.Joystick;
{$ELSEIF Defined(FRAMEWORK_VCL)}

uses
  Gamolf.VCL.Joystick;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Gamolf', [TGamepadManager, TGamepad]);
end;

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
    result := getDPadFromXY(Joystick.Axes[ord(TJoystickAxes.X)],
      Joystick.Axes[ord(TJoystickAxes.Y)])
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
    result := Joystick.Axes[ord(TJoystickAxes.X)]
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
    X := Joystick.Axes[ord(TJoystickAxes.X)];
  if (length(Joystick.Axes) > 1) then
    Y := Joystick.Axes[ord(TJoystickAxes.Y)];
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
    result := Joystick.Axes[ord(TJoystickAxes.Y)]
  else
    result := 0;
end;

function TGamolfCustomJoystickService.getZ(JoystickID: TJoystickID): single;
var
  Joystick: TJoystickInfo;
begin
  getInfo(JoystickID, Joystick);
  if (length(Joystick.Axes) >= ord(TJoystickAxes.Z)) then
    result := Joystick.Axes[ord(TJoystickAxes.Z)]
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
  result := (ButtonID < length(Joystick.Buttons)) and
    Joystick.Buttons[ButtonID];
end;

procedure TGamolfCustomJoystickService.initJoystick(var Joystick
  : TJoystickInfo);
var
  i: integer;
begin
  if length(Joystick.Axes) < 6 then
    setlength(Joystick.Axes, 6);
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
  if (length(Buttons) < CNBButtons) then
    setlength(Buttons, CNBButtons);
  for i := 0 to length(Buttons) - 1 do
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

{ TGamepadDevicesManager }

function TGamepadDevicesManager.ConnectedGamepadCount: integer;
var
  GP: TGamepadDevice;
begin
  result := 0;
  for GP in FGamepads.Values do
    if GP.isConnected then
      inc(result);
end;

{$IF CompilerVersion>33}

class constructor TGamepadDevicesManager.Create;
begin
  FGamepadManager := TGamepadDevicesManager.Create;
end;
{$ENDIF}

constructor TGamepadDevicesManager.Create;
begin
  inherited;
{$IF CompilerVersion>35}
  FGamepads := TGamepadDeviceDict.Create([TDictionaryOwnership.doOwnsValues]);
{$ELSE}
  FGamepads := TGamepadDeviceDict.Create([doOwnsValues]);
{$ENDIF}
  FManagers := TGamepadManagerList.Create;

{$IF Defined(FRAMEWORK_FMX)}
  if not TPlatformServices.Current.SupportsPlatformService
    (IGamolfJoystickService, FGamolfJoystickService) then
    FGamolfJoystickService := nil;
{$ELSEIF Defined(FRAMEWORK_VCL)}
  FGamolfJoystickService := GetGamolfJoystickService;
{$ELSEIF Defined(IDE)}
  // Component package for the IDE
{$ELSE}
{$MESSAGE FATAL 'Project type not implemented.'}
{$ENDIF}
  FLoopIsRunning := false;
  FLoopThread := nil;
  FOnNewGamepadDetected := nil;
  FOnGamepadLost := nil;
  FOnGamepadDirectionPadChange := nil;
  FOnGamepadAxesChange := nil;
  FOnGamepadButtonDown := nil;
  FOnGamepadButtonUp := nil;
  FOnGamepadDirectionPadChange := nil;
  FEnabled := false;
  FSynchronizedEvents := false;
  FTagBool := false;
  FTagFloat := 0;
  FTagString := '';
  FTagObject := nil;
  FTag := 0;

{$IFNDEF IDE}
  // Ne pas faire dans l'IDE en conception de fiche
  // *** penser au define IDE dans les packages ***
  {$IF CompilerVersion < 32}
  TThread.Queue(nil,
    procedure
    begin
      Enabled := IsSupported; // start the gamepad thread loop if its supported
    end);
  {$ELSE}
  TThread.ForceQueue(nil,
    procedure
    begin
      Enabled := IsSupported; // start the gamepad thread loop if its supported
    end);
  {$ENDIF}
{$ENDIF}
end;

class function TGamepadDevicesManager.Current: TGamepadDevicesManager;
begin
  result := FGamepadManager;
end;

destructor TGamepadDevicesManager.Destroy;
var
  i: integer;
begin
  Enabled := false; // stop the gamepad thread loop

  // Wait max 1 minute the end of the thread loop
  i := 0;
  while FLoopIsRunning and (i < 10 * 100 * 60) do
  begin
    TThread.Sleep(10);
    inc(i);
  end;

  FGamepads.Free;
  FManagers.Free;
  inherited;
end;

function TGamepadDevicesManager.GamepadCount: integer;
begin
  result := FGamepads.Count;
end;

function TGamepadDevicesManager.GetGamepad(const AID: integer): TGamepadDevice;
begin
  if (AID < 0) then
  begin
    // TODO : traiter cas de AID=-1 (dernier ayant eu une modification ou le premier disponible)
    result := nil;
  end
  else if not FGamepads.TryGetValue(AID, result) then
    result := TGamepadDevice.Create(AID);
end;

function TGamepadDevicesManager.GetIsSupported: boolean;
begin
  result := assigned(FGamolfJoystickService);
end;

function TGamepadDevicesManager.IsGamepadConnected(const GamepadID
  : integer): boolean;
begin
  result := assigned(FGamolfJoystickService) and
    FGamolfJoystickService.isConnected(GamepadID);
end;

procedure TGamepadDevicesManager.RegisterGamePadDevice(const Gamepad
  : TGamepadDevice);
begin
  if not assigned(self) then
    exit;

  if assigned(Gamepad) and (not FGamepads.ContainsKey(Gamepad.ID)) then
    FGamepads.Add(Gamepad.ID, Gamepad);
end;

procedure TGamepadDevicesManager.RegisterGamePadManager(const Manager
  : TGamepadManager);
begin
  if not assigned(self) then
    exit;

  if assigned(Manager) and (not FManagers.Contains(Manager)) then
    FManagers.Add(Manager);
end;

procedure TGamepadDevicesManager.SetEnabled(const Value: boolean);
begin
  if (not Value) and FLoopIsRunning then
    FLoopThread.Terminate;

  if Value and (not IsSupported) then
    raise EJoystickServiceException.Create('Gamepad API not available.');

  FEnabled := Value;

  if FEnabled and (not FLoopIsRunning) then
    Execute;
end;

procedure TGamepadDevicesManager.SetOnGamepadAxesChange
  (const Value: TOnGamepadAxesChange);
begin
  FOnGamepadAxesChange := Value;
end;

procedure TGamepadDevicesManager.SetOnGamepadButtonDown
  (const Value: TOnGamepadButtonDown);
begin
  FOnGamepadButtonDown := Value;
end;

procedure TGamepadDevicesManager.SetOnGamepadButtonUp
  (const Value: TOnGamepadButtonUp);
begin
  FOnGamepadButtonUp := Value;
end;

procedure TGamepadDevicesManager.SetOnGamepadDirectionPadChange
  (const Value: TOnGamepadDirectionPadChange);
begin
  FOnGamepadDirectionPadChange := Value;
end;

procedure TGamepadDevicesManager.SetOnGamepadLost(const Value: TOnGamepadLost);
begin
  FOnGamepadLost := Value;
end;

procedure TGamepadDevicesManager.SetOnNewGamepadDetected
  (const Value: TOnNewGamepadDetected);
begin
  FOnNewGamepadDetected := Value;
end;

procedure TGamepadDevicesManager.SetSynchronizedEvents(const Value: boolean);
begin
  FSynchronizedEvents := Value;
end;

procedure TGamepadDevicesManager.SetTag(const Value: integer);
begin
  FTag := Value;
end;

procedure TGamepadDevicesManager.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

procedure TGamepadDevicesManager.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

procedure TGamepadDevicesManager.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TGamepadDevicesManager.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

procedure TGamepadDevicesManager.UnRegisterGamePadDevice(const Gamepad
  : TGamepadDevice);
begin
  if not assigned(self) then
    exit;

  if assigned(Gamepad) and FGamepads.ContainsKey(Gamepad.ID) then
    FGamepads.ExtractPair(Gamepad.ID);
end;

procedure TGamepadDevicesManager.UnRegisterGamePadManager
  (const Manager: TGamepadManager);
begin
  if not assigned(self) then
    exit;

  if assigned(Manager) and FManagers.Contains(Manager) then
    FManagers.Extract(Manager);
end;

{$IF CompilerVersion>33}

class destructor TGamepadDevicesManager.Destroy;
begin
  FGamepadManager.Free;
end;
{$ENDIF}

procedure TGamepadDevicesManager.DoGamepadAxesChange(const AGamepadID: integer;
const AAxe: TJoystickAxes; const AValue: single);
var
  LGamepadID: integer;
  LAxe: TJoystickAxes;
  LValue: single;
  LManager: TGamepadManager;
begin
  if FEnabled then
  begin
    if assigned(FOnGamepadAxesChange) then
      if FSynchronizedEvents then
      begin
        LGamepadID := AGamepadID;
        LAxe := AAxe;
        LValue := AValue;
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnGamepadAxesChange) then
              FOnGamepadAxesChange(LGamepadID, LAxe, LValue);
          end);
      end
      else
        FOnGamepadAxesChange(AGamepadID, AAxe, AValue);

    for LManager in FManagers do
      if LManager.Enabled then
        LManager.DoGamepadAxesChange(AGamepadID, AAxe, AValue);
  end;
end;

procedure TGamepadDevicesManager.DoGamepadButtonDown(const AGamepadID: integer;
const AButton: TJoystickButtons);
var
  LGamepadID: integer;
  LButton: TJoystickButtons;
  LManager: TGamepadManager;
begin
  if FEnabled then
  begin
    if assigned(FOnGamepadButtonDown) then
      if FSynchronizedEvents then
      begin
        LGamepadID := AGamepadID;
        LButton := AButton;
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnGamepadButtonDown) then
              FOnGamepadButtonDown(LGamepadID, LButton);
          end);
      end
      else
        FOnGamepadButtonDown(AGamepadID, AButton);

    for LManager in FManagers do
      if LManager.Enabled then
        LManager.DoGamepadButtonDown(AGamepadID, AButton);
  end;
end;

procedure TGamepadDevicesManager.DoGamepadButtonUp(const AGamepadID: integer;
const AButton: TJoystickButtons);
var
  LGamepadID: integer;
  LButton: TJoystickButtons;
  LManager: TGamepadManager;
begin
  if FEnabled then
  begin
    if assigned(FOnGamepadButtonUp) then
      if FSynchronizedEvents then
      begin
        LGamepadID := AGamepadID;
        LButton := AButton;
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnGamepadButtonUp) then
              FOnGamepadButtonUp(LGamepadID, LButton);
          end);
      end
      else
        FOnGamepadButtonUp(AGamepadID, AButton);

    for LManager in FManagers do
      if LManager.Enabled then
        LManager.DoGamepadButtonUp(AGamepadID, AButton);
  end;
end;

procedure TGamepadDevicesManager.DoGamepadDirectionPadChange(const AGamepadID
  : integer; const AValue: TJoystickDPad);
var
  LGamepadID: integer;
  LValue: TJoystickDPad;
  LManager: TGamepadManager;
begin
  if FEnabled then
  begin
    if assigned(FOnGamepadDirectionPadChange) then
      if FSynchronizedEvents then
      begin
        LGamepadID := AGamepadID;
        LValue := AValue;
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnGamepadDirectionPadChange) then
              FOnGamepadDirectionPadChange(LGamepadID, LValue);
          end);
      end
      else
        FOnGamepadDirectionPadChange(AGamepadID, AValue);

    for LManager in FManagers do
      if LManager.Enabled then
        LManager.DoGamepadDirectionPadChange(AGamepadID, AValue);
  end;
end;

procedure TGamepadDevicesManager.DoLostGamepad(const AGamepadID: integer);
var
  GP: TGamepadDevice;
  LGamepadID: integer;
  LManager: TGamepadManager;
begin
  GP := GetGamepad(AGamepadID);
  GP.DoLost;

  if assigned(FOnGamepadLost) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if assigned(FOnGamepadLost) then
            FOnGamepadLost(LGamepadID);
        end);
    end
    else
      FOnGamepadLost(AGamepadID);

  for LManager in FManagers do
    LManager.DoLostGamepad(AGamepadID);
end;

procedure TGamepadDevicesManager.DoNewGamepadDetected(const AGamepadID
  : integer);
var
  LGamepadID: integer;
  GP: TGamepadDevice;
  LManager: TGamepadManager;
begin
  if FEnabled and assigned(FGamolfJoystickService) and
    FGamolfJoystickService.isConnected(AGamepadID) then
  begin
    GP := GetGamepad(AGamepadID);
    GP.FhasDPAD := FGamolfJoystickService.hasDPad(AGamepadID);
    GP.isConnected := true;

    if assigned(FOnNewGamepadDetected) then
      if FSynchronizedEvents then
      begin
        LGamepadID := AGamepadID;
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnNewGamepadDetected) then
              FOnNewGamepadDetected(LGamepadID);
          end);
      end
      else
        FOnNewGamepadDetected(AGamepadID);

    for LManager in FManagers do
      if LManager.Enabled then
        LManager.DoNewGamepadDetected(AGamepadID);
  end;
end;

procedure TGamepadDevicesManager.Execute;
begin
  if (not FLoopIsRunning) and IsSupported then
  begin
    FLoopIsRunning := true;
    try
{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
      // log.d('GamepadManagerLoop: starting');
{$ENDIF}
      FLoopThread := TThread.CreateAnonymousThread(
        procedure
        var
          LJoystickInfo: TJoystickInfo;
        begin
          try
            try
{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
              // log.d('GamepadManagerLoop: started');
{$ENDIF}
              while not TThread.CheckTerminated do
              begin
                TThread.Sleep(10);
                FGamolfJoystickService.ForEach(LJoystickInfo,
                  procedure(JoystickID: TJoystickID;
                    var JoystickInfo: TJoystickInfo; hadError: boolean)
                  var
                    GP: TGamepadDevice;
                  begin
{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
                    // log.d('Joystick '+joystickid.ToString+ ' error ? '+haderror.ToString);
{$ENDIF}
                    GP := GetGamepad(JoystickID);
                    if hadError then
                    begin
                      if GP.isConnected then
                        DoLostGamepad(JoystickID);
                    end
                    else if not GP.isConnected then
                      // TODO : ne faire le test qu'une fois par seconde pour limiter les saturations d'API (notamment Windows) inutiles
                      DoNewGamepadDetected(JoystickID)
                    else
                      GP.SetNewJoystickInfo(JoystickInfo);
                  end);
              end;
            finally
              FLoopIsRunning := false;
            end;
          except
            on e: exception do
            begin
              // TODO : ajouter un evenement pour remonter les erreurs au développeur

{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
              // log.d('GamepadManagerLoop: ' + e.Message);
{$ENDIF}
            end;
          end;
{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
          // log.d('GamepadManagerLoop: stopped');
{$ENDIF}
        end);
      FLoopThread.start;
    except
      FLoopIsRunning := false;
    end;
  end;
end;

{ TGamepadManager }

function TGamepadManager.ConnectedGamepadCount: integer;
begin
  result := TGamepadDevicesManager.Current.ConnectedGamepadCount;
end;

constructor TGamepadManager.Create(AOwner: TComponent);
begin
  inherited;
  FTagBool := false;
  FTagFloat := 0;
  FTagString := '';
  FTagObject := nil;
  FEnabled := true;
  FOnGamepadButtonUp := nil;
  FOnGamepadDirectionPadChange := nil;
  FOnNewGamepadDetected := nil;
  FOnGamepadAxesChange := nil;
  FOnGamepadButtonDown := nil;
  FOnGamepadLost := nil;
  FSynchronizedEvents := false;

  TGamepadDevicesManager.Current.RegisterGamePadManager(self);
end;

destructor TGamepadManager.Destroy;
begin
  TGamepadDevicesManager.Current.UnRegisterGamePadManager(self);
  inherited;
end;

procedure TGamepadManager.DoGamepadAxesChange(const AGamepadID: integer;
const AAxe: TJoystickAxes; const AValue: single);
var
  LGamepadID: integer;
  LAxe: TJoystickAxes;
  LValue: single;
begin
  if FEnabled and assigned(FOnGamepadAxesChange) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      LAxe := AAxe;
      LValue := AValue;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnGamepadAxesChange) then
            FOnGamepadAxesChange(LGamepadID, LAxe, LValue);
        end);
    end
    else
      FOnGamepadAxesChange(AGamepadID, AAxe, AValue);
end;

procedure TGamepadManager.DoGamepadButtonDown(const AGamepadID: integer;
const AButton: TJoystickButtons);
var
  LGamepadID: integer;
  LButton: TJoystickButtons;
begin
  if FEnabled and assigned(FOnGamepadButtonDown) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      LButton := AButton;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnGamepadButtonDown) then
            FOnGamepadButtonDown(LGamepadID, LButton);
        end);
    end
    else
      FOnGamepadButtonDown(AGamepadID, AButton);
end;

procedure TGamepadManager.DoGamepadButtonUp(const AGamepadID: integer;
const AButton: TJoystickButtons);
var
  LGamepadID: integer;
  LButton: TJoystickButtons;
begin
  if FEnabled and assigned(FOnGamepadButtonUp) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      LButton := AButton;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnGamepadButtonUp) then
            FOnGamepadButtonUp(LGamepadID, LButton);
        end);
    end
    else
      FOnGamepadButtonUp(AGamepadID, AButton);
end;

procedure TGamepadManager.DoGamepadDirectionPadChange(const AGamepadID: integer;
const AValue: TJoystickDPad);
var
  LGamepadID: integer;
  LValue: TJoystickDPad;
begin
  if FEnabled and assigned(FOnGamepadDirectionPadChange) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      LValue := AValue;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnGamepadDirectionPadChange) then
            FOnGamepadDirectionPadChange(LGamepadID, LValue);
        end);
    end
    else
      FOnGamepadDirectionPadChange(AGamepadID, AValue);
end;

procedure TGamepadManager.DoLostGamepad(const AGamepadID: integer);
var
  LGamepadID: integer;
begin
  if assigned(FOnGamepadLost) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if assigned(FOnGamepadLost) then
            FOnGamepadLost(LGamepadID);
        end);
    end
    else
      FOnGamepadLost(AGamepadID);
end;

procedure TGamepadManager.DoNewGamepadDetected(const AGamepadID: integer);
var
  LGamepadID: integer;
begin
  if FEnabled and assigned(FOnNewGamepadDetected) then
    if FSynchronizedEvents then
    begin
      LGamepadID := AGamepadID;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnNewGamepadDetected) then
            FOnNewGamepadDetected(LGamepadID);
        end);
    end
    else
      FOnNewGamepadDetected(AGamepadID);
end;

function TGamepadManager.GamepadCount: integer;
begin
  result := TGamepadDevicesManager.Current.GamepadCount;
end;

function TGamepadManager.GetGamepad(const AID: integer): TGamepadDevice;
begin
  result := TGamepadDevicesManager.Current.GetGamepad(AID);
end;

function TGamepadManager.GetIsSupported: boolean;
begin
  result := TGamepadDevicesManager.Current.IsSupported;
end;

procedure TGamepadManager.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TGamepadManager.SetOnGamepadAxesChange(const Value
  : TOnGamepadAxesChange);
begin
  FOnGamepadAxesChange := Value;
end;

procedure TGamepadManager.SetOnGamepadButtonDown(const Value
  : TOnGamepadButtonDown);
begin
  FOnGamepadButtonDown := Value;
end;

procedure TGamepadManager.SetOnGamepadButtonUp(const Value: TOnGamepadButtonUp);
begin
  FOnGamepadButtonUp := Value;
end;

procedure TGamepadManager.SetOnGamepadDirectionPadChange
  (const Value: TOnGamepadDirectionPadChange);
begin
  FOnGamepadDirectionPadChange := Value;
end;

procedure TGamepadManager.SetOnGamepadLost(const Value: TOnGamepadLost);
begin
  FOnGamepadLost := Value;
end;

procedure TGamepadManager.SetOnNewGamepadDetected
  (const Value: TOnNewGamepadDetected);
begin
  FOnNewGamepadDetected := Value;
end;

procedure TGamepadManager.SetSynchronizedEvents(const Value: boolean);
begin
  FSynchronizedEvents := Value;
end;

procedure TGamepadManager.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

procedure TGamepadManager.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

procedure TGamepadManager.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TGamepadManager.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

{ TGamepad }

constructor TGamepad.Create(AOwner: TComponent);
begin
  inherited;
  FID := -1;
  FEnabled := true;
  FSynchronizedEvents := false;
  FOnDirectionPadChange := nil;
  FOnAxesChange := nil;
  FOnButtonDown := nil;
  FOnButtonUp := nil;
  FOnDirectionPadChange := nil;
  FOnLost := nil;
  FTagBool := false;
  FTagFloat := 0;
  FTagString := '';
  FTagObject := nil;
end;

procedure TGamepad.DoAxeChanged(const AAxeID: integer);
var
  LID: integer;
  LAxe: TJoystickAxes;
  LAxeValue: single;
begin
  // TODO : gérer un niveau de sensibilité sur les changements pour ne pas saturer le logiciel et les files d'attentes sur les centièmes et millièmes
  if FEnabled and assigned(FOnAxesChange) then
    if FSynchronizedEvents then
    begin
      LID := FID;
      LAxe := TJoystickAxes(AAxeID);
      LAxeValue := getGamepadData.Axes[LAxe];
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnAxesChange) then
            FOnAxesChange(LID, LAxe, LAxeValue);
        end);
    end
    else
      FOnAxesChange(FID, TJoystickAxes(AAxeID),
        getGamepadData.Axes[TJoystickAxes(AAxeID)]);
end;

procedure TGamepad.DoButtonChanged(const AButtonID: integer);
var
  LID: integer;
  LButton: TJoystickButtons;
begin
  if FEnabled then
    case Buttons[TJoystickButtons(AButtonID)] of
      true:
        if assigned(FOnButtonUp) then
          if FSynchronizedEvents then
          begin
            LID := FID;
            LButton := TJoystickButtons(AButtonID);
            TThread.Queue(nil,
              procedure
              begin
                if not assigned(self) then
                  exit;

                if FEnabled and assigned(FOnButtonUp) then
                  FOnButtonUp(LID, LButton);
              end);
          end
          else
            FOnButtonUp(FID, TJoystickButtons(AButtonID));
    else
      if assigned(FOnButtonDown) then
        if FSynchronizedEvents then
        begin
          LID := FID;
          LButton := TJoystickButtons(AButtonID);
          TThread.Queue(nil,
            procedure
            begin
              if not assigned(self) then
                exit;

              if FEnabled and assigned(FOnButtonDown) then
                FOnButtonDown(LID, LButton);
            end);
        end
        else
          FOnButtonDown(FID, TJoystickButtons(AButtonID));
    end;
end;

procedure TGamepad.DoDirectionPadChanged;
var
  LID: integer;
  LDpad: TJoystickDPad;
begin
  if FEnabled and assigned(FOnDirectionPadChange) then
    if FSynchronizedEvents then
    begin
      LID := FID;
      LDpad := getGamepadData.DPad;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if FEnabled and assigned(FOnDirectionPadChange) then
            FOnDirectionPadChange(LID, LDpad);
        end);
    end
    else
      FOnDirectionPadChange(FID, getGamepadData.DPad);
end;

procedure TGamepad.DoLost;
var
  LID: integer;
begin
  if assigned(FOnLost) then
    if FSynchronizedEvents then
    begin
      LID := FID;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if assigned(FOnLost) then
            FOnLost(LID);
        end);
    end
    else
      FOnLost(FID);
end;

function TGamepad.GetAxes(const AxeID: TJoystickAxes): single;
begin
  result := getGamepadData.GetAxes(AxeID);
end;

function TGamepad.GetButtons(const ButtonID: TJoystickButtons): boolean;
begin
  result := getGamepadData.GetButtons(ButtonID);
end;

function TGamepad.getDPad: TJoystickDPad;
begin
  result := getGamepadData.getDPad;
end;

function TGamepad.getGamepadData: TGamepadDevice;
begin
  result := TGamepadDevicesManager.Current.GetGamepad(FID);
end;

function TGamepad.GetIsConnected: boolean;
begin
  result := getGamepadData.isConnected;
end;

function TGamepad.GetIsSupported: boolean;
begin
  result := TGamepadDevicesManager.Current.IsSupported;
end;

procedure TGamepad.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TGamepad.SetID(const Value: integer);
begin
  getGamepadData.UnRegisterGamePadComponent(self);
  FID := Value;
  getGamepadData.RegisterGamePadComponent(self);
end;

procedure TGamepad.SetOnAxesChange(const Value: TOnGamepadAxesChange);
begin
  FOnAxesChange := Value;
end;

procedure TGamepad.SetOnButtonDown(const Value: TOnGamepadButtonDown);
begin
  FOnButtonDown := Value;
end;

procedure TGamepad.SetOnButtonUp(const Value: TOnGamepadButtonUp);
begin
  FOnButtonUp := Value;
end;

procedure TGamepad.SetOnDirectionPadChange(const Value
  : TOnGamepadDirectionPadChange);
begin
  FOnDirectionPadChange := Value;
end;

procedure TGamepad.SetOnLost(const Value: TOnGamepadLost);
begin
  FOnLost := Value;
end;

procedure TGamepad.SetSynchronizedEvents(const Value: boolean);
begin
  FSynchronizedEvents := Value;
end;

procedure TGamepad.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

procedure TGamepad.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

procedure TGamepad.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TGamepad.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

{ TGamepadDevice }

constructor TGamepadDevice.Create(const AID: integer);
begin
  inherited Create;
  FGamepads := TGamepadList.Create;
  FID := AID;
  FEnabled := true;
  FhasDPAD := false;
  FOnDirectionPadChange := nil;
  FOnAxesChange := nil;
  FOnButtonDown := nil;
  FOnButtonUp := nil;
  FOnLost := nil;
  setlength(FJoystickInfo.Axes, 0);
  setlength(FJoystickInfo.Buttons, 0);
  setlength(FJoystickInfo.PressedButtons, 0);
  FJoystickInfo.DPad := ord(TJoystickDPad.Center);
  FIsConnected := false;
  FSynchronizedEvents := false;
  FTagBool := false;
  FTagFloat := 0;
  FTagString := '';
  FTagObject := nil;
  FTag := 0;

  TGamepadDevicesManager.Current.RegisterGamePadDevice(self);
end;

destructor TGamepadDevice.Destroy;
begin
  TGamepadDevicesManager.Current.UnRegisterGamePadDevice(self);

  FGamepads.Free;
  inherited;
end;

procedure TGamepadDevice.DoAxeChanged(const AAxeID: integer);
var
  GP: TGamepad;
  LID: integer;
  LAxe: TJoystickAxes;
  LAxeValue: single;
begin
  if FEnabled and (AAxeID >= 0) and (AAxeID < length(FJoystickInfo.Axes)) then
  begin
    TGamepadDevicesManager.Current.DoGamepadAxesChange(FID,
      TJoystickAxes(AAxeID), FJoystickInfo.Axes[AAxeID]);

    if assigned(FOnAxesChange) then
      if FSynchronizedEvents then
      begin
        LID := FID;
        LAxe := TJoystickAxes(AAxeID);
        LAxeValue := FJoystickInfo.Axes[AAxeID];
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnAxesChange) then
              FOnAxesChange(LID, LAxe, LAxeValue);
          end);
      end
      else
        FOnAxesChange(FID, TJoystickAxes(AAxeID), FJoystickInfo.Axes[AAxeID]);

    for GP in FGamepads do
      if GP.FEnabled then
        GP.DoAxeChanged(AAxeID);
  end;
end;

procedure TGamepadDevice.DoButtonChanged(const AButtonID: integer);
var
  GP: TGamepad;
  LID: integer;
  LButton: TJoystickButtons;
  Copied: boolean;
  procedure FillLocalVariables;
  begin
    if not Copied then
    begin
      LID := FID;
      LButton := TJoystickButtons(AButtonID);
      Copied := true;
    end;
  end;

begin
  Copied := false;
  if FEnabled and (AButtonID >= 0) and
    (AButtonID < length(FJoystickInfo.Buttons)) then
  begin
    case FJoystickInfo.Buttons[AButtonID] of
      true:
        begin
          TGamepadDevicesManager.Current.DoGamepadButtonDown(FID,
            TJoystickButtons(AButtonID));

          if assigned(FOnButtonDown) then
            if FSynchronizedEvents then
            begin
              FillLocalVariables;
              TThread.Queue(nil,
                procedure
                begin
                  if not assigned(self) then
                    exit;

                  if FEnabled and assigned(FOnButtonDown) then
                    FOnButtonDown(LID, LButton);
                end);
            end
            else
              FOnButtonDown(FID, TJoystickButtons(AButtonID));
        end;
    else
      TGamepadDevicesManager.Current.DoGamepadButtonUp(FID,
        TJoystickButtons(AButtonID));

      if assigned(FOnButtonUp) then
        if FSynchronizedEvents then
        begin
          FillLocalVariables;
          TThread.Queue(nil,
            procedure
            begin
              if not assigned(self) then
                exit;

              if FEnabled and assigned(FOnButtonUp) then
                FOnButtonUp(LID, LButton);
            end);
        end
        else
          FOnButtonUp(FID, TJoystickButtons(AButtonID));
    end;

    for GP in FGamepads do
      if GP.FEnabled then
        GP.DoButtonChanged(AButtonID);
  end;
end;

procedure TGamepadDevice.DoDirectionPadChanged;
var
  GP: TGamepad;
  LID: integer;
  LDpad: TJoystickDPad;
begin
{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
  // log.d('Joystick ' + FID.ToString+' DPAD changed');
{$ENDIF}
  if FEnabled then
  begin
    TGamepadDevicesManager.Current.DoGamepadDirectionPadChange(FID,
      TJoystickDPad(FJoystickInfo.DPad));

    if assigned(FOnDirectionPadChange) then
      if FSynchronizedEvents then
      begin
        LID := FID;
        LDpad := TJoystickDPad(FJoystickInfo.DPad);
        TThread.Queue(nil,
          procedure
          begin
            if not assigned(self) then
              exit;

            if FEnabled and assigned(FOnDirectionPadChange) then
              FOnDirectionPadChange(LID, LDpad);
          end);
      end
      else
        FOnDirectionPadChange(FID, TJoystickDPad(FJoystickInfo.DPad));

    for GP in FGamepads do
      if GP.FEnabled then
        GP.DoDirectionPadChanged;
  end;
end;

procedure TGamepadDevice.DoLost;
var
  GP: TGamepad;
  LID: integer;
begin
  FIsConnected := false;

  if assigned(FOnLost) then
    if FSynchronizedEvents then
    begin
      LID := FID;
      TThread.Queue(nil,
        procedure
        begin
          if not assigned(self) then
            exit;

          if assigned(FOnLost) then
            FOnLost(LID)
        end);
    end
    else
      FOnLost(FID);

  for GP in FGamepads do
    DoLost;
end;

function TGamepadDevice.GetAxes(const AxeID: TJoystickAxes): single;
var
  idx: integer;
begin
  idx := ord(AxeID);
  if (idx >= 0) and (idx < length(FJoystickInfo.Axes)) then
    result := FJoystickInfo.Axes[idx]
  else
    result := 0;
end;

function TGamepadDevice.GetButtons(const ButtonID: TJoystickButtons): boolean;
var
  idx: integer;
begin
  idx := ord(ButtonID);
  if (idx >= 0) and (idx < length(FJoystickInfo.Buttons)) then
    result := FJoystickInfo.Buttons[idx]
  else
    result := false;
end;

function TGamepadDevice.getDPad: TJoystickDPad;
begin
  result := TJoystickDPad(FJoystickInfo.DPad);
end;

function TGamepadDevice.GetIsConnected: boolean;
begin
  if not assigned(self) then
    exit(false);

  result := FIsConnected and TGamepadDevicesManager.Current.
    IsGamepadConnected(FID);
end;

function TGamepadDevice.GetIsSupported: boolean;
begin
  result := TGamepadDevicesManager.Current.IsSupported;
end;

procedure TGamepadDevice.RegisterGamePadComponent(const Gamepad: TGamepad);
var
  i: integer;
  found: boolean;
begin
  if not assigned(self) then
    exit;

  if assigned(Gamepad) then
  begin
    found := false;
    for i := 0 to FGamepads.Count - 1 do
      if FGamepads[i] = Gamepad then
      begin
        found := true;
        break;
      end;
    if not found then
      FGamepads.Add(Gamepad);
  end;
end;

procedure TGamepadDevice.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TGamepadDevice.SetIsConnected(const Value: boolean);
begin
  FIsConnected := Value;
end;

procedure TGamepadDevice.SetNewJoystickInfo(const NewJoystickInfo
  : TJoystickInfo);
var
  CurNb, NewNb: integer;
  i: integer;
begin
{$IF Defined(DEBUG) and  Defined(FRAMEWORK_FMX)}
  // log.d('Joystick ' + FID.ToString+' enabled ? '+FEnabled.ToString);
{$ENDIF}
  if not FEnabled then
    exit;

  CurNb := length(FJoystickInfo.Axes);
  NewNb := length(NewJoystickInfo.Axes);
  if CurNb < NewNb then
  begin
    setlength(FJoystickInfo.Axes, NewNb);
    for i := CurNb to NewNb - 1 do
      FJoystickInfo.Axes[i] := 0;
  end;
  for i := 0 to NewNb - 1 do
    if (FJoystickInfo.Axes[i] <> NewJoystickInfo.Axes[i]) then
    begin
      FJoystickInfo.Axes[i] := NewJoystickInfo.Axes[i];
      DoAxeChanged(i);
    end;

  CurNb := length(FJoystickInfo.Buttons);
  NewNb := length(NewJoystickInfo.Buttons);
  if CurNb < NewNb then
  begin
    setlength(FJoystickInfo.Buttons, NewNb);
    for i := CurNb to NewNb - 1 do
      FJoystickInfo.Buttons[i] := false;
  end;
  for i := 0 to NewNb - 1 do
    if (FJoystickInfo.Buttons[i] <> NewJoystickInfo.Buttons[i]) then
    begin
      FJoystickInfo.Buttons[i] := NewJoystickInfo.Buttons[i];
      DoButtonChanged(i);
    end;

  if (FJoystickInfo.DPad <> NewJoystickInfo.DPad) then
  begin
    FJoystickInfo.DPad := NewJoystickInfo.DPad;
    DoDirectionPadChanged;
  end;
end;

procedure TGamepadDevice.SetOnAxesChange(const Value: TOnGamepadAxesChange);
begin
  FOnAxesChange := Value;
end;

procedure TGamepadDevice.SetOnButtonDown(const Value: TOnGamepadButtonDown);
begin
  FOnButtonDown := Value;
end;

procedure TGamepadDevice.SetOnButtonUp(const Value: TOnGamepadButtonUp);
begin
  FOnButtonUp := Value;
end;

procedure TGamepadDevice.SetOnDirectionPadChange(const Value
  : TOnGamepadDirectionPadChange);
begin
  FOnDirectionPadChange := Value;
end;

procedure TGamepadDevice.SetOnLost(const Value: TOnGamepadLost);
begin
  FOnLost := Value;
end;

procedure TGamepadDevice.SetSynchronizedEvents(const Value: boolean);
begin
  FSynchronizedEvents := Value;
end;

procedure TGamepadDevice.SetTag(const Value: integer);
begin
  FTag := Value;
end;

procedure TGamepadDevice.SetTagBool(const Value: boolean);
begin
  FTagBool := Value;
end;

procedure TGamepadDevice.SetTagFloat(const Value: single);
begin
  FTagFloat := Value;
end;

procedure TGamepadDevice.SetTagObject(const Value: TObject);
begin
  FTagObject := Value;
end;

procedure TGamepadDevice.SetTagString(const Value: string);
begin
  FTagString := Value;
end;

procedure TGamepadDevice.UnRegisterGamePadComponent(const Gamepad: TGamepad);
var
  i: integer;
begin
  if not assigned(self) then
    exit;

  for i := FGamepads.Count - 1 downto 0 do
    if FGamepads[i] = Gamepad then
      FGamepads.Delete(i);
end;

initialization

{$IF CompilerVersion<=33}
  TGamepadDevicesManager.FGamepadManager := TGamepadDevicesManager.Create;
{$ENDIF}

finalization

{$IF CompilerVersion<=33}
  TGamepadDevicesManager.FGamepadManager.Free;
{$ENDIF}

end.
