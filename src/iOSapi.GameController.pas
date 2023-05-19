{ *********************************************************** }
{ }
{ CodeGear Delphi Runtime Library }
{ }
{ Copyright(c) 2012-2014 Embarcadero Technologies, Inc. }
{ }
{ *********************************************************** }

//
// Delphi-Objective-C Bridge
// Interfaces for Cocoa framework GameController
//

unit iOSapi.GameController;

interface

{$IF Defined(IOS)}

uses
  Macapi.CoreFoundation,
  Macapi.CoreServices,
  Macapi.Dispatch,
  Macapi.Mach,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.Foundation,
  iOSapi.UIKit;

const
  GCDevicePhysicalInputElementUnknownChange = -1;
  GCDevicePhysicalInputElementNoChange = 0;
  GCDevicePhysicalInputElementChanged = 1;
  GCDeviceBatteryStateUnknown = -1;
  GCDeviceBatteryStateDischarging = 0;
  GCDeviceBatteryStateCharging = 1;
  GCDeviceBatteryStateFull = 2;
  GCSystemGestureStateEnabled = 0;
  GCSystemGestureStateAlwaysReceive = 1;
  GCSystemGestureStateDisabled = 2;
  GCTouchStateUp = 0;
  GCTouchStateDown = 1;
  GCTouchStateMoving = 2;
  GCDualSenseAdaptiveTriggerDiscretePositionCount = 10;
  GCDualSenseAdaptiveTriggerModeOff = 0;
  GCDualSenseAdaptiveTriggerModeFeedback = 1;
  GCDualSenseAdaptiveTriggerModeWeapon = 2;
  GCDualSenseAdaptiveTriggerModeVibration = 3;
  GCDualSenseAdaptiveTriggerModeSlopeFeedback = 4;
  GCDualSenseAdaptiveTriggerStatusUnknown = -1;
  GCDualSenseAdaptiveTriggerStatusFeedbackNoLoad = 0;
  GCDualSenseAdaptiveTriggerStatusFeedbackLoadApplied = 1;
  GCDualSenseAdaptiveTriggerStatusWeaponReady = 2;
  GCDualSenseAdaptiveTriggerStatusWeaponFiring = 3;
  GCDualSenseAdaptiveTriggerStatusWeaponFired = 4;
  GCDualSenseAdaptiveTriggerStatusVibrationNotVibrating = 5;
  GCDualSenseAdaptiveTriggerStatusVibrationIsVibrating = 6;
  GCDualSenseAdaptiveTriggerStatusSlopeFeedbackReady = 7;
  GCDualSenseAdaptiveTriggerStatusSlopeFeedbackApplyingLoad = 8;
  GCDualSenseAdaptiveTriggerStatusSlopeFeedbackFinished = 9;
  GCExtendedGamepadSnapshotDataVersion1 = 256;
  GCExtendedGamepadSnapshotDataVersion2 = 257;
  GCMicroGamepadSnapshotDataVersion1 = 256;
  GCControllerPlayerIndexUnset = -1;
  GCControllerPlayerIndex1 = 0;
  GCControllerPlayerIndex2 = 1;
  GCControllerPlayerIndex3 = 2;
  GCControllerPlayerIndex4 = 3;

type

  // ===== Forward declarations =====
{$M+}
  GCColor = interface;
  GCPhysicalInputProfile = interface;
  GCDevice = interface;
  GCPhysicalInputElementName = interface;
  GCButtonElementName = interface;
  GCAxisElementName = interface;
  GCSwitchElementName = interface;
  GCDirectionPadElementName = interface;
  GCPhysicalInputElement = interface;
  GCPhysicalInputElementCollection = interface;
  GCLinearInput = interface;
  GCPressedStateInput = interface;
  GCTouchedStateInput = interface;
  GCButtonElement = interface;
  GCAxisInput = interface;
  GCRelativeInput = interface;
  GCAxisElement = interface;
  GCSwitchPositionInput = interface;
  GCSwitchElement = interface;
  GCDirectionPadElement = interface;
  GCDevicePhysicalInputState = interface;
  GCDevicePhysicalInputStateDiff = interface;
  GCDevicePhysicalInput = interface;
  GCDeviceLight = interface;
  GCDeviceBattery = interface;
  GCControllerElement = interface;
  GCControllerAxisInput = interface;
  GCControllerButtonInput = interface;
  GCControllerDirectionPad = interface;
  GCControllerTouchpad = interface;
  GCDualSenseAdaptiveTrigger = interface;
  GCDeviceCursor = interface;
  GCController = interface;
  GCMotion = interface;
  GCGamepadSnapshot = interface;
  GCGamepad = interface;
  GCExtendedGamepadSnapshot = interface;
  GCExtendedGamepad = interface;
  GCKeyboardInput = interface;
  GCMouseInput = interface;
  GCXboxGamepad = interface;
  GCDualShockGamepad = interface;
  GCDualSenseGamepad = interface;
  GCMicroGamepadSnapshot = interface;
  GCMicroGamepad = interface;
  GCDirectionalGamepad = interface;
  GCDeviceHaptics = interface;
  GCKeyboard = interface;
  GCMouse = interface;
  GCEventViewController = interface;
  GCRacingWheelInput = interface;
  GCRacingWheel = interface;
  GCButtonInput = interface;
  GCSwitchInput = interface;
  GCSteeringWheelElement = interface;
  GCGearShifterElement = interface;
  GCRacingWheelInputState = interface;
  GCVirtualControllerConfiguration = interface;
  GCVirtualControllerElementConfiguration = interface;
  GCVirtualController = interface;

  // ===== Framework typedefs =====
{$M+}
  dispatch_queue_t = Pointer;
  Pdispatch_queue_t = ^dispatch_queue_t;
  GCInputElementName = NSString;
  PGCInputElementName = ^GCInputElementName;
  GCInputButtonName = NSString;
  PGCInputButtonName = ^GCInputButtonName;
  GCInputAxisName = NSString;
  PGCInputAxisName = ^GCInputAxisName;
  GCInputSwitchName = NSString;
  PGCInputSwitchName = ^GCInputSwitchName;
  GCInputDirectionPadName = NSString;
  PGCInputDirectionPadName = ^GCInputDirectionPadName;
  NSUInteger = Cardinal;
  PNSUInteger = ^NSUInteger;

  Key = NSString;
  PKey = ^Key;
  Element = Pointer;
  PElement = ^Element;
  TGameControllerValueDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer; param3: Single) of object;
  NSTimeInterval = Double;
  PNSTimeInterval = ^NSTimeInterval;

  TGameControllerPressedDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer; param3: Boolean) of object;
  NSInteger = Integer;
  PNSInteger = ^NSInteger;

  TGameControllerPositionDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer; param3: NSInteger) of object;

  _NSRange = record
    location: NSUInteger;
    length: NSUInteger;
  end;

  P_NSRange = ^_NSRange;
  NSRange = _NSRange;
  PNSRange = ^NSRange;

  GCDevicePhysicalInputElementChange = NSInteger;
  TGameControllerElementValueDidChangeHandler = procedure(param1: Pointer;
    param2: Pointer) of object;
  TGameControllerInputStateAvailableHandler = procedure(param1: Pointer)
    of object;
  GCDeviceBatteryState = NSInteger;
  GCSystemGestureState = NSInteger;
  GCControllerAxisValueChangedHandler = procedure(param1: GCControllerAxisInput;
    param2: Single) of object;
  GCControllerButtonValueChangedHandler = procedure
    (param1: GCControllerButtonInput; param2: Single; param3: Boolean)
    of object;
  GCControllerButtonTouchedChangedHandler = procedure
    (param1: GCControllerButtonInput; param2: Single; param3: Boolean;
    param4: Boolean) of object;
  GCControllerDirectionPadValueChangedHandler = procedure
    (param1: GCControllerDirectionPad; param2: Single; param3: Single)
    of object;
  GCTouchState = NSInteger;
  GCControllerTouchpadHandler = procedure(param1: GCControllerTouchpad;
    param2: Single; param3: Single; param4: Single; param5: Boolean) of object;
  GCDualSenseAdaptiveTriggerMode = NSInteger;
  GCDualSenseAdaptiveTriggerStatus = NSInteger;

  GCDualSenseAdaptiveTriggerPositionalAmplitudes = record
    values: array [0 .. 9] of Single;
  end;

  PGCDualSenseAdaptiveTriggerPositionalAmplitudes = ^
    GCDualSenseAdaptiveTriggerPositionalAmplitudes;

  GCDualSenseAdaptiveTriggerPositionalResistiveStrengths = record
    values: array [0 .. 9] of Single;
  end;

  PGCDualSenseAdaptiveTriggerPositionalResistiveStrengths = ^
    GCDualSenseAdaptiveTriggerPositionalResistiveStrengths;

  GCAcceleration = record
    x: Double;
    y: Double;
    z: Double;
  end;

  PGCAcceleration = ^GCAcceleration;

  GCRotationRate = record
    x: Double;
    y: Double;
    z: Double;
  end;

  PGCRotationRate = ^GCRotationRate;

  GCEulerAngles = record
    pitch: Double;
    yaw: Double;
    roll: Double;
  end;

  PGCEulerAngles = ^GCEulerAngles;

  GCQuaternion = record
    x: Double;
    y: Double;
    z: Double;
    w: Double;
  end;

  PGCQuaternion = ^GCQuaternion;

  GCMotionValueChangedHandler = procedure(param1: GCMotion) of object;
  TGameControllerValueDidChangeHandler1 = procedure
    (param1: GCPhysicalInputProfile; param2: GCControllerElement) of object;
  GCGamepadValueChangedHandler = procedure(param1: GCGamepad;
    param2: GCControllerElement) of object;

  GCGamepadSnapShotDataV100 = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonB: Single;
    buttonX: Single;
    buttonY: Single;
    leftShoulder: Single;
    rightShoulder: Single;
  end;

  PGCGamepadSnapShotDataV100 = ^GCGamepadSnapShotDataV100;

  GCExtendedGamepadValueChangedHandler = procedure(param1: GCExtendedGamepad;
    param2: GCControllerElement) of object;
  GCExtendedGamepadSnapshotDataVersion = NSInteger;

  GCExtendedGamepadSnapshotData = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonB: Single;
    buttonX: Single;
    buttonY: Single;
    leftShoulder: Single;
    rightShoulder: Single;
    leftThumbstickX: Single;
    leftThumbstickY: Single;
    rightThumbstickX: Single;
    rightThumbstickY: Single;
    leftTrigger: Single;
    rightTrigger: Single;
    supportsClickableThumbsticks: Boolean;
    leftThumbstickButton: Boolean;
    rightThumbstickButton: Boolean;
  end;

  PGCExtendedGamepadSnapshotData = ^GCExtendedGamepadSnapshotData;

  GCExtendedGamepadSnapShotDataV100 = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonB: Single;
    buttonX: Single;
    buttonY: Single;
    leftShoulder: Single;
    rightShoulder: Single;
    leftThumbstickX: Single;
    leftThumbstickY: Single;
    rightThumbstickX: Single;
    rightThumbstickY: Single;
    leftTrigger: Single;
    rightTrigger: Single;
  end;

  PGCExtendedGamepadSnapShotDataV100 = ^GCExtendedGamepadSnapShotDataV100;

  CFIndex = LongInt;
  PCFIndex = ^CFIndex;

  GCKeyCode = CFIndex;
  PGCKeyCode = ^GCKeyCode;
  GCKeyboardValueChangedHandler = procedure(param1: GCKeyboardInput;
    param2: GCControllerButtonInput; param3: GCKeyCode; param4: Boolean)
    of object;
  GCMouseMoved = procedure(param1: GCMouseInput; param2: Single; param3: Single)
    of object;
  GCMicroGamepadValueChangedHandler = procedure(param1: GCMicroGamepad;
    param2: GCControllerElement) of object;
  GCMicroGamepadSnapshotDataVersion = NSInteger;

  GCMicroGamepadSnapshotData = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonX: Single;
  end;

  PGCMicroGamepadSnapshotData = ^GCMicroGamepadSnapshotData;

  GCMicroGamepadSnapShotDataV100 = record
    version: Word;
    size: Word;
    dpadX: Single;
    dpadY: Single;
    buttonA: Single;
    buttonX: Single;
  end;

  PGCMicroGamepadSnapShotDataV100 = ^GCMicroGamepadSnapShotDataV100;

  GCControllerPlayerIndex = NSInteger;
  TGameControllerControllerPausedHandler = procedure(param1: GCController)
    of object;
  TGameControllerCompletionHandler = procedure() of object;
  GCHapticsLocality = NSString;
  PGCHapticsLocality = ^GCHapticsLocality;
  TGameControllerReply = procedure(param1: NSError) of object;
  TGameControllerConfiguration = function
    (param1: GCVirtualControllerElementConfiguration)
    : GCVirtualControllerElementConfiguration; cdecl;
  // ===== Interface declarations =====

  GCColorClass = interface(NSObjectClass)
    ['{00BACD8C-15AC-45E1-86F0-FDFCB1ED1498}']
  end;

  GCColor = interface(NSObject)
    ['{758253E7-DA4D-4192-A52E-2337018B3881}']
    function initWithRed(red: Single; green: Single; blue: Single)
      : Pointer { instancetype }; cdecl;
    function red: Single; cdecl;
    function green: Single; cdecl;
    function blue: Single; cdecl;
  end;

  TGCColor = class(TOCGenericImport<GCColorClass, GCColor>)
  end;

  PGCColor = Pointer;

  GCPhysicalInputProfileClass = interface(NSObjectClass)
    ['{63A9C3A8-F737-478D-9638-5D88FA3D360B}']
  end;

  GCPhysicalInputProfile = interface(NSObject)
    ['{32F377A0-AE2F-4D46-A999-4B47D6AB0600}']
    function device: Pointer; cdecl;
    function lastEventTimestamp: NSTimeInterval; cdecl;
    function hasRemappedElements: Boolean; cdecl;
    procedure setValueDidChangeHandler(valueDidChangeHandler
      : TGameControllerValueDidChangeHandler1); cdecl;
    function valueDidChangeHandler
      : TGameControllerValueDidChangeHandler1; cdecl;
    function elements: NSDictionary; cdecl;
    function buttons: NSDictionary; cdecl;
    function axes: NSDictionary; cdecl;
    function dpads: NSDictionary; cdecl;
    function touchpads: NSDictionary; cdecl;
    function allElements: NSSet; cdecl;
    function allButtons: NSSet; cdecl;
    function allAxes: NSSet; cdecl;
    function allDpads: NSSet; cdecl;
    function allTouchpads: NSSet; cdecl;
    function objectForKeyedSubscript(Key: NSString): GCControllerElement; cdecl;
    function capture: Pointer { instancetype }; cdecl;
    procedure setStateFromPhysicalInput(physicalInput
      : GCPhysicalInputProfile); cdecl;
    function mappedElementAliasForPhysicalInputName(inputName: NSString)
      : NSString; cdecl;
    function mappedPhysicalInputNamesForElementAlias(elementAlias: NSString)
      : NSSet; cdecl;
  end;

  TGCPhysicalInputProfile = class(TOCGenericImport<GCPhysicalInputProfileClass,
    GCPhysicalInputProfile>)
  end;

  PGCPhysicalInputProfile = Pointer;

  GCPhysicalInputElementCollectionClass = interface(NSObjectClass)
    ['{F3A5AFE1-2D8D-4505-A9E7-303CD2B96F4C}']
  end;

  GCPhysicalInputElementCollection = interface(NSObject)
    ['{25A6A9E7-7F6B-4441-BFE0-6EB0D771E720}']
    function count: NSUInteger; cdecl;
    function elementForAlias(alias: Key): Element; cdecl;
    function objectForKeyedSubscript(Key: Key): Element; cdecl;
    function elementEnumerator: NSEnumerator; cdecl;
  end;

  TGCPhysicalInputElementCollection = class
    (TOCGenericImport<GCPhysicalInputElementCollectionClass,
    GCPhysicalInputElementCollection>)
  end;

  PGCPhysicalInputElementCollection = Pointer;

  GCDeviceLightClass = interface(NSObjectClass)
    ['{CA0F1371-A9A6-48CF-8468-D6648F609D26}']
  end;

  GCDeviceLight = interface(NSObject)
    ['{5A6BE767-CB30-4785-A643-C577D6152A86}']
    procedure setColor(color: GCColor); cdecl;
    function color: GCColor; cdecl;
  end;

  TGCDeviceLight = class(TOCGenericImport<GCDeviceLightClass, GCDeviceLight>)
  end;

  PGCDeviceLight = Pointer;

  GCDeviceBatteryClass = interface(NSObjectClass)
    ['{DDB36EE1-AF8D-414A-92FB-0D48687782FC}']
  end;

  GCDeviceBattery = interface(NSObject)
    ['{D7660DC4-84DB-4D4B-BE83-1CCDC3447610}']
    function batteryLevel: Single; cdecl;
    function batteryState: GCDeviceBatteryState; cdecl;
  end;

  TGCDeviceBattery = class(TOCGenericImport<GCDeviceBatteryClass,
    GCDeviceBattery>)
  end;

  PGCDeviceBattery = Pointer;

  GCControllerElementClass = interface(NSObjectClass)
    ['{9ACB056B-40D6-434D-98DA-D2464A5FB68A}']
  end;

  GCControllerElement = interface(NSObject)
    ['{83573186-8751-49F0-BB86-4F2BD23E2713}']
    function collection: GCControllerElement; cdecl;
    function isAnalog: Boolean; cdecl;
    function isBoundToSystemGesture: Boolean; cdecl;
    procedure setPreferredSystemGestureState(preferredSystemGestureState
      : GCSystemGestureState); cdecl;
    function preferredSystemGestureState: GCSystemGestureState; cdecl;
    procedure setSfSymbolsName(sfSymbolsName: NSString); cdecl;
    function sfSymbolsName: NSString; cdecl;
    procedure setLocalizedName(localizedName: NSString); cdecl;
    function localizedName: NSString; cdecl;
    procedure setUnmappedSfSymbolsName(unmappedSfSymbolsName: NSString); cdecl;
    function unmappedSfSymbolsName: NSString; cdecl;
    procedure setUnmappedLocalizedName(unmappedLocalizedName: NSString); cdecl;
    function unmappedLocalizedName: NSString; cdecl;
    function aliases: NSSet; cdecl;
  end;

  TGCControllerElement = class(TOCGenericImport<GCControllerElementClass,
    GCControllerElement>)
  end;

  PGCControllerElement = Pointer;

  GCControllerAxisInputClass = interface(GCControllerElementClass)
    ['{958D20AF-A20C-4AE3-93AA-9BB69BE7D1D0}']
  end;

  GCControllerAxisInput = interface(GCControllerElement)
    ['{096CE36C-B157-4AA6-BDB7-3CB0373D2495}']
    procedure setValueChangedHandler(valueChangedHandler
      : GCControllerAxisValueChangedHandler); cdecl;
    function valueChangedHandler: GCControllerAxisValueChangedHandler; cdecl;
    function value: Single; cdecl;
    procedure setValue(value: Single); cdecl;
  end;

  TGCControllerAxisInput = class(TOCGenericImport<GCControllerAxisInputClass,
    GCControllerAxisInput>)
  end;

  PGCControllerAxisInput = Pointer;

  GCControllerButtonInputClass = interface(GCControllerElementClass)
    ['{4A3C8F32-8FAC-4EC4-9D66-8FA923CAC68E}']
  end;

  GCControllerButtonInput = interface(GCControllerElement)
    ['{B90A6410-F156-4D47-9297-FD950C31F6A4}']
    procedure setValueChangedHandler(valueChangedHandler
      : GCControllerButtonValueChangedHandler); cdecl;
    function valueChangedHandler: GCControllerButtonValueChangedHandler; cdecl;
    procedure setPressedChangedHandler(pressedChangedHandler
      : GCControllerButtonValueChangedHandler); cdecl;
    function pressedChangedHandler
      : GCControllerButtonValueChangedHandler; cdecl;
    procedure setTouchedChangedHandler(touchedChangedHandler
      : GCControllerButtonTouchedChangedHandler); cdecl;
    function touchedChangedHandler
      : GCControllerButtonTouchedChangedHandler; cdecl;
    function value: Single; cdecl;
    function isPressed: Boolean; cdecl;
    function isTouched: Boolean; cdecl;
    procedure setValue(value: Single); cdecl;
  end;

  TGCControllerButtonInput = class
    (TOCGenericImport<GCControllerButtonInputClass, GCControllerButtonInput>)
  end;

  PGCControllerButtonInput = Pointer;

  GCControllerDirectionPadClass = interface(GCControllerElementClass)
    ['{019BA20D-2C7E-41A5-BA88-F9D30B8DFC99}']
  end;

  GCControllerDirectionPad = interface(GCControllerElement)
    ['{1EBFF383-F6BB-44AE-876D-297842B01FD0}']
    procedure setValueChangedHandler(valueChangedHandler
      : GCControllerDirectionPadValueChangedHandler); cdecl;
    function valueChangedHandler
      : GCControllerDirectionPadValueChangedHandler; cdecl;
    function xAxis: GCControllerAxisInput; cdecl;
    function yAxis: GCControllerAxisInput; cdecl;
    function up: GCControllerButtonInput; cdecl;
    function down: GCControllerButtonInput; cdecl;
    function left: GCControllerButtonInput; cdecl;
    function right: GCControllerButtonInput; cdecl;
    procedure setValueForXAxis(xAxis: Single; yAxis: Single); cdecl;
  end;

  TGCControllerDirectionPad = class
    (TOCGenericImport<GCControllerDirectionPadClass, GCControllerDirectionPad>)
  end;

  PGCControllerDirectionPad = Pointer;

  GCControllerTouchpadClass = interface(GCControllerElementClass)
    ['{0497449F-FB88-41F5-8B31-40CD8A8514A4}']
  end;

  GCControllerTouchpad = interface(GCControllerElement)
    ['{2B905219-7FAA-4A97-B52D-B1867B3EDF68}']
    function button: GCControllerButtonInput; cdecl;
    procedure setTouchDown(touchDown: GCControllerTouchpadHandler); cdecl;
    function touchDown: GCControllerTouchpadHandler; cdecl;
    procedure setTouchMoved(touchMoved: GCControllerTouchpadHandler); cdecl;
    function touchMoved: GCControllerTouchpadHandler; cdecl;
    procedure setTouchUp(touchUp: GCControllerTouchpadHandler); cdecl;
    function touchUp: GCControllerTouchpadHandler; cdecl;
    function touchSurface: GCControllerDirectionPad; cdecl;
    function touchState: GCTouchState; cdecl;
    procedure setReportsAbsoluteTouchSurfaceValues
      (reportsAbsoluteTouchSurfaceValues: Boolean); cdecl;
    function reportsAbsoluteTouchSurfaceValues: Boolean; cdecl;
    procedure setValueForXAxis(xAxis: Single; yAxis: Single; touchDown: Boolean;
      buttonValue: Single); cdecl;
  end;

  TGCControllerTouchpad = class(TOCGenericImport<GCControllerTouchpadClass,
    GCControllerTouchpad>)
  end;

  PGCControllerTouchpad = Pointer;

  GCDualSenseAdaptiveTriggerClass = interface(GCControllerButtonInputClass)
    ['{15BA9541-C85E-4A56-ADDA-9CA542C35047}']
  end;

  GCDualSenseAdaptiveTrigger = interface(GCControllerButtonInput)
    ['{51BB8526-1CF2-49B4-AA23-A2C247D4697E}']
    function mode: GCDualSenseAdaptiveTriggerMode; cdecl;
    function status: GCDualSenseAdaptiveTriggerStatus; cdecl;
    function armPosition: Single; cdecl;
    procedure setModeSlopeFeedbackWithStartPosition(startPosition: Single;
      endPosition: Single; startStrength: Single; endStrength: Single); cdecl;
    procedure setModeFeedbackWithStartPosition(startPosition: Single;
      resistiveStrength: Single); cdecl;
    procedure setModeFeedbackWithResistiveStrengths(positionalResistiveStrengths
      : GCDualSenseAdaptiveTriggerPositionalResistiveStrengths); cdecl;
    procedure setModeWeaponWithStartPosition(startPosition: Single;
      endPosition: Single; resistiveStrength: Single); cdecl;
    procedure setModeVibrationWithStartPosition(startPosition: Single;
      amplitude: Single; frequency: Single); cdecl;
    procedure setModeVibrationWithAmplitudes(positionalAmplitudes
      : GCDualSenseAdaptiveTriggerPositionalAmplitudes;
      frequency: Single); cdecl;
    procedure setModeOff; cdecl;
  end;

  TGCDualSenseAdaptiveTrigger = class
    (TOCGenericImport<GCDualSenseAdaptiveTriggerClass,
    GCDualSenseAdaptiveTrigger>)
  end;

  PGCDualSenseAdaptiveTrigger = Pointer;

  GCDeviceCursorClass = interface(GCControllerDirectionPadClass)
    ['{7ADF8E25-1406-40B6-91F0-B40F6C8492A5}']
  end;

  GCDeviceCursor = interface(GCControllerDirectionPad)
    ['{12AC88EB-CCE0-4D5F-B0D2-DA1B60CE5F08}']
  end;

  TGCDeviceCursor = class(TOCGenericImport<GCDeviceCursorClass, GCDeviceCursor>)
  end;

  PGCDeviceCursor = Pointer;

  GCControllerClass = interface(NSObjectClass)
    ['{EC0D7A35-533A-41EF-8D4F-C829517D40EE}']
    { class } function controllers: NSArray; cdecl;
    { class } procedure startWirelessControllerDiscoveryWithCompletionHandler
      (completionHandler: TGameControllerCompletionHandler); cdecl;
    { class } procedure stopWirelessControllerDiscovery; cdecl;
    { class } function controllerWithMicroGamepad: GCController; cdecl;
    { class } function controllerWithExtendedGamepad: GCController; cdecl;
  end;

  GCController = interface(NSObject)
    ['{937BDAED-41B1-4DB7-995D-BBB4F862359F}']
    procedure setControllerPausedHandler(controllerPausedHandler
      : TGameControllerControllerPausedHandler); cdecl;
    function controllerPausedHandler
      : TGameControllerControllerPausedHandler; cdecl;
    procedure setCurrent(current: GCController); cdecl;
    function current: GCController; cdecl;
    procedure setShouldMonitorBackgroundEvents(shouldMonitorBackgroundEvents
      : Boolean); cdecl;
    function shouldMonitorBackgroundEvents: Boolean; cdecl;
    function isAttachedToDevice: Boolean; cdecl;
    function isSnapshot: Boolean; cdecl;
    procedure setPlayerIndex(playerIndex: GCControllerPlayerIndex); cdecl;
    function playerIndex: GCControllerPlayerIndex; cdecl;
    function battery: GCDeviceBattery; cdecl;
    function physicalInputProfile: GCPhysicalInputProfile; cdecl;
    function gamepad: GCGamepad; cdecl;
    function microGamepad: GCMicroGamepad; cdecl;
    function extendedGamepad: GCExtendedGamepad; cdecl;
    function motion: GCMotion; cdecl;
    function light: GCDeviceLight; cdecl;
    function haptics: GCDeviceHaptics; cdecl;
    function capture: GCController; cdecl;
  end;

  TGCController = class(TOCGenericImport<GCControllerClass, GCController>)
  end;

  PGCController = Pointer;

  GCMotionClass = interface(NSObjectClass)
    ['{FFEE8C6B-C97D-48B0-BA2A-0F39AF199659}']
  end;

  GCMotion = interface(NSObject)
    ['{1080C47C-231F-467D-8233-87346A5D5E76}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCMotionValueChangedHandler); cdecl;
    function valueChangedHandler: GCMotionValueChangedHandler; cdecl;
    function sensorsRequireManualActivation: Boolean; cdecl;
    procedure setSensorsActive(sensorsActive: Boolean); cdecl;
    function sensorsActive: Boolean; cdecl;
    function hasGravityAndUserAcceleration: Boolean; cdecl;
    function gravity: GCAcceleration; cdecl;
    function userAcceleration: GCAcceleration; cdecl;
    function acceleration: GCAcceleration; cdecl;
    function hasAttitudeAndRotationRate: Boolean; cdecl;
    function hasAttitude: Boolean; cdecl;
    function hasRotationRate: Boolean; cdecl;
    function attitude: GCQuaternion; cdecl;
    function rotationRate: GCRotationRate; cdecl;
    procedure setGravity(gravity: GCAcceleration); cdecl;
    procedure setUserAcceleration(userAcceleration: GCAcceleration); cdecl;
    procedure setAcceleration(acceleration: GCAcceleration); cdecl;
    procedure setAttitude(attitude: GCQuaternion); cdecl;
    procedure setRotationRate(rotationRate: GCRotationRate); cdecl;
    procedure setStateFromMotion(motion: GCMotion); cdecl;
  end;

  TGCMotion = class(TOCGenericImport<GCMotionClass, GCMotion>)
  end;

  PGCMotion = Pointer;

  GCGamepadClass = interface(GCPhysicalInputProfileClass)
    ['{03B59A42-5499-44A3-B7DC-D50EEE76DD91}']
  end;

  GCGamepad = interface(GCPhysicalInputProfile)
    ['{8CC33AD1-E207-4D97-BF42-0AE93267147D}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCGamepadValueChangedHandler); cdecl;
    function valueChangedHandler: GCGamepadValueChangedHandler; cdecl;
    function saveSnapshot: GCGamepadSnapshot; cdecl;
    function dpad: GCControllerDirectionPad; cdecl;
    function buttonA: GCControllerButtonInput; cdecl;
    function buttonB: GCControllerButtonInput; cdecl;
    function buttonX: GCControllerButtonInput; cdecl;
    function buttonY: GCControllerButtonInput; cdecl;
    function leftShoulder: GCControllerButtonInput; cdecl;
    function rightShoulder: GCControllerButtonInput; cdecl;
  end;

  TGCGamepad = class(TOCGenericImport<GCGamepadClass, GCGamepad>)
  end;

  PGCGamepad = Pointer;

  GCGamepadSnapshotClass = interface(GCGamepadClass)
    ['{6B1C1F0A-9342-43FE-9FBE-165ADF5AF089}']
  end;

  GCGamepadSnapshot = interface(GCGamepad)
    ['{B02C4101-C1B5-4A57-9A80-A9F358A1729D}']
    procedure setSnapshotData(snapshotData: NSData); cdecl;
    function snapshotData: NSData; cdecl;
    function initWithSnapshotData(data: NSData)
      : Pointer { instancetype }; cdecl;
    function initWithController(controller: GCController; snapshotData: NSData)
      : Pointer { instancetype }; cdecl;
  end;

  TGCGamepadSnapshot = class(TOCGenericImport<GCGamepadSnapshotClass,
    GCGamepadSnapshot>)
  end;

  PGCGamepadSnapshot = Pointer;

  GCExtendedGamepadClass = interface(GCPhysicalInputProfileClass)
    ['{410E9B85-1EE4-4F4B-B747-C89A62E4C579}']
  end;

  GCExtendedGamepad = interface(GCPhysicalInputProfile)
    ['{9B50B41A-1F1A-405E-A3D1-8856C79B9A2C}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCExtendedGamepadValueChangedHandler); cdecl;
    function valueChangedHandler: GCExtendedGamepadValueChangedHandler; cdecl;
    function saveSnapshot: GCExtendedGamepadSnapshot; cdecl;
    function dpad: GCControllerDirectionPad; cdecl;
    function buttonA: GCControllerButtonInput; cdecl;
    function buttonB: GCControllerButtonInput; cdecl;
    function buttonX: GCControllerButtonInput; cdecl;
    function buttonY: GCControllerButtonInput; cdecl;
    function buttonMenu: GCControllerButtonInput; cdecl;
    function buttonOptions: GCControllerButtonInput; cdecl;
    function buttonHome: GCControllerButtonInput; cdecl;
    function leftThumbstick: GCControllerDirectionPad; cdecl;
    function rightThumbstick: GCControllerDirectionPad; cdecl;
    function leftShoulder: GCControllerButtonInput; cdecl;
    function rightShoulder: GCControllerButtonInput; cdecl;
    function leftTrigger: GCControllerButtonInput; cdecl;
    function rightTrigger: GCControllerButtonInput; cdecl;
    function leftThumbstickButton: GCControllerButtonInput; cdecl;
    function rightThumbstickButton: GCControllerButtonInput; cdecl;
    procedure setStateFromExtendedGamepad(extendedGamepad
      : GCExtendedGamepad); cdecl;
  end;

  TGCExtendedGamepad = class(TOCGenericImport<GCExtendedGamepadClass,
    GCExtendedGamepad>)
  end;

  PGCExtendedGamepad = Pointer;

  GCExtendedGamepadSnapshotClass = interface(GCExtendedGamepadClass)
    ['{F82957A0-B2C4-4B93-8202-4D2430E36BCB}']
  end;

  GCExtendedGamepadSnapshot = interface(GCExtendedGamepad)
    ['{D5DF85A6-321E-4959-9575-20E6ABEB0D38}']
    procedure setSnapshotData(snapshotData: NSData); cdecl;
    function snapshotData: NSData; cdecl;
    function initWithSnapshotData(data: NSData)
      : Pointer { instancetype }; cdecl;
    function initWithController(controller: GCController; snapshotData: NSData)
      : Pointer { instancetype }; cdecl;
  end;

  TGCExtendedGamepadSnapshot = class
    (TOCGenericImport<GCExtendedGamepadSnapshotClass,
    GCExtendedGamepadSnapshot>)
  end;

  PGCExtendedGamepadSnapshot = Pointer;

  GCKeyboardInputClass = interface(GCPhysicalInputProfileClass)
    ['{3A194FCF-D701-4D82-BDF8-0E1AC0351802}']
  end;

  GCKeyboardInput = interface(GCPhysicalInputProfile)
    ['{FEC86A48-63D6-4C50-835C-FBCDF2A96BD7}']
    procedure setKeyChangedHandler(keyChangedHandler
      : GCKeyboardValueChangedHandler); cdecl;
    function keyChangedHandler: GCKeyboardValueChangedHandler; cdecl;
    function isAnyKeyPressed: Boolean; cdecl;
    function buttonForKeyCode(code: GCKeyCode): GCControllerButtonInput; cdecl;
  end;

  TGCKeyboardInput = class(TOCGenericImport<GCKeyboardInputClass,
    GCKeyboardInput>)
  end;

  PGCKeyboardInput = Pointer;

  GCMouseInputClass = interface(GCPhysicalInputProfileClass)
    ['{B4D8E9EC-ABD2-468D-A9FE-ABBF437C23DC}']
  end;

  GCMouseInput = interface(GCPhysicalInputProfile)
    ['{A25CA338-8C6F-4531-91C4-27F15C492041}']
    procedure setMouseMovedHandler(mouseMovedHandler: GCMouseMoved); cdecl;
    function mouseMovedHandler: GCMouseMoved; cdecl;
    function scroll: GCDeviceCursor; cdecl;
    function leftButton: GCControllerButtonInput; cdecl;
    function rightButton: GCControllerButtonInput; cdecl;
    function middleButton: GCControllerButtonInput; cdecl;
    function auxiliaryButtons: NSArray; cdecl;
  end;

  TGCMouseInput = class(TOCGenericImport<GCMouseInputClass, GCMouseInput>)
  end;

  PGCMouseInput = Pointer;

  GCXboxGamepadClass = interface(GCExtendedGamepadClass)
    ['{79318950-2064-41E7-8CE7-7FB13B43EF03}']
  end;

  GCXboxGamepad = interface(GCExtendedGamepad)
    ['{3EDBB76F-479F-4E62-93E5-6099C800C8C5}']
    function paddleButton1: GCControllerButtonInput; cdecl;
    function paddleButton2: GCControllerButtonInput; cdecl;
    function paddleButton3: GCControllerButtonInput; cdecl;
    function paddleButton4: GCControllerButtonInput; cdecl;
    function buttonShare: GCControllerButtonInput; cdecl;
  end;

  TGCXboxGamepad = class(TOCGenericImport<GCXboxGamepadClass, GCXboxGamepad>)
  end;

  PGCXboxGamepad = Pointer;

  GCDualShockGamepadClass = interface(GCExtendedGamepadClass)
    ['{08641ADB-2F0B-47A7-9934-495C4B89A4DA}']
  end;

  GCDualShockGamepad = interface(GCExtendedGamepad)
    ['{0A5FCC3C-21CD-408A-B582-4339FE36BE2B}']
    function touchpadButton: GCControllerButtonInput; cdecl;
    function touchpadPrimary: GCControllerDirectionPad; cdecl;
    function touchpadSecondary: GCControllerDirectionPad; cdecl;
  end;

  TGCDualShockGamepad = class(TOCGenericImport<GCDualShockGamepadClass,
    GCDualShockGamepad>)
  end;

  PGCDualShockGamepad = Pointer;

  GCDualSenseGamepadClass = interface(GCExtendedGamepadClass)
    ['{CC3AC0B5-0059-42FF-8A01-CAC995FD0D15}']
  end;

  GCDualSenseGamepad = interface(GCExtendedGamepad)
    ['{98DCCADF-51C4-48A1-927E-4E2DE1BC8B8E}']
    function touchpadButton: GCControllerButtonInput; cdecl;
    function touchpadPrimary: GCControllerDirectionPad; cdecl;
    function touchpadSecondary: GCControllerDirectionPad; cdecl;
    function leftTrigger: GCDualSenseAdaptiveTrigger; cdecl;
    function rightTrigger: GCDualSenseAdaptiveTrigger; cdecl;
  end;

  TGCDualSenseGamepad = class(TOCGenericImport<GCDualSenseGamepadClass,
    GCDualSenseGamepad>)
  end;

  PGCDualSenseGamepad = Pointer;

  GCMicroGamepadClass = interface(GCPhysicalInputProfileClass)
    ['{E1541C16-80AA-461F-A78B-40A2AC687F31}']
  end;

  GCMicroGamepad = interface(GCPhysicalInputProfile)
    ['{1679579D-4C1E-4D0F-8C51-C897E476A1BF}']
    function controller: GCController; cdecl;
    procedure setValueChangedHandler(valueChangedHandler
      : GCMicroGamepadValueChangedHandler); cdecl;
    function valueChangedHandler: GCMicroGamepadValueChangedHandler; cdecl;
    function saveSnapshot: GCMicroGamepadSnapshot; cdecl;
    function dpad: GCControllerDirectionPad; cdecl;
    function buttonA: GCControllerButtonInput; cdecl;
    function buttonX: GCControllerButtonInput; cdecl;
    function buttonMenu: GCControllerButtonInput; cdecl;
    procedure setReportsAbsoluteDpadValues(reportsAbsoluteDpadValues
      : Boolean); cdecl;
    function reportsAbsoluteDpadValues: Boolean; cdecl;
    procedure setAllowsRotation(allowsRotation: Boolean); cdecl;
    function allowsRotation: Boolean; cdecl;
    procedure setStateFromMicroGamepad(microGamepad: GCMicroGamepad); cdecl;
  end;

  TGCMicroGamepad = class(TOCGenericImport<GCMicroGamepadClass, GCMicroGamepad>)
  end;

  PGCMicroGamepad = Pointer;

  GCMicroGamepadSnapshotClass = interface(GCMicroGamepadClass)
    ['{7D7E151E-3DC5-4389-816F-76F2C98A3219}']
  end;

  GCMicroGamepadSnapshot = interface(GCMicroGamepad)
    ['{006B7871-596E-4FF3-8B8F-90A524FEC97D}']
    procedure setSnapshotData(snapshotData: NSData); cdecl;
    function snapshotData: NSData; cdecl;
    function initWithSnapshotData(data: NSData)
      : Pointer { instancetype }; cdecl;
    function initWithController(controller: GCController; snapshotData: NSData)
      : Pointer { instancetype }; cdecl;
  end;

  TGCMicroGamepadSnapshot = class(TOCGenericImport<GCMicroGamepadSnapshotClass,
    GCMicroGamepadSnapshot>)
  end;

  PGCMicroGamepadSnapshot = Pointer;

  GCDirectionalGamepadClass = interface(GCMicroGamepadClass)
    ['{4CA62AF3-71CF-47E6-85F7-951AF20B6328}']
  end;

  GCDirectionalGamepad = interface(GCMicroGamepad)
    ['{34B3164C-3407-4841-8B4E-AA3C67A21D2E}']
  end;

  TGCDirectionalGamepad = class(TOCGenericImport<GCDirectionalGamepadClass,
    GCDirectionalGamepad>)
  end;

  PGCDirectionalGamepad = Pointer;

  GCDeviceHapticsClass = interface(NSObjectClass)
    ['{943EFB8C-7C66-4D03-B35A-4BE390E2C721}']
  end;

  GCDeviceHaptics = interface(NSObject)
    ['{6FF1040E-6B11-4815-B265-1EA3ADC022D5}']
    function supportedLocalities: NSSet; cdecl;
    function createEngineWithLocality(locality: GCHapticsLocality)
      : CHHapticEngine; cdecl;
  end;

  TGCDeviceHaptics = class(TOCGenericImport<GCDeviceHapticsClass,
    GCDeviceHaptics>)
  end;

  PGCDeviceHaptics = Pointer;

  GCKeyboardClass = interface(NSObjectClass)
    ['{906ADFA6-5153-407A-B38F-B131ECDF9D35}']
  end;

  GCKeyboard = interface(NSObject)
    ['{2F095109-1831-4C84-B56F-3BC5B9F3D9DF}']
    function keyboardInput: GCKeyboardInput; cdecl;
    procedure setCoalescedKeyboard(coalescedKeyboard: GCKeyboard); cdecl;
    function coalescedKeyboard: GCKeyboard; cdecl;
  end;

  TGCKeyboard = class(TOCGenericImport<GCKeyboardClass, GCKeyboard>)
  end;

  PGCKeyboard = Pointer;

  GCMouseClass = interface(NSObjectClass)
    ['{AC23CD5D-ECD4-4AE6-A2A9-35166D33C55D}']
    { class } function mice: NSArray; cdecl;
  end;

  GCMouse = interface(NSObject)
    ['{0C524B2E-5B3A-4D82-9792-52BDB755095D}']
    function mouseInput: GCMouseInput; cdecl;
    procedure setCurrent(current: GCMouse); cdecl;
    function current: GCMouse; cdecl;
  end;

  TGCMouse = class(TOCGenericImport<GCMouseClass, GCMouse>)
  end;

  PGCMouse = Pointer;

  GCEventViewControllerClass = interface(UIViewControllerClass)
    ['{276A6271-BCFC-4808-BE76-20692467679B}']
  end;

  GCEventViewController = interface(UIViewController)
    ['{BD1B399D-84A6-4D21-901F-349FB7F278B9}']
    procedure setControllerUserInteractionEnabled
      (controllerUserInteractionEnabled: Boolean); cdecl;
    function controllerUserInteractionEnabled: Boolean; cdecl;
  end;

  TGCEventViewController = class(TOCGenericImport<GCEventViewControllerClass,
    GCEventViewController>)
  end;

  PGCEventViewController = Pointer;

  GCRacingWheelInputStateClass = interface(NSObjectClass)
    ['{A3EC7408-8496-4210-90D5-DFC6379FD699}']
  end;

  GCRacingWheelInputState = interface(NSObject)
    ['{DF1ED87B-0576-41F1-86FC-1D79769AD81B}']
    function wheel: GCSteeringWheelElement; cdecl;
    function acceleratorPedal: Pointer; cdecl;
    function brakePedal: Pointer; cdecl;
    function clutchPedal: Pointer; cdecl;
    function shifter: GCGearShifterElement; cdecl;
  end;

  TGCRacingWheelInputState = class
    (TOCGenericImport<GCRacingWheelInputStateClass, GCRacingWheelInputState>)
  end;

  PGCRacingWheelInputState = Pointer;

  GCRacingWheelInputClass = interface(GCRacingWheelInputStateClass)
    ['{92317EB2-3651-4A21-9045-784187C9A1C1}']
  end;

  GCRacingWheelInput = interface(GCRacingWheelInputState)
    ['{13625ECC-9C82-4716-BF7A-9E4AED74391A}']
    function capture: GCRacingWheelInputState; cdecl;
    function nextInputState: GCRacingWheelInputState; cdecl;
  end;

  TGCRacingWheelInput = class(TOCGenericImport<GCRacingWheelInputClass,
    GCRacingWheelInput>)
  end;

  PGCRacingWheelInput = Pointer;

  GCRacingWheelClass = interface(NSObjectClass)
    ['{F48E007B-4E18-4ABE-B645-01366F73B898}']
  end;

  GCRacingWheel = interface(NSObject)
    ['{48A4686E-2313-425F-BD46-9E1357ACA371}']
    function connectedRacingWheels: NSSet; cdecl;
    function acquireDeviceWithError(error: NSError): Boolean; cdecl;
    procedure relinquishDevice; cdecl;
    function isAcquired: Boolean; cdecl;
    function wheelInput: GCRacingWheelInput; cdecl;
    function isSnapshot: Boolean; cdecl;
    function capture: GCRacingWheel; cdecl;
  end;

  TGCRacingWheel = class(TOCGenericImport<GCRacingWheelClass, GCRacingWheel>)
  end;

  PGCRacingWheel = Pointer;

  GCSteeringWheelElementClass = interface(NSObjectClass)
    ['{2AD51A2F-F7E5-4DE3-B9C3-87224F652212}']
  end;

  GCSteeringWheelElement = interface(NSObject)
    ['{1DD095B0-26F2-405A-946C-DE954ADE17E8}']
    function maximumDegreesOfRotation: Single; cdecl;
  end;

  TGCSteeringWheelElement = class(TOCGenericImport<GCSteeringWheelElementClass,
    GCSteeringWheelElement>)
  end;

  PGCSteeringWheelElement = Pointer;

  GCGearShifterElementClass = interface(NSObjectClass)
    ['{338DCB98-50EA-49E5-B135-1D5F336D66C6}']
  end;

  GCGearShifterElement = interface(NSObject)
    ['{2EF95E98-F7E6-44E4-83D6-5567286DCA2B}']
    function patternInput: Pointer; cdecl;
    function sequentialInput: Pointer; cdecl;
  end;

  TGCGearShifterElement = class(TOCGenericImport<GCGearShifterElementClass,
    GCGearShifterElement>)
  end;

  PGCGearShifterElement = Pointer;

  GCVirtualControllerConfigurationClass = interface(NSObjectClass)
    ['{2D869BBB-7BD0-4006-9793-6864017DA67F}']
  end;

  GCVirtualControllerConfiguration = interface(NSObject)
    ['{2C4BB063-6AB4-4EA6-8C82-3564568DE11A}']
    procedure setElements(elements: NSSet); cdecl;
    function elements: NSSet; cdecl;
  end;

  TGCVirtualControllerConfiguration = class
    (TOCGenericImport<GCVirtualControllerConfigurationClass,
    GCVirtualControllerConfiguration>)
  end;

  PGCVirtualControllerConfiguration = Pointer;

  GCVirtualControllerElementConfigurationClass = interface(NSObjectClass)
    ['{D0B9A3A8-F2C7-4606-B012-7959B6681692}']
  end;

  GCVirtualControllerElementConfiguration = interface(NSObject)
    ['{1757B04D-3415-4275-B356-E37BB8080275}']
    procedure setHidden(hidden: Boolean); cdecl;
    function isHidden: Boolean; cdecl;
    procedure setPath(path: UIBezierPath); cdecl;
    function path: UIBezierPath; cdecl;
    procedure setActsAsTouchpad(actsAsTouchpad: Boolean); cdecl;
    function actsAsTouchpad: Boolean; cdecl;
  end;

  TGCVirtualControllerElementConfiguration = class
    (TOCGenericImport<GCVirtualControllerElementConfigurationClass,
    GCVirtualControllerElementConfiguration>)
  end;

  PGCVirtualControllerElementConfiguration = Pointer;

  GCVirtualControllerClass = interface(NSObjectClass)
    ['{46A65E6B-848F-403A-8D35-095092AB186C}']
    { class } function virtualControllerWithConfiguration
      (configuration: GCVirtualControllerConfiguration)
      : GCVirtualController; cdecl;
  end;

  GCVirtualController = interface(NSObject)
    ['{384E6E72-4355-4C1E-9EEC-F8E9FC70DF51}']
    function initWithConfiguration(configuration
      : GCVirtualControllerConfiguration): Pointer { instancetype }; cdecl;
    procedure connectWithReplyHandler(reply: TGameControllerReply); cdecl;
    procedure disconnect; cdecl;
    function controller: GCController; cdecl;
    procedure updateConfigurationForElement(Element: NSString;
      configuration: TGameControllerConfiguration); cdecl;
  end;

  TGCVirtualController = class(TOCGenericImport<GCVirtualControllerClass,
    GCVirtualController>)
  end;

  PGCVirtualController = Pointer;

  // ===== Protocol declarations =====

  GCDevice = interface(IObjectiveC)
    ['{07AC1712-91AA-469F-AB74-54A1BFF87D88}']
    procedure setHandlerQueue(handlerQueue: dispatch_queue_t); cdecl;
    function handlerQueue: dispatch_queue_t; cdecl;
    function vendorName: NSString; cdecl;
    function productCategory: NSString; cdecl;
    function physicalInputProfile: GCPhysicalInputProfile; cdecl;
  end;

  GCPhysicalInputElementName = interface(IObjectiveC)
    ['{7348AAEC-834F-4FD6-B95D-EB7A8F7CF560}']
  end;

  GCButtonElementName = interface(IObjectiveC)
    ['{EED633B6-3AD0-4704-9104-A8D4D609EFF1}']
  end;

  GCAxisElementName = interface(IObjectiveC)
    ['{60A3FAB3-8315-47DB-A847-C3F621D1BCDD}']
  end;

  GCSwitchElementName = interface(IObjectiveC)
    ['{B4598472-7146-498C-8937-250B286E742F}']
  end;

  GCDirectionPadElementName = interface(IObjectiveC)
    ['{AE9E933E-879E-4067-A6BF-B68572875E57}']
  end;

  GCPhysicalInputElement = interface(IObjectiveC)
    ['{515C0FEB-70A1-41C5-821E-2D98615E96A0}']
    function sfSymbolsName: NSString; cdecl;
    function localizedName: NSString; cdecl;
    function aliases: NSSet; cdecl;
  end;

  GCLinearInput = interface(IObjectiveC)
    ['{1622B60C-0972-4F10-A6C9-FA10CD16CA8E}']
    procedure setValueDidChangeHandler(valueDidChangeHandler
      : TGameControllerValueDidChangeHandler); cdecl;
    function valueDidChangeHandler: TGameControllerValueDidChangeHandler; cdecl;
    function value: Single; cdecl;
    function isAnalog: Boolean; cdecl;
    function canWrap: Boolean; cdecl;
    function lastValueTimestamp: NSTimeInterval; cdecl;
    function lastValueLatency: NSTimeInterval; cdecl;
  end;

  GCPressedStateInput = interface(IObjectiveC)
    ['{691E28DD-534D-4ADA-9904-68C165121D32}']
    procedure setPressedDidChangeHandler(pressedDidChangeHandler
      : TGameControllerPressedDidChangeHandler); cdecl;
    function pressedDidChangeHandler
      : TGameControllerPressedDidChangeHandler; cdecl;
    function isPressed: Boolean; cdecl;
    function lastPressedStateTimestamp: NSTimeInterval; cdecl;
    function lastPressedStateLatency: NSTimeInterval; cdecl;
  end;

  GCTouchedStateInput = interface(IObjectiveC)
    ['{5CAFA0B7-3103-4215-A7E0-3DFE509E59A9}']
    procedure setTouchedDidChangeHandler(touchedDidChangeHandler
      : TGameControllerPressedDidChangeHandler); cdecl;
    function touchedDidChangeHandler
      : TGameControllerPressedDidChangeHandler; cdecl;
    function isTouched: Boolean; cdecl;
    function lastTouchedStateTimestamp: NSTimeInterval; cdecl;
    function lastTouchedStateLatency: NSTimeInterval; cdecl;
  end;

  GCButtonElement = interface(IObjectiveC)
    ['{FBE455DE-8D5B-4D83-8CA2-A75A1347B07D}']
    function pressedInput: Pointer; cdecl;
    function touchedInput: Pointer; cdecl;
  end;

  GCAxisInput = interface(IObjectiveC)
    ['{877BD734-93B2-4878-AC3F-F377B6B29536}']
    procedure setValueDidChangeHandler(valueDidChangeHandler
      : TGameControllerValueDidChangeHandler); cdecl;
    function valueDidChangeHandler: TGameControllerValueDidChangeHandler; cdecl;
    function value: Single; cdecl;
    function isAnalog: Boolean; cdecl;
    function canWrap: Boolean; cdecl;
    function lastValueTimestamp: NSTimeInterval; cdecl;
    function lastValueLatency: NSTimeInterval; cdecl;
  end;

  GCRelativeInput = interface(IObjectiveC)
    ['{75AD3826-4B1B-424E-80DF-5863E06D96D2}']
    procedure setDeltaDidChangeHandler(deltaDidChangeHandler
      : TGameControllerValueDidChangeHandler); cdecl;
    function deltaDidChangeHandler: TGameControllerValueDidChangeHandler; cdecl;
    function delta: Single; cdecl;
    function isAnalog: Boolean; cdecl;
    function lastDeltaTimestamp: NSTimeInterval; cdecl;
    function lastDeltaLatency: NSTimeInterval; cdecl;
  end;

  GCAxisElement = interface(IObjectiveC)
    ['{73719C6B-2CAF-4838-9621-B12D41A223E9}']
    function absoluteInput: Pointer; cdecl;
    function relativeInput: Pointer; cdecl;
  end;

  GCSwitchPositionInput = interface(IObjectiveC)
    ['{95FB5744-2828-4195-BFD9-39976E67F57E}']
    procedure setPositionDidChangeHandler(positionDidChangeHandler
      : TGameControllerPositionDidChangeHandler); cdecl;
    function positionDidChangeHandler
      : TGameControllerPositionDidChangeHandler; cdecl;
    function position: NSInteger; cdecl;
    function positionRange: NSRange; cdecl;
    function isSequential: Boolean; cdecl;
    function canWrap: Boolean; cdecl;
    function lastPositionTimestamp: NSTimeInterval; cdecl;
    function lastPositionLatency: NSTimeInterval; cdecl;
  end;

  GCSwitchElement = interface(IObjectiveC)
    ['{E9D4490A-E036-4332-A6CE-BE05BD513E0A}']
    function positionInput: Pointer; cdecl;
  end;

  GCDirectionPadElement = interface(IObjectiveC)
    ['{F27828F5-ABEF-4FF3-9249-D390F235EF6A}']
    function xAxis: Pointer; cdecl;
    function yAxis: Pointer; cdecl;
    function up: Pointer; cdecl;
    function down: Pointer; cdecl;
    function left: Pointer; cdecl;
    function right: Pointer; cdecl;
  end;

  GCDevicePhysicalInputState = interface(IObjectiveC)
    ['{C1E40409-49F7-4A77-8AD9-5DE0F7292E71}']
    function device: Pointer; cdecl;
    function lastEventTimestamp: NSTimeInterval; cdecl;
    function lastEventLatency: NSTimeInterval; cdecl;
    function elements: GCPhysicalInputElementCollection; cdecl;
    function buttons: GCPhysicalInputElementCollection; cdecl;
    function axes: GCPhysicalInputElementCollection; cdecl;
    function switches: GCPhysicalInputElementCollection; cdecl;
    function dpads: GCPhysicalInputElementCollection; cdecl;
    function objectForKeyedSubscript(Key: NSString): Pointer; cdecl;
  end;

  GCDevicePhysicalInputStateDiff = interface(IObjectiveC)
    ['{DC4248E4-1B92-43E8-8B36-64B7D6DC44FD}']
    function changeForElement(Element: Pointer)
      : GCDevicePhysicalInputElementChange; cdecl;
    function changedElements: NSEnumerator; cdecl;
  end;

  GCDevicePhysicalInput = interface(IObjectiveC)
    ['{8CCDDF87-7EE2-4220-810A-62B81641A1BE}']
    function device: Pointer; cdecl;
    procedure setElementValueDidChangeHandler(elementValueDidChangeHandler
      : TGameControllerElementValueDidChangeHandler); cdecl;
    function elementValueDidChangeHandler
      : TGameControllerElementValueDidChangeHandler; cdecl;
    function capture: Pointer; cdecl;
    procedure setInputStateAvailableHandler(inputStateAvailableHandler
      : TGameControllerInputStateAvailableHandler); cdecl;
    function inputStateAvailableHandler
      : TGameControllerInputStateAvailableHandler; cdecl;
    procedure setInputStateQueueDepth(inputStateQueueDepth: NSInteger); cdecl;
    function inputStateQueueDepth: NSInteger; cdecl;
    function nextInputState: Pointer; cdecl;
  end;

  GCButtonInput = interface(IObjectiveC)
    ['{527D82AD-DB66-41CA-84AC-3F7AD35511C4}']
  end;

  GCSwitchInput = interface(IObjectiveC)
    ['{9C771086-1FF0-4164-A830-42C380675569}']
  end;

  // ===== Exported string consts =====

function GCInputButtonA: Pointer;
function GCInputButtonB: Pointer;
function GCInputButtonX: Pointer;
function GCInputButtonY: Pointer;
function GCInputDirectionPad: Pointer;
function GCInputLeftThumbstick: Pointer;
function GCInputRightThumbstick: Pointer;
function GCInputLeftShoulder: Pointer;
function GCInputRightShoulder: Pointer;
function GCInputLeftTrigger: Pointer;
function GCInputRightTrigger: Pointer;
function GCInputLeftThumbstickButton: Pointer;
function GCInputRightThumbstickButton: Pointer;
function GCInputButtonHome: Pointer;
function GCInputButtonMenu: Pointer;
function GCInputButtonOptions: Pointer;
function GCInputButtonShare: Pointer;
function GCInputXboxPaddleOne: Pointer;
function GCInputXboxPaddleTwo: Pointer;
function GCInputXboxPaddleThree: Pointer;
function GCInputXboxPaddleFour: Pointer;
function GCInputDualShockTouchpadOne: Pointer;
function GCInputDualShockTouchpadTwo: Pointer;
function GCInputDualShockTouchpadButton: Pointer;
function GCInputSteeringWheel: Pointer;
function GCInputShifter: Pointer;
function GCInputPedalAccelerator: Pointer;
function GCInputPedalBrake: Pointer;
function GCInputPedalClutch: Pointer;
function GCInputLeftPaddle: Pointer;
function GCInputRightPaddle: Pointer;
function GCCurrentExtendedGamepadSnapshotDataVersion: Pointer;
function GCKeyCodeKeyA: Pointer;
function GCKeyCodeKeyB: Pointer;
function GCKeyCodeKeyC: Pointer;
function GCKeyCodeKeyD: Pointer;
function GCKeyCodeKeyE: Pointer;
function GCKeyCodeKeyF: Pointer;
function GCKeyCodeKeyG: Pointer;
function GCKeyCodeKeyH: Pointer;
function GCKeyCodeKeyI: Pointer;
function GCKeyCodeKeyJ: Pointer;
function GCKeyCodeKeyK: Pointer;
function GCKeyCodeKeyL: Pointer;
function GCKeyCodeKeyM: Pointer;
function GCKeyCodeKeyN: Pointer;
function GCKeyCodeKeyO: Pointer;
function GCKeyCodeKeyP: Pointer;
function GCKeyCodeKeyQ: Pointer;
function GCKeyCodeKeyR: Pointer;
function GCKeyCodeKeyS: Pointer;
function GCKeyCodeKeyT: Pointer;
function GCKeyCodeKeyU: Pointer;
function GCKeyCodeKeyV: Pointer;
function GCKeyCodeKeyW: Pointer;
function GCKeyCodeKeyX: Pointer;
function GCKeyCodeKeyY: Pointer;
function GCKeyCodeKeyZ: Pointer;
function GCKeyCodeOne: Pointer;
function GCKeyCodeTwo: Pointer;
function GCKeyCodeThree: Pointer;
function GCKeyCodeFour: Pointer;
function GCKeyCodeFive: Pointer;
function GCKeyCodeSix: Pointer;
function GCKeyCodeSeven: Pointer;
function GCKeyCodeEight: Pointer;
function GCKeyCodeNine: Pointer;
function GCKeyCodeZero: Pointer;
function GCKeyCodeReturnOrEnter: Pointer;
function GCKeyCodeEscape: Pointer;
function GCKeyCodeDeleteOrBackspace: Pointer;
function GCKeyCodeTab: Pointer;
function GCKeyCodeSpacebar: Pointer;
function GCKeyCodeHyphen: Pointer;
function GCKeyCodeEqualSign: Pointer;
function GCKeyCodeOpenBracket: Pointer;
function GCKeyCodeCloseBracket: Pointer;
function GCKeyCodeBackslash: Pointer;
function GCKeyCodeNonUSPound: Pointer;
function GCKeyCodeSemicolon: Pointer;
function GCKeyCodeQuote: Pointer;
function GCKeyCodeGraveAccentAndTilde: Pointer;
function GCKeyCodeComma: Pointer;
function GCKeyCodePeriod: Pointer;
function GCKeyCodeSlash: Pointer;
function GCKeyCodeCapsLock: Pointer;
function GCKeyCodeF1: Pointer;
function GCKeyCodeF2: Pointer;
function GCKeyCodeF3: Pointer;
function GCKeyCodeF4: Pointer;
function GCKeyCodeF5: Pointer;
function GCKeyCodeF6: Pointer;
function GCKeyCodeF7: Pointer;
function GCKeyCodeF8: Pointer;
function GCKeyCodeF9: Pointer;
function GCKeyCodeF10: Pointer;
function GCKeyCodeF11: Pointer;
function GCKeyCodeF12: Pointer;
function GCKeyCodeF13: Pointer;
function GCKeyCodeF14: Pointer;
function GCKeyCodeF15: Pointer;
function GCKeyCodeF16: Pointer;
function GCKeyCodeF17: Pointer;
function GCKeyCodeF18: Pointer;
function GCKeyCodeF19: Pointer;
function GCKeyCodeF20: Pointer;
function GCKeyCodePrintScreen: Pointer;
function GCKeyCodeScrollLock: Pointer;
function GCKeyCodePause: Pointer;
function GCKeyCodeInsert: Pointer;
function GCKeyCodeHome: Pointer;
function GCKeyCodePageUp: Pointer;
function GCKeyCodeDeleteForward: Pointer;
function GCKeyCodeEnd: Pointer;
function GCKeyCodePageDown: Pointer;
function GCKeyCodeRightArrow: Pointer;
function GCKeyCodeLeftArrow: Pointer;
function GCKeyCodeDownArrow: Pointer;
function GCKeyCodeUpArrow: Pointer;
function GCKeyCodeKeypadNumLock: Pointer;
function GCKeyCodeKeypadSlash: Pointer;
function GCKeyCodeKeypadAsterisk: Pointer;
function GCKeyCodeKeypadHyphen: Pointer;
function GCKeyCodeKeypadPlus: Pointer;
function GCKeyCodeKeypadEnter: Pointer;
function GCKeyCodeKeypad1: Pointer;
function GCKeyCodeKeypad2: Pointer;
function GCKeyCodeKeypad3: Pointer;
function GCKeyCodeKeypad4: Pointer;
function GCKeyCodeKeypad5: Pointer;
function GCKeyCodeKeypad6: Pointer;
function GCKeyCodeKeypad7: Pointer;
function GCKeyCodeKeypad8: Pointer;
function GCKeyCodeKeypad9: Pointer;
function GCKeyCodeKeypad0: Pointer;
function GCKeyCodeKeypadPeriod: Pointer;
function GCKeyCodeKeypadEqualSign: Pointer;
function GCKeyCodeNonUSBackslash: Pointer;
function GCKeyCodeApplication: Pointer;
function GCKeyCodePower: Pointer;
function GCKeyCodeInternational1: Pointer;
function GCKeyCodeInternational2: Pointer;
function GCKeyCodeInternational3: Pointer;
function GCKeyCodeInternational4: Pointer;
function GCKeyCodeInternational5: Pointer;
function GCKeyCodeInternational6: Pointer;
function GCKeyCodeInternational7: Pointer;
function GCKeyCodeInternational8: Pointer;
function GCKeyCodeInternational9: Pointer;
function GCKeyCodeLANG1: Pointer;
function GCKeyCodeLANG2: Pointer;
function GCKeyCodeLANG3: Pointer;
function GCKeyCodeLANG4: Pointer;
function GCKeyCodeLANG5: Pointer;
function GCKeyCodeLANG6: Pointer;
function GCKeyCodeLANG7: Pointer;
function GCKeyCodeLANG8: Pointer;
function GCKeyCodeLANG9: Pointer;
function GCKeyCodeLeftControl: Pointer;
function GCKeyCodeLeftShift: Pointer;
function GCKeyCodeLeftAlt: Pointer;
function GCKeyCodeLeftGUI: Pointer;
function GCKeyCodeRightControl: Pointer;
function GCKeyCodeRightShift: Pointer;
function GCKeyCodeRightAlt: Pointer;
function GCKeyCodeRightGUI: Pointer;
function GCKeyA: NSString;
function GCKeyB: NSString;
function GCKeyC: NSString;
function GCKeyD: NSString;
function GCKeyE: NSString;
function GCKeyF: NSString;
function GCKeyG: NSString;
function GCKeyH: NSString;
function GCKeyI: NSString;
function GCKeyJ: NSString;
function GCKeyK: NSString;
function GCKeyL: NSString;
function GCKeyM: NSString;
function GCKeyN: NSString;
function GCKeyO: NSString;
function GCKeyP: NSString;
function GCKeyQ: NSString;
function GCKeyR: NSString;
function GCKeyS: NSString;
function GCKeyT: NSString;
function GCKeyU: NSString;
function GCKeyV: NSString;
function GCKeyW: NSString;
function GCKeyX: NSString;
function GCKeyY: NSString;
function GCKeyZ: NSString;
function GCKeyOne: NSString;
function GCKeyTwo: NSString;
function GCKeyThree: NSString;
function GCKeyFour: NSString;
function GCKeyFive: NSString;
function GCKeySix: NSString;
function GCKeySeven: NSString;
function GCKeyEight: NSString;
function GCKeyNine: NSString;
function GCKeyZero: NSString;
function GCKeyReturnOrEnter: NSString;
function GCKeyEscape: NSString;
function GCKeyDeleteOrBackspace: NSString;
function GCKeyTab: NSString;
function GCKeySpacebar: NSString;
function GCKeyHyphen: NSString;
function GCKeyEqualSign: NSString;
function GCKeyOpenBracket: NSString;
function GCKeyCloseBracket: NSString;
function GCKeyBackslash: NSString;
function GCKeyNonUSPound: NSString;
function GCKeySemicolon: NSString;
function GCKeyQuote: NSString;
function GCKeyGraveAccentAndTilde: NSString;
function GCKeyComma: NSString;
function GCKeyPeriod: NSString;
function GCKeySlash: NSString;
function GCKeyCapsLock: NSString;
function GCKeyF1: NSString;
function GCKeyF2: NSString;
function GCKeyF3: NSString;
function GCKeyF4: NSString;
function GCKeyF5: NSString;
function GCKeyF6: NSString;
function GCKeyF7: NSString;
function GCKeyF8: NSString;
function GCKeyF9: NSString;
function GCKeyF10: NSString;
function GCKeyF11: NSString;
function GCKeyF12: NSString;
function GCKeyF13: NSString;
function GCKeyF14: NSString;
function GCKeyF15: NSString;
function GCKeyF16: NSString;
function GCKeyF17: NSString;
function GCKeyF18: NSString;
function GCKeyF19: NSString;
function GCKeyF20: NSString;
function GCKeyPrintScreen: NSString;
function GCKeyScrollLock: NSString;
function GCKeyPause: NSString;
function GCKeyInsert: NSString;
function GCKeyHome: NSString;
function GCKeyPageUp: NSString;
function GCKeyDeleteForward: NSString;
function GCKeyEnd: NSString;
function GCKeyPageDown: NSString;
function GCKeyRightArrow: NSString;
function GCKeyLeftArrow: NSString;
function GCKeyDownArrow: NSString;
function GCKeyUpArrow: NSString;
function GCKeyKeypadNumLock: NSString;
function GCKeyKeypadSlash: NSString;
function GCKeyKeypadAsterisk: NSString;
function GCKeyKeypadHyphen: NSString;
function GCKeyKeypadPlus: NSString;
function GCKeyKeypadEnter: NSString;
function GCKeyKeypad1: NSString;
function GCKeyKeypad2: NSString;
function GCKeyKeypad3: NSString;
function GCKeyKeypad4: NSString;
function GCKeyKeypad5: NSString;
function GCKeyKeypad6: NSString;
function GCKeyKeypad7: NSString;
function GCKeyKeypad8: NSString;
function GCKeyKeypad9: NSString;
function GCKeyKeypad0: NSString;
function GCKeyKeypadPeriod: NSString;
function GCKeyKeypadEqualSign: NSString;
function GCKeyNonUSBackslash: NSString;
function GCKeyApplication: NSString;
function GCKeyPower: NSString;
function GCKeyInternational1: NSString;
function GCKeyInternational2: NSString;
function GCKeyInternational3: NSString;
function GCKeyInternational4: NSString;
function GCKeyInternational5: NSString;
function GCKeyInternational6: NSString;
function GCKeyInternational7: NSString;
function GCKeyInternational8: NSString;
function GCKeyInternational9: NSString;
function GCKeyLANG1: NSString;
function GCKeyLANG2: NSString;
function GCKeyLANG3: NSString;
function GCKeyLANG4: NSString;
function GCKeyLANG5: NSString;
function GCKeyLANG6: NSString;
function GCKeyLANG7: NSString;
function GCKeyLANG8: NSString;
function GCKeyLANG9: NSString;
function GCKeyLeftControl: NSString;
function GCKeyLeftShift: NSString;
function GCKeyLeftAlt: NSString;
function GCKeyLeftGUI: NSString;
function GCKeyRightControl: NSString;
function GCKeyRightShift: NSString;
function GCKeyRightAlt: NSString;
function GCKeyRightGUI: NSString;
function GCInputMicroGamepadDpad: NSString;
function GCInputMicroGamepadButtonA: NSString;
function GCInputMicroGamepadButtonX: NSString;
function GCInputMicroGamepadButtonMenu: NSString;
function GCCurrentMicroGamepadSnapshotDataVersion: Pointer;
function GCInputDirectionalDpad: NSString;
function GCInputDirectionalTouchSurfaceButton: NSString;
function GCInputDirectionalCardinalDpad: NSString;
function GCInputDirectionalCenterButton: NSString;
function GCProductCategoryDualSense: NSString;
function GCProductCategoryDualShock4: NSString;
function GCProductCategoryMFi: NSString;
function GCProductCategoryXboxOne: NSString;
function GCProductCategoryHID: NSString;
function GCProductCategorySiriRemote1stGen: NSString;
function GCProductCategorySiriRemote2ndGen: NSString;
function GCProductCategoryControlCenterRemote: NSString;
function GCProductCategoryUniversalElectronicsRemote: NSString;
function GCProductCategoryCoalescedRemote: NSString;
function GCProductCategoryMouse: NSString;
function GCProductCategoryKeyboard: NSString;
function GCControllerDidConnectNotification: NSString;
function GCControllerDidDisconnectNotification: NSString;
function GCControllerDidBecomeCurrentNotification: NSString;
function GCControllerDidStopBeingCurrentNotification: NSString;
function GCControllerUserCustomizationsDidChangeNotification: NSString;
function GCKeyboardDidConnectNotification: NSString;
function GCKeyboardDidDisconnectNotification: NSString;
function GCMouseDidConnectNotification: NSString;
function GCMouseDidDisconnectNotification: NSString;
function GCMouseDidBecomeCurrentNotification: NSString;
function GCMouseDidStopBeingCurrentNotification: NSString;
function GCRacingWheelDidConnectNotification: NSString;
function GCRacingWheelDidDisconnectNotification: NSString;
function GCHapticsLocalityDefault: Pointer;
function GCHapticsLocalityAll: Pointer;
function GCHapticsLocalityHandles: Pointer;
function GCHapticsLocalityLeftHandle: Pointer;
function GCHapticsLocalityRightHandle: Pointer;
function GCHapticsLocalityTriggers: Pointer;
function GCHapticsLocalityLeftTrigger: Pointer;
function GCHapticsLocalityRightTrigger: Pointer;
function GCHapticDurationInfinite: Pointer;


// ===== External functions =====

const
  libGameController =
    '/System/Library/Frameworks/GameController.framework/GameController';
function GCGamepadSnapShotDataV100FromNSData(snapshotData
  : PGCGamepadSnapShotDataV100; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU + 'GCGamepadSnapShotDataV100FromNSData';
function NSDataFromGCGamepadSnapShotDataV100(snapshotData
  : PGCGamepadSnapShotDataV100): Pointer { NSData }; cdecl;
  external libGameController name _PU + 'NSDataFromGCGamepadSnapShotDataV100';
function GCExtendedGamepadSnapshotDataFromNSData(snapshotData
  : PGCExtendedGamepadSnapshotData; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU +
  'GCExtendedGamepadSnapshotDataFromNSData';
function NSDataFromGCExtendedGamepadSnapshotData(snapshotData
  : PGCExtendedGamepadSnapshotData): Pointer { NSData }; cdecl;
  external libGameController name _PU +
  'NSDataFromGCExtendedGamepadSnapshotData';
function GCExtendedGamepadSnapShotDataV100FromNSData(snapshotData
  : PGCExtendedGamepadSnapShotDataV100; data: Pointer { NSData } ): Boolean;
  cdecl; external libGameController name _PU +
  'GCExtendedGamepadSnapShotDataV100FromNSData';
function NSDataFromGCExtendedGamepadSnapShotDataV100(snapshotData
  : PGCExtendedGamepadSnapShotDataV100): Pointer { NSData }; cdecl;
  external libGameController name _PU +
  'NSDataFromGCExtendedGamepadSnapShotDataV100';
function GCMicroGamepadSnapshotDataFromNSData(snapshotData
  : PGCMicroGamepadSnapshotData; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU + 'GCMicroGamepadSnapshotDataFromNSData';
function NSDataFromGCMicroGamepadSnapshotData(snapshotData
  : PGCMicroGamepadSnapshotData): Pointer { NSData }; cdecl;
  external libGameController name _PU + 'NSDataFromGCMicroGamepadSnapshotData';
function GCMicroGamepadSnapShotDataV100FromNSData(snapshotData
  : PGCMicroGamepadSnapShotDataV100; data: Pointer { NSData } ): Boolean; cdecl;
  external libGameController name _PU +
  'GCMicroGamepadSnapShotDataV100FromNSData';
function NSDataFromGCMicroGamepadSnapShotDataV100(snapshotData
  : PGCMicroGamepadSnapShotDataV100): Pointer { NSData }; cdecl;
  external libGameController name _PU +
  'NSDataFromGCMicroGamepadSnapShotDataV100';

implementation

{$IF defined(IOS) and NOT defined(CPUARM)}

uses
  Posix.Dlfcn;

var
  GameControllerModule: THandle;

{$ENDIF IOS}

function GCKeyA: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyA');
end;

function GCKeyB: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyB');
end;

function GCKeyC: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyC');
end;

function GCKeyD: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyD');
end;

function GCKeyE: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyE');
end;

function GCKeyF: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF');
end;

function GCKeyG: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyG');
end;

function GCKeyH: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyH');
end;

function GCKeyI: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyI');
end;

function GCKeyJ: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyJ');
end;

function GCKeyK: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyK');
end;

function GCKeyL: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyL');
end;

function GCKeyM: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyM');
end;

function GCKeyN: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyN');
end;

function GCKeyO: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyO');
end;

function GCKeyP: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyP');
end;

function GCKeyQ: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyQ');
end;

function GCKeyR: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyR');
end;

function GCKeyS: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyS');
end;

function GCKeyT: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyT');
end;

function GCKeyU: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyU');
end;

function GCKeyV: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyV');
end;

function GCKeyW: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyW');
end;

function GCKeyX: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyX');
end;

function GCKeyY: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyY');
end;

function GCKeyZ: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyZ');
end;

function GCKeyOne: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyOne');
end;

function GCKeyTwo: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyTwo');
end;

function GCKeyThree: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyThree');
end;

function GCKeyFour: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyFour');
end;

function GCKeyFive: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyFive');
end;

function GCKeySix: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySix');
end;

function GCKeySeven: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySeven');
end;

function GCKeyEight: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEight');
end;

function GCKeyNine: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyNine');
end;

function GCKeyZero: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyZero');
end;

function GCKeyReturnOrEnter: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyReturnOrEnter');
end;

function GCKeyEscape: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEscape');
end;

function GCKeyDeleteOrBackspace: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyDeleteOrBackspace');
end;

function GCKeyTab: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyTab');
end;

function GCKeySpacebar: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySpacebar');
end;

function GCKeyHyphen: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyHyphen');
end;

function GCKeyEqualSign: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEqualSign');
end;

function GCKeyOpenBracket: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyOpenBracket');
end;

function GCKeyCloseBracket: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyCloseBracket');
end;

function GCKeyBackslash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyBackslash');
end;

function GCKeyNonUSPound: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyNonUSPound');
end;

function GCKeySemicolon: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySemicolon');
end;

function GCKeyQuote: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyQuote');
end;

function GCKeyGraveAccentAndTilde: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyGraveAccentAndTilde');
end;

function GCKeyComma: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyComma');
end;

function GCKeyPeriod: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPeriod');
end;

function GCKeySlash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeySlash');
end;

function GCKeyCapsLock: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyCapsLock');
end;

function GCKeyF1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF1');
end;

function GCKeyF2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF2');
end;

function GCKeyF3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF3');
end;

function GCKeyF4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF4');
end;

function GCKeyF5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF5');
end;

function GCKeyF6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF6');
end;

function GCKeyF7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF7');
end;

function GCKeyF8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF8');
end;

function GCKeyF9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF9');
end;

function GCKeyF10: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF10');
end;

function GCKeyF11: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF11');
end;

function GCKeyF12: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF12');
end;

function GCKeyF13: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF13');
end;

function GCKeyF14: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF14');
end;

function GCKeyF15: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF15');
end;

function GCKeyF16: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF16');
end;

function GCKeyF17: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF17');
end;

function GCKeyF18: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF18');
end;

function GCKeyF19: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF19');
end;

function GCKeyF20: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyF20');
end;

function GCKeyPrintScreen: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPrintScreen');
end;

function GCKeyScrollLock: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyScrollLock');
end;

function GCKeyPause: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPause');
end;

function GCKeyInsert: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInsert');
end;

function GCKeyHome: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyHome');
end;

function GCKeyPageUp: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPageUp');
end;

function GCKeyDeleteForward: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyDeleteForward');
end;

function GCKeyEnd: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyEnd');
end;

function GCKeyPageDown: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPageDown');
end;

function GCKeyRightArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightArrow');
end;

function GCKeyLeftArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftArrow');
end;

function GCKeyDownArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyDownArrow');
end;

function GCKeyUpArrow: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyUpArrow');
end;

function GCKeyKeypadNumLock: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadNumLock');
end;

function GCKeyKeypadSlash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadSlash');
end;

function GCKeyKeypadAsterisk: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadAsterisk');
end;

function GCKeyKeypadHyphen: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadHyphen');
end;

function GCKeyKeypadPlus: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadPlus');
end;

function GCKeyKeypadEnter: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadEnter');
end;

function GCKeyKeypad1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad1');
end;

function GCKeyKeypad2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad2');
end;

function GCKeyKeypad3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad3');
end;

function GCKeyKeypad4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad4');
end;

function GCKeyKeypad5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad5');
end;

function GCKeyKeypad6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad6');
end;

function GCKeyKeypad7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad7');
end;

function GCKeyKeypad8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad8');
end;

function GCKeyKeypad9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad9');
end;

function GCKeyKeypad0: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypad0');
end;

function GCKeyKeypadPeriod: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadPeriod');
end;

function GCKeyKeypadEqualSign: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyKeypadEqualSign');
end;

function GCKeyNonUSBackslash: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyNonUSBackslash');
end;

function GCKeyApplication: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyApplication');
end;

function GCKeyPower: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyPower');
end;

function GCKeyInternational1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational1');
end;

function GCKeyInternational2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational2');
end;

function GCKeyInternational3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational3');
end;

function GCKeyInternational4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational4');
end;

function GCKeyInternational5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational5');
end;

function GCKeyInternational6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational6');
end;

function GCKeyInternational7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational7');
end;

function GCKeyInternational8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational8');
end;

function GCKeyInternational9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyInternational9');
end;

function GCKeyLANG1: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG1');
end;

function GCKeyLANG2: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG2');
end;

function GCKeyLANG3: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG3');
end;

function GCKeyLANG4: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG4');
end;

function GCKeyLANG5: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG5');
end;

function GCKeyLANG6: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG6');
end;

function GCKeyLANG7: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG7');
end;

function GCKeyLANG8: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG8');
end;

function GCKeyLANG9: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLANG9');
end;

function GCKeyLeftControl: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftControl');
end;

function GCKeyLeftShift: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftShift');
end;

function GCKeyLeftAlt: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftAlt');
end;

function GCKeyLeftGUI: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyLeftGUI');
end;

function GCKeyRightControl: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightControl');
end;

function GCKeyRightShift: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightShift');
end;

function GCKeyRightAlt: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightAlt');
end;

function GCKeyRightGUI: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCKeyRightGUI');
end;

function GCInputMicroGamepadDpad: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputMicroGamepadDpad');
end;

function GCInputMicroGamepadButtonA: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputMicroGamepadButtonA');
end;

function GCInputMicroGamepadButtonX: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputMicroGamepadButtonX');
end;

function GCInputMicroGamepadButtonMenu: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputMicroGamepadButtonMenu');
end;

function GCInputDirectionalDpad: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCInputDirectionalDpad');
end;

function GCInputDirectionalTouchSurfaceButton: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputDirectionalTouchSurfaceButton');
end;

function GCInputDirectionalCardinalDpad: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputDirectionalCardinalDpad');
end;

function GCInputDirectionalCenterButton: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCInputDirectionalCenterButton');
end;

function GCProductCategoryDualSense: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryDualSense');
end;

function GCProductCategoryDualShock4: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryDualShock4');
end;

function GCProductCategoryMFi: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryMFi');
end;

function GCProductCategoryXboxOne: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryXboxOne');
end;

function GCProductCategoryHID: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryHID');
end;

function GCProductCategorySiriRemote1stGen: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategorySiriRemote1stGen');
end;

function GCProductCategorySiriRemote2ndGen: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategorySiriRemote2ndGen');
end;

function GCProductCategoryControlCenterRemote: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryControlCenterRemote');
end;

function GCProductCategoryUniversalElectronicsRemote: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryUniversalElectronicsRemote');
end;

function GCProductCategoryCoalescedRemote: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCProductCategoryCoalescedRemote');
end;

function GCProductCategoryMouse: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryMouse');
end;

function GCProductCategoryKeyboard: NSString;
begin
  Result := CocoaNSStringConst(libGameController, 'GCProductCategoryKeyboard');
end;

function GCControllerDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidConnectNotification');
end;

function GCControllerDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidDisconnectNotification');
end;

function GCControllerDidBecomeCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidBecomeCurrentNotification');
end;

function GCControllerDidStopBeingCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerDidStopBeingCurrentNotification');
end;

function GCControllerUserCustomizationsDidChangeNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCControllerUserCustomizationsDidChangeNotification');
end;

function GCKeyboardDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCKeyboardDidConnectNotification');
end;

function GCKeyboardDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCKeyboardDidDisconnectNotification');
end;

function GCMouseDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidConnectNotification');
end;

function GCMouseDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidDisconnectNotification');
end;

function GCMouseDidBecomeCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidBecomeCurrentNotification');
end;

function GCMouseDidStopBeingCurrentNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCMouseDidStopBeingCurrentNotification');
end;

function GCRacingWheelDidConnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCRacingWheelDidConnectNotification');
end;

function GCRacingWheelDidDisconnectNotification: NSString;
begin
  Result := CocoaNSStringConst(libGameController,
    'GCRacingWheelDidDisconnectNotification');
end;

function GCInputButtonA: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonA');
end;

function GCInputButtonB: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonB');
end;

function GCInputButtonX: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonX');
end;

function GCInputButtonY: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonY');
end;

function GCInputDirectionPad: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputDirectionPad');
end;

function GCInputLeftThumbstick: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftThumbstick');
end;

function GCInputRightThumbstick: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightThumbstick');
end;

function GCInputLeftShoulder: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftShoulder');
end;

function GCInputRightShoulder: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightShoulder');
end;

function GCInputLeftTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftTrigger');
end;

function GCInputRightTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightTrigger');
end;

function GCInputLeftThumbstickButton: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftThumbstickButton');
end;

function GCInputRightThumbstickButton: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCInputRightThumbstickButton');
end;

function GCInputButtonHome: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonHome');
end;

function GCInputButtonMenu: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonMenu');
end;

function GCInputButtonOptions: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonOptions');
end;

function GCInputButtonShare: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputButtonShare');
end;

function GCInputXboxPaddleOne: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleOne');
end;

function GCInputXboxPaddleTwo: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleTwo');
end;

function GCInputXboxPaddleThree: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleThree');
end;

function GCInputXboxPaddleFour: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputXboxPaddleFour');
end;

function GCInputDualShockTouchpadOne: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputDualShockTouchpadOne');
end;

function GCInputDualShockTouchpadTwo: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputDualShockTouchpadTwo');
end;

function GCInputDualShockTouchpadButton: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCInputDualShockTouchpadButton');
end;

function GCInputSteeringWheel: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputSteeringWheel');
end;

function GCInputShifter: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputShifter');
end;

function GCInputPedalAccelerator: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputPedalAccelerator');
end;

function GCInputPedalBrake: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputPedalBrake');
end;

function GCInputPedalClutch: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputPedalClutch');
end;

function GCInputLeftPaddle: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputLeftPaddle');
end;

function GCInputRightPaddle: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCInputRightPaddle');
end;

function GCCurrentExtendedGamepadSnapshotDataVersion: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCCurrentExtendedGamepadSnapshotDataVersion');
end;

function GCKeyCodeKeyA: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyA');
end;

function GCKeyCodeKeyB: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyB');
end;

function GCKeyCodeKeyC: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyC');
end;

function GCKeyCodeKeyD: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyD');
end;

function GCKeyCodeKeyE: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyE');
end;

function GCKeyCodeKeyF: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyF');
end;

function GCKeyCodeKeyG: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyG');
end;

function GCKeyCodeKeyH: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyH');
end;

function GCKeyCodeKeyI: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyI');
end;

function GCKeyCodeKeyJ: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyJ');
end;

function GCKeyCodeKeyK: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyK');
end;

function GCKeyCodeKeyL: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyL');
end;

function GCKeyCodeKeyM: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyM');
end;

function GCKeyCodeKeyN: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyN');
end;

function GCKeyCodeKeyO: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyO');
end;

function GCKeyCodeKeyP: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyP');
end;

function GCKeyCodeKeyQ: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyQ');
end;

function GCKeyCodeKeyR: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyR');
end;

function GCKeyCodeKeyS: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyS');
end;

function GCKeyCodeKeyT: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyT');
end;

function GCKeyCodeKeyU: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyU');
end;

function GCKeyCodeKeyV: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyV');
end;

function GCKeyCodeKeyW: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyW');
end;

function GCKeyCodeKeyX: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyX');
end;

function GCKeyCodeKeyY: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyY');
end;

function GCKeyCodeKeyZ: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeyZ');
end;

function GCKeyCodeOne: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeOne');
end;

function GCKeyCodeTwo: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeTwo');
end;

function GCKeyCodeThree: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeThree');
end;

function GCKeyCodeFour: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeFour');
end;

function GCKeyCodeFive: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeFive');
end;

function GCKeyCodeSix: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSix');
end;

function GCKeyCodeSeven: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSeven');
end;

function GCKeyCodeEight: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEight');
end;

function GCKeyCodeNine: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeNine');
end;

function GCKeyCodeZero: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeZero');
end;

function GCKeyCodeReturnOrEnter: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeReturnOrEnter');
end;

function GCKeyCodeEscape: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEscape');
end;

function GCKeyCodeDeleteOrBackspace: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeDeleteOrBackspace');
end;

function GCKeyCodeTab: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeTab');
end;

function GCKeyCodeSpacebar: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSpacebar');
end;

function GCKeyCodeHyphen: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeHyphen');
end;

function GCKeyCodeEqualSign: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEqualSign');
end;

function GCKeyCodeOpenBracket: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeOpenBracket');
end;

function GCKeyCodeCloseBracket: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeCloseBracket');
end;

function GCKeyCodeBackslash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeBackslash');
end;

function GCKeyCodeNonUSPound: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeNonUSPound');
end;

function GCKeyCodeSemicolon: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSemicolon');
end;

function GCKeyCodeQuote: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeQuote');
end;

function GCKeyCodeGraveAccentAndTilde: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCKeyCodeGraveAccentAndTilde');
end;

function GCKeyCodeComma: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeComma');
end;

function GCKeyCodePeriod: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePeriod');
end;

function GCKeyCodeSlash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeSlash');
end;

function GCKeyCodeCapsLock: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeCapsLock');
end;

function GCKeyCodeF1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF1');
end;

function GCKeyCodeF2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF2');
end;

function GCKeyCodeF3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF3');
end;

function GCKeyCodeF4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF4');
end;

function GCKeyCodeF5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF5');
end;

function GCKeyCodeF6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF6');
end;

function GCKeyCodeF7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF7');
end;

function GCKeyCodeF8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF8');
end;

function GCKeyCodeF9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF9');
end;

function GCKeyCodeF10: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF10');
end;

function GCKeyCodeF11: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF11');
end;

function GCKeyCodeF12: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF12');
end;

function GCKeyCodeF13: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF13');
end;

function GCKeyCodeF14: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF14');
end;

function GCKeyCodeF15: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF15');
end;

function GCKeyCodeF16: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF16');
end;

function GCKeyCodeF17: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF17');
end;

function GCKeyCodeF18: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF18');
end;

function GCKeyCodeF19: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF19');
end;

function GCKeyCodeF20: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeF20');
end;

function GCKeyCodePrintScreen: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePrintScreen');
end;

function GCKeyCodeScrollLock: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeScrollLock');
end;

function GCKeyCodePause: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePause');
end;

function GCKeyCodeInsert: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInsert');
end;

function GCKeyCodeHome: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeHome');
end;

function GCKeyCodePageUp: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePageUp');
end;

function GCKeyCodeDeleteForward: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeDeleteForward');
end;

function GCKeyCodeEnd: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeEnd');
end;

function GCKeyCodePageDown: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePageDown');
end;

function GCKeyCodeRightArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightArrow');
end;

function GCKeyCodeLeftArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftArrow');
end;

function GCKeyCodeDownArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeDownArrow');
end;

function GCKeyCodeUpArrow: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeUpArrow');
end;

function GCKeyCodeKeypadNumLock: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadNumLock');
end;

function GCKeyCodeKeypadSlash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadSlash');
end;

function GCKeyCodeKeypadAsterisk: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadAsterisk');
end;

function GCKeyCodeKeypadHyphen: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadHyphen');
end;

function GCKeyCodeKeypadPlus: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadPlus');
end;

function GCKeyCodeKeypadEnter: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadEnter');
end;

function GCKeyCodeKeypad1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad1');
end;

function GCKeyCodeKeypad2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad2');
end;

function GCKeyCodeKeypad3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad3');
end;

function GCKeyCodeKeypad4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad4');
end;

function GCKeyCodeKeypad5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad5');
end;

function GCKeyCodeKeypad6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad6');
end;

function GCKeyCodeKeypad7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad7');
end;

function GCKeyCodeKeypad8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad8');
end;

function GCKeyCodeKeypad9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad9');
end;

function GCKeyCodeKeypad0: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypad0');
end;

function GCKeyCodeKeypadPeriod: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadPeriod');
end;

function GCKeyCodeKeypadEqualSign: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeKeypadEqualSign');
end;

function GCKeyCodeNonUSBackslash: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeNonUSBackslash');
end;

function GCKeyCodeApplication: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeApplication');
end;

function GCKeyCodePower: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodePower');
end;

function GCKeyCodeInternational1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational1');
end;

function GCKeyCodeInternational2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational2');
end;

function GCKeyCodeInternational3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational3');
end;

function GCKeyCodeInternational4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational4');
end;

function GCKeyCodeInternational5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational5');
end;

function GCKeyCodeInternational6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational6');
end;

function GCKeyCodeInternational7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational7');
end;

function GCKeyCodeInternational8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational8');
end;

function GCKeyCodeInternational9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeInternational9');
end;

function GCKeyCodeLANG1: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG1');
end;

function GCKeyCodeLANG2: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG2');
end;

function GCKeyCodeLANG3: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG3');
end;

function GCKeyCodeLANG4: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG4');
end;

function GCKeyCodeLANG5: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG5');
end;

function GCKeyCodeLANG6: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG6');
end;

function GCKeyCodeLANG7: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG7');
end;

function GCKeyCodeLANG8: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG8');
end;

function GCKeyCodeLANG9: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLANG9');
end;

function GCKeyCodeLeftControl: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftControl');
end;

function GCKeyCodeLeftShift: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftShift');
end;

function GCKeyCodeLeftAlt: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftAlt');
end;

function GCKeyCodeLeftGUI: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeLeftGUI');
end;

function GCKeyCodeRightControl: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightControl');
end;

function GCKeyCodeRightShift: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightShift');
end;

function GCKeyCodeRightAlt: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightAlt');
end;

function GCKeyCodeRightGUI: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCKeyCodeRightGUI');
end;

function GCCurrentMicroGamepadSnapshotDataVersion: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCCurrentMicroGamepadSnapshotDataVersion');
end;

function GCHapticsLocalityDefault: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityDefault');
end;

function GCHapticsLocalityAll: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityAll');
end;

function GCHapticsLocalityHandles: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityHandles');
end;

function GCHapticsLocalityLeftHandle: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityLeftHandle');
end;

function GCHapticsLocalityRightHandle: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCHapticsLocalityRightHandle');
end;

function GCHapticsLocalityTriggers: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticsLocalityTriggers');
end;

function GCHapticsLocalityLeftTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCHapticsLocalityLeftTrigger');
end;

function GCHapticsLocalityRightTrigger: Pointer;
begin
  Result := CocoaPointerConst(libGameController,
    'GCHapticsLocalityRightTrigger');
end;

function GCHapticDurationInfinite: Pointer;
begin
  Result := CocoaPointerConst(libGameController, 'GCHapticDurationInfinite');
end;

{$IF defined(IOS) and NOT defined(CPUARM)}

initialization

GameControllerModule := dlopen(MarshaledAString(libGameController), RTLD_LAZY);

finalization

dlclose(GameControllerModule);
{$ENDIF IOS}
{$ELSE}

implementation

{$ENDIF}

end.
