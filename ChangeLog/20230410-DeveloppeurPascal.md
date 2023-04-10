# 20230410 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* renamed repository from "FMXGameEngine" to Delphi-Game-Engine
* renamed FMX WinAPI unit in RTL unit and removed all FMX dependencies
* updated the Joystick sample project after units renaming
* changed inheritance and added a common TGamolfCustomJoystickService for all platform implementations
* added a TGamolfCustomJoystickService.isDPad() with a list of values, with a JoystickID or a DPad as value to compare
* added TGamolfCustomJoystickService.getDPadFromXY()
* added TGamolfCustomJoystickService.getXYFromDPad() and some isDPad() methods
* added copyright infos in Gamolf.xxx.Joystick units header
* added a hasCaps() function method
* updated TGamolfCustomJoystickService.getDPad() parameters
* updated TGamolfCustomJoystickService.getDPadFromXY() return type (word instead of TJoystickDPad)
* changed isConnected() and getDevCaps() methods for joystick management
* added a IGamolfJoystickService.StartDiscovery method to refresh the number of connected(or total) game controller and the list of thei capabilities (for Windows API)
* added a IGamolfJoystickService.ForEach() method to get infos for all declared game controllers
* added a IGamolfJoystickService.ForEachConnectedDevice() method to get infos for all connected game controllers
* updated sample program to use the new IGamolfJoystickService.ForEach() method
* added a release on GitHub repository
