# 20240725 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

Travail principal du jour : récupérer les modifications faites dans [Gamepad-UI-Tests](https://github.com/DeveloppeurPascal/one-shot-tools/tree/main/gamepad-ui-tests) du dépôt [One Shot Tools](https://github.com/DeveloppeurPascal/one-shot-tools) pour les réinjecter dnas le moteur de gestion des contrôleurs de jeux.

* collage des fichiers et modifications faites sur OneShotTools : Gamolf.VCL.Joystick en tant que fichier, uJoystickManager.pas fusionné dans Gamolf.RTL.Joystick.pas
* vérification du fonctionnement des exemples existants utilisant les joysticks sous Windows et Mac
* [changement du mapage](https://github.com/DeveloppeurPascal/Delphi-Game-Engine/issues/129) des axes pour Windows.DirectInput afin de coller à ce qui se fait sous Mac/iOS pour un contrôleur Xbox **ATTENTION rupture de compatibilité ascendante !!!** 
* mise en place d'un paquet de composants pour Delphi 12 Athens contenant TGamepad et TGamepadManager
