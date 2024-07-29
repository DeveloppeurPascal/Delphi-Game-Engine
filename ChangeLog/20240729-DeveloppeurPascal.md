# 20240729 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Travail sur les composants TDGEGamepadXXX.

* renommage du composant TGamepadManager en TDGEGamepadManager
* renommage du composant TGamepad en TDGEGamepad
* ajout d'une unité Gamolf.RTL.Joystick.Deprecated.pas pour référencer les anciens composants (en descendants du nouveau nom) tout en indiquant qu'ils sont dépréciés (pour simplifier l'ouverture des projets existants et la migration vers les bons composants)
* création d'un paquet de composants "with deprecated" avec l'intégralité des composants (corrects et dépréciés) pour Delphi 12 Athens
* création d'un paquet de composants "with deprecated" avec l'intégralité des composants (corrects et dépréciés) pour Delphi 11 Alexandria
* mise à jour des exemples de projets utilisants les composants TGamepad et TGamepadManager afin de passer sur le bon nom

----- mise à jour des exemples sur One shot Tools
----- mise à jour sur Ploumtris

* renommage de TGamepadList en TDGEGamepadList pour être cohérent avec son contenu
* correction de la détection des pertes de connexion des contrôleurs de jeux et de la propagation de l'information
* optimisation de la détection d'une nouvelle connexion
* un test de connexion de chaque device détecté (mais non connecté) est effectuée chaque seconde
* seuls les devices connectés sont interrogés pour limiter les interrogations inutiles sur devices déconnectés
* correction de la détection de déconnexion sur Mac (on "perdait" le device mais on n'avait pas l'info sur la librairie)

## Travail sur le composant TDGEGamepadDetected

* création d'un programme FMX de test de ce composant
* création de l'unité Gamolf.RTL.GamepadDetected.pas
* référencement du composant TDGEGamepadDetected auprès de FireMonkey
* codage de la classe servant de composant non visuel
