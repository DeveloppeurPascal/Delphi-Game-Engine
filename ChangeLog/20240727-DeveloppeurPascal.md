# 20240727 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Travail sur Game Controller.

* Tour et tri des TODOs à traiter sur GitHub.
* Ajout de la fonction hasJoystickButtonsAPI() à l'interface IGamolfJoystickService pour s'assurer qu'elle est bien implémentée et accessible partout.
* Publication de la propriété "Enabled" sur le composant TGameManager
* Création du paquet de composants pour Delphi 11.3 Alexandria et adaptation du code source de l'unité Gamolf.RTL.Joystick pour le rendre compatible avec cette version (valeurs énumérées en création de dictionnaire)
* Création du paquet de composants pour Delphi 10.4 Sydney
* Création du paquet de composants pour Delphi 10.3 Rio et adaptation du code source de l'unité Gamolf.RTL.Joystick pour le rendre compatible avec cette version (class constructor et class destructor non compatibles)
* Création du paquet de composants pour Delphi 10.2 Tokyo
* Création du paquet de composants pour Delphi 10.1 Berlin
* ajout d'un projet de test en 10.1 Berlin afin de s'assurer que les composants fonctionnent au moins sous Windows en VCL
* ajout d'un projet de test en 10.1 Berlin afin de s'assurer que les composants fonctionnent au moins sous Windows en FireMonkey
* adaptation des unités de la librairie pour que la compilation fonctione avec ce projet de test et que les fonctionnalités de base soient opérationnelles dans les versions de Delphi prises en charge
* changement de l'ordre des valeurs dans TJoystickButtons afin de mapper les boutons du contrôleur de jeu Xbox avec DirectInput sur les mêmes boutons que sur les autres plateformes (à tester sur d'autres contrôleur de jeu sous windows pour compatibilité)
