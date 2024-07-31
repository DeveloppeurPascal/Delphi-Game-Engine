# 20240731 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Travail sur le composant TDGEGamepadDetected

* adaptation du composant pour les projets VCL

## Travail sur tous composants

* ajout de la liste des plateformes supportées pour TGamepad, TGamepadManager et TDGEGamepadDetected
* dépublication de la propriété TagObject qui n'avait rien à faire dans l'inspecteur d'objet en conception de fiche

## Travail sur le composant TDGEGamepadDetected

* paramétrage de la durée d'affichage en secondes à l'aide de la nouvelle propriété "Duration"

## Travail sur le composant TDGEHelpBar

* création d'un programme VCL de test de ce composant
* création d'un programme FMX de test de ce composant
* création de l'unité Gamolf.RTL.HelpBar.pas
* création de la classe du composant avec ses propriétés
* ajout de son unité dans les paquets de composants du projet
* codage du composant pour les projets FireMonkey
* ajout d'images SVG d'exemples du pack "Input Prompts" de Kenney.nl
* tests OK sur Windows et MacOS malgré une perte d'un TTabList (à identifier plus tard, problème mineur non bloquant)
* création d'un paquet de composants Delphi 12 Alexandria pour les composants FMX dont TDGEFMXHelpBar
