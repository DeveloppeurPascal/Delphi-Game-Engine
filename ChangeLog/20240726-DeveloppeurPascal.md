# 20240726 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)


* déplacement du paquet de composants pour Delphi 12 Athens vers le bon dossier
* correction du lien vers la licence AGPL 3 dans lea doc FR
* réorganisation de l'arborescence des exemples dans le sous-dossier "samples" par type de fonctionalité
* mise à jour des docs FR/EN (reprise des infos du template + correction des liens + ajout de liens manquants)

Session de travail sur la partie Game Controller (gamepad, joystick, télécommandes).

* tri des tickets GitHub concernant les contrôleurs de jeux
* élimination des messages d'information et d'avertissement fournis par le compilateur/lieur pour les unités "gamepad"
* modifications autour de la prise en charge des boutons pressés (Windows, macOS, iOS)
* simulation de clic sur les boutons "trigger left/right" non pris en charge par DirectInput avec la manette Xbox de test
* modification de la classe/composant TGamepadManager pour la rendre indépendante du TGamepadDevicesManager au niveau de son état Enabled, de la synchronisation du traitement des événements et de l'exécution de ses événements.
* ajout d'un référencement automatique des instances du composant TGamepadManager créées au niveau du projet dans le singleton TGamepadDevicesManager
* refactoring des événements et des passages en background pour traiter correctement l'activation ou pas des instances (manager, devices, gamepad)
