# 20240820 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

## Travail sur le composant TDGEHelpBar

* Correction de la fuite mémoire lie au TTabList par changement des propriétaires des composants visuels créés par la barre d'aide. Ils semblent prendre le TTabList de leur propriétaire et en créent un s'ils n'en trouvent pas, sauf qu'il n'est pas détruit en fermeture du code.
