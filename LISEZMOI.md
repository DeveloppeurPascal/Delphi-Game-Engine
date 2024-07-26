# Delphi Game Engine

[This page in english.](README.md)

FireMonkey est le framework de composants et librairies multiplateforme fourni avec [Delphi](https://www.embarcadero.com/products/delphi). Il permet de développer des logiciels natifs (= compilés) pour Windows, Mac, iOS, Android et Linux à partir d'une seule base de code.

Comme ce framework est basé sur un moteur graphique il est idéal pour développer des jeux vidéo 2D et 3D sans apprendre le fonctionnement d'un moteur de jeux spécialisé (comme par exemple le génial [Castle Game Engine](https://castle-engine.io) qui embarque en plus de nombreux outils (éditeurs de map, sprites, ...) pour les développeurs de jeux en Pascal).

Ce dépôt de code propose des unités et exemples réutilisables dans vos projets de jeux vidéo.

Il est issu d'années de développement de jeux pour le fun et des sessions de [codage en direct sur Twitch](https://www.twitch.tv/patrickpremartin) démarrées en 2021 (et bien avant Twitch).

Vous trouverez des exemples de fonctionnalités classiques sur des jeux vidéos dans [Delphi FMX Game Snippets](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets) si vous cherchez l'inspiration.

Une présentation des fonctionnalités "game engine" de FireMonkey a été proposée à la [DelphiCon 2021](https://delphicon.embarcadero.com/) (en anglais, voir [la rediffusion](https://delphicon.embarcadero.com/talks/using-firemonkey-as-a-game-engine-on-demand/)) et [sur Twitch](https://www.twitch.tv/patrickpremartin) le samedi 11 décembre 2021 à 15 heures (en français, voir [la rediffusion](https://developpeur-pascal.fr/p/_200p-webinaire-du-11-decembre-2021-utiliser-firemonkey-comme-moteur-de-jeu-video.html)).

Certain de [mes jeux en Delphi](https://gamolf.fr) sont disponibles en open source sur GitHub.Vous les trouverez en sous-modules de [ce dépôt](https://github.com/DeveloppeurPascal/DevPas-Games-Pack).

Ce dépôt de code contient un projet développé en langage Pascal Objet sous Delphi. Vous ne savez pas ce qu'est Dephi ni où le télécharger ? Vous en saurez plus [sur ce site web](https://delphi-resources.developpeur-pascal.fr/).

## Installation des codes sources

Pour télécharger ce dépôt de code il est recommandé de passer par "git" mais vous pouvez aussi télécharger un ZIP directement depuis [son dépôt GitHub](https://github.com/DeveloppeurPascal/Delphi-Game-Engine).

Ce projet utilise des dépendances sous forme de sous modules. Ils seront absents du fichier ZIP. Vous devrez les télécharger à la main.

* aucune

Si vous voulez utiliser les fonctionnalités de contrôleur de jeu sur iOS et macOS, pensez à ajouter le framework GameController à vos SDK(s) dans Outils/options/Déploiement/SDK puis mettre à jour leurs fichiers.

## Licence d'utilisation de ce dépôt de code et de son contenu

Ces codes sources sont distribués sous licence [AGPL 3.0 ou ultérieure](https://choosealicense.com/licenses/agpl-3.0/).

Vous êtes globalement libre d'utiliser le contenu de ce dépôt de code n'importe où à condition :
* d'en faire mention dans vos projets
* de diffuser les modifications apportées aux fichiers fournis dans ce projet sous licence AGPL (en y laissant les mentions de copyright d'origine (auteur, lien vers ce dépôt, licence) obligatoirement complétées par les vôtres)
* de diffuser les codes sources de vos créations sous licence AGPL

Si cette licence ne convient pas à vos besoins vous pouvez acheter un droit d'utilisation de ce projet sous la licence [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) ou une licence commerciale dédiée ([contactez l'auteur](https://developpeur-pascal.fr/nous-contacter.php) pour discuter de vos besoins).

Ces codes sources sont fournis en l'état sans garantie d'aucune sorte.

Certains éléments inclus dans ce dépôt peuvent dépendre de droits d'utilisation de tiers (images, sons, ...). Ils ne sont pas réutilisables dans vos projets sauf mention contraire.

## Comment demander une nouvelle fonctionnalité, signaler un bogue ou une faille de sécurité ?

Si vous voulez une réponse du propriétaire de ce dépôt la meilleure façon de procéder pour demander une nouvelle fonctionnalité ou signaler une anomalie est d'aller sur [le dépôt de code sur GitHub](https://github.com/DeveloppeurPascal/Delphi-Game-Engine) et [d'ouvrir un ticket](https://github.com/DeveloppeurPascal/Delphi-Game-Engine/issues).

Si vous avez trouvé une faille de sécurité n'en parlez pas en public avant qu'un correctif n'ait été déployé ou soit disponible. [Contactez l'auteur du dépôt en privé](https://developpeur-pascal.fr/nous-contacter.php) pour expliquer votre trouvaille.

Vous pouvez aussi cloner ce dépôt de code et participer à ses évolutions en soumettant vos modifications si vous le désirez. Lisez les explications dans le fichier [CONTRIBUTING.md](CONTRIBUTING.md).

## Supportez ce projet et son auteur

Si vous trouvez ce dépôt de code utile et voulez le montrer, merci de faire une donation [à son auteur](https://github.com/DeveloppeurPascal). Ca aidera à maintenir le projet (codes sources et binaires).

Vous pouvez utiliser l'un de ces services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

ou si vous parlez français vous pouvez [vous abonner à Zone Abo](https://zone-abo.fr/nos-abonnements.php) sur une base mensuelle ou annuelle et avoir en plus accès à de nombreuses ressources en ligne (vidéos et articles).
