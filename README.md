# Delphi Game Engine

[Cette page en français.](LISEZMOI.md)

FireMonkey is the cross-platform component and library framework provided with [Delphi](https://www.embarcadero.com/products/delphi). It allows to develop native (= compiled) software for Windows, Mac, iOS, Android and Linux from a single code base.

As this framework is based on a graphics engine it is ideal for developing 2D and 3D video games without learning how to use a specialized game engine (like for example the great [Castle Game Engine](https://castle-engine.io) which also includes many tools (map editors, sprites, ...) for Pascal game developers).

You'll find more game engines in Pascal (Delphi, Lazarus, ...) listed on [Awesome Pascal](https://github.com/Fr0sT-Brutal/awesome-pascal).

This code repository offers reusable units and examples for your video game projects.

It comes from years of game development for fun and [live coding sessions on Twitch](https://www.twitch.tv/patrickpremartin) started in 2021 (and a long time before Twitch).

You can find examples of classic game features in [Delphi FMX Game Snippets](https://github.com/DeveloppeurPascal/Delphi-FMX-Game-Snippets) if you are looking for inspiration.

Some of [my video games](https://gamolf.fr) are open sourced on GitHub. Find them as submodules of [this repository](https://github.com/DeveloppeurPascal/DevPas-Games-Pack).

Since July 2024, a game coding starter kit in the form of FireMonkey projects is available. This is the [Gamolf FMX Game Starter Kit](https://github.com/DeveloppeurPascal/Gamolf-FMX-Game-Starter-Kit). It uses [Delphi Game Engine](https://github.com/DeveloppeurPascal/Delphi-Game-Engine) for the basic routines (sound, game controllers, user interface, scores, ...) and provides you with a Delphi project template to create your own games, with numerous examples.

This code repository contains a project developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

## Talks and conferences

### Code Rage 2018

* [How to code a Christmas Game in Delphi with FireMonkey](https://serialstreameur.fr/coderage2018-christmasgame-howto.php) (in English)
* [Code revue of the Christmas Game](https://serialstreameur.fr/coderage2018-christmasgame-codereview.php) (in English)

### Learn To Code Summer Camp 2021

* [Des resources disponibles pour apprendre et des exemples d'animations simples](https://serialstreameur.fr/ltcsc2021-04.php) (in French)
* [Développer un casse briques avec Delphi](https://serialstreameur.fr/ltcsc2021-05.php) (in French)

### DelphiCon 2021

* [Using FireMonkey as a game engine](https://serialstreameur.fr/delphicon-2021-fmx-game-engine.php) (in English)
* [Utiliser FireMonkey comme moteur de jeux vidéo](https://serialstreameur.fr/webinaire-20211211.php) (in French)

### Dev Days of Summer 2024

* [Make games in Delphi (2024 edition)](https://serialstreameur.fr/make-games-in-delphi-2024-edition.html) (in English)
* [Make games in Delphi (2024 edition)](https://youtu.be/CAW-wpb9HJg) (live session replay in English)
* [Faites des jeux en Delphi (v2024)](https://serialstreameur.fr/faites-des-jeux-en-delphi-edition-2024.html) (in French)

### Twitch

Follow my development streams of software, video games, mobile applications and websites on [my Twitch channel](https://www.twitch.tv/patrickpremartin) or as replays on [Serial Streameur](https://serialstreameur.fr/jeux-video.php) mostly in French.

## Source code installation

To download this code repository, we recommend using "git", but you can also download a ZIP file directly from [its GitHub repository](https://github.com/DeveloppeurPascal/Delphi-Game-Engine).

This project uses dependencies in the form of sub-modules. They will be absent from the ZIP file. You'll have to download them by hand.

* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) must be installed in the ./lib-externes/librairies 	subfolder.

The project's technical documentation, generated with [DocInsight](https://devjetsoftware.com/products/documentation-insight/), is available in the ./docs folder and on [GitHub Pages](https://developpeurpascal.github.io/Delphi-Game-Engine). Further information and related links are available on [the project website](https://delphigameengine.developpeur-pascal.fr).

If you want to use game controllers features for iOS or macOS, don't forget to add the GameController framework to your SDK(s) in Tools/Options/Deployment/SDK and refresh the files.

The components packages need only the RTL package to avoid conflicts during their installation.

For components TDGEGamepadDetected and TDGEHelpBar you need Skia4Delphi enabled in your projects. It's used by unit Olf.RTL.SVGToBitmap.pas from [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) to draw the SVG into images bitmap.

If you need explanations or help in using this project in your own, please [contact me](https://developpeur-pascal.fr/nous-contacter.php). I can either direct you to an online resource, or offer you assistance on a fee-for-service basis. You can also contact me at a conference or an online presentation.

## Compatibility

As an [Embarcadero MVP](https://www.embarcadero.com/resources/partners/mvp-directory), I benefit from the latest versions of [Delphi](https://www.embarcadero.com/products/delphi) and [C++ Builder](https://www.embarcadero.com/products/cbuilder) in [RAD Studio](https://www.embarcadero.com/products/rad-studio) as soon as they are released. I therefore work with these versions.

Normally, my libraries and components should also run on at least the current version of [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

There's no guarantee of compatibility with earlier versions, even though I try to keep my code clean and avoid using too many of the new ways of writing in it (type inference, inline var and multiline strings).

If you detect any anomalies on earlier versions, please don't hesitate to [report them](https://github.com/DeveloppeurPascal/Delphi-Game-Engine/issues) so that I can test and try to correct or provide a workaround.

## License to use this code repository and its contents

This source code is distributed under the [AGPL 3.0 or later](https://choosealicense.com/licenses/agpl-3.0/) license.

You are free to use the contents of this code repository anywhere provided :
* you mention it in your projects
* distribute the modifications made to the files provided in this AGPL-licensed project (leaving the original copyright notices (author, link to this repository, license) must be supplemented by your own)
* to distribute the source code of your creations under the AGPL license.

If this license doesn't suit your needs (especially for a commercial project) I also offer [classic licenses for developers and companies](https://delphigameengine.developpeur-pascal.fr).

Some elements included in this repository may depend on third-party usage rights (images, sounds, etc.). They are not reusable in your projects unless otherwise stated.

The source codes of this code repository as well as any compiled version are provided “as is” without warranty of any kind.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/Delphi-Game-Engine) and [open a new issue](https://github.com/DeveloppeurPascal/Delphi-Game-Engine/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [in French](https://ko-fi.com/patrick_premartin_fr) or [in English](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

or if you speack french you can [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) on a monthly or yearly basis and get a lot of resources as videos and articles.
