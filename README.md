# Configuration Files for Emacs

There are many .emacs files, this one is mine.

## Setup

```
 mkdir ~/git
 cd ~/git
 git clone git@github.com:randomstring/dotemacs.git
 ln -s ~/git/dotemacs/.emacs .emacs
 ln -s ~/git/dotemacs/.emacs.d .emacs.d
```

Need to manually install use-package in emacs first. ``M-x package-install RET use-package RET``

## Mac Compatibility

Stopped using the default Emacs that ships with Mac. This Emacs is old (version 22) and doesn't play well with iTerm2.

Using [Emacs for Mac OS X](https://emacsformacosx.com/)

## Fonts

Picking DejaVu Mono spaced font. Top contenders were DejaVu, Inconsolata (-dz and -g) versions, and the Mac New Courier font. I've fallen out of love with Inconsolata-dz because the annoying -dz in the name confuses so many font parsers. Including Emacs. The Mac's New Courier has a nice feel, but the 1/l/O/0 characters are too similar. Microsoft's Consolata looks great, but requires either buying the fonts or installing Microsoft Office. DejaVu is Open Source and has extensive Unicode support.

http://dejavu-fonts.org/wiki/index.php?title=Main_Page

Using Inconsolata (-dz and -g) versions, and the Mac New Courier font. I've fallen out of love with Inconsolata-dz because the annoying -dz in the name confuses so many font parsers. Including Emacs. The Mac's New Courier has a nice feel, but the 1/l/O/0 characters are too similar. Microsoft's Consolata looks great, but requires either buying the fonts or installing Microsoft Office. DejaVu is Open Source and has extensive Unicode support.

[Get DejaVu fonts Here](http://dejavu-fonts.org/wiki/index.php?title=Main_Page)

## Background Colors

Background is black to avoid wearing out the eyes or lighting up the room (or airplane) when programming in low-light situations.

Foreground is almost white: #eeeeee.

## Markdown mode

http://jblevins.org/projects/markdown-mode/

Need to install the mac markdown command on the Mac:

```
 brew install markdown
```
Or to install on a raspberry pi:

```
 sudo apt-get install markdown
```

Then just hit ``C-c C-c p`` to open a preview page in your default browser, or ``C-c C-c l`` to open a side-by-side preview.


## Installing pip Modules

```
 pip install jsbeautifier
 pip install rope jedi flake8 importmagic yapf autopep8
```

## Problems

If emacs gets stuck loading a package, you may need to manually refresh the packages with ``package-refresh-contents``. Although ``.emacs`` should handle this automatically now.

## Acknowledgments

There were many sources of inspiration here are a few:

* [Adam Schwartz's emacs.d](https://github.com/anschwa/emacs.d)
* [Steve Purcell](https://github.com/purcell/emacs.d)
* [Jessica B. Hamrick](https://github.com/jhamrick/emacs)
* [Getting started with use-package](https://github.com/CachesToCaches/getting_started_with_use_package)
