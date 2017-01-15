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

### Installing Fonts via brew

Installing fonts on the Mac.

```
 brew tap caskroom/fonts
 brew cask install font-inconsolata
 brew cask install font-dejavu-sans-mono-for-powerline
 brew cask install font-dejavu-sans
```

## Background Colors

Background is black to avoid wearing out the eyes or lighting up the room (or airplane) when programming in low-light situations.

Foreground is almost white: #eeeeee.

## Markdown mode

http://jblevins.org/projects/markdown-mode/

Need to install the mac markdown command on the Mac:

```
 brew install multimarkdown
```

Or to install on a raspberry pi, install plain old markdown. 

```
 sudo apt-get install markdown
```

Then just hit ``C-c C-c p`` to open a preview page in your default browser, or ``C-c C-c l`` to open a side-by-side preview.


## Installing pip Modules

Start by creating a default python virtual environment, this is where emacs will look first. Use ```M-x pyvenv-workon``` (or use the menu bar ```Virtual Envs```) to switch to a different virtual environment. 

```
 mkvirtualenv default
 pip install rope jedi flake8 importmagic yapf autopep8 jsbeautifier
```

## Problems

I keep running into the problem that I need to install ```use-package`` first. To do this run ```M-x package-install use-package``` and restart emacs.

If emacs gets stuck loading a package, you may need to manually refresh the packages with ``package-refresh-contents``. 

More recent changes should prevent the above problems.

If when loading a python .py file you see flake8 related loading errors. Chances are there is a problem with not having a virtual environment set. Run ```M-x elpy-config``` and make sure things look right. See ``Installing pip Modules`` above.

## Acknowledgments

There were many sources of inspiration here are a few:

* [Adam Schwartz's emacs.d](https://github.com/anschwa/emacs.d)
* [Steve Purcell](https://github.com/purcell/emacs.d)
* [Jessica B. Hamrick](https://github.com/jhamrick/emacs)
* [Getting started with use-package](https://github.com/CachesToCaches/getting_started_with_use_package)
