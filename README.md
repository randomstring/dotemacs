# Configuration Files for Emacs

There are many .emacs files, this one is mine.

## Setup

```
 mkdir ~/git
 cd ~/git
 git clone git@github.com:randomstring/dotemacs.git
 mv ~/.emacs ~/.emacs.backup
 mv ~/.emacs.d ~/.emacs.d.backup
 ln -s ~/git/dotemacs/emacs.d .emacs.d
```

## Mac Compatibility

Stopped using the default Emacs that ships with Mac. This Emacs is old (version 22) and doesn't play well with iTerm2.

Using the latest version 25.1 from [Emacs for Mac OS X](https://emacsformacosx.com/)

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
 brew cask install font-fira-code
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

Step one, google to see if there's a new and improved way to fetch *melpa* packages. Last know location is at the top of the ```init.el``` file. If you're upgrading to a new version of emacs, then you probably want to nuke ```~/.emacs.d/elpa/``` and start over.

Make sure you can access the melpa repo. Try running ```M-x package-refresh-contents``` and then a test of installing a package with ```M-x package-install RET flycheck``` (just as an example to instal). 

I keep running into the problem that I need to install ```use-package``` first. To do this run ```M-x package-install RET use-package RET``` and restart emacs.

If emacs gets stuck loading a package, you may need to manually refresh the packages with ``M-x package-refresh-contents``. 

More recent changes should prevent the above problems.

Make sure the chosen python virtualenv is valid and working. If when loading a python .py file you see flake8 related loading errors. Chances are there is a problem with not having a virtual environment set. Run ```M-x elpy-config``` and make sure things look right. See ``Installing pip Modules`` above.

## Acknowledgments

There were many sources of inspiration here are a few:

* [Adam Schwartz's emacs.d](https://github.com/anschwa/emacs.d)
* [Steve Purcell](https://github.com/purcell/emacs.d)
* [Jessica B. Hamrick](https://github.com/jhamrick/emacs)
* [Getting started with use-package](https://github.com/CachesToCaches/getting_started_with_use_package)
* [Howard Abrams's dot-files](https://github.com/howardabrams/dot-files)

