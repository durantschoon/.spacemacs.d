# .spacemacs.d
spacemacs dot file(s)

## Install Emacs

These days, I use emacs mostly on a mac (but also windows and linux).

On a mac:

I use a different (arbitrary) icon for each version, so I can tell at a glance
at startup which version I'm using (helpful when I run emacs on multiple
machines, e.g. home vs. work):

| Version       | Icon                             |
|---------------|----------------------------------|
| emacs-plus@27 | --with-EmacsIcon4-icon           |
| emacs-plus@28 | --with-emacs-card-blue-deep-icon |

```zsh
# uninstall old
brew uninstall emacs-plus
# install new
brew install emacs-plus@28 --with-xwidgets --with-emacs-card-blue-deep-icon
# update link in /Applications in a zsh shell
[[ -L /Applications/Emacs.app ]] && =rm /Applications/Emacs.app
ln -si /usr/local/opt/emacs-plus@28/Emacs.app /Applications/
```

Or:

[Download emacs](https://www.gnu.org/software/emacs/download.html) for your operating system. Test that it works. Try the tutorial.

Make sure you don't already have ~/.spacemacs.d before running this (but you should get a warning if you do):

## Set up Spacemacs

```
cd
git clone https://github.com/durantschoon/.spacemacs.d.git .
```

Here's how to try using spacemacs if you already have a .emacs.d/ folder (from the [FAQ](https://github.com/syl20bnr/spacemacs/blob/develop/doc/FAQ.org#try-spacemacs-without-modifying-my-existing-emacs-configuration)):

```
mkdir ~/spacemacs
git clone https://github.com/syl20bnr/spacemacs.git ~/spacemacs/.emacs.d
HOME=~/spacemacs emacs
```

Or just install it to your ~/.emacs.d folder. Refer to the [origina repo](https://github.com/syl20bnr/spacemacs).

```
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

Launch emacs
