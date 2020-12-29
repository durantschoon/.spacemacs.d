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

Or (linux/windows):

[Download emacs](https://www.gnu.org/software/emacs/download.html) for your operating system. Test that it works. Try the tutorial.

## Set up Spacemacs

Make sure you don't already have ~/.spacemacs.d before running this (but you should get a warning if you do):

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

## switch_spacemacs alias

I did this in zsh to switch back and forth easily (handy for migrating from original emacs to spacemacs over time):

Set up links:

```zsh
cd
mv .emacs.d .emacs.d_ORIG_EMACS # move over your original dot files
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d_SPACEMACS
```

Then make an alias (in your ~/.aliases file), source that file and run `switch_spacemacs` to toggle while emacs is not running. Next time you run emacs, it will be the other version.

```zsh
    function switch_spacemacs() {
        local EMACS_D_LINK=$HOME/.emacs.d
        local EMACS_ORIG_D=$HOME/.emacs.d_ORIG_EMACS
        local SPACEMACS_D=$HOME/.emacs.d_SPACEMACS
        if [[ -d $EMACS_ORIG_D && -d $SPACEMACS_D && -h $EMACS_D_LINK ]]; then
            if [[ "$(readlink $EMACS_D_LINK)" == "$EMACS_ORIG_D" ]]; then
                =rm $EMACS_D_LINK
                ln -s $SPACEMACS_D $EMACS_D_LINK
                echo "Switched to spacemacs"
            elif [[ "$(readlink $EMACS_D_LINK)" == "$SPACEMACS_D" ]]; then
                =rm $EMACS_D_LINK
                ln -s $EMACS_ORIG_D $EMACS_D_LINK
                echo "Switched to original emacs"
            fi
        else
            echo "Not configured to test spacemacs"
        fi
    }
```
