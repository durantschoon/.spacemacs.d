# .spacemacs.d
spacemacs dot file(s)

These days, I use emacs mostly on a mac (but also windows and linux). I have updated my [dot_files repo](https://github.com/durantschoon/dot_files/) so that when I run `make` on a new machine from that repo, it should install emacs/spacemacs with this custom directory so I don't have to manually run the commands listed below under "Install Emacs"

Just as a quick reminder to myself, the way this generally works is that

* **syl20bnr/spacemacs** repo is cloned into ~/.emacs.d
* **durantschoon/.spacemacs.d** repo is cloned into ~/.spacemacs.d
* The environment variable `SPACEMACSDIR` is set to tell spacemacs where to find my customizations (ie. ~/.spacemacs.d)

## Install Emacs

On a mac:

I use a different (arbitrary) icon for each version, so I can tell at a glance
at startup which version I'm using (helpful when I run emacs on multiple
machines, e.g. home vs. work):

| Version       | Icon                             |
|---------------|----------------------------------|
| emacs-plus@27 | --with-EmacsIcon4-icon           |
| emacs-plus@28 | --with-emacs-card-blue-deep-icon |
| emacs-plus@29 | --with-retro-sink-icon           |
| emacs-plus@30 | --with-spacemacs-icon            |
| emacs-plus@31 | --with-spacemacs-icon            |

```zsh
# uninstall old
brew uninstall emacs-plus
# install new
brew install emacs-plus@31 --with-xwidgets --with-imagemagick --with-spacemacs-icon
# update link in /Applications in a zsh shell
[[ -L /Applications/Emacs.app ]] && =rm /Applications/Emacs.app
ln -si /usr/local/opt/emacs-plus/Emacs.app /Applications/
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

Or just install it to your ~/.emacs.d folder. Refer to the [original repo](https://github.com/syl20bnr/spacemacs).

```
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

Launch emacs

## switch_spacemacs command

I did this in zsh to switch back and forth easily (handy for migrating from original emacs to spacemacs over time):

Set up links:

```zsh
cd
mv .emacs.d .emacs.d_ORIG_EMACS # move over your original dot files
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d_SPACEMACS
```

Then write a function (in your ~/.aliases file), source that file (eg. `source ~/.aliases`) and run `switch_spacemacs` to toggle while emacs is not running. Next time you run emacs, it will be the other version.

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
