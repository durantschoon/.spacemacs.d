;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defvar bds/light-theme 'modus-operandi-tinted)
(defvar bds/dark-theme 'modus-vivendi-tinted)

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets"
                      auto-completion-enable-sort-by-usage t)
     better-defaults
     ;; need to install clojure for your system, also brew install clojure-lsp on darwin
     (clojure :variables
              clojure-enable-sayid t
              clojure-enable-clj-refactor t
              clojure-enable-linters 'clj-kondo
              cider-prompt-for-symbol nil)
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     command-log
     csv
     ;; docker installs
     ;; for lsp
     ;; npm i -g dockerfile-language-server-nodejs
     ;; linting on osx
     ;; brew install hadolint
     (docker :variables
             docker-dockerfile-backend 'lsp)
     emacs-lisp
     emoji
     git
     graphql
     ;; gtags ;; see layer for set up first, modify install emacs script
     haskell
     (helm :variables
           spacemacs-helm-rg-max-column-number 1024
           helm-follow-mode t)
     helpful
     html
     (javascript :variables
                 js2-mode-show-strict-warnings nil
                 javascript-import-tool 'import-js
                 javascript-fmt-tool 'prettier
                 javascript-fmt-on-save t
                 ;; node-add-modules-path t ; instead of adding node binaries to exec-path
                 javascript-backend 'lsp)
     (lsp ;; only available in develop branch of spacemacs
      :variables lsp-lens-enable t)
     markdown
     ;; mermaid
     multiple-cursors
     (org :variables
          org-hide-emphasis-markers t ;; like on /italics/ and *bold*
          org-enable-modern-support nil ;; has a bad unicode for ***
          org-enable-valign t)
     osx ;; hopefully this only activates on osx
     ;; python packages installed into "default" virtual environment
     ;; NEW see https://gist.github.com/durantschoon/f20657f038400859329303dded831c86
     (python :variables
             python-formatter 'black
             tab-width 2
             python-indent-offset 2
             python-format-on-save t
             python-backend 'lsp ;; new trying this
             python-sort-imports-on-save t)
     react
     rust
     search-engine
     ;; user `M-m '` like vscode to open/hide a shell
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     sql
     syntax-checking
     terraform
     (tern :variables
           tern-command '("node" ""))
     (treemacs :variables
               treemacs-use-git-mode 'deferred
               treemacs-lock-width t)
     typescript
     (unicode-fonts :variables
                    unicode-fonts-force-multi-color-on-mac t
                    unicode-fonts-ligature-modes '(js-mode org-mode))
     version-control
     yaml)
   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(avy
                                      buffer-move
                                      ;; copilot off, trying to use cody instead
                                      ;; (copilot :location (recipe
                                      ;;                     :fetcher github
                                      ;;                     :repo "copilot-emacs/copilot.el"
                                      ;;                     :files ("*.el" "dist")))
                                      editorconfig
                                      (emacs-cody :location local)
                                      expand-region
                                      helm-rg
                                      jsonnet-mode
                                      jsonrpc
                                      key-chord
                                      keychain-environment
                                      multiple-cursors
                                      prettier-js
                                      rg
                                      spinner)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper t

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper t
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   ;; dotspacemacs-startup-banner (let ((favorite "~/.spacemacs.d/core/banners/img/EmacsIcon.png"))
   ;;                               (if (file-exists-p favorite)
   ;;                                   favorite
   ;;                                 'official))
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   ;; dotspacemacs-themes '(spacemacs-dark spacemacs-light)
   dotspacemacs-themes (list bds/light-theme bds/dark-theme)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   ;; Note 'all-the-icons requires fonts to be installed, use
   ;; M-x all-the-icons-install-fonts
   dotspacemacs-mode-line-theme 'all-the-icons

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Source Code Pro for Powerline"
                               :size 12.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'right-then-bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-fu', `undo-redo' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system. The default is currently `undo-fu' as `undo-tree'
   ;; is not maintained anymore and `undo-redo' is very basic."
   dotspacemacs-undo-system 'undo-fu

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  )


(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; work-around
  (use-package helm-rg
    :config (setq helm-rg-default-directory 'git-root))

  (setq make-backup-files t)

  (keychain-refresh-environment)

  ;; ----------------------------------------------------------------------------
  ;; adding a spinner during cider--completing-read-host

  (use-package spinner
    :ensure t)

  (defvar cider-jack-in-stats-file (expand-file-name "~/.emacs.d/cider-jack-in-stats.el"))
  (defvar cider-jack-in-timings '())

  (defun load-cider-jack-in-stats ()
    "Load the cider-jack-in statistics from the stats file."
    (when (file-exists-p cider-jack-in-stats-file)
      (load-file cider-jack-in-stats-file)))

  (defun save-cider-jack-in-stats ()
    "Save the cider-jack-in statistics to the stats file."
    (with-temp-file cider-jack-in-stats-file
      (insert (format "(setq cider-jack-in-timings '%S)" cider-jack-in-timings))))

  (defun calculate-stats (timings)
    "Calculate mean and standard deviation of TIMINGS."
    (let* ((n (length timings))
           (mean (/ (apply #'+ timings) (float n)))
           (variance (/ (apply #'+ (mapcar (lambda (x) (expt (- x mean) 2)) timings)) n))
           (std-dev (sqrt variance)))
      (list :mean mean :std-dev std-dev)))

  (defun cider-jack-in-wrapper (original-fn &rest args)
    "Wrapper around CIDER jack-in commands to measure time and report stats.
     ORIGINAL-FN is the wrapped command, and ARGS are its arguments."
    (load-cider-jack-in-stats)
    (let* ((spinner (spinner-start 'progress-bar))
           (start-time (float-time))
           result)
      (setq result (apply original-fn args))
      (spinner-stop spinner)
      (let* ((elapsed (- (float-time) start-time))
             (stats (progn
                      (add-to-list 'cider-jack-in-timings elapsed)
                      (save-cider-jack-in-stats)
                      (calculate-stats cider-jack-in-timings)))
             (mean (plist-get stats :mean))
             (std-dev (plist-get stats :std-dev))
             (is-outlier (or (< elapsed (- mean std-dev))
                             (> elapsed (+ mean std-dev)))))
        (message "CIDER jack-in took %.2f seconds. Average: %.2f, Std Dev: %.2f. %s"
                 elapsed mean std-dev
                 (if is-outlier "This run is an outlier!" "This run is typical.")))
      result))

  (dolist (command '(cider-jack-in-clj cider-jack-in-cljs cider-jack-in-clj&cljs))
    (advice-add command :around #'cider-jack-in-wrapper))

  (with-eval-after-load 'cider
    (spacemacs/set-leader-keys
      "mjc" 'cider-jack-in-clj
      "mjs" 'cider-jack-in-cljs
      "mjC" 'cider-jack-in-clj&cljs))

  (setq spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

  ;; ----------------------------------------------------------------------------
  ;; set theme based on (darwin) system from emacs-plus
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme bds/light-theme t))
      ('dark (load-theme bds/dark-theme t))))

  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
  (my/apply-theme 'light)


  ;; use ~/.authinfo.gpg with emacs
  (require 'epa-file)
  ;; (epa-file-enable) ;; already enabled by spacemacs?
  (setq epa-file-select-keys nil)

  (use-package cody
    :commands (cody-login cody-restart cody-chat cody-mode)

    ;; Some common key bindings.
    :bind (("C-M-n" . cody-completion-cycle-next-key-dispatch)
           ("C-M-p" . cody-completion-cycle-prev-key-dispatch)
           ("M-TAB" . cody-completion-accept-key-dispatch)
           ("C-M-g" . cody-quit-key-dispatch))
    :init
    (setq cody--sourcegraph-host "sourcegraph.com") ; for clarity; this is the default.

    ;; cody--access-token is set in my ~/.authinfo.gpg
    (setopt cody-workspace-root "~/Repos/others/Tallyfor/balance") ; optional

    ;; for now cody seems to want this version of node
    (customize-set-variable 'cody-node-executable
                            (expand-file-name "~/.nvm/versions/node/v20.4.0/bin/node"))

    :config
    (defalias 'cody-start 'cody-login))

  ;;   (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
  ;;   (copilot-node-executable (executable-find "node")))
  ;; (add-hook 'prog-mode-hook 'copilot-mode)

  (with-eval-after-load 'key-chord
    (key-chord-define-global "hh" 'win-swap-horizontal)
    (key-chord-define-global "vv" 'win-swap-vertical)
    (key-chord-define-global "ww" 'toggle-window-split)
    (key-chord-define-global "jj" 'avy-goto-char)   ; type the character rapidly
    (key-chord-define-global "jk" 'avy-goto-char-2) ; type the first 2 characters rapidly
    (key-chord-define-global "jl" 'avy-goto-line)
    (key-chord-define-global "jw" 'avy-goto-word-1) ; type 1st char for beginnings of words
    )
  (key-chord-mode 1)

  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq projectile-switch-project-action 'magit-status)

  ;; clojure
  (setq clojure-enable-fancify-symbols t)

  (defalias 'u/pretty
    (kmacro "C-w ( u / p r e t t y SPC C-y \)"))

  ;; (defun jack-in-universal-cider-this-buffer ()
  ;;   "NOT WORKING YET
  ;;    The same commands I always seem to have to run to start up cider with the current buffer"
  ;;   (interactive)
  ;;   (cider-jack-in-universal 2) ;; 2 is for leiningen
  ;;   (cider-load-buffer)
  ;;   (cider-switch-to-repl-buffer))

  ;; suppress warning from emacs27
  (setq byte-compile-warnings '(cl-functions))

  (defun insert-current-date () (interactive)
         (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

  ;; 2024 how did I live so long w/out these
  (global-set-key (kbd "M-m <") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "M-m >") 'eyebrowse-next-window-config)

  ;;; key-bindings I immediately miss
  (global-set-key (kbd "M-s s") 'helm-swoop)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;;; try these new ones
  (global-set-key (kbd "C-c f") 'select-frame-by-name)

  ;;; enable easy-templates in org-mode
  ;;; I might want
  ;;; (with-eval-after-load 'org (require 'org-tempo))
  (require 'org-tempo)
  (setq-default org-hide-leading-stars t)

  ;; maybe this has the weird unicode bullet
  ;; (with-eval-after-load 'org (global-org-modern-mode))

  ;;; need to recompile all elc files to get org-archive-subtree to work
  ;;; https://github.com/syl20bnr/spacemacs/issues/11801

  ;;; Spacemacs need frame titles (helps with viewing multiple frames)
  (setq-default frame-title-format
                '((:eval (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name))
                           "%b"))))

  (add-hook 'live-py-mode-hook (lambda ()
                                 (progn
                                   (setq-default live-py-version (executable-find "python"))
                                   (live-py-update-all))))

  ;; react layer and relevant to web
  (setq-default
   ;; js2-mode
   ;; js2-basic-offset 2 ;; this is elsewhere aliased to js-indent-level
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)
  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)))

  (add-hook 'web-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'prettier nil 'make-it-local)))

  ;; this should already be a part of the better-defaults layer
  ;; (use-package unfill
  ;;   :bind ([remap fill-paragraph] . unfill-toggle))

  (add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode))

  (defun my-after-save-actions ()
    "Used in `after-save-hook'."
    (if (string= "py" (file-name-extension (buffer-name)))
        (if (member "Makefile" (projectile-current-project-files))
            (let ((default-directory (projectile-project-root))
                  (can-lint-p
                   (not (string= "" (shell-command-to-string "grep lint: Makefile"))))
                  (can-test-p
                   (not (string= "" (shell-command-to-string "grep test: Makefile")))))
              (if can-lint-p
                  (comint-send-string (get-buffer-process (shell)) "make lint\n")))
          ;; (if can-test-p
          ;;     (Comint-send-string (get-buffer-process (shell)) "make test\n"))
          )))

  (add-hook 'after-save-hook 'my-after-save-actions)

  (push '("*shell*" :height 10 :position bottom) popwin:special-display-config)

  ;; to use org-link-jira-from-middle:
  ;; paste into a new line: PTS-XYZ-link title text here
  ;; place cursor between link and title, then run the macro
  (fset 'org-link-jira-from-middle
        [?\C-  ?\C-a ?\M-\\ ?\C-x ?\C-x ?\C-w ?\[ ?\[ ?\C-f ?\C-f backspace ?\C-b ?\C-y ?\C-  ?\M-b ?\M-b ?\C-w ?\C-y ?\C-f ?\[ ?\C-y ?\C-f ?\] ?\C-a tab])

  ;; Settings

  (with-eval-after-load 'prog-mode
    ;; s- is super, aka Alt on darwin
    (define-key prog-mode-map (kbd "s-<double-mouse-1>") 'hs-toggle-hiding))

  (with-eval-after-load 'smerge-mode
    ;; s- is super, aka Option on darwin
    (define-key prog-mode-map (kbd "s-<up>") 'smerge-keep-upper)    ;; aka keep mine
    (define-key prog-mode-map (kbd "s-<down>") 'smerge-keep-lower)) ;; aka keep other

  (with-eval-after-load 'magit
    ;; s- is super, aka Option on darwin
    (define-key magit-hunk-section-map (kbd "s-<up>") 'magit-smerge-keep-upper)    ;; aka keep mine
    (define-key magit-hunk-section-map (kbd "s-<down>") 'magit-smerge-keep-lower)) ;; aka keep other

  (with-eval-after-load 'projectile
    (setq projectile-enable-caching t)
    (setq projectile-tags-command "ctags -e -R ."))

  (global-set-key (kbd "C-M-/") 'comint-dynamic-complete-filename)

  (define-key global-map (kbd "RET") 'newline-and-indent)

  (when (eq system-type 'darwin)          ; mac specific settings
    ;; ---------- REMAP KEYS ----------
    ;; (setq mac-option-modifier 'alt)    ; not needed, I think
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)     ; make opt key do Super
    (setq mac-control-modifier 'control)  ; make Control key do Control
    (setq ns-function-modifier 'hyper)    ; make Fn key do Hyper
    ;; ---------- SCROLLING ----------    ; for trackpads
    (global-set-key [wheel-right] 'scroll-left)
    (global-set-key [wheel-left] 'scroll-right))
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete

  (when (eq system-type 'darwin) ; mac specific settings
    (global-set-key "\M-`" 'other-frame)) ; act like other mac programs

  (when (eq system-type 'darwin)
    (add-to-list 'exec-path "/opt/homebrew/bin")) ; for apple silicon anyway

  ;; if placed at the beginning of dotspacemacs/user-config,
  ;; this breaks my binding of command to Meta on mac
  ;; (when (memq window-system '(mac ns x))
  ;; (trying to get this to work for (executable-find ""))

  ;; this shouldn't be necessary since spacemacs uses exec-path by default
  ;; (when (eq system-type 'darwin)
  ;;   (exec-path-from-shell-initialize))

  (global-set-key [(meta down)] 'scroll-other-window)    ; C-M-v
  (global-set-key [(meta up)] 'scroll-other-window-down) ; C-M-S-v

                                        ; was just f11, bad on Darwin
                                        ; similar to M-<f10> which is toggle-frame-maximized
  (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

  (global-set-key (kbd "C-x 4 o") 'switch-to-buffer-other-window-return)
  (global-set-key (kbd "C-x 4 k") 'kill-buffer-other-window)

  (require 'windmove)

  (defun win-swap-horizontal ()
    "Swap windows left/right using buffer-move.el"
    (interactive)
    (if (null (windmove-find-other-window 'right))
        (buf-move-left) (buf-move-right)))

  (global-set-key (kbd "C-c h") 'win-swap-horizontal)

  (defun win-swap-vertical ()
    "Swap windows up/down using buffer-move.el"
    (interactive)
    (if (null (windmove-find-other-window 'above))
        (buf-move-down) (buf-move-up)))

  (global-set-key (kbd "C-c v") 'win-swap-vertical)

  (defun switch-to-buffer-other-window-return ()
    "Like `switch-to-buffer-other-window`, but return to original buffer."
    (interactive)
    (switch-to-buffer-other-window (other-buffer))
    (other-window 1))

  (defun kill-buffer-other-window ()
    "Kill the buffer in the other window.
      I usually work with 2 windows side by side so when I do anything
      that opens a buffer in the other window (eg. looking at a function
      definition), I'll want to kill it after when I'm done. That's when
      I use kill-buffer-other-window."
    (interactive)
    (other-window 1)
    (kill-buffer (current-buffer))
    (other-window 1))

  ;; toggle-window-split
  ;; See https://www.emacswiki.org/emacs/ToggleWindowSplit
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (global-set-key (kbd "C-x |") 'toggle-window-split)


  (global-set-key [f5] 'global-whitespace-mode)
  (global-set-key [f6] 'toggle-truncate-lines)

  (global-set-key (kbd "C-c o") 'browse-url-at-point) ; like "o"pen


  (defun my-zap-to-char ()
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
     you are deleting forward, the CHAR is replaced and the point is
     put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))
  (advice-add 'zap-to-char :after #'my-zap-to-char)

  (use-package avy
    :ensure t)

  (use-package buffer-move
    :ensure t
    :bind (("<C-s-up>"    . buf-move-up) ; Control-super-up
           ("<C-s-down>"  . buf-move-down)
           ("<C-s-left>"  . buf-move-left)
           ("<C-s-right>" . buf-move-right)))

  (use-package expand-region
    :ensure t
    :bind ("C-=" . er/expand-region))

  (use-package multiple-cursors
    :ensure t
    :init
    ;; (require 'cl) -- testing this commented out
    :bind (("C-S-c C-S-c" . mc/edit-lines)
           ("C->"         . mc/mark-next-like-this)
           ("C-<"         . mc/mark-previous-like-this)
           ("C-c C-<"     . mc/mark-all-like-this)
           ("C-!"         . mc/mark-next-symbol-like-this)
           ("s-r"         . mc/mark-all-in-region)
           ("s-d"         . mc/mark-all-dwim)))

  (use-package helm-projectile
    :ensure t)

  ;; Run Last

  (setq locations '("home" "work"))
  (dolist (loc locations)
    (let ((init-file (concat "~/.spacemacs.d/" (concat loc "_init.el"))))
      (if (file-exists-p init-file)
          (progn
            (message (concat "loading " init-file))
            (load init-file)))))

  ;; END
  )




;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-anaconda yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode anaconda-mode pythonic reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl xterm-color unfill smeargle shell-pop orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download mwim multi-term mmm-mode markdown-toc markdown-mode  htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode fringe-helper gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck  with-editor transient eshell-z eshell-prompt-extras esh-help diff-hl company-statistics company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint indent-guide hydra lv hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation helm-themes helm-swoop helm-projectile projectile pkg-info epl helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell smartparens iedit anzu goto-chg undo-tree eval-sexp-fu elisp-slime-nav dumb-jump f dash s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(cider-enrich-classpath t)
   '(ignored-local-variable-values '((cider-preferred-build-tool . "clojure-cli")))
   '(package-selected-packages
     '(ac-ispell ace-jump-helm-line ace-link ace-window adaptive-wrap
                 aggressive-indent alert anaconda-mode anzu async auto-compile
                 auto-complete auto-dictionary auto-highlight-symbol
                 auto-yasnippet avy bind-key bind-map clean-aindent-mode
                 column-enforce-mode company company-anaconda company-statistics
                 cython-mode dash dash-functional define-word diff-hl diminish
                 dumb-jump elisp-slime-nav epl esh-help eshell-prompt-extras
                 eshell-z eval-sexp-fu exec-path-from-shell expand-region
                 eyebrowse f fancy-battery fill-column-indicator flx flx-ido
                 flycheck flycheck-pos-tip flyspell-correct flyspell-correct-helm
                 fringe-helper fuzzy gh-md gitattributes-mode gitconfig-mode
                 gitignore-mode gntp gnuplot golden-ratio google-translate
                 goto-chg graphql-mode helm helm-ag helm-c-yasnippet helm-company
                 helm-core helm-descbinds helm-flx helm-gitignore helm-make
                 helm-mode-manager helm-projectile helm-pydoc helm-swoop
                 helm-themes highlight-indentation highlight-numbers
                 highlight-parentheses hl-todo htmlize hungry-delete hy-mode hydra
                 iedit indent-guide launchctl link-hint linum-relative
                 live-py-mode log4e lorem-ipsum lv macrostep markdown-mode
                 markdown-toc memoize mmm-mode move-text multi-term mwim neotree
                 open-junk-file org-bullets org-category-capture org-download
                 org-mime org-plus-contrib org-pomodoro org-present org-projectile
                 orgit osx-dictionary osx-trash packed paradox parent-mode pbcopy
                 pcre2el persp-mode pip-requirements pkg-info popup popwin pos-tip
                 projectile py-isort pyenv-mode pytest pythonic pyvenv
                 rainbow-delimiters request restart-emacs reveal-in-osx-finder s
                 shell-pop smartparens smeargle spaceline spaceline-all-the-icons
                 spinner toc-org transient undo-tree unfill use-package uuidgen
                 vi-tilde-fringe volatile-highlights which-key winum with-editor
                 ws-butler xterm-color yapfify yasnippet)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(comint-highlight-prompt ((t (:inherit minibuffer-prompt :foreground "gray80"))))
   '(highlight-parentheses-highlight ((nil (:weight ultra-bold))) t))
  )
