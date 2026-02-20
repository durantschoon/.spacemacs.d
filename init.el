;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defcustom bds/light-theme 'modus-operandi-deuteranopia
  "The theme to use when the background is set to light."
  :type 'symbol
  :group 'bds)

(defcustom bds/dark-theme 'modus-vivendi-deuteranopia
  "The theme to use when the background is set to dark."
  :type 'symbol
  :group 'bds)

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
   '(;; ----------------------------------------------------------------
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
     (c-c++ :variables
            c-c++-backend 'lsp-clangd
            c-c++-lsp-enable-semantic-highlight t
            c-c++-enable-clang-format-on-save t)
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
     dap
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
     go
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
     (llm-client :variables
                 ;; Note I have my api-key set in my ~/.authinfo.gpg (on mac)
                 ;; in this format:
                 ;; machine api.openai.com login apikey password TOKEN
                 ;; but I'm starting testing with local ollama, not openai api
                 llm-client-enable-gptel t
                 llm-client-enable-ellama t)
     (lsp ;; only available in develop branch of spacemacs
      :variables lsp-lens-enable t)
     (major-modes :variables
                  c-multiline-string-start-char "/*"
                  arduino-executable "arduino-cli"
                  arduino-cli-verify t
                  arduino-cli-warnings 'all)
     markdown
     ;; mermaid
     multiple-cursors
     (org :variables
          org-hide-emphasis-markers t ;; like on /italics/ and *bold*
          org-enable-modern-support nil ;; has a bad unicode for ***
          org-startup-indented t
          org-enable-valign t
          org-agenda-block-separator 8411 ;; fancy dots at top
          )
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
     ;; react
     (rust :variables
           rust-backend 'lsp
           lsp-rust-analyzer-cargo-auto-reload t
           rustic-format-on-save t)
     scheme
     search-engine
     ;; user `M-m '` like vscode to open/hide a shell
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     sml ;; Standard ML ... why do I need this again?
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
   dotspacemacs-additional-packages '(auto-minor-mode
                                      avy
                                      buffer-move
                                      ;; copilot off, trying to use cody instead
                                      ;; (copilot :location (recipe
                                      ;;                     :fetcher github
                                      ;;                     :repo "copilot-emacs/copilot.el"
                                      ;;                     :files ("*.el" "dist")))
                                      editorconfig
                                      (emacs-cody :location local)
                                      exec-path-from-shell
                                      expand-region
                                      helm-rg
                                      jsonnet-mode
                                      jsonrpc
                                      key-chord
                                      keychain-environment
                                      markdown-toc
                                      multiple-cursors
                                      org-tidy
                                      prettier-js
                                      rg
                                      spinner
                                      visual-regexp-steroids
                                      pcre2el)
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

  ;; Set warning suppression as early as possible to catch package loading warnings
  (setq warning-suppress-types
        '((defadvice obsolete deprecated callf destructuring-bind
            define-minor-mode case invalid-face)))
  (setq warning-suppress-log-types
        '((defadvice obsolete deprecated callf destructuring-bind
            define-minor-mode case invalid-face)))
  (setq warning-minimum-level :emergency)
  (setq byte-compile-warnings '(cl-functions))

  ;; Auto-compile packages without asking
  (setq package-quickstart t)
  (setq package-native-compile t)
  (setq native-comp-async-report-warnings-errors 'silent)



  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
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
   dotspacemacs-startup-banner-scale 0.2

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
                                (projects . 7)
                                (bookmarks . 3)
                                (agenda . 7)
                                (todos . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
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
   dotspacemacs-themes (list bds/dark-theme bds/light-theme)

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
   dotspacemacs-default-font '("Fira Code"
                               :size 24
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'all-the-icons

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
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

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
   dotspacemacs-enable-load-hints t

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
   dotspacemacs-fullscreen-at-startup t

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
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil

   ;; Suppress deprecation warnings from outdated packages
   ;; This must be set early to catch warnings during package loading
   warning-suppress-types '((defadvice obsolete deprecated callf destructuring-bind
                              define-minor-mode case invalid-face))
   warning-suppress-log-types '((defadvice obsolete deprecated callf destructuring-bind
                                  define-minor-mode case invalid-face))
   warning-minimum-level :emergency
   byte-compile-warnings '(cl-functions)))

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
  (add-to-list 'load-path (expand-file-name "lisp" dotspacemacs-directory))
  (setq native-comp-async-report-warnings-errors 'silent))

;; =========================================================================
;; * dotspacemacs/user-config () *
;; =========================================================================

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; =======================================================================
  ;; ** 🔧 Configuration Flags **
  ;; =======================================================================

  ;; Set to t to enable experimental/testing features, nil to disable
  ;; These are currently off because of some infinite macro-evaluation(?)
  ;; from the defadvice patching
  (defvar bds/enable-experiments t
    "Enable experimental features in the testing zone.")

  ;; =======================================================================
  ;; ** 📋 Meta (use outline minor mode in elisp especially for user-config) **
  ;; =======================================================================

  ;; Use C-x c i for helm imenu to jump to a section

  (defun my-setup-imenu-outline ()
    "Add outline-style headings to imenu in Emacs Lisp mode."
    (setq-local outline-regexp ";; \\*+ ")
    (setq-local imenu-generic-expression
                '(;; ("Functions" "^\\s-*(defun \\([^( \n]+\\)" 1)
                  ;; ("Variables" "^\\s-*(def\\(var\\|const\\) \\([^( \n]+\\)" 2)
                  ("Sections" "^\\s-*;; \\*+\\s-+\\(.*?\\)\\s-*\\**\\s-*$" 1)))
    (outline-minor-mode 1))

  (add-hook 'emacs-lisp-mode-hook #'my-setup-imenu-outline)

  ;; Folding

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (setq outline-regexp ";; \\*+ ")

  ;; ======================================================================
  ;; ** 🧪 Testing Zone **
  ;; ======================================================================

  ;; I add new code here and move it below when it seems to be working
  ;; or at least when I add the next new code
  ;;
  ;; WORKFLOW FOR NEW EXPERIMENTAL FEATURES:
  ;; 1. Add new experimental code in the condition-case block below
  ;; 2. Test the feature thoroughly
  ;; 3. When stable, move the code to its proper section in user-config
  ;; 4. Create a new section if needed (e.g., "📦 Package Configuration")
  ;; 5. Update commit message to note what was moved out and into Testing Zone
  ;; 6. Keep the condition-case wrapper for safety during testing
  ;;
  ;; CURRENT EXPERIMENTS:
  ;; - LLM section changes: Recent updates to LLM configuration (seems stable)

  (if bds/enable-experiments
      (condition-case err
          (progn
            (message "🧪 Running experimental config...")
            ;; ⬇ Put new or untested code here

            ;; Once accepted, this will move to
            ;; ** 🪟 Window & Buffer Management **
            (defun dotspacemacs/user-config ()
              ;; Add perspective.el
              (use-package perspective
                :ensure t
                :init
                (persp-mode)
                :config
                ;; Optional: Configure keybindings or other settings here
                ;; Example: Bind perspective commands to a custom prefix
                ;; (general-define-key :prefix "SPC p" 'perspective-map)
                ))


            ;; ✅ Success message
            (message "✅ Experimental config loaded successfully."))
        (error
         (message "⚠️ Error in experimental config: %S" err)))
    (message "🧪 Experimental features disabled (set bds/enable-experiments to t to enable)"))

  ;; ======================================================================
  ;; ** 🌍 System Environment & Paths **
  ;; ======================================================================

  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "CARGO_HOME" "RUSTUP_HOME"))
    (exec-path-from-shell-initialize))

  ;; this helps on macos to avoid using /var/folders
  (setenv "TMPDIR" "/tmp")
  (setq temporary-file-directory "/tmp/")

  (keychain-refresh-environment)

  (setq backup-directory-alist `(("." . "~/.emacs_backups"))
        backup-by-copying t    ; Don't clobber symlinks
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)     ; Use versioned backups

  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))


  ;; ======================================================================
  ;; ** 🍎 MacOS Specific Settings **
  ;; ======================================================================

  (when (eq system-type 'darwin)          ; mac specific settings
    ;; ---------- REMAP KEYS ----------
    ;; (setq mac-option-modifier 'alt)    ; not needed, I think
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)     ; make opt key do Super
    (setq mac-control-modifier 'control)  ; make Control key do Control
    (setq ns-function-modifier 'hyper)    ; make Fn key do Hyper
    ;; ---------- SCROLLING ----------    ; for trackpads
    (global-set-key [wheel-right] 'scroll-left)
    (global-set-key [wheel-left] 'scroll-right)
    ;; Enable smooth pixel scrolling
    (pixel-scroll-precision-mode 1)

    ;; Make scrolling "natural" (like macOS)
    (setq pixel-scroll-precision-use-momentum t)
    (setq mouse-wheel-scroll-amount '(1))  ;; fine-grained scroll
    (setq mouse-wheel-tilt-scroll t)

    ;; Invert the direction for "natural" scrolling
    (setq mouse-wheel-flip-direction t))

  (when (eq system-type 'darwin) ; mac specific settings
    (global-set-key "\M-`" 'other-frame)) ; act like other mac programs

  ;; ======================================================================
  ;; ** 💻 Shell & Terminal Configuration **
  ;; ======================================================================

  (defun my/shell-reminder-use-vterm ()
    "Reminder to use vterm instead of shell, then runs vterm."
    (interactive)
    (message "⚠️ You're trying to use `M-x shell`. Giving you vterm instead, `M-x vterm`, for better performance.")
    (vterm))

  (advice-add 'shell :override #'my/shell-reminder-use-vterm)

  (push '("*shell*" :height 10 :position bottom) popwin:special-display-config)

  ;; do not allow vterm to rebind C-d (I still want to delete forward)
  (with-eval-after-load 'vterm
    (setq vterm-keymap-exceptions
          (remove "C-d" vterm-keymap-exceptions))
    (define-key vterm-mode-map (kbd "C-d") #'delete-forward-char))


  ;; ======================================================================
  ;; ** 📝 Org Mode Configuration **
  ;; ======================================================================

  (setq org-capture-templates
        '(("r" "Robotics Task" entry
           (file+headline "~/Dropbox/home/org/robotics/summer2025.org" "Captured Tasks")
           "* TODO %^{Task description}
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U
:END:
%?"
           :empty-lines 1)

          ("j" "Robotics Journal" entry
           (file+datetree "~/Dropbox/home/org/robotics/journal.org")
           "* %U - %^{Brief title}
%?
"
           :empty-lines 1)

          ("f" "Food" entry
           (file+datetree "~/Dropbox/home/org/capture/food.org")
           "* %U - %^{Brief title}
%?
"
           :empty-lines 1)

          ("i" "Inbox (to categorize later)" entry
           (file+headline "~/Dropbox/home/org/capture/inbox.org" "📥 Inbox for Fleeting Ideas")
           "* %?\nEntered on %U\n"
           :empty-lines 1)))

  (setq org-agenda-files '("~/Org/home/agenda/tasks.org"
                           "~/Org/home/agenda/personal.org"
                           "~/Org/home/robotics/summer2025.org"
                           "~/Org/home/capture/inbox.org"))

  ;;; enable easy-templates in org-mode
  ;;; hopefully this makes '<s'+TAB create src blocks again
  (with-eval-after-load 'org (require 'org-tempo))
  (setq-default org-hide-leading-stars t)

  ;; to use org-link-jira-from-middle:
  ;; paste into a new line: PTS-XYZ-link title text here
  ;; place cursor between link and title, then run the macro
  (fset 'org-link-jira-from-middle
        [?\C-  ?\C-a ?\M-\\ ?\C-x ?\C-x ?\C-w ?\[ ?\[ ?\C-f ?\C-f backspace ?\C-b ?\C-y ?\C-  ?\M-b ?\M-b ?\C-w ?\C-y ?\C-f ?\[ ?\C-y ?\C-f ?\] ?\C-a tab])

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'org-align-all-tags nil 'local)
              (auto-fill-mode 1)))

  ;; ======================================================================
  ;; ** 🤖 LLM & AI Configuration **
  ;; ======================================================================

  ;; Useful to remember this in gptel-mode
  ;; C-c RET					gptel-send
  ;; for gptel, to format a new response, I just format everything manually:
  ;;   C-x h followed by M-q
  ;; ellama seems better at that, but these are slow compared to claude code

  ;; ollama-models: first model is default
  (let ((ollama-models '(qwen2.5-coder:14b-instruct-q4_k_m
                         deepseek-coder-v2
                         nous-hermes2-mixtral))
        (ollama-host "localhost:11434")) ;; yes, it spells llama
    (gptel-make-ollama "Ollama Local"
      :host ollama-host
      :stream t
      :models ollama-models)

    (setq
     gptel-model (car ollama-models)
     gptel-backend (gptel-make-ollama
                       (format "Ollama Local %s"
                               (mapconcat #'symbol-name ollama-models " or "))
                     :host ollama-host
                     :stream t
                     :models ollama-models)))

  ;; cody
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

  ;; ======================================================================
  ;; ** 🌐 Web Browser Configuration **
  ;; ======================================================================

  (setq browse-url-browser-function #'xwidget-webkit-browse-url)

  (spacemacs/set-leader-keys "ox" 'xwidget-webkit-browse-url) ;; open with SPC o x

  (defun my-xwidget-webkit-display-right (url &optional new-session)
    "Open xwidget-webkit browser in a window split to the right."
    (let ((new-window (split-window-right)))
      (select-window new-window) ;; Switch to the right-hand window
      ;; Create a new xwidget buffer and display the URL in it
      (let ((xwidget-buffer (get-buffer-create "*xwidget-webkit*")))
        (with-current-buffer xwidget-buffer
          (xwidget-webkit-mode)) ;; Ensure the buffer is in xwidget-webkit-mode
        (set-window-buffer new-window xwidget-buffer) ;; Attach buffer to the right window
        (xwidget-webkit-browse-url url new-session)
        (select-window new-window)))) ;; Ensure focus stays on the right window

  ;; Set `browse-url-browser-function` to use the custom function
  (setq browse-url-browser-function #'my-xwidget-webkit-display-right)

  ;; ======================================================================
  ;; ** 🎨 Theme & Appearance **
  ;; ======================================================================

  ;; set theme based on (darwin) system from emacs-plus
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (condition-case err
                  (load-theme bds/light-theme t)
                (error (message "Failed to load light theme: %s" err))))
      ('dark (condition-case err
                 (load-theme bds/dark-theme t)
               (error (message "Failed to load dark theme: %s" err))))))

  ;; Set up automatic theme switching based on system appearance
  (when (and (eq system-type 'darwin) window-system)
    (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
    ;; Apply initial theme if ns-appearance is available
    (when (fboundp 'ns-appearance)
      (my/apply-theme (ns-appearance))))

  (defun my/fontify-after-change (beg end len)
    "Refontify the changed region or buffer."
    (when font-lock-mode
      (font-lock-ensure beg end)))

  (defun my/add-fontify-on-change ()
    "Add auto-refontify hook if font-lock is enabled."
    (add-hook 'after-change-functions #'my/fontify-after-change nil t))

  (add-hook 'after-change-major-mode-hook #'my/add-fontify-on-change)

  ;; ======================================================================
  ;; ** 🔐 Security & Authentication **
  ;; ======================================================================

  ;; use ~/.authinfo.gpg with emacs
  (require 'epa-file)
  ;; (epa-file-enable) ;; already enabled by spacemacs?
  (setq epa-file-select-keys nil)
  (when (file-exists-p "/usr/local/MacGPG2/bin/gpg2") ;; macos at least for now
    (custom-set-variables
     '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg2")))
  ;; (epa-file-enable) ;; this is already enabled it seems

  ;; ======================================================================
  ;; ** ☕ Clojure Development **
  ;; ======================================================================

  ;; clojure
  (setq clojure-enable-fancify-symbols t)

  (defalias 'u/pretty
    (kmacro "C-w ( u / p r e t t y SPC C-y \)"))

  ;; ----------------------------------------------------------------------------
  ;; adding a spinner during cider--completing-read-host

  (defvar cider--completing-read-spinner nil
    "Spinner object for `cider--completing-read-host'.")

  (defun cider--completing-read-spinner-start (&rest _args)
    "Advice to start a spinner before `cider--completing-read-host'."
    (unless (and cider--completing-read-spinner
                 (spinner--active-p cider--completing-read-spinner))
      ;; Ensure spinner type is valid
      (setq cider--completing-read-spinner (spinner-create 'rotating-line t)))
    (message "About to set host")
    (spinner-start cider--completing-read-spinner))

  (defun cider--completing-read-spinner-stop (&rest _args)
    "Advice to stop the spinner after `cider--completing-read-host'."
    (when (and cider--completing-read-spinner
               (spinner--active-p cider--completing-read-spinner))
      (spinner-stop cider--completing-read-spinner)
      (setq cider--completing-read-spinner nil)
      (message "Host is set. Searching for a port...")))

  (defun spinner--timer-function (spinner)
    "Function called to update SPINNER. Includes check for valid frames."
    (let ((buffer (spinner--buffer spinner)))
      (if (or (not (spinner--active-p spinner))
              (and buffer (not (buffer-live-p buffer))))
          (spinner-stop spinner)
        ;; Ensure frames are valid before proceeding
        (let ((frames (spinner--frames spinner)))
          (when frames ;; Proceed only if frames are not nil
            ;; Increment counter safely
            (cl-callf (lambda (x) (if (< x 0)
                                      (1+ x)
                                    (% (1+ x) (length frames))))
                (spinner--counter spinner))
            ;; Update mode-line
            (if (buffer-live-p buffer)
                (with-current-buffer buffer
                  (force-mode-line-update))
              (force-mode-line-update)))))))

  ;; Add advice around `cider--completing-read-host`
  (advice-add 'cider--completing-read-host :before #'cider--completing-read-spinner-start)
  (advice-add 'cider--completing-read-host :after #'cider--completing-read-spinner-stop)

  ;; ======================================================================
  ;; ** 🐍 Python Development **
  ;; ======================================================================

  (add-hook 'live-py-mode-hook (lambda ()
                                 (progn
                                   (setq-default live-py-version (executable-find "python"))
                                   (live-py-update-all))))

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

  ;; ======================================================================
  ;; ** 🕸️ Web Development **
  ;; ======================================================================

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

  ;; ======================================================================
  ;; ** 📁 File Types & Modes **
  ;; ======================================================================

  (add-to-list 'auto-minor-mode-alist '("\\.ino\\'" . arduino-cli-mode))
  (add-to-list 'auto-mode-alist '("\\.libsonnet\\'" . jsonnet-mode))

  ;; ======================================================================
  ;; ** 🔍 Search & Navigation **
  ;; ======================================================================

  ;; work-around
  (use-package helm-rg
    :config (setq helm-rg-default-directory 'git-root))

  ;; ======================================================================
  ;; ** 🪟 Window & Buffer Management **
  ;; ======================================================================

  ;; Use a more reliable Magit display function
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq projectile-switch-project-action 'magit-status)

  ;; 2024 how did I live so long w/out these
  (global-set-key (kbd "M-m <") 'eyebrowse-prev-window-config)
  (global-set-key (kbd "M-m >") 'eyebrowse-next-window-config)

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

  ;; ======================================================================
  ;; ** ⌨️ Key Bindings & Shortcuts **
  ;; ======================================================================

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

  ;;; key-bindings I immediately miss
  (global-set-key (kbd "M-s s") 'helm-swoop)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;;; try these new ones
  (global-set-key (kbd "C-c f") 'select-frame-by-name)

  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete

  (global-set-key [(meta down)] 'scroll-other-window)    ; C-M-v
  (global-set-key [(meta up)] 'scroll-other-window-down) ; C-M-S-v

                                        ; was just f11, bad on Darwin
                                        ; similar to M-<f10> which is toggle-frame-maximized
  (global-set-key (kbd "M-<f11>") 'toggle-frame-fullscreen)

  (global-set-key [f5] 'global-whitespace-mode)
  (global-set-key [f6] 'toggle-truncate-lines)

  (global-set-key (kbd "C-c o") 'browse-url-at-point) ; like "o"pen

  (global-set-key (kbd "C-M-/") 'comint-dynamic-complete-filename)

  (define-key global-map (kbd "RET") 'newline-and-indent)

  ;; ======================================================================
  ;; ** 🔧 Mode-Specific Key Bindings **
  ;; ======================================================================

  (with-eval-after-load 'prog-mode
    ;; code folding
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

  ;; ======================================================================
  ;; ** 📂 Project Management **
  ;; ======================================================================

  (with-eval-after-load 'projectile
    (setq projectile-enable-caching t)
    (setq projectile-tags-command "ctags -e -R ."))

  ;; ======================================================================
  ;; ** 🛠️ Utility Functions **
  ;; ======================================================================

  (defun insert-current-date () (interactive)
         (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

    ;; Load date utilities
  (load-file (expand-file-name "lisp/date-utils.el" dotspacemacs-directory))

  (global-set-key (kbd "s-<up>") (lambda () (interactive) (adjust-date-on-line 1)))
  (global-set-key (kbd "s-<down>") (lambda () (interactive) (adjust-date-on-line -1)))

  (defun my-zap-to-char ()
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
     you are deleting forward, the CHAR is replaced and the point is
     put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))
  (advice-add 'zap-to-char :after #'my-zap-to-char)

  (defun md-bold-to-h3 ()
    "Convert a Markdown bold string (**text**) at point into a Markdown H3 heading."
    (interactive)
    (save-excursion
      ;; Go to beginning of line
      (beginning-of-line)
      ;; Check if line starts with "**"
      (when (looking-at "\\*\\*\\(.*?\\)\\*\\*")
        (let ((content (match-string 1)))
          (delete-region (point) (line-end-position))
          (insert (concat "### " content))))))

  ;; -- Load Dropbox Paper utilities --------------------------------------
  ;; Main benefits are:
  ;; s-<up>   (Alt) change date later
  ;; s-<down> (Alt) change date earlier
  (load-file (expand-file-name "lisp/dropbox-paper-utils.el" dotspacemacs-directory))

  (defun convert-dp-to-md ()
    "Clean up whitespace, collapse double newlines, and run dp-icon-convert-buffer-to-unicode."
    (interactive)
    (whitespace-cleanup)
    (save-excursion
      (goto-char (point-min))
      (replace-string
       (concat "\C-j\C-j")   ; two newlines
       "\C-j"                ; one newline
       nil))                 ; don’t ask for confirmation
    (dp-icon-convert-buffer-to-unicode))

  ;; ======================================================================
  ;; ** 📦 Package Configuration **
  ;; ======================================================================

  ;; Load defadvice-patch-advanced to eliminate defadvice warnings
  ;; This uses functional package readiness detection instead of crude timeouts
  (condition-case load-err
      (progn
        (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
        (require 'defadvice-patch-advanced))
    (error (message "⚠️ Failed to load defadvice-patch-advanced: %S" load-err)))

  ;; Markdown mode configuration
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "C-c m t") #'markdown-toc-generate-toc)
    (define-key markdown-mode-map (kbd "C-c m r") #'markdown-toc-refresh-toc))

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
    :bind (;; spacemacs ("M-m s m r" . mc/edit-lines)
           ;; spacemacs ("M-m s m b" . mc/mark-all-like-this)
           ("C->"         . mc/mark-next-like-this)
           ("C-<"         . mc/mark-previous-like-this)
           ("C-!"         . mc/mark-next-symbol-like-this)
           ("s-r"         . mc/mark-all-in-region)
           ("s-d"         . mc/mark-all-dwim)))

  (use-package visual-regexp-steroids
    :ensure t
    :config
    (setq vr/engine 'pcre2el)  ; Use pcre2el for better regex support
    :bind (("M-%" . vr/replace)
           ("C-M-%" . vr/query-replace)))

  (use-package pcre2el
    :ensure t
    :config
    ;; Enable PCRE syntax in regex patterns
    (pcre-mode 1))

  (use-package helm-projectile
    :ensure t)

  ;; ======================================================================
  ;; ** ⚙️ Miscellaneous Settings **
  ;; ======================================================================

  ;; Suppress startup messages and warnings
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)

  ;; Reduce verbosity in messages buffer
  (setq message-log-max 1000)

  ;; Font-lock configuration for proper syntax highlighting
  (setq font-lock-verbose nil)
  (setq font-lock-support-mode 'jit-lock-mode)
  (setq font-lock-maximum-decoration t)
  (setq font-lock-global-modes '(not shell-mode))

  ;; Ensure font-lock is enabled
  (global-font-lock-mode t)

  ;; Ensure proper fontification for Emacs Lisp files
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (font-lock-mode t)
              (font-lock-ensure)))

  ;; Force fontification for current buffer if it's an Emacs Lisp file
  (when (and (buffer-file-name)
             (string-match "\\.el\\'" (buffer-file-name)))
    (font-lock-mode t)
    (font-lock-ensure))

  ;; Create a dummy 'quote' face to prevent warnings
  (defface quote nil
    "Dummy face to prevent 'Invalid face reference: quote' warnings."
    :group 'basic-faces)

  (setq make-backup-files t)

  (setq fill-column 99)

  ;;; Spacemacs need frame titles (helps with viewing multiple frames)
  (setq-default frame-title-format
                '((:eval (if (buffer-file-name)
                             (abbreviate-file-name (buffer-file-name))
                           "%b"))))

  ;; ======================================================================
  ;; ** 🏠 Location-Specific Configuration **
  ;; ======================================================================

  (setq locations '("home" "work"))
  (dolist (loc locations)
    (let ((init-file (concat "~/.spacemacs.d/" (concat loc "_init.el"))))
      (if (file-exists-p init-file)
          (progn
            (message (concat "loading " init-file))
            (load init-file)))))

  ;; END
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
 '(epg-gpg-program "/usr/local/MacGPG2/bin/gpg2")
 '(package-selected-packages
   '(ace-link aggressive-indent anzu arduino-mode attrap auto-compile
              auto-highlight-symbol auto-minor-mode auto-yasnippet
              avy-jump-helm-line blacken browse-at-remote buffer-move
              centered-cursor-mode cider-eval-sexp-fu clean-aindent-mode
              clj-refactor clojure-snippets cmm-mode code-cells code-review
              color-identifiers-mode column-enforce-mode command-log-mode
              company-c-headers company-cabal company-emoji company-quickhelp
              company-statistics company-terraform company-web cpp-auto-include
              csv-mode cython-mode dante dap-mode devdocs diff-hl diminish
              dired-quick-sort disable-mouse disaster docker dockerfile-mode
              dotenv-mode drag-stuff dumb-jump eat ebuild-mode edit-indirect
              elisp-def elisp-demos elisp-slime-nav ellama emmet-mode
              emoji-cheat-sheet-plus emr engine-mode esh-help
              eshell-prompt-extras eshell-z evil-anzu evil-args
              evil-cleverparens evil-escape evil-evilified-state evil-exchange
              evil-goggles evil-iedit-state evil-indent-plus evil-lion
              evil-lisp-state evil-matchit evil-mc evil-nerd-commenter
              evil-numbers evil-org evil-surround evil-textobj-line evil-tutor
              evil-unimpaired evil-visual-mark-mode evil-visualstar
              exec-path-from-shell expand-region eyebrowse fancy-battery
              flycheck-clj-kondo flycheck-elsa flycheck-haskell flycheck-package
              flycheck-pos-tip flyspell-correct-helm geiser gemini-mode gendoxy
              gh-md git-link git-messenger git-modes git-timemachine
              gitignore-templates gnuplot go-fill-struct go-gen-test go-mode
              godoctor golden-ratio google-c-style google-translate gptel
              graphql-mode haskell-snippets helm-ag helm-c-yasnippet helm-cider
              helm-comint helm-company helm-css-scss helm-descbinds
              helm-git-grep helm-hoogle helm-ls-git helm-lsp helm-make
              helm-mode-manager helm-org helm-org-rifle helm-projectile
              helm-purpose helm-pydoc helm-rg helm-themes helm-xref helpful
              hide-comnt highlight-indentation highlight-numbers
              highlight-parentheses hl-todo hlint-refactor holy-mode hoon-mode
              hungry-delete hybrid-mode impatient-mode indent-guide info+
              inspector js-doc js2-refactor json-mode json-navigator
              json-reformat jsonnet-mode key-chord keycast keychain-environment
              launchctl link-hint live-py-mode livid-mode logcat lorem-ipsum
              lsp-haskell lsp-origami lsp-ui macrostep markdown-toc matlab-mode
              multi-line multi-term multi-vterm mwim nameless nodejs-repl
              npm-mode nyan-mode open-junk-file org-cliplink org-contrib
              org-download org-mime org-pomodoro org-present org-projectile
              org-rich-yank org-superstar orgit-forge osx-clipboard
              osx-dictionary osx-trash overseer page-break-lines paradox
              password-generator pcre2el pip-requirements pipenv pippel
              pkgbuild-mode poetry popwin prettier-js pug-mode py-isort pydoc
              pyenv-mode pylookup pytest qml-mode quickrun rainbow-delimiters
              rainbow-identifiers rainbow-mode restart-emacs
              reveal-in-osx-finder rg rjsx-mode ron-mode rustic sass-mode sayid
              scad-mode scss-mode shell-pop slim-mode smeargle sml-mode
              space-doc spaceline-all-the-icons spacemacs-purpose-popwin
              spacemacs-whitespace-cleanup sphinx-doc sql-indent stan-mode
              string-edit-at-point string-inflection symbol-overlay symon
              tagedit term-cursor terminal-here tern thrift toc-org toml-mode
              transient treemacs-icons-dired treemacs-magit treemacs-persp
              treemacs-projectile typescript-mode undo-fu undo-fu-session unfill
              unicode-fonts vala-mode vala-snippets valign vi-tilde-fringe
              visual-regexp visual-regexp-steroids volatile-highlights vundo
              web-beautify web-mode winum wolfram-mode writeroom-mode ws-butler
              yaml-mode yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
