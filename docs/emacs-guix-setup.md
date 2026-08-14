# Handoff: set up `emacs-guix` in this Spacemacs config

**Status:** DONE 2026-08-13 on the Guix System box (`geeeks`) — see "Outcome"
at the bottom for what was learned; the plan below is kept as written.
**Written:** 2026-08-13, from the macOS side of this same dotfiles repo.

## Why this note exists

I asked for `emacs-guix` while sitting on macOS. Guix has no Darwin port — the
daemon and `/gnu/store` depend on glibc/Linux — and `emacs-guix` is only a
front-end that drives a *local* `guix` plus a local Guile REPL loaded with the
guix modules. It cannot drive a remote Guix over TRAMP. So the work belongs on
the Guix System box; nothing was installed on the Mac.

## Target

`M-x guix` working inside this Spacemacs config (holy mode — Emacs keybindings,
no evil), on Guix System.

## Plan

1. **Install the package from Guix, not MELPA.**

   ```sh
   guix pull            # do this first, see the ABI note below
   guix install emacs-guix
   ```

   Or, preferred if a Guix Home / system config is already in use, add
   `emacs-guix` to that declaration instead of an imperative `guix install`.

2. **Do NOT add `guix` to `dotspacemacs-additional-packages`.**

   The upstream README warns against mixing installations (Guix + MELPA), because
   the source `.scm` and compiled `.go` Guile files get out of sync between the
   two. Since Guix is providing the package, keep package.el out of it.

3. **Wire it into `dotspacemacs/user-config` in `init.el`** with a guarded
   require, so this same config still loads on the Mac where the package is
   absent:

   ```elisp
   ;; emacs-guix: installed via `guix install emacs-guix', NOT via package.el.
   ;; Guard the require so this config still loads on non-Guix machines.
   ;; MUST come after the System Environment section -- see the exec-path
   ;; note below; `executable-find' is unreliable before it runs.
   (when (executable-find "guix")
     (require 'guix nil t)
     ;; Scheme buffers under a Guix checkout get guix-devel-mode.
     (add-hook 'scheme-mode-hook #'guix-devel-mode))
   ```

   Emacs must be the Guix-installed Emacs for the profile's
   `share/emacs/site-lisp` to be picked up automatically (Guix's
   `guix-emacs-autoload-packages` handles this). If Emacs came from somewhere
   else, add the profile path to `load-path` by hand first.

## What that machine's `init.el` already does — read commit `f7a399a`

`f7a399a fix(env): put ~/.local/bin and ~/bin on exec-path under Guix` landed on
2026-08-09 and changes two assumptions this note would otherwise get wrong:

- **`executable-find` is not trustworthy early in `user-config` on Guix.**
  `exec-path-from-shell` asks a *login* shell, and `~/.config/zsh/.zprofile`
  sources `/etc/profile` and `~/.profile`, both of which rebuild `PATH` from the
  Guix profiles and discard what `.zshenv` prepended. The config compensates in
  its "🌍 System Environment & Paths" section (around `init.el:1124`) with
  `bds/prepend-to-exec-path`. **The `(when (executable-find "guix") ...)` guard
  above therefore has an ordering requirement: it must sit after that section.**
  `guix` itself should survive the clobbering, since the Guix profiles are
  exactly what `/etc/profile` rebuilds `PATH` *from* — but verify rather than
  assume, especially under a daemon.

- **That Emacs is a pgtk build under Wayland, and may be running as a daemon.**
  The same commit widened a `(memq window-system '(mac ns x))` guard to
  `(or (memq window-system '(mac ns x pgtk)) (daemonp))` precisely because
  `window-system` is `pgtk` there, and is nil in a daemon until a frame exists.
  Any `emacs-guix` setup that keys off `window-system`, or that assumes a frame
  exists at load time, needs the same treatment. Note also that a sibling commit
  (`686b806`) fixes an origami face that *hangs pgtk frames* — so pgtk-specific
  rendering fragility is a live theme on this machine, worth remembering if BUI
  list buffers misbehave.

There is also a house idiom worth following: the `claude-code-ide` block
resolves its binary with `executable-find` plus ordered fallbacks, and warns at
startup rather than failing on first use ("Succeeds loudly or fails loudly --
never silently"). If `emacs-guix` needs a path or a profile located, do it the
same way.

4. **Verify:** `M-x guix` should open the main interface. Then check
   `M-x guix-packages-by-name`, `M-x guix-installed-user-packages`, and
   `M-x guix-generations` all render a BUI list buffer without errors.

## Known hazards — read before debugging

- **ABI coupling between `emacs-guix` and `guix` itself.** Guix bug
  [#59864](https://issues.guix.gnu.org/59864): `emacs-guix` built against one
  Guix broke against another when `<package>` changed from a record descriptor
  to a macro, so every record type check failed. Symptom is `Wrong type
  argument` from `guix-packages-by-name`, `guix-all-services`,
  `guix-installed-system-packages`. That specific instance was fixed upstream
  (commit `81191e3410cc00c6438f532599dd0b96d521982f`), and the general fix is to
  `guix pull` so the two stay in lockstep. If those commands throw type errors,
  suspect a stale mismatch first, not the Elisp.

- **Geiser.** `emacs-guix` talks to Guile through Geiser, and Geiser split into
  `geiser` + `geiser-guile` around 0.16. Reported breakage in this area is
  recurring. **Unverified from macOS** — confirm which Geiser the Guix package
  pulls in, and make sure Spacemacs' own `scheme` layer (if enabled) isn't
  installing a second, conflicting Geiser from MELPA. This is the most likely
  place the setup goes wrong.

- **Maintenance.** Upstream (alezost/guix.el, mirrored at
  gitlab.com/emacs-guix/emacs-guix) has been quiet for years, and there has been
  on-list discussion about deprecating it. Set expectations accordingly; if it
  turns out to be dead on a current Guix, say so rather than fighting it.

## Entry points worth knowing

| Command | What it does |
| --- | --- |
| `M-x guix` | Main interface / dispatch popup |
| `guix-packages-by-name` | Search and act on packages |
| `guix-installed-user-packages` | Browse the current profile |
| `guix-generations` | Browse and roll back profile generations |
| `guix-edit` | Jump to a package's definition in the Guix checkout |
| `guix-devel-mode` | Minor mode for editing Guix `.scm` files |

## Tutorials / reference

- Emacs-Guix manual — <https://emacs-guix.gitlab.io/website/manual/latest/emacs-guix.html>
  (**note:** this URL 302'd into a GitLab Pages auth redirect on 2026-08-13, so
  it may be dead; `info "(emacs-guix)"` after installing is the reliable copy)
- Upstream repo / README — <https://github.com/alezost/guix.el>
- GitLab mirror + issue tracker — <https://gitlab.com/emacs-guix/emacs-guix>
- Guix Reference Manual, "The Perfect Setup" (Emacs + Geiser for Guix hacking) —
  <https://guix.gnu.org/manual/en/html_node/The-Perfect-Setup.html>
- Guix Reference Manual, "Using Guix Interactively" —
  <https://guix.gnu.org/manual/1.5.0/en/html_node/Using-Guix-Interactively.html>

Latest Guix release as of writing: **1.5.0** (2026-01-22).

## Open questions for whoever picks this up

1. Is `emacs-guix` still packaged and working on current Guix, or has it been
   deprecated? (I was blocked from checking `packages.guix.gnu.org` here.)
2. Is the Spacemacs `scheme` layer enabled on that machine? If so, resolve the
   Geiser overlap before installing.
3. Should the config live in `init.el`'s `user-config`, or in a Guix Home
   declaration that installs the package and the Emacs together? (Partly
   answered: `init.el` already has a Guix-aware environment section as of
   `f7a399a`, so `user-config` is a reasonable home — but the package install
   itself still wants to be declarative if a Guix Home config exists.)
4. Does `guix` survive the `/etc/profile` PATH rebuild described above, under
   both a normal frame and a daemon? That determines whether the
   `executable-find` guard is enough or needs a fallback path.

## Outcome (2026-08-13, on `geeeks`)

Answers to the open questions, in order:

1. **Still packaged and working.** Guix 1.5.0-era master ships `emacs-guix`
   0.7.0 built against its own current `guix`, which neutralizes the ABI
   hazard by construction. Verified end to end: the deployed Guix Home
   `emacs-pgtk` loads `guix.el` and a `guix-eval-read` round-trip through the
   Guile REPL counted the full package set (32,944). Upstream alezost repo is
   dormant (open issues 2019–2023, a maintainership handoff after an
   abandonware period), but Guix itself keeps it building — which is the
   maintenance that matters for a Guix-only tool.
2. **The Spacemacs `scheme` layer IS enabled**, and installs its own Geiser
   from MELPA while the Guix package pulls `emacs-geiser` 0.33.1 +
   `emacs-geiser-guile` 0.28.5 (post-split, so the split hazard is packaged
   away). Both copies are current-generation; no conflict observed in batch
   tests, but Spacemacs was not loaded there. **Watch item:** if `M-x guix`
   misbehaves inside Spacemacs, suspect load-path order handing `require`
   the MELPA Geiser first.
3. **Both, as the note guessed:** the package is declared in Guix Home —
   `%wayland-packages` in `dot_files/home/common.scm` (the Wayland session is
   the Guix System box; the foreign session has no daemon to drive) — and the
   elisp lives in `init.el`'s "📦 Guix Integration" section, directly after
   the 🌍 System Environment section per the ordering requirement.
4. **Yes, `guix` survives the PATH rebuild** (it lives in the profiles
   `/etc/profile` rebuilds PATH from), so the `executable-find` guard
   sufficed; the config warns loudly if `guix` exists but the require fails.

Not yet verified: `M-x guix-packages-by-name` etc. rendering BUI buffers in an
interactive pgtk frame (the batch REPL test covers the plumbing beneath them,
not the rendering; recall the pgtk fragility note above).
