#!/bin/bash
set -u

echo "🧹 Cleaning Emacs packages and cache..."
rm -rf ~/.emacs.d/elpa
rm -rf ~/.emacs.d/.cache

echo "🚀 Starting Emacs with auto-compilation..."
emacs --batch \
  --eval "(setq package-quickstart t)" \
  --eval "(setq package-native-compile t)" \
  --eval "(setq native-comp-async-report-warnings-errors 'silent)" \
  --eval "(message \"Clean install complete!\")"

# --- GitHub-only packages, vendored into local/ -----------------------------
#
# These are declared in init.el with a string :location pointing into this
# directory (NOT the symbol `local', which Spacemacs resolves to
# ~/.emacs.d/private/local/ for dotfile-owned packages), so Spacemacs puts
# them on load-path and never manages them through ELPA. Two reasons it has
# to work this way:
#
#   1. Installing claude-code-ide from within our own config -- via quelpa or
#      package-vc, either one -- fails with an infinite eager macro-expansion
#      (excessive-lisp-nesting). The recursion comes from something in init.el,
#      not from the package: it installs cleanly under emacs -Q.
#   2. Anything placed in package-user-dir that Spacemacs did not install
#      itself gets pruned on the next start ("--> deleting claude-code-ide").
#
# local/ sidesteps both. The clones are gitignored, so this is what puts them
# on a new machine.

LOCAL_DIR="$HOME/.spacemacs.d/local"
mkdir -p "$LOCAL_DIR"

clone_local_package() {
  local name="$1" url="$2"
  if [ -d "$LOCAL_DIR/$name/.git" ]; then
    echo "  $name: already present, pulling..."
    git -C "$LOCAL_DIR/$name" pull --quiet --ff-only || echo "  ⚠️  $name: pull failed"
  else
    echo "  $name: cloning..."
    git clone --quiet "$url" "$LOCAL_DIR/$name" || echo "  ⚠️  $name: clone failed"
  fi
}

echo "📦 Fetching local packages..."
clone_local_package claude-code-ide "https://github.com/manzaltu/claude-code-ide.el"

echo "✅ Clean install script finished!"
