#!/bin/bash
echo "🧹 Cleaning Emacs packages and cache..."
rm -rf ~/.emacs.d/elpa
rm -rf ~/.emacs.d/.cache

echo "🚀 Starting Emacs with auto-compilation..."
emacs --batch \
  --eval "(setq package-quickstart t)" \
  --eval "(setq package-native-compile t)" \
  --eval "(setq native-comp-async-report-warnings-errors 'silent)" \
  --eval "(message \"Clean install complete!\")"

echo "✅ Clean install script finished!"
