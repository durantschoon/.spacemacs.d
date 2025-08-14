# Extended Warning Suppression System

This script automatically patches downloaded packages to eliminate obsolete warnings in Emacs 30.1+ by adding warning suppression to files that contain obsolete function calls.

## Problem

Many packages still use obsolete functions and macros, which trigger warnings in modern Emacs versions:

```
Warning: 'defadvice' is an obsolete macro (as of 30.1); use 'advice-add' or 'define-advice'
Warning: 'destructuring-bind' is an obsolete alias (as of 27.1); use 'cl-destructuring-bind'
Warning: 'callf' is an obsolete alias (as of 27.1); use 'cl-callf'
```

These warnings appear even when the obsolete calls are in conditional branches that won't be executed, because Emacs processes the entire form during macro expansion.



## Solution

This script automatically:

1. Finds all `.el` files in your `elpa/*/develop/` directories
2. Identifies files containing obsolete function calls (`defadvice`, `destructuring-bind`, `callf`)
3. Adds comprehensive warning suppression to each file:
   ```elisp
   ;; Suppress obsolete warnings
   (put 'defadvice 'byte-obsolete-info nil)
   (put 'destructuring-bind 'byte-obsolete-info nil)
   (put 'callf 'byte-obsolete-info nil)
   ```
4. Runs automatically after package installation/updates

## Installation

1. Save `defadvice-patch-advanced.el` to your Emacs configuration directory
2. Add to your `init.el`:

```elisp
(require 'defadvice-patch-advanced)
```

## Usage

### Automatic (Recommended)

The script runs automatically after package operations via hooks:

- `package-post-install-hook`
- `package-post-update-hook`

### Manual

You can also run it manually:

```elisp
M-x defadvice-patch-manual
```

### Cleanup and Reapply

If you need to clean up old patches and reapply the new approach:

```elisp
M-x defadvice-patch-cleanup-and-reapply
```

## How It Works

1. **Finds Packages**: Scans `package-user-dir` for files in `elpa/*/develop/` directories
2. **Detects Obsolete Calls**: Searches for files containing `defadvice`, `destructuring-bind`, or `callf` calls
3. **Adds Warning Suppression**: Inserts comprehensive warning suppression after the header comment
4. **Preserves Functionality**: All obsolete calls continue to work exactly as before

## Example

**Before:**

```elisp
;;; some-package.el --- Some package -*- lexical-binding: t -*-

(if (not (fboundp 'advice-add))
    (defadvice some-function (around some-advice activate)
      "Some advice."
      (message "advised"))
  (advice-add 'some-function :around #'some-advice))

(destructuring-bind (a b) some-list
  (callf some-function a b))
```

**After:**

```elisp
;;; some-package.el --- Some package -*- lexical-binding: t -*-

;; Suppress obsolete warnings
(put 'defadvice 'byte-obsolete-info nil)
(put 'destructuring-bind 'byte-obsolete-info nil)
(put 'callf 'byte-obsolete-info nil)

(if (not (fboundp 'advice-add))
    (defadvice some-function (around some-advice activate)
      "Some advice."
      (message "advised"))
  (advice-add 'some-function :around #'some-advice))

(destructuring-bind (a b) some-list
  (callf some-function a b))
```

## Benefits

- ✅ **No warnings** - Eliminates obsolete warnings completely
- ✅ **Comprehensive** - Handles `defadvice`, `destructuring-bind`, and `callf`
- ✅ **Automatic** - Runs after package operations
- ✅ **Safe** - Preserves all functionality exactly as before
- ✅ **Simple** - Just suppresses warnings, no code replacement
- ✅ **Non-invasive** - Doesn't require package authors to change their code
- ✅ **Extensible** - Easy to add more obsolete functions in the future

## Files

- `defadvice-patch-advanced.el` - Main script with extended warning suppression
- `defadvice-patch.el` - Basic version (defadvice only)

## Notes

- The script only patches files in `elpa/*/develop/` directories
- It checks for existing warning suppression to avoid duplicates
- Failed patches are logged but don't stop the process
- The script is idempotent - running it multiple times is safe
- Currently handles: `defadvice`, `destructuring-bind`, `callf`
- Easy to extend for additional obsolete functions

## Troubleshooting

### Emergency Disable

If you encounter any issues, you can easily disable the system:

#### Option 1: Quick Disable (Recommended)

```elisp
;; In your init.el, just comment out the require:
;; (require 'defadvice-patch-advanced)
```

#### Option 2: Nuclear Option (If needed)

```bash
# Delete all patched packages and reinstall
rm -rf ~/.emacs.d/elpa/30.1/develop/*
# Then restart Emacs - it will reinstall packages cleanly
```

#### Option 3: Selective Cleanup

```bash
# Remove just the warning suppression lines from all files
find ~/.emacs.d/elpa -name "*.el" -exec sed -i '' '/put.*byte-obsolete-info/d' {} \;
```

### What to Expect

- **After commenting out**: No new patches applied, existing patches remain
- **After deleting packages**: Clean reinstall, all patches removed
- **After restart**: Packages work normally, warnings return (but no damage done)

### Autoload Warnings

You may see temporary autoload warnings like:

```
Error loading autoloads: (file-missing Cannot open load file No such file or directory /path/to/package-autoloads)
```

These are normal and will resolve automatically. Emacs regenerates autoload files when needed.
