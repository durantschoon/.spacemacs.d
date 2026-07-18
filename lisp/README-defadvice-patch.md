# Obsolete-macro Warning Suppression

Silences the obsolescence warnings that older packages trigger on Emacs 30.1+:

```
Warning: 'defadvice' is an obsolete macro (as of 30.1); use 'advice-add' or 'define-advice'
Warning: 'destructuring-bind' is an obsolete alias (as of 27.1); use 'cl-destructuring-bind'
Warning: 'callf' is an obsolete alias (as of 27.1); use 'cl-callf'
```

These appear even when the obsolete calls sit in conditional branches that never
execute, because Emacs processes the whole form during macro expansion.

## How it works

`defadvice-patch-advanced.el` clears the `byte-obsolete-info` property on the
three symbols, from `emacs-startup-hook`:

```elisp
(put 'defadvice 'byte-obsolete-info nil)
(put 'destructuring-bind 'byte-obsolete-info nil)
(put 'callf 'byte-obsolete-info nil)
```

That is the entire mechanism. Deferring to `emacs-startup-hook` keeps it from
racing package initialization.

## Usage

`init.el` requires it in the Package Configuration section; `lisp/` is already on
`load-path`:

```elisp
(require 'defadvice-patch-advanced)
```

Nothing else to run — there are no interactive commands. To disable, comment out
the `require`. Nothing is written to disk, so there is nothing to undo.

## History: what this used to be

Through v1.0 this was ~394 lines that walked `package-user-dir` and rewrote
package `.el` files on disk to insert the same `put` forms, driven by package
hooks and a readiness-detection layer.

**None of it ever ran.** An unbalanced paren made the file fail to load from the
outset, so `require` always signalled and the caller swallowed the error. Fixing
the paren revealed the code was independently unsound:

| Referenced | Reality |
| ---------- | ------- |
| `package--pending-actions`, `package--busy`, `package--download-in-progress` | never existed in Emacs |
| `comp-async-compilation` | never existed (real: `comp-files-queue`, `comp-async-compilations`) |
| `package-post-install-hook`, `package-post-update-hook` | package.el provides no post-install hooks |
| `(return)` in the "skip critical files" guard | Common Lisp; unbound in elisp, so the guard crashed rather than returning |

The approach was also wrong on its own terms, bugs aside:

- Emacs loads `.elc`, not `.el` — patching source post-install has no effect
- the warnings are emitted at **byte-compile time**, before any such patch applies
- rewriting `.el` leaves it newer than its `.elc`, inviting a different warning
- any package reinstall silently reverts the edits

The `put` forms achieve the intended result directly, so the machinery was
removed rather than repaired. Commands it documented — `defadvice-patch-manual`,
`defadvice-patch-cleanup-and-reapply`, `defadvice-patch-all-packages` — no longer
exist, and never worked. A companion `defadvice-patch.el` referenced by the old
README is not present in this directory.
