# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

org-utils is an Emacs Lisp package providing utility functions for working with org-mode documents. It's a single-file package intended for distribution via MELPA or manual installation.

## Package Dependencies

- Emacs 29.1+
- Org mode 9.5+
- cl-lib 0.7+

## Provided Functions

- `org-utils/copy-property-value` - Copy a property value from current Org heading to kill ring
- `org-utils/browse-property-url` - Open URL from a property in default browser
- `org-utils/set-org-agenda-files` - Interactively set `org-agenda-files` with completion and wildcard support
- `org-utils/restore-org-agenda-files` - Restore `org-agenda-files` to saved custom value
- `org-utils/scheduled-monthly-dates` - Generate N monthly dates from a heading's SCHEDULED timestamp

## Development

To test changes, load the file in Emacs:
```elisp
(load-file "org-utils.el")
```

For linting, use `package-lint` or `flycheck` with the Emacs Lisp checker.

No build system or tests are currently present.