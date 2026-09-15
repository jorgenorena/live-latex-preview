# live-tex-preview

Personal Emacs package for asynchronous math previews in TeX, Markdown and Quarto.
GPL-3.0-or-later. This is an extracted and maintained personal implementation,
not a MELPA release.

## Architecture

- `live-tex-preview-engine.el`: asynchronous string-to-SVG rendering, batch
  compilation, geometry, hashing, persistent metadata, cancellation and cleanup.
  Uses `make-process` and sentinels. It has no buffer-overlay, major-mode, Org,
  Doom, parsing or export dependency.
- `live-tex-preview.el`: buffer placement, image specs, overlay edit state,
  cursor reveal/restore, lazy display, inline live strings and block posframes.
  Also owns the shared interactive commands and minor mode. Small buffer-local
  callbacks supply scanning, local fragment lookup, block classification,
  rendering settings and document directory; there is no provider registry.
- `live-tex-preview-tex.el`: TeX scanner, master/preamble resolution, optional
  numbering removal and document-specific rendering settings.
- `live-tex-preview-markdown.el`: conservative math delimiters with markdown-mode
  code and metadata exclusions, plus a separate configurable math preamble.
- `live-tex-preview-polymode.el`: optional host-overlay ownership and live math
  views for poly-markdown/poly-quarto. Executable inner buffers never enable the
  preview minor mode.

The renderer writes uncached fragments into one `preview.sty` document, runs
`latex` once, then converts the DVI pages using `dvisvgm`. It reads height,
depth, width and tightpage margins, uses font-relative image height and baseline
ascent, and rewrites a foreground stand-in to SVG `currentColor`. Explicit
LaTeX colors remain intact. Zoom and theme changes do not require recompilation.
No precompiled formats are generated or used.

Process chaining and on-disk metadata are rewritten for this package. Geometry
conversion and rendering conventions are adapted from upstream. Cursor and live
display functions are substantially retained from the old local frontend,
including its Org-derived functions, with package-owned names and properties.
Timers retain their source buffer and are cancelled on teardown; generation
tokens reject results for overlays edited or deleted during compilation.

## Dependencies and installation

- Emacs 29.1 or later; SVG support and a graphical frame for the preview UI.
  Rendering and metadata creation also work in batch/terminal Emacs.
- `latex`, `dvisvgm`, and TeX packages `preview`, `xcolor`; the default preamble
  also uses `amsmath`, `amssymb`, `amsfonts`. A document's actual preamble can
  require additional TeX packages, fonts, classes and local files.
- `posframe` for the default block live preview. Missing posframe produces a
  message; static and inline previews still work.
- AUCTeX is optional: its master-file information is used when available.
  Built-in `latex-mode` works too. No Org or Doom dependency.
- `markdown-mode` 2.6+ for Markdown/Quarto. Polymode and Quarto are optional,
  needed only when using those editing modes. No Quarto CLI is needed to preview
  math. Their own dependencies remain managed by their packages.

Outside Doom:

```elisp
(add-to-list 'load-path "/path/to/dotfiles/live-tex-preview")
(require 'live-tex-preview)
(add-hook 'LaTeX-mode-hook #'live-tex-preview-mode)
(add-hook 'latex-mode-hook #'live-tex-preview-mode)
(add-hook 'markdown-mode-hook #'live-tex-preview-mode)
(add-hook 'poly-quarto-mode-hook #'live-tex-preview-mode)
```

The repository's Doom `packages.el` declares a straight local recipe, plus
posframe. `config.org` is the authoritative configuration; tangle it to update
`config.el`, then run `doom sync` and restart Emacs. Org uses Doom's normal
recipe/pin; Org buffers use stable Org's preview command and scale settings.
The development-only automatic Org preview minor mode is no longer enabled.

Existing LaTeX localleader keys are preserved: `v v` toggle live interaction,
`v P` preview buffer, `v p` preview at point, `v c` clear previews, `v C` clear
cached images. The raw-TeX toggle uses the new commands too.
Markdown/Quarto host buffers use the same `v` bindings. Loading the old
`live-tex-preview-tex` feature still works; commands now dispatch in the common
library. A TeX mode hook firing in a Quarto indirect buffer cannot enable a TeX
frontend there. The GitHub recipe migration is separate from this frontend work;
the dotfiles currently still use the local package checkout.

## Markdown and Quarto policy

Recognized delimiters are `$...$`, `$$...$$`, `\(...\)` and `\[...\]`.
Double dollars take precedence. Display math can span lines, but no fragment
crosses a blank paragraph, code or metadata. Arbitrary LaTeX environments outside
these delimiters are not recognized.

Single-dollar math is deliberately conservative: one line, non-whitespace beside
the inner delimiters, no word character immediately before the opener or after
the closer. Numeric-leading expressions such as `$2x$`, `$20$`, and `$0.5$`
are accepted when both delimiters occur on the same line in prose. The first
unescaped dollar must be a valid closer. Dollar runs of three or more are
rejected. Lone currency such as `$20` and amounts such as `$20 and $30` are
ignored. Deliberately paired text like `$20$` or `$USD$` remains inherently
ambiguous and is treated as math; escape currency dollars when necessary.
Multiline math uses display delimiters such as `$$...$$`.

The frontend uses markdown-mode's syntax properties and code matchers for inline
code (including multiple backticks), fenced code, recognized indented code,
YAML front matter and comments. It also checks unfinished fence openers, since
markdown-mode may not mark their unfinished bodies. Syntax properties are
updated incrementally; math fontification settings are not changed. Polymode
non-math inner spans provide an additional exclusion for executable/raw regions.
This is not an independent complete CommonMark or HTML parser: other syntax
extensions are only excluded when the host mode identifies them as code.

Quarto prose uses that same scanner. Source overlays stay in the Markdown host;
commands invoked in an indirect buffer target that host, preserving caller point
and narrowing. When Polymode places math in a LaTeX inner buffer, a temporary
inline image or block posframe displays the host's result while source is edited.
The source overlays and render jobs never belong to executable inner buffers.
Chunk navigation, syntax highlighting and execution configuration are unchanged.

Markdown has its own `live-tex-preview-markdown-preamble` (article, AMS and colors)
and `live-tex-preview-markdown-extra-preamble`. For project-specific macros, set
the latter through Customize or a directory-local variable, for example:

```elisp
((markdown-mode
  . ((live-tex-preview-markdown-extra-preamble
      . "\\newcommand{\\Pk}{P(k)}\n"))))
```

YAML `header-includes`, YAML includes/anchors, `_quarto.yml`, profiles and Quarto
format inheritance are **not resolved**. The installed Quarto mode supplies no
YAML/configuration parser; explicit preamble configuration avoids partial or
incorrect macro extraction. TeX document-preamble resolution is unchanged.

Live edits inspect the existing fragment with its overlay end as a strict limit;
they do not invoke the whole-document scanner. Structural Markdown changes
debounce validation of existing overlays, so wrapping a rendered equation in code
removes its preview. Newly typed formulas still need an initial explicit preview.

## Rendering and placement APIs

```elisp
(live-tex-preview-render
 '("$x_1$" "\\[\\frac{a}{b}\\]")
 (lambda (results error-text)
   ;; RESULTS is a vector in input order. ERROR-TEXT is nil on success.
   (unless error-text
     (message "%s" (plist-get (aref results 0) :file))))
 :preamble "\\documentclass{article}\n\\usepackage{amsmath}\n"
 :page-width "475pt"
 :input-directory "/path/to/project/"
 :cache-directory "/path/to/project/.cache/")
```

Returns a `live-tex-preview-job` immediately. The callback runs once,
asynchronously even for cache hits. Each result is a plist with `:file`, `:key`,
`:image-type` (`svg`), `:height`, `:depth`, `:width`. Dimensions are em units;
height includes depth and padding. Unproduced entries are nil. Inspect
`live-tex-preview-job-status` / `live-tex-preview-job-error`, or cancel with
`live-tex-preview-cancel`. `live-tex-preview-image` converts metadata to an
image spec with optional zoom. The caller chooses how to display it.

```elisp
;; In a buffer, ENTRIES are (BEG END LATEX) lists.
(live-tex-preview-place entries
                        :preamble preamble
                        :page-width "475pt"
                        :input-directory project-directory
                        :cache-directory cache-directory)
```

Placement returns the same job type and installs previews when results arrive.
Interactive `live-tex-preview-buffer`, `-region`, `-at-point`, `-clear`,
`-clear-cache`, and `-mode` dispatch to the appropriate frontend.

## Cache and limitations

The TeX frontend defaults to a `.cache` directory beside the main document.
Only `live-tex-<sha256>.svg` and `.eld` files persist. Each job owns a separate
temporary directory under the cache and removes it on success, failure or
cancellation. Metadata is read as data, never evaluated. The cache key includes
fragment, preamble, page width, input directory, executable settings and format
version. Cache clearing only removes package-owned files, not the entire shared
`.cache` directory.

Included preamble files and installed TeX packages are not recursively hashed:
clear previews' cache after changing those. The preamble and master selection
are refreshed by explicit preview commands. Macros defined only in the document
body are not captured. Cache files are not automatically expired; edited
fragments also use the persistent content cache. An interrupted Emacs process
can leave a `live-tex-job-*` scratch directory.

This version supports the `latex` → DVI → `dvisvgm` path only. The previous
optional dvipng compatibility advice was not retained. Remote TeX rendering and
XeLaTeX/LuaLaTeX-only preambles are not supported. TeX runs with shell escape
disabled. Newly typed fragments need an initial explicit preview before live
editing starts, as in the former implementation. The scanner handles configured
math delimiters and comments, not arbitrary TeX macro expansion or verbatim
syntax. Deleting a delimiter invalidates its overlay rather than consuming the
next equation. A failed compilation reports its diagnostics through the job
callback and leaves source editable. Errors confined to individual fragments
cause a retry batch of the surviving fragments, so one bad equation does not
prevent the others from rendering. Results for failed fragments remain nil.

The Markdown/Quarto addition did not change the engine or public rendering API.
A future notebook-output integration can call `live-tex-preview-render` directly;
it still needs to choose a preamble/cache policy, manage output replacement and
cancellation, and display the returned image metadata in its own UI. No notebook
package was modified here.

## Provenance and licensing

Extraction source: GNU Org's `lisp/org-latex-preview.el` from
<https://git.tecosaur.net/tec/org-mode.git>, commit
`1ef59f0aa02e3cff40bae68b756a29bc2001739e`, as installed in this repository's
Doom environment when extracted. Upstream source notice:

> Copyright (C) 2022-2024 Free Software Foundation, Inc.
> Authors: TEC <contact@tecosaur.net> and Karthik Chikmagalur

TEC is also known as Tecosaur. Modifications, standalone process/cache plumbing,
and the LaTeX frontend are attributed separately to Jorge Noreña (2026).
The earlier local `latex-live-preview.el` is the source of the TeX scanner,
master-file handling and popup refinements.

Substantially derived portions: the `preview.sty` batching template; scaled-point
geometry and optical correction; currentColor stand-in and font-relative
height/ascent conventions; cursor pre/post/open/close logic; live-display
debounce/throttle, string installation and update-hook flow. Org parsing, export,
numbering tables, persistence, precompilation and its asynchronous task runner
were not imported. Names have been changed, but upstream authorship and
copyright have not been replaced.

All package sources use SPDX `GPL-3.0-or-later`. [COPYING](COPYING) contains
the full GPLv3 text. There is no warranty.

## Verification

```sh
emacs -Q --batch -L live-tex-preview \
  -l live-tex-preview/test/live-tex-preview-test.el \
  -f ert-run-tests-batch-and-exit
emacs -Q --batch -L live-tex-preview -f batch-byte-compile \
  live-tex-preview/live-tex-preview-engine.el \
  live-tex-preview/live-tex-preview.el \
  live-tex-preview/live-tex-preview-tex.el
```

The Markdown suite includes the TeX regression suite:

```sh
emacs -Q --batch -L live-tex-preview -L live-tex-preview/test \
  -L /path/to/markdown-mode \
  -l markdown-test -f ert-run-tests-batch-and-exit
```

Also supply `-L` paths for Polymode, poly-markdown, quarto-mode and its request
dependency to run the real Quarto indirect-buffer tests (otherwise they skip).
To byte-compile the optional frontends, put markdown-mode and Polymode on
`load-path` and include `live-tex-preview-markdown.el` and
`live-tex-preview-polymode.el` in the compilation command.

Tests cover scanning, comments/escapes, document bounds, master selection,
number rewriting, sanitization, hashes, actual rendering/cache hits, API shape,
stale callbacks, delimiter deletion, enter/edit/leave and terminal behavior.
They also exercise master-file inputs, corrupt/missing cache metadata and
isolation of malformed fragments in a batch.
The actual rendering tests require the TeX dependencies above.
`test/gui-smoke.el` runs a separate graphical smoke test with stable Org;
launch it with `emacs -Q`, the package and test directories, and posframe on
`load-path`. It writes `/tmp/live-tex-preview-gui-result.el` and exits. It checks
image dimensions, timed live updates, inline display, block posframe and text
scaling without loading the development preview library.

`test/markdown-gui-smoke.el` tests actual Quarto buffer switches, host ownership,
timed math-inner edits, image restoration, block posframes and code exclusion.
Run it in a clean graphical Emacs with the same dependencies plus posframe on
`load-path`. It writes `/tmp/live-tex-preview-markdown-gui-result.el` and exits.

Verified on 2026-09-15: 34/34 ERT tests pass, including real rendering and Quarto
indirect buffers; package files byte-compile without warnings on Emacs 30.2.
The Quarto GUI smoke test passes. A static Markdown scan of 500 prose equations
interleaved with 500 code chunks took about 0.15 seconds on the development
machine. Live-lookup tests explicitly forbid calling the whole-document scanner.
The original TeX GUI test checks system Org 9.7.11, timed regeneration, cursor
restoration, posframe and text scaling. Full interactive Evil/AUCTeX stress
testing in the user's normal Doom session remains a manual check after
`doom sync` and restart.
