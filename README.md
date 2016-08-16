glob
====
[![Build Status](https://travis-ci.org/bennn/glob.svg)](https://travis-ci.org/bennn/glob)
[![Coverage Status](https://coveralls.io/repos/bennn/glob/badge.svg?branch=master&service=github)](https://coveralls.io/github/bennn/glob?branch=master)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](http://docs.racket-lang.org/glob/index.html)

Unix-style globbing, in Racket.

```
#lang racket
(require glob) ;; Use `glob/typed` in Typed Racket

;; Prints the first line of each Racket file
(for ([filename (in-glob "*.rkt")])
  (displayln (with-input-from-file filename read-line)))
```


API Functions
---
- `(glob glob-path-string)` Returns a list of all files or directories matched by the glob `glob-path-string`.
- `(in-glob glob-path-string)` Returns a lazy sequence of all matches.
- `(glob-match? glob-path-string path-string)` Return `#t` if `path-string` would be captured by `glob-path-string`.

The matches returned by either function should be exactly the same as those returned by the Unix glob `file . -name glob-path-string`.
Please submit an issue if you find a counterexample.

Both functions accept an optional keyword argument `#:with-dotfiles?`.
When this argument is `#t`, search results will include dotfiles matching the pattern.
(Dotfiles are filtered by default unless matched for explicitly.)

Install
---

You have three choices:

1. Run `raco pkg install glob` to install through the [Racket package server](pkgs.racket-lang.org).

2. Clone this repository, run `raco pkg install glob/` on the newly-made directory.

3. Clone this repository and manually link to the file `main.rkt` inside it.

Globbing 101
---

Globs are path strings that may contain the following special characters.

- `*` is a wildcard.
  It means "match anything".
  For example, `(glob "*.rkt")` will match all files in the current directory with a `.rkt` file extension.
- `?` is an option.
  It means "the previous character might not be there".
  For example, `(glob "*.rktd?")` will match all `.rkt` and all `.rktd` files in the current directory.
- Square braces `[]` are for branching on single characters.
  The pattern `[abc]` means "match `a`, or match `b`, or match `c`".
  For example, `(glob "*.[co]")` will match all `.c` and all `.o` files in the current directory.

Besides these pattern variables, a glob is just a path.

Notes
-----
- To match a literal `*` or `?` or square brace, escape it with a preceding `\`.
  Be careful though, with Racket's string reader.
  For example, `(glob "foo\*")` is an error: `unknown escape character \*.`
  Instead, escape the slash with `(glob "foo\\*")` or create a string literal with `(glob (string #\f #\o #\o #\\ #\*))`.

- The file `main.rkt` contains a unit test suite.
  You can run this test with `raco test main.rkt`, but beware.
  It creates a few directories in `/tmp`.
  The suite checks not to overwrite or delete any existing directories, but will loop forever if the `/tmp` directory does not exist.

Credits
----

Inspired by the Python [glob](https://docs.python.org/2/library/glob.html) library.

