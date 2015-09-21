#lang scribble/manual
@require[racket/include]
@require[scribble/eval]

@title[#:tag "top"]{@bold{Glob: Unix-Style globbing in Racket}}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[glob]

A glob is like a path string, but allows wildcard characters.
This library brings Unix-style globbing to Racket.

@section{API Functions}
@(define glob-eval (make-base-eval '(begin (require glob racket/sequence))))

@defproc[(glob [glob-string string] [#:with-dotfiles? dotfiles? boolean #f]) (listof path-string)]{
  Builds a list of all paths matched by the glob @code{glob-string}.
  Dotfiles are filtered by default unless matched for explicitly.
  Set the keyword argument to @code{#t} to override this behavior.
}

@examples[#:eval glob-eval
  (glob "*.scrbl")
  (glob "glob*")
  (glob "*.rkt")
]

@defproc[(in-glob [glob-string string] [#:with-dotfiles? dotfiles? boolean #f]) (sequenceof path-string)]{
  Returns a sequence of all paths matched by the glob @code{glob-string}, rather than storing each match explicitly in a list.
  When the keyword argument is @code{#t}, returns all matching results including dotfiles.
}

@examples[#:eval glob-eval
 (let ([tmp (path->string (find-system-path 'temp-dir))])
  (sequence-length (in-glob (string-append tmp "/*"))))
 (let ([tmp (path->string (find-system-path 'temp-dir))])
  (sequence-length (in-glob (string-append tmp "/*")
                            #:with-dotfiles? #t)))
]

The matches returned by either function should be exactly the same as those returned by the Unix glob @code{file \. -name glob-path-string}.
Please submit an @hyperlink["http://github.com/bennn/glob/issues"]{issue} if you find a counterexample.

@section{Globbing 101}

Globs are path strings that may contain the following special characters.

@itemlist[
@item{
  @code{*} is a wildcard.
  It means "match anything".
  For example, @code{(glob "*.rkt")} will match all files in the current directory with a @code{.rkt} file extension.
}
@item{
  @code{?} is an option.
  It means "the previous character might not be there".
  For example, @code{(glob "*.rktd?")} will match all @code{.rkt} and all @code{.rktd} files in the current directory.
}
@item{
  Square braces @code{[]} are for branching on single characters.
  The pattern @code{[abc]} means "match 'a', or match 'b', or match 'c'".
  For example, @code{(glob "*.[co]")} will match all @code{.c} and all @code{.o} files in the current directory.
}
]
Aside from these pattern variables, a glob is just a path-string.

@section{Credits}

Inspired by the Python @hyperlink["https://docs.python.org/2/library/glob.html"]{glob} library.
