#lang scribble/manual
@require[scribble/eval scriblib/footnote]

@title[#:tag "top"]{Glob: Unix-Style globbing}
@author[@hyperlink["https://github.com/bennn"]{Ben Greenman}]

@defmodule[glob]

A glob is a pattern over path strings.

Typed Racket users should @racket[(require glob/typed)].@note{
 You can use @racket[glob] in a typed module via @racket[require/typed];
 however, bindings from @racket[glob/typed] cannot be used in untyped code.
}

@section{API Functions}
@(define glob-eval (make-base-eval '(begin (require glob racket/sequence))))

@defproc[(glob [pattern string] [#:with-dotfiles? dotfiles? boolean #f]) (listof path-string)]{
  Builds a list of all paths matching the glob @racket[pattern].
  By default, wildcards ("*") in @racket[pattern] will not capture files or directories whose name begins with a period (aka "dotfiles").
  Setting @racket[dotfiles?] to @racket[#t] overrides this behavior.
}

@examples[#:eval glob-eval
  (glob "*.scrbl")
  (glob "glob*")
  (glob "*.rkt")
]

@defproc[(in-glob [pattern string] [#:with-dotfiles? dotfiles? boolean #f]) (sequenceof path-string)]{
  Returns a sequence of all paths matching the glob @racket[pattern], instead of building a list.
  When the keyword argument is @racket[#t], wildcards will capture dotfiles.
}

@examples[#:eval glob-eval
 (let ([tmp (path->string (find-system-path 'temp-dir))])
  (sequence-length (in-glob (string-append tmp "/*"))))
 (let ([tmp (path->string (find-system-path 'temp-dir))])
  (sequence-length (in-glob (string-append tmp "/*")
                            #:with-dotfiles? #t)))
]

The matches returned by either globbing function should be exactly the same as those returned by the Unix glob @tt{file \. -name pattern}.
Please submit an @hyperlink["http://github.com/bennn/glob/issues"]{issue} if you find a counterexample.

@defproc[(glob-match? [pattern string] [path path-string]) boolean]{
  Returns the same result as:
  @racketblock[
    (member path (glob pattern))
  ]
  except that it can be faster.
  This operation queries the filesystem to ensure that @racket[path] exists.
}


@section{Typed API}
@defmodule[glob/typed]{
  Provides a Typed Racket version of the @racket[glob] API.
}


@section{Globbing 101}

Globs are path strings that may contain the following special characters.

@itemlist[
@item{
  @racket{*} is a wildcard.
  It means "match anything".
  For example, @racket[(glob "*.rkt")] will match all files in the current directory with a @tt{.rkt} file extension.
}
@item{
  @racket{?} is an option.
  It means "the previous character might not be there".
  For example, @racket[(glob "*.rktd?")] will match all @tt{.rkt} and all @tt{.rktd} files in the current directory.
}
@item{
  Square braces @racket{[]} are for branching on single characters.
  The pattern @racket{[abc]} means "match `a', or match `b', or match `c'".
  For example, @racket[(glob "*.[co]")] will match all @tt{.c} and all @tt{.o} files in the current directory.
}
]
Aside from these pattern variables, a glob is just a path-string.

@section{Credits}

Inspired by the Python @hyperlink["https://docs.python.org/2/library/glob.html"]{glob} library.

