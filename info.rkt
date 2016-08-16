#lang info
(define collection "glob")
(define deps '("base" "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define pkg-desc "Unix-style globbing")
(define version "0.2")
(define pkg-authors '(ben))
(define scribblings '(("scribblings/glob.scrbl" () ("Scripting"))))
