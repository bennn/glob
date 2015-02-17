#lang racket/base

;; Unix-like globbing for filepaths.
;; Used to select a set of relative filepaths.

;; Recognizes the *, ?, and [] operators.
;; - * is a wildcard pattern, like "*.rkt"
;; - ? is an optional pattern, like "notes?.txt"
;; - [] specifies a character range, like "file[12345].c"

(provide
  ;; (-> String (Listof Path))
  ;; Return a list of all files/directories matching the argument glob.
  glob
  ;; (-> String (Sequenceof Path))
  ;; Same as glob, but returns an iterable instead of a list.
  in-glob)

;; --------------------------------------------------------------------------------------------------

;; --- API functions

;; (: glob (-> String (Listof Path)))
(define (glob s)
  (for/list ([fp (in-glob s)]) fp))

;; (: in-glob (-> String (Listof Path)))
(define (in-glob s)
  (error "nope"))

;; --- Helpers

;; --- Test globs for main.rkt, in the current directory
(module+ test
  (require rackunit)
  (require (for-syntax racket/base syntax/parse racket/syntax))
  ;; Assert that 'main.rkt' exists.
  (unless (file-exists? "main.rkt")
    (error "Could not find file 'main.rkt' in the current directory."))
  ;; Test successes
  (define-syntax-rule (test-glob-pass stx)
    (syntax-parse stx
      [(_ strs:string ...)
       #'((check-equal? (glob strs) '("main.rkt")) ...)]))
  (test-glob-pass
    "main.rkt"
    ;; * patterns
    "main*"
    "*.rkt"
    "m*n.rkt"
    "*ain*"
    "*in*t"
    "m*a*i*n*.*r*k*t*"
    "mai*kt"
    ;; ? patterns
    "m?ain.rkt"
    "z?main.rkt"
    "mm?ain.rkt"
    "m?ainx?.rkt?x?"
    "main.rkt"
    "main.rkt"
    ;; [] patterns
    "[m][a][i]n.rkt"
    "main.rk[tttt]"
    "[abcm]ain.rkt"
    "[ma][ma]in.rkt"
    "[ma][ma]in.[xr]kt"
    ;; * ?
    ;; * []
    "[m][a][i]n.r*"
    ;; ? []
    "[xyz]?main.rkt"
    ;; * ? []
    "[xyz]?main*[x]?rkt"
  )

  ;; TODO tests should use shell for expected output
)

;; Make fake directory, test some longer paths
(module+ test
  (require racket/system)
  ;; Create files
  (system "mkdir /tmp/globtest1")
  (system "mkdir /tmp/globtest2")
  (system "touch /tmp/globtest1/file1")
  (system "touch /tmp/globtest1/file2")
  (system "touch /tmp/globtest1/file3")
  (system "touch /tmp/globtest2/file1")
  (system "touch /tmp/globtest2/file2")
  (system "touch /tmp/globtest2/file3")
  ;; Test catch-alls
  (define-syntax-rule (test-glob-all stx)
    (syntax-parse stx
      [(_ strs:str ...)
       #'((check-equal? (glob strs) '("/tmp/globtest1/file1"
                                      "/tmp/globtest1/file2"
                                      "/tmp/globtest1/file3"
                                      "/tmp/globtest2/file1"
                                      "/tmp/globtest2/file2"
                                      "/tmp/globtest2/file3")) ...)]))
  (test-glob-all
    "/tmp/globtest*/file*"
    "/tmp/globtest*/file[123]"
    "/tmp/globtest[12]/file*"
    "/tmp/globtest1?2?/file1?2?3?"
    "/tmp/globtest*/*"
  )
)
