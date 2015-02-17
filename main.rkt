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

(module+ test
  ;; TODO tests should use shell for expected output
)
