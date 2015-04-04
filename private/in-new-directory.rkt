#lang racket/base

;; Original author: Dave Herman
;; Got file from: https://github.com/samth/in-new-directory
;; Modified: 2015-04-03

;; Defines syntax (in-new-directory dir expr ...)
;; For executing each `expr ...` with the `current-directory` parameter
;; bound to `dir`.

(require racket/file (for-syntax racket/base))

(define (rm-rf path)
 (when (or (file-exists? path) (directory-exists? path))
  (delete-directory/files path)))

(define-syntax in-new-directory
 (syntax-rules ()
  [(_ dir-e e1 e2 ...)
  (let ([dir dir-e])
   (dynamic-wind
    void
    (lambda ()
     (when (directory-exists? dir)
      (error 'in-new-directory "can't create directory ~a; directory exists" dir))
     (make-directory* dir)
     (parameterize ([current-directory dir])
      e1 e2 ...))
    (lambda ()
      (rm-rf dir))))]))

(provide
 in-new-directory)
