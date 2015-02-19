#lang racket/base

;; Unix-like globbing for filepaths.
;; Used to select a set of relative filepaths.

;; Recognizes the *, ?, and [] operators.
;; - * is a wildcard pattern, like "*.rkt"
;; - ? is an optional pattern, like "notes?.txt"
;; - [] specifies a character range, like "file[12345].c"

;; Terminology:
;; - "file" refers to any member of the filesystem (a file OR directory)
;; - a sextile (*) denotes a list i.e. "path*" should read as "a list of paths"

(provide
  ;; (-> String (Listof Path))
  ;; Return a list of all files/directories matching the argument glob.
  glob
  ;; (-> String (Sequenceof Path))
  ;; Same as glob, but returns an iterable instead of a list.
  in-glob)

(require racket/match
         racket/generator
         (only-in racket/list drop)
         (only-in racket/string string-join string-split))

;; --------------------------------------------------------------------------------------------------
;; --- API functions

;; (: glob (-> String (Listof Path)))
(define (glob s)
  (for/list ([fp (in-glob s)]) fp))

;; (: in-glob (-> String (Sequenceof Path)))
(define (in-glob pattern)
  (match (parse pattern)
    [#f
     (error (format "glob: Invalid pattern '~a'" pattern))]
    [(cons prefix path*)
     (glob-gen path* prefix (directory-list prefix))]))

;; -----------------------------------------------------------------------------
;; --- Helpers

(define GLOB-PATTERNS '(#\* #\? #\[ #\]))

;; True if the argument contains any glob pattern
;; (: glob? (-> String Boolean))
(define (glob? str)
  (glob?-aux #f #f (string->list str)))

;; TODO gosh I'm worried about escapes
;; TODO check for unmatched []
(define (glob?-aux prev1 prev2 char*)
  (match char*
    ;; No characters = not a glob
    ['() #f]
    ;; Character is an unescaped GLOB-PATTERN 
    [(cons c _)
     #:when (and (member c GLOB-PATTERNS)
                 (or (not (eq? prev1 #\/)) ;; Previous is not a backslash
                     (and (eq? prev1 #\/) (eq? prev2 #\/)))) ;; Previous is an escaped backslash
     #t]
    [(cons c char*)
     (glob?-aux prev2 c char*)]))

;; Try convering a pattern into a filepath prefix and a list of patterns.
;; (: parse (-> String (U #f (cons (Listof String) (Listof String)))))
(define (parse pattern)
  (define path*  (pattern->path* pattern))
  (define prefix (parse-prefix path*))
  (define posfix (drop path* (length prefix)))
  (if (validate posfix)
      (cons prefix posfix)
      #f))

;; Assert that a list of path fragments constitute a valid glob.
;; (: validate (-> (Listof String) Boolean))
(define (validate paths)
  (for/and ([p paths]) (not (member p INVALID_AFTER_GLOB))))

;; Strings that cannot appear after a glob pattern
(define INVALID_AFTER_GLOB '(".."))

;; Divide a glob pattern into filepath fragments.
;; Some fragments may be globs, others may be valid filenames.
;; (: pattern->path* (-> String (Listof String)))
(define (pattern->path* pattern)
  (string-split pattern "/"))

;; Collect the non-pattern prefix of a list of strings.
;; (: parse-prefix (-> (Listof String) (Listof String)))
(define (parse-prefix path*)
  (match path*
    ;; Non-glob
    [(cons p path*) #:when (not (glob? p))
     (cons p (parse-prefix path*))]
    ;; Empty list, or a glob pattern at head
    [(or '() (cons _ _))
     '()]))

;; Follow a glob pattern (segmented into paths) through the filesystem.
;; Generate a sequence of matches.
;; (: glob-gen (-> (Listof String) String (Listof Path) (Sequenceof Path)))
(define glob-gen (generator (path* prefix file*)
  (match (cons path* file*)
    ;; Finished searching, yield all matches
    [(cons '() _)
     (for ([f file*]) (yield f))]
    ;; Match current files with current glob patterns
    [(cons (cons p path*) file*)
     (for* ([f file*] #:when (glob-match? p f))
       (let ([prefix (append prefix (list p))])
         ;; TODO
         (glob-gen path* prefix (directory-list prefix))))]
    ;; No matching files. Die.
    [(cons _ '())
     (void)])))

;; True if the second argument matches the pattern given as the first argument.
;; (: glob-match? (-> String String Boolean))
(define (glob-match? pattern str)
  (error "TODO"))

;; Join a list of path fragments into one whole path
;; (: path*->path (-> (Listof String) Path))
(define (path*->path path*)
  (string->path (string-join path* "/")))

;; -----------------------------------------------------------------------------
;; --- Testing

;; -- Test globs for main.rkt, in the current directory
(module+ test
  (require rackunit)
  (require (for-syntax racket/base syntax/parse racket/syntax))
  ;; Assert that 'main.rkt' exists.
  (unless (file-exists? "main.rkt")
    (error "Could not find file 'main.rkt' in the current directory."))
  ;; Test successes
  ;; (define-syntax-rule (test-glob-pass stx)
  ;;   (syntax-parse stx
  ;;     [(_ strs:string ...)
  ;;      #'((check-equal? (glob strs) '("main.rkt")) ...)]))
  ;; TODO macro-ize
  (define (test-glob-pass str*)
    (for ([s str*]) (check-equal? (glob s) '("main.rkt"))))
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
  ;; (define-syntax-rule (test-glob-all stx)
  ;;   (syntax-parse stx
  ;;     [(_ strs:str ...)
  ;;      #'((check-equal? (glob strs) '("/tmp/globtest1/file1"
  ;;                                     "/tmp/globtest1/file2"
  ;;                                     "/tmp/globtest1/file3"
  ;;                                     "/tmp/globtest2/file1"
  ;;                                     "/tmp/globtest2/file2"
  ;;                                     "/tmp/globtest2/file3")) ...)]))
  ;; TODO macroize
  (define (test-glob-all str*)
    (for ([s str*]) (check-equal? (glob s) '("/tmp/globtest1/file1"
                                             "/tmp/globtest1/file2"
                                             "/tmp/globtest1/file3"
                                             "/tmp/globtest2/file1"
                                             "/tmp/globtest2/file2"
                                             "/tmp/globtest2/file3"))))
  (test-glob-all
    "/tmp/globtest*/file*"
    "/tmp/globtest*/file[123]"
    "/tmp/globtest[12]/file*"
    "/tmp/globtest1?2?/file1?2?3?"
    "/tmp/globtest*/*"
  )
)
