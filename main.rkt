#lang racket/base

;; Unix-like globbing for filepaths.
;; Used to select a set of relative filepaths.

;; Recognizes the *, ?, and [] operators.
;; - * is a wildcard pattern, like "*.rkt"
;; - ? is an optional pattern, like "notes?.txt"
;; - [] specifies a character range, like "file[12345].c"

;; Terms / Conventions:
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
         (only-in racket/list drop empty?)
         (only-in racket/string string-join string-split))

;; --------------------------------------------------------------------------------------------------
;; --- API functions

;; (: glob (-> String (Listof Path-String)))
(define (glob s)
  (for/list ([fp (in-glob s)]) fp))

;; (: in-glob (-> String (Sequenceof Path-String)))
(define (in-glob pattern)
  (match (parse pattern)
    [#f
     (error (format "glob: Invalid pattern '~a'" pattern))]
    [(cons prefix path*)
     (glob-gen path* prefix (directory-list prefix))]))

;; -----------------------------------------------------------------------------
;; --- Helpers

;; -- Constants

;; (: GLOB-PATTERNS (Listof Char))
(define GLOB-PATTERNS '(#\* #\? #\[ #\]))

;; (: REGEXP-CHARS (Listof Char))
(define REGEXP-CHARS '(#\. #\( #\) #\| #\+ #\$ #\^))

;; Strings that cannot appear after a glob pattern
(define INVALID_AFTER_GLOB '(".."))

;; -- Glob parsing

;; Try converting a pattern into a filepath prefix and a list of patterns.
;; (: parse (-> String (U #f (cons (Listof String) (Listof String)))))
(define (parse pattern)
  (define path*  (pattern->path* pattern))
  (define prefix (parse-prefix path*))
  (define posfix (drop path* (length prefix)))
  (if (validate posfix)
      (cons prefix posfix)
      #f))

;; Return the prefix sequence of non-glob elements in `path*`
;; (Stop iteration as soon as we see a glob and return the collected sequence.)
;; (: parse-prefix (-> (Listof String) (Listof String)))
(define (parse-prefix path*)
  (match path*
    ;; Non-glob
    [(cons p path*)
     #:when (not (glob? p))
     (cons p (parse-prefix path*))]
    ;; Empty list, or a glob pattern at head
    [(or '() (cons _ _))
     '()]))

;; Assert that a list of path fragments constitute a valid glob.
;; 2015-03-28: So far as I know, only the string ".." is invalid
;; (: validate (-> (Listof String) Boolean))
(define (validate paths)
  (for/and ([p paths]) (not (member p INVALID_AFTER_GLOB))))

;; -- Glob matching

;; Follow a glob pattern (segmented into paths) through the filesystem.
;; Generate a sequence of matches.
;; (: glob-gen (-> (Listof String) Path-String (Listof Path) (Sequenceof Path)))
(define (glob-gen path* prefix file*) (generator ()
  (match (cons path* file*)
    ;; Finished searching, yield all matches
    [(cons '() _)
     (for ([f file*]) (yield (string-append prefix (path->string f))))
     (yield (void))]
    ;; No matching files. Die.
    [(cons _ '())
     (yield (void))]
    ;; Match current files with current glob patterns,
    ;; spawn a recursive search for each match.
    [(cons (cons pattern path*) file*)
     (for ([file (in-list (glob-filter pattern file*))]
           ;; Check that no patterns left, or the matched `file` is a directory
           #:when (or (empty? path*) ;; No patterns left
                      (directory-exists? (string-append prefix (path->string file)))))
        (let* ([new-prefix (string-append prefix (path->string file))]
               [all (glob-gen path* new-prefix (directory-list new-prefix))])
          (for ([result (in-producer all)]) (yield result))))])))

;; Return all members of `file*` that match the glob `pattern`.
;; (: glob-filter (-> String (Listof Path) (Listof Path-String)))
(define (glob-filter pattern file*)
  (define rx (glob->regexp pattern))
  (for/list ([file (in-list file*)]
             #:when (regexp-match rx (path->string file)))
    file))

;; Compile a glob pattern to a regular expression.
;; (: glob->regexp (-> String String))
(define (glob->regexp str)
  (define body
    (for/list ([i (in-range (string-length str))])
      (cond [(escaped? (safe-string-ref str (- i 2)) (safe-string-ref str (sub1 i)))
             ;; Character is escaped, keep it
             (string (string-ref str i))]
            [(eq? (string-ref str i) #\*)
             ;; Convert glob * to regex .*
             ".*"]
            [(member (string-ref str i) REGEXP-CHARS)
             ;; Escape characters that the regexp might interpret
             (string #\\ (string-ref str i))]
            [else
             ;; Keep everything else
             (string (string-ref str i))])))
  (format "^~a$" (apply string-append body)))

;; -- utils

;; True if the supplied `prev1` and `prev2` are an escape.
;; i.e., `prev2` must be a slash and `prev1` must NOT be a slash
;; (: escaped? (-> Char Char Boolean))
(define (escaped? prev2 prev1)
  (and (eq? prev1 #\\)
       (not (eq? prev2 #\\))))

;; True if the argument contains any glob pattern
;; (: glob? (-> String Boolean))
(define (glob? str)
  (for/or ([i (in-range (string-length str))])
    (and (member (string-ref str i) GLOB-PATTERNS)
         (not (escaped? (safe-string-ref str (- i 2)) (safe-string-ref str (- i 1)))))))

;; Divide a glob pattern into filepath fragments.
;; Some fragments may be globs, others may be valid filenames.
;; (: pattern->path* (-> String (Listof String)))
(define (pattern->path* pattern)
  (append
    ;; If the string begins with a /, keep it
    (if (eq? #\/ (safe-string-ref pattern 0)) '("/") '())
    ;; Filter "" and "." from the split string
    (for/list ([path (in-list (string-split pattern "/"))]
               #:when (and (< 0 (string-length path))
                           (not (equal? "." path))))
      path)))

;; Access the i-th position of `str`, return #f if
;; the reference is out of bounds.
;; (: safe-get (-> String Natural (U #f Char)))
(define (safe-string-ref str i)
  (and (<= 0 i)
       (< i (string-length str))
       (string-ref str i)))

;; -----------------------------------------------------------------------------
;; --- Testing

;; -- Test globs for main.rkt, in the current directory
(module+ test
  ;; Test libs
  (require rackunit)
  (require (for-syntax racket/base syntax/parse racket/syntax))
  ;(define-syntax-rule (raw-string char ...)
  ;  #'(string '#'\char ...))
)

(module+ test
  ;; Test utils
  ;; -- escaped?
  (check-false (escaped? #\a #\b))
  (check-false (escaped? #\2 #\/))
  (check-false (escaped? #\O #\r))
  (check-false (escaped? #\\ #\\))
  (check-false (escaped? #\\ #\q))
  (check-true (escaped? #\a #\\))
  (check-true (escaped? #\2 #\\))
  (check-true (escaped? #\O #\\))
  (check-true (escaped? #\& #\\))
  (check-true (escaped? #\! #\\))
  ;; -- glob?
  (check-true (glob? "*"))
  (check-true (glob? "?"))
  (check-true (glob? "["))
  (check-true (glob? "]"))
  (check-true (glob? "[]"))
  (check-true (glob? "]["))
  (check-true (glob? "***"))
  (check-true (glob? "foo?bar*"))
  (check-true (glob? "???*"))
  (check-true (glob? "ar[gh]r*"))
  (check-true (glob? "*o?bar*"))
  (check-true (glob? "?foo][bar*"))
  (check-true (glob? (string #\\ #\\ #\*)))
  (check-false (glob? "asdf"))
  (check-false (glob? "not-a-glob"))
  (check-false (glob? (string #\l #\i #\t #\\ #\* #\s #\t #\a #\r)))
  (check-false (glob? (string #\\ #\? #\\ #\?)))
  (check-false (glob? (string #\\ #\[ #\\ #\] #\h #\a #\h)))
  (check-false (glob? ""))
  ;; -- pattern->path
  (check-equal? (pattern->path* "foo") (list "foo"))
  (check-equal? (pattern->path* "/") (list "/"))
  (check-equal? (pattern->path* ".") (list ))
  (check-equal? (pattern->path* "") (list ))
  (check-equal? (pattern->path* "foo/bar/baz/") (list "foo" "bar" "baz"))
  (check-equal? (pattern->path* "foo/bar//baz/") (list "foo" "bar" "baz"))
  (check-equal? (pattern->path* "/foo/../") (list "/" "foo" ".."))
  (check-equal? (pattern->path* "./foo/bar") (list "foo" "bar"))
  (check-equal? (pattern->path* "~/foo/bar") (list "~" "foo" "bar"))
  ;; -- safe-string-ref
  (check-equal? (safe-string-ref "" 0) #f)
  (check-equal? (safe-string-ref "" 1) #f)
  (check-equal? (safe-string-ref "" -1) #f)
  (check-equal? (safe-string-ref "abcd" 0) #\a)
  (check-equal? (safe-string-ref "abcd" 2) #\c)
  (check-equal? (safe-string-ref "abcd" 3) #\d)
  (check-equal? (safe-string-ref "abcd" 4) #f)
)

(module+ test
  ;; Generator tests
  ;; -- glob->regexp
  (check-equal? (glob->regexp "foobar") "^foobar$")
  (check-equal? (glob->regexp ".") "^\\.$")
  (check-equal? (glob->regexp "*") "^.*$")
  (check-equal? (glob->regexp "foo*.txt") "^foo.*\\.txt$")
  (check-equal? (glob->regexp "(hello world)") "^\\(hello world\\)$")
  (check-equal? (glob->regexp "^foo|bar$") "^\\^foo\\|bar\\$$")
  (check-equal? (glob->regexp "things?") "^things?$")
  (check-equal? (glob->regexp "thang[sies]") "^thang[sies]$")
  (check-equal? (glob->regexp ".?.?.?") "^\\.?\\.?\\.?$")
  ;; -- glob-filter
  (define-syntax-rule (make-paths str ...)
    (list (string->path str) ...))
  (check-equal? (glob-filter "foo" '()) '())
  (check-equal? (glob-filter "foo" (make-paths "foo")) (make-paths "foo"))
  (check-equal? (glob-filter "foo" (make-paths "qux" "foo" "bar")) (make-paths "foo"))
  (check-equal? (glob-filter "*" (make-paths "cat" "dog" "goa")) (make-paths "cat" "dog" "goa"))
  (check-equal? (glob-filter "*.txt" (make-paths "file.txt" "sheet.txt" "work.jar" "play.tab")) (make-paths "file.txt" "sheet.txt"))
  (check-equal? (glob-filter "cat?" (make-paths "ca" "car" "cat")) (make-paths "ca" "cat"))
  (check-equal? (glob-filter "cat?at" (make-paths "ca" "car" "catat" "caat")) (make-paths "catat" "caat"))
  (check-equal? (glob-filter ".?.?.?" (make-paths "a" "ab" "abc" "abcd")) '())
  (check-equal? (glob-filter ".?.?.?" (make-paths "." ".." "..." "....")) (make-paths "." ".." "..."))
  ;; -- glob-gen
  (define (gen->list g) (for/list ([x (in-producer g (void))]) x))
  (check-equal? (gen->list (glob-gen '() "" '())) '())
  (check-equal? (gen->list (glob-gen (list "cat") "" '())) '())
  (check-equal? (gen->list (glob-gen '() "" (make-paths "cat"))) (list "cat"))
  (check-equal? (gen->list (glob-gen '() "with-prefix" (make-paths "cat"))) (list "with-prefixcat"))
  ;; TODO more tests
;  (check-equal? (gen->list (glob-gen (list "foo") "" (make-paths "asdf" "woof" "foosh" "foo"))) (list "foo"))
;  (check-equal? (gen->list (glob-gen (list "foo") "" (make-paths "asdf" "woof" "foosh" "foo"))) (list "foo"))
)

(module+ test
  ;; Parsing tests
)

;;(module+ test
;;  ;; Simple, end-to-end test
;;  (check-equal? '() (glob "foo")))

;;  ;; Assert that 'main.rkt' exists.
;;  (unless (file-exists? "main.rkt")
;;    (error "Could not find file 'main.rkt' in the current directory."))
;;  ;; Test successes
;;  ;; (define-syntax-rule (test-glob-pass stx)
;;  ;;   (syntax-parse stx
;;  ;;     [(_ strs:string ...)
;;  ;;      #'((check-equal? (glob strs) '("main.rkt")) ...)]))
;;  ;; TODO macro-ize
;;  (define (test-glob-pass str*)
;;    (for ([s str*]) (check-equal? (glob s) '("main.rkt"))))
;;  (test-glob-pass
;;    "main.rkt"
;;    ;; * patterns
;;    "main*"
;;    "*.rkt"
;;    "m*n.rkt"
;;    "*ain*"
;;    "*in*t"
;;    "m*a*i*n*.*r*k*t*"
;;    "mai*kt"
;;    ;; ? patterns
;;    "m?ain.rkt"
;;    "z?main.rkt"
;;    "mm?ain.rkt"
;;    "m?ainx?.rkt?x?"
;;    "main.rkt"
;;    "main.rkt"
;;    ;; [] patterns
;;    "[m][a][i]n.rkt"
;;    "main.rk[tttt]"
;;    "[abcm]ain.rkt"
;;    "[ma][ma]in.rkt"
;;    "[ma][ma]in.[xr]kt"
;;    ;; * ?
;;    ;; * []
;;    "[m][a][i]n.r*"
;;    ;; ? []
;;    "[xyz]?main.rkt"
;;    ;; * ? []
;;    "[xyz]?main*[x]?rkt"
;;  )

  ;; TODO tests should use shell for expected output

;(module+ test
;  (require racket/system)
;  ;; Create files
;  (system "mkdir /tmp/globtest1")
;  (system "mkdir /tmp/globtest2")
;  (system "touch /tmp/globtest1/file1")
;  (system "touch /tmp/globtest1/file2")
;  (system "touch /tmp/globtest1/file3")
;  (system "touch /tmp/globtest2/file1")
;  (system "touch /tmp/globtest2/file2")
;  (system "touch /tmp/globtest2/file3")
;  ;; Test catch-alls
;  ;; (define-syntax-rule (test-glob-all stx)
;  ;;   (syntax-parse stx
;  ;;     [(_ strs:str ...)
;  ;;      #'((check-equal? (glob strs) '("/tmp/globtest1/file1"
;  ;;                                     "/tmp/globtest1/file2"
;  ;;                                     "/tmp/globtest1/file3"
;  ;;                                     "/tmp/globtest2/file1"
;  ;;                                     "/tmp/globtest2/file2"
;  ;;                                     "/tmp/globtest2/file3")) ...)]))
;  ;; TODO macroize
;  (define (test-glob-all str*)
;    (for ([s str*]) (check-equal? (glob s) '("/tmp/globtest1/file1"
;                                             "/tmp/globtest1/file2"
;                                             "/tmp/globtest1/file3"
;                                             "/tmp/globtest2/file1"
;                                             "/tmp/globtest2/file2"
;                                             "/tmp/globtest2/file3"))))
;  (test-glob-all
;    "/tmp/globtest*/file*"
;    "/tmp/globtest*/file[123]"
;    "/tmp/globtest[12]/file*"
;    "/tmp/globtest1?2?/file1?2?3?"
;    "/tmp/globtest*/*"
;  )
