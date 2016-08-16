#lang racket/base

;; Unix-style globbing for filepaths.
;; Used to select a set of relative filepaths.

;; Recognizes the *, ?, and [] operators.
;; - * is a wildcard pattern, like "*.rkt"
;; - ? is an optional pattern, like "notes?.txt"
;; - [] specifies a character range, like "file[12345].c"

;; Terms / Conventions:
;; - "file" refers to any member of the filesystem (a file OR directory)
;; - a sextile (*) denotes a list i.e. "path*" should read as "a list of paths"

(provide
  glob
  ;; (->* (String) (#:with-dotfiles? Boolean) (Listof Path))
  ;; Return a list of all files/directories matching the argument glob.

  in-glob
  ;; (->* (String) (#:with-dotfiles? Boolean) (Sequenceof Path))
  ;; Same as glob, but returns an iterable instead of a list.

  glob-match?
  ;; (-> String Path-String Boolean)
  ;; Returns #t if second argument would be a result captured by the first argument.
  ;; Analogous to `regexp-match?`
)

(require
  racket/match
  racket/generator
  (only-in racket/list drop)
  (only-in racket/string string-join string-split))

;; --------------------------------------------------------------------------------------------------
;; --- API functions

;; (: glob (->* (String) (#:with-dotfiles? Boolean) (Listof Path-String)))
(define (glob s #:with-dotfiles? [dotfiles? #f])
  (for/list ([fp (in-glob s #:with-dotfiles? dotfiles?)]) fp))

;; (: in-glob (->* (String) (#:with-dotfiles? Boolean) (Sequenceof Path-String)))
(define (in-glob pattern #:with-dotfiles? [dotfiles? #f])
  (match (parse pattern)
    [#f
     (error 'glob (format "Invalid pattern '~e'" pattern))]
    ;; No globs, just make sure prefix is valid
    [(cons prefix '())
     #:when (or (file-exists? prefix) (directory-exists? prefix))
     (in-list (list prefix))]
    ;; Multiple globs, recurse on the files within `prefix` directory
    [(cons prefix path*)
     #:when (directory-exists? prefix)
     (in-producer (glob-gen path* prefix (directory-list prefix) dotfiles?) (void))]
    ;; Fallback: file / directory does not exist
    [_
     (in-list '())]))

;; (: glob-match? (-> String Path-String Boolean))
(define (glob-match? pattern ps)
  (and (or (file-exists? ps)
           (directory-exists? ps))
       (let ([simpl (simplify-path ps)])
         (let loop ([pat* (pattern->path* pattern)]
                    [path* (pattern->path* (path->string simpl))])
           (or
            ;; -- base case
            (and (null? pat*) (null? path*))
            ;; -- check first elem, recurse
            (and (not (null? pat*))
                 (not (null? path*))
                 (regexp-match? (glob->regexp (car pat*)) (car path*))
                 (loop (cdr pat*) (cdr path*))))))))

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
      (cons (path-concat prefix) posfix)
      #f))

;; Return the prefix sequence of non-glob elements in `path*`
;; (Stop iteration as soon as we see a glob and return the collected sequence.)
;; (: parse-prefix (-> (Listof String) (Listof String)))
(define (parse-prefix path* #:expand-home-dir? [expand? #t])
  (match path*
    ;; Expand ~, if it's the first thing
    [(cons "~" path*)
     #:when expand?
     ;; Get home directory, convert to string, strip trailing /
     (let* ([home (path->string (find-system-path 'home-dir))])
       (cons home (parse-prefix path* #:expand-home-dir? #f)))]
    ;; Non-glob
    [(cons p path*)
     #:when (not (glob? p))
     (cons p (parse-prefix path* #:expand-home-dir? #f))]
    ;; Empty list, or a glob pattern at head
    [(or '() (cons _ _))
     '()]))

;; Assert that each fragment in a path constitutes a valid glob.
;; 2015-03-28: So far as I know, only the string ".." is invalid
;; (: validate (-> (Listof String) Boolean))
(define (validate paths)
  (for/and ([p paths]) (not (member p INVALID_AFTER_GLOB))))

;; -- Glob matching

;; Follow a glob pattern (segmented into paths) through the filesystem.
;; Generate a sequence of matches.
;; (: glob-gen (-> (Listof String) Path-String (Listof Path) Boolean (Sequenceof Path)))
(define (glob-gen path* prefix file* dotfiles?) (generator ()
  (match (cons path* file*)
    ;; Finished searching, yield all matches
    [(cons '() _)
     (for ([f file*]) (yield (path-concat (list prefix (path->string f)))))
     (yield (void))]
    ;; No matching files. Die.
    [(cons _ '())
     (yield (void))]
    ;; Match current files with current glob patterns,
    ;; spawn a recursive search for each match.
    [(cons (cons pattern path*) file*)
     (for ([file (in-list (glob-filter pattern file* dotfiles?))]
           ;; Check that no patterns left, or the matched `file` is a directory
           #:when (or (null? path*) ;; No patterns left
                      (directory-exists? (path-concat (list prefix (path->string file))))))
        (let* ([new-prefix (path-concat (list prefix (path->string file)))])
          (cond [(null? path*) (yield new-prefix)]
                [else ;; Yield all recursive results
                 (for ([result (in-producer (glob-gen path* new-prefix (directory-list new-prefix) dotfiles?) (void))])
                   (yield result))])))
     (yield (void))])))

;; Return all members of `file*` that match the glob `pattern`.
;; Unless `dotfiles?` is set, filters matches that begin with a '.'
;; (: glob-filter (-> String (Listof Path) Boolean (Listof Path-String)))
(define (glob-filter pattern file* dotfiles?)
  (define rx (glob->regexp pattern))
  (for/list ([file (in-list file*)]
             #:when (regexp-match? rx (path->string file))
             #:when (or dotfiles?
                        (eq? #\. (safe-string-ref pattern 0))
                        (not (eq? #\. (safe-string-ref (path->string file) 0)))))
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

;; Concat a list of strings into a filepath
(define (path-concat path*)
  (cond
    [(null? path*) "."]
    [else
      (define non-empty*
        (for/list ([path path*]
                   #:when (< 0 (string-length path)))
          (if (equal? path "/") "" path)))
      (string-join non-empty* "/")]))

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
  ;; -- Test libs
  (require
    rackunit
    (only-in racket/syntax generate-temporary)
    (only-in racket/file delete-directory/files display-to-file)
    "in-new-directory.rkt")

  ;; -- Test utils
  (define-syntax-rule (make-paths str ...)
    (list (string->path str) ...))

  ;; Find a unique tmp/ directory name, create a new directory
  (define (gen-tmp-dirname tag)
    (define dirname (string-append (path->string (find-system-path 'temp-dir)) "/" tag))
    (if (directory-exists? dirname)
      (gen-tmp-dirname (symbol->string (syntax->datum (generate-temporary tag))))
      dirname))

  (define (touch-file fname)
    (display-to-file "" fname #:exists 'error))

  (define (prefix-all str xs)
    (for/list ([x xs]) (string-append str x)))

  (define (check-glob-match? g)
    (or (for/fold ([any? #f])
                  ([p (in-glob g)])
          (check-true (glob-match? g p) (format "glob '~a' should match ~a" g p))
          #t)
        (check-false (glob-match? g g) (format "glob '~a' should not match ~a" g g))))

  ;; -----------------------------------------------------------------------------
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
  (check-equal? (pattern->path* "/") (list "/"))
  (check-equal? (pattern->path* "//") (list "/"))
  (check-equal? (pattern->path* "///") (list "/"))
  (check-equal? (pattern->path* "/a/b/c") (list "/" "a" "b" "c"))
  ;; -- path-concat
  (check-equal? (path-concat '()) ".")
  (check-equal? (path-concat (list "a" "b" "c")) "a/b/c")
  (check-equal? (path-concat (list "" "foo")) "foo")
  (check-equal? (path-concat (list "" "foo" "")) "foo")
  (check-equal? (path-concat (list "yes" "." "" "no")) "yes/./no")
  (check-equal? (path-concat (list "//" ".." "/")) "///../")
  (check-equal? (path-concat (list "/" "a" "b" "c")) "/a/b/c")
  ;; -- safe-string-ref
  (check-equal? (safe-string-ref "" 0) #f)
  (check-equal? (safe-string-ref "" 1) #f)
  (check-equal? (safe-string-ref "" -1) #f)
  (check-equal? (safe-string-ref "abcd" 0) #\a)
  (check-equal? (safe-string-ref "abcd" 2) #\c)
  (check-equal? (safe-string-ref "abcd" 3) #\d)
  (check-equal? (safe-string-ref "abcd" 4) #f)

  ;; -----------------------------------------------------------------------------
  ;; Generator tests
  ;; -- glob->regexp
  (check-equal? (glob->regexp "foobar") "^foobar$")
  (check-equal? (glob->regexp ".") "^\\.$")
  (check-equal? (glob->regexp "*") "^.*$")
  (check-equal? (glob->regexp "foo*.txt") "^foo.*\\.txt$")
  (check-equal? (glob->regexp "(hello world)") "^\\(hello world\\)$")
  (check-equal? (glob->regexp "^foo|bar$") "^\\^foo\\|bar\\$$")
  (check-equal? (glob->regexp "things?") "^things?$")
  (check-equal? (glob->regexp "\tescaped\\things\n?") "^\tescaped\\things\n?$")
  (check-equal? (glob->regexp "thang[sies]") "^thang[sies]$")
  (check-equal? (glob->regexp ".?.?.?") "^\\.?\\.?\\.?$")
  ;; -- glob-filter
  (check-equal? (glob-filter "foo" '() #f) '())
  (check-equal? (glob-filter "foo" (make-paths "foo") #f) (make-paths "foo"))
  (check-equal? (glob-filter "*" (make-paths "foo" ".foo") #f) (make-paths "foo"))
  (check-equal? (glob-filter "*" (make-paths "foo" ".foo") #t) (make-paths "foo" ".foo"))
  (check-equal? (glob-filter "foo" (make-paths "qux" "foo" "bar") #f) (make-paths "foo"))
  (check-equal? (glob-filter "*" (make-paths "cat" "dog" "goa") #f) (make-paths "cat" "dog" "goa"))
  (check-equal? (glob-filter "*.txt" (make-paths "file.txt" "sheet.txt" "work.jar" "play.tab") #f) (make-paths "file.txt" "sheet.txt"))
  (check-equal? (glob-filter "cat?" (make-paths "ca" "car" "cat") #f) (make-paths "ca" "cat"))
  (check-equal? (glob-filter "cat?at" (make-paths "ca" "car" "catat" "caat") #f) (make-paths "catat" "caat"))
  (check-equal? (glob-filter ".?.?.?" (make-paths "a" "ab" "abc" "abcd") #f) '())
  ;; Literal dots escape the filter
  (check-equal? (glob-filter ".?.?.?" (make-paths "." ".." "..." "....") #f) (make-paths "." ".." "..."))
  (check-equal? (glob-filter ".?.?.?" (make-paths "." ".." "..." "....") #t) (make-paths "." ".." "..."))
  ;; -- glob-gen
  (define (gen->list g) (for/list ([x (in-producer g (void))]) x))
  (check-equal? (gen->list (glob-gen '() "" '() #f)) '())
  (check-equal? (gen->list (glob-gen (list "cat") "" '() #f)) '())
  (check-equal? (gen->list (glob-gen '() "" (make-paths "cat") #f)) (list "cat"))
  (check-equal? (gen->list (glob-gen '() "with-prefix" (make-paths "cat") #f)) (list "with-prefix/cat"))
  (check-equal? (gen->list (glob-gen (list "foo") "" (make-paths "asdf" "woof" "foosh" "foo") #f)) (list "foo"))
  (check-equal? (gen->list (glob-gen (list "foo") "./" (make-paths "asdf" "woof" "foosh" "foo") #f)) (list ".//foo"))
  (check-equal? (gen->list (glob-gen (list "foo*") "" (make-paths "asdf" "woof" "foosh" "foo") #f)) (list "foosh" "foo"))
  (check-equal? (gen->list (glob-gen (list "*oo*") "" (make-paths "asdf" "woof" "foosh" "foo") #f)) (list "woof" "foosh" "foo"))
  (check-equal? (gen->list (glob-gen (list "*oo*") "" (make-paths ".asdf" ".woof" ".foosh" "foo") #f)) (list "foo"))
  (check-equal? (gen->list (glob-gen (list "*oo*") "" (make-paths ".asdf" ".woof" ".foosh" "foo") #t)) (list ".woof" ".foosh" "foo"))

  ;; -----------------------------------------------------------------------------
  ;; Make some files & directories
  ;; Run tests
  (in-new-directory (gen-tmp-dirname "glob")
    (define cwd (path->string (current-directory)))
    (check-equal?
      (gen->list (glob-gen (list "*") cwd (make-paths "baz" "zap") #f))
      (prefix-all cwd (list "/baz" "/zap")))
    (check-equal? (gen->list (glob-gen (list "*") "" (make-paths cwd) #f)) (list cwd))
    (check-equal? (gen->list (glob-gen (list "") "" (make-paths cwd) #f)) (list))
    ; Fail, no files exist yet
    (check-equal? (gen->list (glob-gen (list "*" "foo.txt") "" (make-paths cwd) #f)) (list))
    ; Create one file
    (touch-file (string-append cwd "/foo.txt"))
    (check-equal? (gen->list (glob-gen (list "*" "foo.txt") "" (make-paths cwd) #f)) (prefix-all cwd (list "/foo.txt")))
    (check-equal? (gen->list (glob-gen (list "*" "*") "" (make-paths cwd) #f)) (prefix-all cwd (list "/foo.txt")))
    ; Add a few more files
    (touch-file (string-append cwd "/bar.txt"))
    (touch-file (string-append cwd "/baz.txt"))
    (touch-file (string-append cwd "/qux.txt"))
    (check-equal? (gen->list (glob-gen (list "*" "*.txt") "" (make-paths cwd) #f)) (prefix-all cwd (list "/bar.txt" "/baz.txt" "/foo.txt" "/qux.txt")))
    (check-equal? (gen->list (glob-gen (list "*" "b*.txt") "" (make-paths cwd) #f)) (prefix-all cwd (list "/bar.txt" "/baz.txt")))
    ; Fail, no directories
    (check-equal? (gen->list (glob-gen (list "*" "*" "*") "" (make-paths cwd) #f)) (list ))
    (make-directory (string-append cwd "/dir"))
    ; Still finds nothing, no files
    (check-equal? (gen->list (glob-gen (list "*" "*" "*") "" (make-paths cwd) #f)) (list ))
    (touch-file (string-append cwd "/dir" "/red.c"))
    (check-equal? (gen->list (glob-gen (list "*" "*" "*") "" (make-paths cwd) #f)) (prefix-all cwd (list "/dir/red.c")))
    (touch-file (string-append cwd "/dir" "/blue.c"))
    (check-equal? (gen->list (glob-gen (list "*" "*" "*.c") "" (make-paths cwd) #f)) (prefix-all cwd (list "/dir/blue.c" "/dir/red.c")))
    (touch-file (string-append cwd "/dir" "/red.co"))
    (check-equal? (gen->list (glob-gen (list "*" "*" "*.co") "" (make-paths cwd) #f)) (prefix-all cwd (list "/dir/red.co")))
    (check-equal? (gen->list (glob-gen (list "*" "*" "*.co?") "" (make-paths cwd) #f)) (prefix-all cwd (list "/dir/blue.c" "/dir/red.c" "/dir/red.co")))
  )
  ;; -----------------------------------------------------------------------------
  ;; Parsing tests
  ;; -- validate
  (check-true (validate (list)))
  (check-true (validate (list "a" "b" "c" "d")))
  (check-true (validate (list "///" "/" "." "asdfadf")))
  (check-true (validate (list "[asdf]" "[a][b][d]" "[[[]]]" "][[[][]" "a][b]")))
  (check-true (validate (list "?" "??" "???" "*?*[}.a")))
  (check-false (validate (list "..")))
  (check-false (validate (list " " "..")))
  (check-false (validate (list "a" "b" "c" "d" ".." "e")))
  ;; -- parse-prefix
  (check-equal? (parse-prefix (list)) '())
  (check-equal? (parse-prefix (list "/")) (list "/"))
  (check-equal? (parse-prefix (list "/" "/")) (list "/" "/"))
  (check-equal? (parse-prefix (list "//")) (list "//"))
  (check-equal? (parse-prefix (list "a" "b")) (list "a" "b"))
  (check-equal? (parse-prefix (list "*" "a" "b")) (list))
  (check-equal? (parse-prefix (list "yes" "yes" "?" "no")) (list "yes" "yes"))
  (check-equal? (parse-prefix (list "../" ".." "." ".//" "haha[ha?]")) (list "../" ".." "." ".//"))
  (check-equal? (parse-prefix (list "foobar" "baz" "quu?x")) (list "foobar" "baz"))
  ;; home directory
  (check-equal? (parse-prefix (list "~")) (list (path->string (find-system-path 'home-dir))))
  (check-equal? (parse-prefix (list "~") #:expand-home-dir? #f) (list "~"))
  (check-equal? (parse-prefix (list "~" "foo")) (list (path->string (find-system-path 'home-dir)) "foo"))
  (check-equal? (parse-prefix (list "~" "~")) (list (path->string (find-system-path 'home-dir)) "~"))
  (check-equal? (parse-prefix (list "~" "~") #:expand-home-dir? #f) (list "~" "~"))
  ;; -- parse
  (check-equal? (parse "") (cons "." '()))
  (check-equal? (parse "/a/b/c") (cons "/a/b/c" '()))
  (check-equal? (parse "/////a") (cons "/a" '()))
  (check-equal? (parse "/a/b/c?") (cons "/a/b" (list "c?")))
  (check-equal? (parse "/a/b/c?/d/e") (cons "/a/b" (list "c?" "d" "e")))
  (check-equal? (parse "/a/b/c?/../e") #f)
  (check-equal? (parse "~/foo.txt") (cons (string-append (path->string (find-system-path 'home-dir)) "/foo.txt") '()))
  (check-equal? (parse "~/foo/bar/baz.md") (cons (string-append (path->string (find-system-path 'home-dir)) "/foo/bar/baz.md") '()))
  (check-equal? (parse "~/foo/bar?/baz.md") (cons (string-append (path->string (find-system-path 'home-dir)) "/foo") (list "bar?" "baz.md")))


  ;; -----------------------------------------------------------------------------
  ;; End-to-end tests
  (in-new-directory (gen-tmp-dirname "glob")
    (define cwd (path->string (current-directory)))
    (define (tmp-file fname) (string-append cwd fname))

    (let ([no-match* (map tmp-file '("/tmp-dir2/foo" "foo" "*" ".?" "[]"))])
      (for ([no-match (in-list no-match*)])
        (check-equal? (glob no-match) '())
        (check-glob-match? no-match)))

    ;; -- Just ONE match
    (let* ([fname (tmp-file "main.rkt")]
           [_ (touch-file fname)]
           [check-equal-fname? (lambda (g)
                                 (check-equal? (glob g) (list fname))
                                 (check-glob-match? g))])
      ; Simple patterns
      (check-equal? (glob (tmp-file "notmain.rkt")) '())
      (check-equal-fname? fname)
      ; * patterns
      (check-equal-fname? fname)
      (check-equal-fname? (tmp-file "main*"))
      (check-equal-fname? (tmp-file "*.rkt"))
      (check-equal-fname? (tmp-file "*rkt"))
      (check-equal-fname? (tmp-file "m*n.rkt"))
      (check-equal-fname? (tmp-file "*ain*"))
      (check-equal-fname? (tmp-file "*in*t"))
      (check-equal-fname? (tmp-file "m*a*i*n*.*r*k*t"))
      (check-equal-fname? (tmp-file "mai*kt"))
      ; ? patterns
      (check-equal-fname? (tmp-file "m?ain.rkt"))
      (check-equal-fname? (tmp-file "z?main.rkt"))
      (check-equal-fname? (tmp-file "m?main.rkt"))
      (check-equal-fname? (tmp-file "m?ainx?.rkt?x?"))
      ; [] patterns
      (check-equal-fname? (tmp-file "[m][a][i]n.rkt"))
      (check-equal-fname? (tmp-file "main.rk[tttt]"))
      (check-equal-fname? (tmp-file "[abcm]ain.rkt"))
      (check-equal-fname? (tmp-file "[ma][ma]in.rkt"))
      (check-equal-fname? (tmp-file "[ma][ma]in.[xr]kt"))
      ; misc
      (check-equal-fname? (tmp-file "[m][a][i]n.r*"))
      (check-equal-fname? (tmp-file "[xyz]?main.rkt"))
      (check-equal-fname? (tmp-file "[xyz]?main*[x]?rkt"))
    )

    ;; -- More than one match
    (make-directory (tmp-file "test1"))
    (make-directory (tmp-file "test2"))
    (touch-file (tmp-file "test1/file1"))
    (touch-file (tmp-file "test1/file2"))
    (touch-file (tmp-file "test1/file3"))
    (touch-file (tmp-file "test2/file1"))
    (touch-file (tmp-file "test2/file2"))
    (touch-file (tmp-file "test2/file3"))

    (define all-files (map tmp-file (list "test1/file1" "test1/file2" "test1/file3" "test2/file1" "test2/file2" "test2/file3")))

    (define (check-equal-all-files? g)
      (check-equal? (glob g) all-files)
      (check-glob-match? g))

    (let ([g (tmp-file "*")])
      (check-equal? (glob g) (list (tmp-file "main.rkt") (tmp-file "test1") (tmp-file "test2")))
      (check-glob-match? g))

    (check-equal-all-files? (tmp-file "*/*"))
    (check-equal-all-files? (tmp-file "test*/*"))
    (check-equal-all-files? (tmp-file "test[12]/*"))
    (check-equal-all-files? (tmp-file "test1?2?/*"))
    (check-equal-all-files? (tmp-file "*/file*"))
    (check-equal-all-files? (tmp-file "*/**file**"))
    (check-equal-all-files? (tmp-file "**/file**"))
    (check-equal-all-files? (tmp-file "*/file[123]"))
    (check-equal-all-files? (tmp-file "test[12]/file[123]"))
    (check-equal-all-files? (tmp-file "m?test[12]/file[123]"))
    (check-equal-all-files? (tmp-file "test1?2?/file1?2?3?"))

    (let ([g (tmp-file "test1/file*")])
      (check-equal? (glob g) (list (tmp-file "test1/file1") (tmp-file "test1/file2") (tmp-file "test1/file3")))
      (check-glob-match? g))

    ;; -- glob-match?
    (define (check-glob-match?-theorem g p)
      (if (member (path->string (simplify-path p)) (glob g))
        (check-true (glob-match? g p) (format "glob '~a' should match '~a'" g p))
        (check-false (glob-match? g p) (format "glob '~a' should not match '~a'" g p))))

    (check-glob-match?-theorem (tmp-file "test1/../main.rkt") (tmp-file "main.rkt"))
    (check-glob-match?-theorem (tmp-file "ma*in.rkt") (tmp-file "test1/../main.rkt"))
    (check-glob-match?-theorem (tmp-file "moon.rkt") (tmp-file "main.rkt"))

    ;; -- dotfiles
    (touch-file (tmp-file ".ignoreme"))
    (check-equal? (glob (tmp-file "*")) (list (tmp-file "main.rkt") (tmp-file "test1") (tmp-file "test2")))
    (check-equal? (glob (tmp-file "*") #:with-dotfiles? #f) (list (tmp-file "main.rkt") (tmp-file "test1") (tmp-file "test2")))
    (check-equal? (glob (tmp-file "*") #:with-dotfiles? #t) (list (tmp-file ".ignoreme") (tmp-file "main.rkt") (tmp-file "test1") (tmp-file "test2")))
    (check-equal? (glob (tmp-file ".*") #:with-dotfiles? #t) (list (tmp-file ".ignoreme")))
    (check-equal? (glob (tmp-file ".*") #:with-dotfiles? #f) (list (tmp-file ".ignoreme")))

    ;; -- in-glob
    (let ([p (tmp-file "*")]
          [to-list (lambda (x) (for/list ([y x]) y))])
      (check-equal? (to-list (in-glob p))
                    (glob p))
      (check-equal? (to-list (in-glob p #:with-dotfiles? #t))
                    (glob p #:with-dotfiles? #t))
      (check-equal? (to-list (in-glob p #:with-dotfiles? #f))
                    (glob p #:with-dotfiles? #f)))
    (let ([bad-pattern "/c?/../e"])
      (check-exn #rx"glob"
        (lambda () (in-glob bad-pattern))))
  )
)

