#lang racket/base

(require
  "private/glob.rkt"
  racket/contract)

(provide
  (contract-out
    [glob (->* (string?) (#:with-dotfiles? boolean?) (listof string?))]
    [in-glob (->* (string?) (#:with-dotfiles? boolean?) sequence?)]
    [glob-match? (-> string? path-string? boolean?)]))

