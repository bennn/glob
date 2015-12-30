#lang racket/base

(require
  (only-in "private/glob.rkt" glob in-glob)
  racket/contract)

(provide
  (contract-out
    [glob (->* (string?) (#:with-dotfiles? boolean?) (listof string?))]
    [in-glob (->* (string?) (#:with-dotfiles? boolean?) sequence?)]))

