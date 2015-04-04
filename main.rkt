#lang racket/base

(require
  (only-in "private/glob.rkt" glob in-glob)
  racket/contract)

(provide
  (contract-out
    [glob (->* (string?) (#:with-dotfiles? boolean?) (listof string?))]
    ;; TODO more descriptive contract on range, like the type Sequenceof String
    [in-glob (->* (string?) (#:with-dotfiles? boolean?) sequence?)]))

