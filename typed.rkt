#lang typed/racket/base

(provide glob in-glob glob-match?)

(require typed/racket/unsafe)
(unsafe-require/typed glob/private/glob
  (glob (->* [String] [#:with-dotfiles? Boolean] (Listof String)))
  (in-glob (->* [String] [#:with-dotfiles? Boolean] (Sequenceof String)))
  (glob-match? (-> String Path-String Boolean)))
