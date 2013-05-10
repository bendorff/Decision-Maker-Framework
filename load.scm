;;;; File:  load.scm -- Loader for pattern matching system

; Pattern matcher:

(load "eq-properties")
(load "ghelper")
(load "matcher")
(load "tokenizer-parser")
(load "knowledge-base")
(know:initialize-knowledge-base!)
(load "deparser")
