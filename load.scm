;;;; Loader for the English-Based Inference Engine

(load "eq-properties")
(load "ghelper")
(load "matcher")
(load "tokenizer-parser")
(load "deparser")
(load "knowledge-base")
; Initialize knowledge database
(know:initialize-knowledge-base!)
