;;; Demo Code

; Load tokenizer/parser and knowledge-base
(load "load")

(parse-string "America is type country.")
(parse-string "America is powerful.")
(parse-string "If America attacks Britain, then America and Britain go to war.")
(parse-string "Zimbabwe runs.")

(know-learn-file! "demo.txt")

(know:ask "What is America")
(know:ask "Who is America?")
(know:ask "What happens if America attacks Britain?")
(know:ask "When do America and Britain go to war?")
(know:ask "Who runs?")

