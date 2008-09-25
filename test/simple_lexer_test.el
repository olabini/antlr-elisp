(load "test_lexer_elispLexer.el")

(defun print-token (token)
  (message "Token: [%d] {%d -> %d} %s" (lexer-token-type token) (common-token-start token) (common-token-stop token) (lexer-token-text token)))

(defun collect-lex-tokens (name str)
  (let ((all-tokens ()))
    (lex-string name str #'(lambda (token) (setq all-tokens (cons (cons (lexer-token-type token) (lexer-token-text token)) all-tokens))))
    (reverse all-tokens)))

(test "Simple lexing"
      (assert-equal "Basic match"
                    '((4 . "0")
                      (-1 . nil))
                    (collect-lex-tokens 'test_lexer_elispLexer "0"))
      (assert-equal "Basic double match"
                    '((4 . "0")
                      (4 . "0")
                      (-1 . nil))
                    (collect-lex-tokens 'test_lexer_elispLexer "00"))
      (assert-error "Basic failure"
                    'mismatched-token
                    (collect-lex-tokens 'test_lexer_elispLexer "1")))
