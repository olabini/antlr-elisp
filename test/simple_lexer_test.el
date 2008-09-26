(load "test_lexer_elispLexer.el")

(defun print-token (token)
  (message "Token: [%d] {%d -> %d} %s" (lexer-token-type token) (common-token-start token) (common-token-stop token) (lexer-token-text token)))

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
