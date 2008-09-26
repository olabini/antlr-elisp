(load "calc_elispLexer.el")

(test "Calc lexing"
      (assert-equal "Basic match"
                    '((9 . "123") ; 9 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "123"))
      (assert-equal "Basic match2"
                    '((9 . "321") ; 9 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "321"))
      )
