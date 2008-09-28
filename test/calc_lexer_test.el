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
      (assert-equal "Match decimal"
                    '((10 . "2.2") ; 10 == decimal
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "2.2"))
      (assert-equal "Match single token E"
                    '((7 . "E") ; 7 == E
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "E"))
      (assert-equal "Match single token PI"
                    '((6 . "PI") ; 6 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "PI"))
      (assert-equal "Basic match2"
                    '((9 . "1") ; 9 == integer
                      (9 . "1") ; 9 == integer
                      (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "1 1"))

      (assert-equal "Really complicated string"
                    '(
                      (4 . "(") 
                      (9 . "1")
                      (9 . "2")
                      (9 . "3443")
                      (10 . "232434353.1")
                      (5 . ")") 
                      (6 . "PI") 
                      (10 . "0.0")
                      (7 . "E") 
                      (7 . "E") 
                      (7 . "E") 
                      (7 . "E") 
                      (7 . "E") 
                     (-1 . nil))
                    (collect-lex-tokens 'calc_elispLexer "(1 2   3443 232434353.1 ) PI 0.0EEEEE"))
      )
