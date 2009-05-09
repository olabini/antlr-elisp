(load "full_calc_elispLexer.el")
(load "full_calc_elispParser.el")

(test "Full calc parsing"
      (assert-equal "Basic number"
                    nil
                    (do-parse 'full_calc_elispLexer 'full_calc_elispParser 'evaluate "42"))
      )
