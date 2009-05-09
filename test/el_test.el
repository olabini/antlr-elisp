
(put 'test-failed 'error-conditions
     '(error test-error))
(put 'test-failed 'error-message "Test case failed")

(put 'tests-failed 'error-conditions
     '(error test-error))
(put 'tests-failed 'error-message "One or more tests failed")

(defmacro assert-equal (name first second)
  `(let ((first-param ,first)
         (second-param ,second))
     (unless (equal first-param second-param)
       (signal 'test-failed (concat ,name " - expected: " 
                                    (format "%s" first-param) 
                                    " but was: " 
                                    (format "%s" second-param))))))

(defmacro assert-error (name error code)
  `(condition-case err
       (progn 
         ,code
         (signal 'test-failed (concat ,name ": " 
                                    (format "%s" ',code) 
                                    " should raise " 
                                    (format "%s" ,error))))
     (error (unless (equal (car err) ,error)
         (signal 'test-failed (concat ,name ": " 
                                    (format "%s" ',code) 
                                    " should raise " 
                                    (format "%s" ,error)))))))

(defmacro assert-not-equal (name first second)
  `(let ((first-param ,first)
         (second-param ,second))
     (if (equal first-param second-param)
       (signal 'test-failed (concat ,name ": " 
                                    (format "%s" first-param) 
                                    " should not be equal to " 
                                    (format "%s" second-param))))))

(defmacro test (name &rest body)
  `(let ((failures ()))
     ,@(mapcar #'(lambda (te) `(condition-case failure
                                   ,te
;                                 (error (setq failures (cons failure failures)))
                                 )) 
               body)
     (if (not (null failures))
         (signal 'tests-failed (reverse failures)))))

(provide 'el_test)
