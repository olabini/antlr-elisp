;;; antlr-runtime.el --- Antlr runtime

;; Copyright (C) 2008  Ola Bini

;; Author: Ola Bini <ola.bini@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(require 'cl)

(defvar *antlr-runtime-lexers* (make-hash-table)
  "Keeps track of all lexers defined in the system, including their definitions") 

(defvar *antlr-runtime-parsers* (make-hash-table)
  "Keeps track of all parsers defined in the system, including their definitions") 

(defconst *antlr-token-default-channel* 0)
(defconst *antlr-token-invalid-token-type* 0)

(defmacro deflexer (name)
  `(puthash ',name 
            (make-antlr-lexer :name ',name) 
            *antlr-runtime-lexers*))

(defmacro defparser (name)
  `(puthash ',name 
            (make-antlr-parser :name ',name) 
            *antlr-runtime-parsers*))

(defstruct antlr-lexer
  "An Antlr lexer"
  name
  (tokens (make-hash-table))
  (rules (make-hash-table))
  (dfas (make-hash-table)))

(defstruct common-token
  "A Common Antlr token"
  input
  type
  channel
  (start -1)
  (stop -1)
  line
  text
  char-position-in-line)

(defstruct DFA
  recognizer
  decision-number
  eot
  eof
  min
  max
  accept
  special
  transition
  description)

(defun lexer-input-LA (n)
  (let ((at (+ (point) (- n 1))))
    (if (= at (point-max))
      -1
      (char-after at))))
  
(defun lexer-input-consume ()
  (goto-char (+ (point) 1)))

(defun dfa-special-state-transition (state)
  -1)

(defun predict-DFA-with (dfa)
  (save-excursion
    (catch 'return
      (let ((s 0))
        (while t
          (catch 'continue
            (let ((special-state (aref (DFA-special dfa) s)))
              (when (>= special-state 0)
                (setq s (dfa-special-state-transition special-state))
                (lexer-input-consume)
                (throw 'continue nil))
              (when (>= (aref (DFA-accept dfa) s) 1)
                (throw 'return (aref (DFA-accept dfa) s)))
              (let ((c (lexer-input-LA 1)))
                (when (and (>= c (aref (DFA-min dfa) s)) (<= c (aref (DFA-max dfa) s)))
                  (let ((snext (aref (aref (DFA-transition dfa) s) (- c (aref (DFA-min dfa) s)) )))
                    (when (< snext 0)
                      (when (>= (aref (DFA-eot dfa) s) 0)
                        (setq s (aref (DFA-eot dfa) s))
                        (lexer-input-consume)
                        (throw 'continue nil))
                      (dfa-no-viable-alt s)
                      (throw 'return 0))
                    (setq s snext)
                    (lexer-input-consume)
                    (throw 'continue nil)))
                (when (>= (aref (DFA-eot dfa) s) 0)
                  (setq s (aref (DFA-eot dfa) s))
                  (lexer-input-consume)
                  (throw 'continue nil))
                (when (and (eq c *antlr-token-eof-token*) (>= (aref (DFA-eof dfa) s) 0))
                  (throw 'return (aref (DFA-accept dfa) (aref (DFA-eof dfa) s))))
                (dfa-no-viable-alt s)
                (throw 'return 0)))))))))


(defun dfa-no-viable-alt (s)
  (signal 'no-viable-alt (list s)))

(defmacro predictDFA (name)
  `(predict-DFA-with (gethash ',name (antlr-lexer-dfas current-lexer))))

(defmacro setDFA (name value)
  `(puthash ',name ,value (antlr-lexer-dfas current-lexer)))

(defmacro defDFA (name value)
  `(set (intern (concat "*" (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name) "*")) ,value))

(defmacro getDFA (name)
  `(symbol-value (intern (concat "*" (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name) "*"))))

(defmacro defDFAstruct (name &rest defaults)
  `(progn 
     (fset (intern (concat (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name))) 
           #'(lambda (reco)
               (make-DFA
                :recognizer reco
                ,@defaults
                )))
     (unless (fboundp (intern (concat "make-DFAstruct-" (format "%s" ',name))))
       (fset (intern (concat "make-DFAstruct-" (format "%s" ',name))) 
             #'(lambda ()
                 (funcall (intern (concat (format "%s" (antlr-lexer-name current-lexer)) "-" (format "%s" ',name))) nil))))))

(defun lexer-token-type (token)
  (common-token-type token))

(defun lexer-token-text (token)
  (cond
    ((not (null (common-token-text token))) 
     common-token-text token)
    ((not (null (common-token-input token)))
     (let ((text (with-current-buffer (common-token-input token)
                   (buffer-substring (common-token-start token) 
                                     (common-token-stop token)))))
       (setf (common-token-text token) text)
       text))
    (t nil)))

(defconst *antlr-token-eof-token* (make-common-token :type -1 :channel 0))
(defconst *antlr-token-invalid-token* (make-common-token :type *antlr-token-invalid-token-type* :channel 0))
(defconst *antlr-token-skip-token* (make-common-token :type *antlr-token-invalid-token-type* :channel 0))

(defstruct antlr-lexer-context
  "Context used for a lexing"
  (lexer nil)
  (input nil)
  (token nil)
  (token-start-char-index -1)
  (token-start-line -1)
  (token-start-char-position-in-line -1)
  (channel nil)
  (type nil)
  (text nil)
  (failed nil))

(put 'mismatched-token 'error-conditions
     '(error antlr-error))
(put 'mismatched-token 'error-message "Mismatched token")

(put 'no-viable-alt 'error-conditions
     '(error antlr-error))
(put 'no-viable-alt 'error-message "No viable alternative")

(defun lexer-set-type (type)
  (setf (antlr-lexer-context-type context) type))

(defun lexer-set-channel (c)
  (setf (antlr-lexer-context-channel context) c))

(defmacro lexer-call-rule (name)
  `(progn 
     ;(message (concat "calling rule " (format "%s" ',name))) 
     (funcall (gethash ',name (antlr-lexer-rules (antlr-lexer-context-lexer context))) context)))

(defmacro lexer-token-id (name)
  `(gethash ',name (antlr-lexer-tokens (antlr-lexer-context-lexer context))))

(defmacro deftoken (name value)
  `(puthash ',name ,value 
            (if (boundp 'current-lexer) 
                (antlr-lexer-tokens current-lexer) 
                (antlr-parser-tokens current-parser))))

(defmacro defrule (name params &rest body)
  `(puthash ',name (lambda (context ,@params) ,@body) 
            (if (boundp 'current-lexer) 
                (antlr-lexer-rules current-lexer) 
                (antlr-parser-rules current-parser))))

(defmacro lexer-match-range (a b)
  (let ((la (lexer-input-LA 1)))
    (when (or (< la a) (> la b))
      (signal 'mismatched-range (list a b)))
    (lexer-input-consume)
    (setf (antlr-lexer-context-failed context) nil)))

(defmacro lexer-match (s)
  (cond
    ((numberp s) 
     `(if (= (lexer-input-LA 1) ,s)
          (goto-char (+ (point) 1))
          (signal 'mismatched-token (list ,(char-to-string s) context))))
    ((stringp s) 
     `(let ((i 0)
            (str ,s))
        (while (< i (length str))
          (unless (= (lexer-input-LA 1) (elt str i))
            (signal 'mismatched-token (list (point) (char-to-string (lexer-input-LA 1)) (lexer-input-LA 1) (char-to-string (elt str i)) (elt str i) str context)))
          (incf i)
          (lexer-input-consume)
          (setf (antlr-lexer-context-failed context) nil))))
    (t (signal 'error "Implement t case"))))

(defmacro with-lexer (name &rest body)
  `(progn 
     (let ((current-lexer (gethash ',name *antlr-runtime-lexers*)))
       ,@body)))

(defun lex-string (name str method)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name "*antlr string lexing*"))))
    (save-excursion
      (with-current-buffer buffer
        (insert str)
        (goto-char (point-min))
        (lex-buffer name method buffer 0 (buffer-size buffer))
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))))

(defun lex-emit (token)
  (setf (antlr-lexer-context-token context) token))

(defun lex-emit-token ()
  (let ((token (make-common-token
                :input (current-buffer)
                :type (antlr-lexer-context-type context)
                :channel (antlr-lexer-context-channel context)
                :start (antlr-lexer-context-token-start-char-index context)
                :stop (point)
                :line (antlr-lexer-context-token-start-line context)
                :text (antlr-lexer-context-text context)
                :char-position-in-line (antlr-lexer-context-token-start-char-position-in-line context))))
    (lex-emit token)
    token))

(defun lex-buffer (lexer-name method buffer start end)
  (let* ((current-lexer (gethash lexer-name *antlr-runtime-lexers*))
         (context (make-antlr-lexer-context 
                   :lexer current-lexer)))
    (save-excursion
      (with-current-buffer buffer
        (goto-char start)
        (catch 'at-end
          (while t
            (setf (antlr-lexer-context-token context) nil
                  (antlr-lexer-context-channel context) *antlr-token-default-channel*
                  (antlr-lexer-context-token-start-char-index context) (point)
                  (antlr-lexer-context-token-start-char-position-in-line context) (current-column)
                  (antlr-lexer-context-token-start-line context) (line-number-at-pos)
                  (antlr-lexer-context-text context) nil)
            (when (= (point) (point-max))
              (funcall method *antlr-token-eof-token*)
              (throw 'at-end nil))
            (condition-case nil
                (progn
                  (funcall (gethash 'Tokens (antlr-lexer-rules (antlr-lexer-context-lexer context))) context)
                  (when (null (antlr-lexer-context-token context))
                    (lex-emit-token))
                  (unless (eq (antlr-lexer-context-token context) *antlr-token-skip-token*)
                    (funcall method (antlr-lexer-context-token context))))
              (message "TODO handling error here"))))))))



(defstruct antlr-parser
  "An Antlr parser"
  name
  (token-names nil)
  (tokens (make-hash-table))
  (rules (make-hash-table))
  (bitsets (make-hash-table))
)

(defmacro with-parser (name &rest body)
  `(progn 
     (let ((current-parser (gethash ',name *antlr-runtime-parsers*)))
       ,@body)))

(defun parser-token-names (&rest names)
  (setf (antlr-parser-token-names current-parser) names))

(defmacro parser-initialization (&rest body))

(defmacro parser-bitset (name bitsets)
  `(puthash ',name 
            (create-bitset ',bitsets)
            (antlr-parser-bitsets current-parser)))

(defstruct bitset
  "An Antlr bitset"
  bits)

(defun create-bitset (bitsets)
  (make-bitset :bits bitsets))

(provide 'antlr-runtime)
;;; antlr-runtime.el ends here
