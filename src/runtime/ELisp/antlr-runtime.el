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

(defconst *antlr-token-default-channel* 0)
(defconst *antlr-token-invalid-token-type* 0)

(defmacro deflexer (name)
  `(puthash ',name 
            (make-antlr-lexer) 
            *antlr-runtime-lexers*))

(defstruct antlr-lexer
  "An Antlr lexer"
  (tokens (make-hash-table))
  (rules (make-hash-table)))

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

(defconst *antlr-token-eof-token* (make-common-token :type -1))
(defconst *antlr-token-invalid-token* (make-common-token :type *antlr-token-invalid-token-type*))
(defconst *antlr-token-skip-token* (make-common-token :type *antlr-token-invalid-token-type*))

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
  (text nil))

(put 'mismatched-token 'error-conditions
     '(error antlr-error))
(put 'mismatched-token 'error-message "Mismatched token")

(defun lexer-set-type (type)
  (setf (antlr-lexer-context-type context) type))

(defmacro lexer-call-rule (name)
  `(funcall (gethash ',name (antlr-lexer-rules (antlr-lexer-context-lexer context))) context))

(defmacro lexer-token-id (name)
  `(gethash ',name (antlr-lexer-tokens (antlr-lexer-context-lexer context))))

(defmacro deftoken (name value)
  `(puthash ',name ,value (antlr-lexer-tokens current-lexer)))

(defmacro defrule (name params &rest body)
  `(puthash ',name (lambda (context ,@params) ,@body) (antlr-lexer-rules current-lexer)))

(defmacro lexer-match (s)
  (cond
    ((numberp s) 
     `(if (= (char-after) ,s)
          (goto-char (+ (point) 1))
          (signal 'mismatched-token (list ,(char-to-string s) context))))
    ((stringp s) )
    (t )))

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
  (let ((context (make-antlr-lexer-context 
                  :lexer (gethash lexer-name *antlr-runtime-lexers*))))
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


(provide 'antlr-runtime)
;;; antlr-runtime.el ends here
