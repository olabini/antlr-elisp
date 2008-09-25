;;; run_tests.el --- Run all tests for Antlr lexers and parsers

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


(setq load-path (cons (expand-file-name "build/test/grammars") load-path))
(setq load-path (cons (expand-file-name "src/runtime/ELisp") load-path))
(setq load-path (cons (expand-file-name "test") load-path))

(require 'antlr-runtime)

(require 'el_test)

(load "simple_lexer_test.el")

;;; run_tests.el ends here
