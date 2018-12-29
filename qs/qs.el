;;; qs.el --- hacks to accompany my python QS code   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  John Sturdy

;; Author: John Sturdy <jcg.sturdy@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun highlight-large-transactions (major-digits)
  "Highlight number sequences in particular columns with MAJOR-DIGITS."
  (interactive "nNumber of major digits: ")
  (font-lock-mode -1)
  (save-excursion
    (dolist (pair '(("^[-0-9]\\{10\\},[:0-9]\\{8\\},[-0-9.]*,[-0-9.]*,\\(-?[0-9]\\{%d,\\}\\)" . hi-red-b)
		    ("^[-0-9]\\{10\\},[:0-9]\\{8\\},\\(-?[0-9]\\{%d,\\}\\)" . hi-green)))
      (goto-char (point-min))
      (let ((pattern (format (car pair) major-digits)))
	(while (re-search-forward pattern
				  (point-max) t)
	  (put-text-property (match-beginning 1) (match-end 1) 'face (cdr pair)))))))

(defun csv-row-column-widths ()
  "Get the widths of the columns on the current line."
  (mapcar 'length
	  (split-string (buffer-substring-no-properties
			 (line-beginning-position)
			 (line-end-position))
			",")))

(defun csv-get-column-widths ()
  "Get the column widths of the current buffer, excluding commas."
  (save-excursion
    (goto-char (point-min))
    (let ((widths (csv-row-column-widths)))
      (while (not (eobp))
	(setq widths (mapcar* 'max widths (csv-row-column-widths)))
	(beginning-of-line 2))
      widths)))

(defun csv-get-whole-column-widths ()
  "Get the column widths of the current buffer, including commas."
  (mapcar '1+ (csv-get-column-widths)))

;; todo: use text property 'display (space :align-to HPOS) to position the columns

(defun q (ws ps)
  "Q WS PS."
  (if (null ws)
      ps
    (let ((to-left (q (cdr ws) ps)))
      (cons (+ (car ws) (car to-left))
	    to-left))))

(defun csv-get-column-positions ()
  "Get the column positions."
  (cdr
   (nreverse
    (q (nreverse (csv-get-whole-column-widths))
       '(0)))))

(defun csv-adjust-column-display ()
  "Adjust the column display."
  (interactive)
  (let ((positions (csv-get-column-positions)))
    (setq positions (butlast (- (length positions) 8)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((line-end line-end-position))
	  (dolist (pos positions)
	    (search-forward "," line-end t)
	    (put-text-property (match-beginning 0)
			       (match-end 0)
			       'display
			       (list 'space :align-to pos))))
	(beginning-of-line 2)))))

(provide 'qs)
;;; qs.el ends here
