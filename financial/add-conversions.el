;;;; assistance for adding conversions

(require 'finances-entry)

(defun add-conversions ()
  (interactive)
  (finances-read-completions)
  (let ((completion-ignore-case t))
    (while t
      (re-search-forward "^[^,]+,[^,]*,,")
      (backward-char 1)
      (let ((category (completing-read "Category: "
                                       category-completions)))
        (unless (string= category "")
          (insert category)))
      (beginning-of-line 2))))
