(defun drewc.org:match-strings-all (&optional string)
    "Return the list of all expressions matched in last search.
  
  STRING is optionally what was given to `string-match'."
    (let ((n-matches (1- (/ (length (match-data)) 2))))
      (mapcar (lambda (i) (match-string i string))
              (number-sequence 0 n-matches))))

(defun drewc.org:replace-<i>-icons (string)
  (let* ((re-icon (rx 
		   "\<i\>"
		   (submatch 
		    (and "icon-" 
			 (one-or-more (or alphabetic (any "  -")))))
		   "\</i\>"))
	 (match (string-match re-icon string))
	 (all (drewc.org:match-strings-all string))
	 (class (second (and match all )))
	 (future (and match (substring string (+ 1 match))))
	 (next (when future (string-match re-icon future)))
	 (string-to-replace (if next 
				(substring string 0 (+ match (length (first all))))
			      string))

	 (next-string (if next
			  (substring string (length string-to-replace))
			"")))
    (if class
        (concatenate
	 'string
	 (replace-regexp-in-string
	  re-icon
	  (concatenate 'string "<i class=\"" class "\"></i>")
	  string-to-replace)
	 (if next (drewc.org:replace-<i>-icons next-string) "") 
	 )
      string-to-replace)))
