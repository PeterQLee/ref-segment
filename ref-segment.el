
(defun ref-template (title path author subject keywords url)
  "Org-mode template for adding a new article"
  (format "* %s\n** [path]\n[[%s][%s]]\n** tags\n:@tag::@tag:\n** [Isread]\n** [Author]\n%s\n** [subject]\n%s\n** [keywords]\n%s\n** [url]\n%s\n** [notes]\n** [reference]\n" title path path author subject keywords url)
  )


(defun download-article-to-org (url)
  " Adds article to org mode from a url"
  (interactive "surl: ")

  ;; Get default file name
  (setq outpath (car (last (split-string url "/"))))


  ;; Get file
  (if (url-copy-file url outpath) (progn
      ;; Get raw meta data
    (setq outpath (expand-file-name outpath))
    (setq meta-data (pdf-info-metadata outpath))

    ;; Get important meta data
    (setq title (cdr (assoc 'title meta-data)))
    (setq author (cdr (assoc 'author meta-data)))
    (setq subject (cdr (assoc 'subject meta-data)))
    (setq keywords (cdr (assoc 'keywords meta-data)))
    
    ;; Insert entry into 
    (insert (ref-template title outpath author subject keywords url))
    )
    (message "Cannot get result")
    )
  )

(defun add-file-article-to-org ()
  "Adds pdf file to org file "
  (interactive)
  (setq outpath (read-file-name "outpath: "))
  (setq outpath (expand-file-name outpath))
  (setq meta-data (pdf-info-metadata outpath))

  ;; Get important meta data
  (setq title (cdr (assoc 'title meta-data)))
  (setq author (cdr (assoc 'author meta-data)))
  (setq subject (cdr (assoc 'subject meta-data)))
  (setq keywords (cdr (assoc 'keywords meta-data)))

  (setq url "unknown")
  ;; Insert entry into 
  (insert (ref-template title outpath author subject keywords url))
  )

					;<file:~/test/org/b.org::*tag>
(defconst tag-regex ":@tag:.*:@tag:")

(defun get-tags (begin end)
  "Gets tags in org entry"
  (setq contents (buffer-substring-no-properties begin end))
					; Apply regex to get the tags
  (setq offset 6) ; offset for :@tag:
  (setq a (string-match tag-regex contents))
  (if a
      (split-string (substring contents (+ a offset) (- (match-end 0) offset) ) ";"))

  )
    
  
 


(defun segment-articles-by-tag ()
  " Partition articles according to tag."
  (interactive)
  
  (setq parsetree (org-element-parse-buffer 'headline))

  (setq itemlist (org-element-map parsetree 'headline 
		   (lambda (hl)
		     (if (= (org-element-property :level hl) 1)
			 (if (setq tagelem (get-tags (org-element-property :contents-begin hl) (org-element-property :contents-end hl)))
			     (list (org-element-property :title hl) tagelem)
		       
		     )))))
		     
  
  ;; Create hashset containing unique tags
  (setq rclist itemlist)
  (setq taghash (make-hash-table :test 'equal))
  (setq curfilepath (buffer-file-name))
  
  (while rclist
    (setq elem (car rclist))
    (setq linkdest (org-link-escape (format "file:%s::* %s" curfilepath (car elem))))
    (setq outstr (format "[[%s][%s]]" linkdest (car elem)))
    (setq tags (car (last elem)))
    (while tags
      (setq curkey (car tags))
      (setq res (gethash curkey taghash))
      (if res
    	  (puthash curkey (cons outstr res) taghash)
    	  (puthash curkey (list outstr) taghash)
    	)
      (setq tags (cdr tags))
      )
    (setq rclist (cdr rclist))
    
    )
  
  ;;; Create buffer
  (setq temp-buffer-setup-hook 'org-mode)
  (with-output-to-temp-buffer "*segments*"

  ;;; Print total number of articles in list
  (princ (format "Total articles categorized: %d\n" (length itemlist)))
  
  ;;; Insert hashes
  (maphash
   (lambda (k v)
     
     (princ (format "* %s : total = %d\n" k (length v)))
     (setq vlist v)
     (while vlist
       (princ (format "%s\n" (pop vlist)))
       )
     
     )
   taghash)
   (org-mode)

 ))


(defun add-log-to-db (start end)
  "Adds updated info to the database"
  (interactive "r")
  (setq contents (buffer-substring-no-properties start end))
  (setq subject (read-string "subject:"))
  (princ (call-process-region start end "pap_org_convert" nil nil nil subject))
  )

(defun gen-paper-log-from-db ()
  "Generates a list of entries from the database"
  (interactive)
  (setq subject (read-string "subject:"))
  (setq result (shell-command-to-string (format "pap_gen_org %s" subject)))

  (with-output-to-temp-buffer (format "*%s*" subject)
    (princ result))
  (org-mode)
  )

(defun open-article-in-okular ()
  "opens the article into okular"
  (interactive)
  (setq buf (org-element-context))
  (setq pdfpath (plist-get (car (cdr buf)) :path))
  (call-process-shell-command (format "okular %s &" pdfpath))
  )
  
			      
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-1") 'add-log-to-db)
	    (local-set-key (kbd "C-c C-2") 'gen-paper-log-from-db)
	    (local-set-key (kbd "C-c C-3") 'open-article-in-okular)
	    ))
