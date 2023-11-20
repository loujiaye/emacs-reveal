(require 'ox-reveal)

(org-export-define-derived-backend 'loujiaye-reveal 'reveal
  :translate-alist '((plain-list . loujiaye-org-reveal-plain-list)
                     (item . loujiaye-org-reveal-item)))

(defun loujiaye-org-reveal-plain-list (plain-list contents info)
  "重新定义列表解析"
  (let ((layout (loujiaye-get-page-layout plain-list))
        (level (loujiaye-get-item-level plain-list)))
    (if (and (string= "row" layout)
             (= level 1))
        (loujiaye-layout-row plain-list contents info)
      (org-reveal-plain-list plain-list contents info))))

(defun loujiaye-org-reveal-item (item contents info)
  "格式化item tag 保留 type  checkbox 去掉"
  (let* ((level (loujiaye-get-item-level item))
         (plain-list (org-export-get-parent item))
         (list-length (length (org-element-contents plain-list)))
	 (type (org-element-property :type plain-list))        
	 (counter (org-element-property :counter item))
         (attributes (org-export-read-attribute :attr_html item))
	 (checkbox (org-element-property :checkbox item))
	 (tag (let ((tag (org-element-property :tag item)))
	        (and tag (org-export-data tag info)))))
    (if (= level 1)
        (loujiaye-org-reveal-format-list-item
         contents level list-length checkbox attributes info (or tag counter))
      (org-reveal-format-list-item
       contents type checkbox attributes info (or tag counter)))))

(defun loujiaye-org-reveal-format-list-item
    (contents level plain-list-item-length checkbox attributes info &optional term-counter-id headline)
  (let* ((width (/ 100 plain-list-item-length))
         (attr-html (concat
                     (cond (attributes (format " %s" (org-html--make-attribute-string attributes)))
                           (checkbox (format " class=\"%s\"" (symbol-name checkbox)))
                           (t ""))
                     ;; " style=\"width:"
                     ;; (number-to-string width)
                     ;; "%\" ")))
                     )))
    (concat
     (format "<div id=\"loujiaye-layout-column\" %s>" attr-html)
     (and contents (org-trim contents))
     "</div>")))



(defun loujiaye-get-item-level (item)
     "获取item层级"
     (let ((level 0))
       (while item
         (if (string= (org-element-type item) 'plain-list)
             (setq level (+ 1 level)))
         (setq item (org-export-get-parent item)))
       level))

(defun loujiaye-get-page-layout (element)
  "获取headline REVEAL_PAGE_LAYOUT 属性"
  (let ((parent (org-element-property :parent element)))
    (while (and parent
                (not (string= (org-element-type parent) 'headline)))
      (setq parent (org-element-property :parent parent)))
    (org-element-property :REVEAL_PAGE_LAYOUT parent)))



(defun loujiaye-layout-row (plain-list contents info)
  "列表横向布局方式html格式化"
  (let ((attrs (org-export-read-attribute :attr_html plain-list)))
    (format "<div %s>\n%s\n</div>"
            (concat " "
                    "id=\"loujiaye-layout-row\""
                    (if attrs (concat " " (org-html--make-attribute-string attrs)) ""))
            contents)))



;; 重定义导出函数
(defun org-reveal-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "重新定义reveal 导出, org-export-to-file 调整成 my-reveal"
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (clientfile (org-export-output-file-name (concat "_client" extension) subtreep))
         (org-export-exclude-tags (cons "noexport_reveal" org-export-exclude-tags))
         (client-multiplex nil))
    ; export filename_client HTML file if multiplexing
    (let ((retfile (org-export-to-file 'loujiaye-reveal file
                     async subtreep visible-only body-only ext-plist)))

       ; export the client HTML file if client-multiplex is set true
       ; by previous call to org-export-to-file
      (when client-multiplex
        (org-export-to-file 'loujiaye-reveal clientfile
          async subtreep visible-only body-only ext-plist))
      retfile)))


(provide 'my-ox-reveal)
