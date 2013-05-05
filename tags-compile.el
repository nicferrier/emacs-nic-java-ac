;;; An elisp script to compile the Tags.java program properly

(compile
 (let ((current-dir (file-name-directory (buffer-file-name))))
   (format
    "javac -d %s %s/src/Tags.java"
    current-dir
    current-dir)))

;;; tags-compile.el ends here
