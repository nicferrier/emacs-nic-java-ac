;;; java-complete.el --- Java completion

;; Copyright (C) 2013 Nic Ferrier
;; Copyright (C) 2004 Tapsell-Ferrier Limited

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Completion in Java via a tags file.

;;; Code:

(defgroup java-complete nil
  "Completion via tags for Java."
  :group 'java)

(defvar java-complete-class-p 't
  "A switch that controls the completer.
If it's true then the completer does class completion, if false
it does member completion.")

(defvar java-complete-package-re ""
  "A regex that matches packages prefixs.
This is used to search across everything that a file imports.")

(defun java-complete ()
  (interactive)
  (save-excursion
    (if (looking-back "\\.[A-Za-z0-9]*" (line-beginning-position))
        (java-complete-member)
      (java-complete-class))))

(defun java-complete-class ()
  (let ((sym (progn
               (re-search-backward "[ \t]\\([A-Za-z0-9_]*\\)" (line-beginning-position) 't)
               (match-string 1))))
    (save-match-data
      (let* ((java-complete-package-re (java-complete-import-re)) ; java-complete-package-re is used dynamically
             (completed (try-completion sym 'java-completer)))
        (cond
         ((eq completed 't)
          (message "already complete"))
     
         ((eq completed nil)
          (message "no completion"))

         ((stringp completed) ; it's a completion
          (if (equal completed sym)
              (with-output-to-temp-buffer "*Completions*"
                (display-completion-list
                 (all-completions completed 'java-completer)))
            ;; Else the completion did cause an expansion, so put it in.
            (progn
              (replace-match completed 't 't nil 1)
              (indent-region (line-beginning-position) (line-end-position))))))))))

(defun java-complete-member ()
  ;; This func is only called when point is preceeded by a "xxx." pattern
  ;; So we already know there's a variable to be found
  (let* (java-complete-class-name
         variable
         (pt (point))
         (sym (progn
                (re-search-backward "\\.\\([A-Za-z0-9]*\\)" (line-beginning-position) 't)
                (match-string 1))))
    ;; Find some other bits but protect the last re stats
    (save-excursion
      (save-match-data
        (re-search-backward " \\([a-zA-Z0-9]+\\)" (line-beginning-position) 't)
        (setq variable (match-string 1))
        ;; Finding the variable's decl is tricky.
        ;; We should really use C mode's syntactic marking to find the right decl
        ;; but you have to try to make a sensible decision about where the decl is.
        ;; It could be in the current scope, in which case we could search backwards
        ;; (but we might hit previous, deeper scopes)
        ;; If the decl is in clas scope it could be anywhere in relation to point.
        ;; So this will probably have to learn some scoping rules.
        (if (not (re-search-backward (concat "[ \t]\\([A-Za-z0-9.]+\\)[ ]*" variable "[ ]*=[ ]*") nil 't))
            (if (not (re-search-backward (concat "[ \t]\\([A-Za-z0-9.]+\\)[ ]*" variable "[ ]*;") nil 't))
                (if (not (re-search-backward (concat "([ \t,]*\\([A-Za-z0-9.]+\\)[ ]*" variable "[ ]*[,)]") nil 't))
                    (error (concat "No decl found for " sym)))))
        (setq java-complete-class-name (match-string 1))))
    ;; Now call the completer
    (let* ((java-complete-package-re (java-complete-import-re))
           (java-complete-class-p nil)
           (completed (try-completion sym 'java-completer)))
      (cond
       ((stringp completed)
        (if (equal completed sym)
            (with-output-to-temp-buffer "*Completions*"
              (display-completion-list
               (all-completions completed 'java-completer)))
          ;; Else the completion did cause an expansion, so put it in.
          (progn
            (replace-match completed 't 't nil 1)
            (indent-region (line-beginning-position) (line-end-position)))))))))

(defun java-complete-import-re ()
  "make a regex to match the packages in the import statements"
  (let ((regex "java\\.lang\\."))
    (save-match-data
      (save-excursion
        (beginning-of-buffer)
        (let ((class-start
               (save-excursion
                 (re-search-forward "^.* \\(class \\|interface \\)" nil 't))))
          (if (not class-start)
              (error "this not a java class or interface"))
          (while (re-search-forward "^import \\([a-z0-9.]+\\).*;" class-start 't)
            (setq regex (concat regex "\\|" (regexp-quote (match-string 1))))
            (end-of-line))))
      (concat "\\(" regex "\\)"))))


(defvar java-complete-tags nil
  "Buffer local tags file reference.")

(make-local-variable 'java-complete-tags)

(defcustom java-complete-build-file-names (list 
                                           "pom.xml"
                                           "build.xml")
  "A list of filenames that might define the dominating file."
  :group 'java-complete
  :type '(repeat string))

(defun java-complete-root (buffer)
  "Find the project root of BUFFER, a Java file.

The project root is the location of the pom.xml or the build.xml."
  (or (locate-dominating-file (buffer-file-name buffer) "pom.xml")
      (locate-dominating-file (buffer-file-name buffer) "build.xml")
      (file-truename (concat (file-name-directory (buffer-file-name buffer)) ".."))))

(defun java-complete-tags-finder (buffer &optional directory)
  "Find a java.tags file for the specified buffer.

The buffer might have the variable `java-completer-tag-buffer'
set.  If so then the value of that is used.  Otherwise the tag
file is seacrhed for from the current directory towards the root.

If a tag file still isn't found then a user specific tags file is
tried: ~/.java.tags If that isn't found either then an error is
signalled.

Returns the tags buffer for the current file."
  (flet ((mkfilename (dir name)
           (file-truename (expand-file-name (concat dir name)))))
    ;; Main func.
    (with-current-buffer buffer
      (if (bufferp java-complete-tags)
          java-complete-tags
          (let* ((project-root (java-complete-root buffer))
                 (home-root (file-truename (expand-file-name "~/")))
                 (tags-file
                  (car-safe
                   (or
                    (directory-files project-root t "\\.java\\.tags")
                    (directory-files home-root t "\\.java\\.tags")))))
            ;; Visit any file found
            (if tags-file
                (with-current-buffer buffer
                  (setq java-complete-tags
                        (find-file-noselect tags-file)))
                ;; Else 
                (error "could not find a java tags file - make one?")))))))

(defun java-completer (string predicate all-completions)
  "Ccomplete STRING with PREDICATE using ALL-COMPLETIONS.

This does most of the completion work scanning the buffer
`java-complete-tags'."
  (save-excursion
    (save-match-data
      (with-current-buffer (java-complete-tags-finder (current-buffer))
        (beginning-of-buffer)
        (let ((case-fold-search nil)
              (result (list '()))
              (re (if java-complete-class-p
                      (concat "^" java-complete-package-re "\\(" (regexp-quote string) "[A-Za-z0-9_]*\\)$")
                    ;; Else it's member completion
                    (concat "^[ \t]*\\(public \\|protected \\|abstract \\|static \\|final \\)"
                            java-complete-package-re 
                            java-complete-class-name "\\." ; the "." here ensures we catch only members (not constructors)
                            "\\(" (regexp-quote string) "[A-Za-z0-9]*\\)(.*)"))))
          (while (re-search-forward re nil 't)
            (nconc result (list (cons (match-string (if java-complete-class-p 2 3))
                                      (match-string 0))))
            (end-of-line))
          ;; What we do with the result depends on whether we were called as
          ;; try-completion or as all-completions
          (if all-completions
              ;; FIXME::: returning the list directly should work!!
              ;; seems to be a bug in display-completion-list that causes it to fail
              (mapcar (lambda (pair)
                        (car pair))  (cdr result))
            (try-completion string (cdr result) predicate)))))))


(provide 'java-complete)

;; End.
