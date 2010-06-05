(defconst git-edit-index-marker "@GITINDEX@/")

(defun git-edit-index-index (filename)
  (car (split-string filename git-edit-index-marker)))

(defun git-edit-index-file (filename)
  (cadr (split-string filename git-edit-index-marker)))

(defun git-index-file-name (&optional file-name)
  (or (getenv "GIT_INDEX_FILE")
      (concat
       (let ((default-directory (if file-name
                                    (file-name-directory file-name)
                                  default-directory)))
         (substring (git-call-process-string "rev-parse" "--git-dir")
                    0 -1))
       "/index")))

(defun git-edit-file-attributes (file-name &optional id-format)
  (let ((index-attributes (file-attributes
                           (git-edit-index-index file-name)
                           id-format)))
    (list nil
          1
          (nth 2 index-attributes)
          (nth 3 index-attributes)
          (nth 4 index-attributes)
          (nth 5 index-attributes)
          (nth 6 index-attributes)
          0 ;; FIXME size
          (nth 8 index-attributes)
          (nth 9 index-attributes)
          nil ;; inode number
          (nth 11 index-attributes))))

(defun git-edit-file-modes (filename)
  (file-modes (git-edit-index-index filename)))

(defun git-index-worktree (indexfile)
  (let ((top-dir (directory-file-name
                  (git-get-top-dir (file-name-directory index)))))
    (if (string= (file-name-nondirectory top-dir) ".git")
        (file-name-directory top-dir)
      (file-name-as-directory top-dir))))

(defun git-edit-insert-file-contents (filename &optional visit beg end replace)
  (let* ((path (git-edit-index-file filename))
         (index (git-edit-index-index filename))
         (process-environment (append (cons (concat "GIT_INDEX_FILE=" index)
                                            process-environment)))
         (default-directory (git-index-worktree index)))
    (let ((output (git-call-process-string-display-error
                   "checkout-index" "--temp" path)))
      (if output
          (let ((tmpfile (car (split-string output "\t"))))
            (unwind-protect
                (prog1 (insert-file-contents tmpfile visit beg end replace)
                  (setq buffer-file-name filename))
              (delete-file tmpfile)))
        (list filename 0)))))

(defun git-edit-verify-visited-file-modtime (buf)
  ;; FIXME
  t)

(defun git-edit-write-region (start end filename
                              &optional append visit lockname mustbenew)
  (let* ((tmpfile (make-temp-file ".gitedit-"))
         (index (git-edit-index-index filename))
         (process-environment (append (cons (concat "GIT_INDEX_FILE=" index)
                                            process-environment)))
         (default-directory (git-index-worktree index))
         (inhibit-file-name-handlers '(git-edit-index))
         (inhibit-file-name-operation 'write-region)
         (path (git-edit-index-file filename)))
    (write-region start end tmpfile append
                  (if (eq visit t)
                      buffer-file-name
                    visit)
                  lockname mustbenew)
    (let ((hash (car (split-string
                      (git-call-process-string-display-error
                       "hash-object" "--path" path "-w" tmpfile)
                      "\n"))))
      ;; FIXME: mode
      (git-call-process-display-error
       "update-index" "--cacheinfo" "0644" hash path))

    (delete-file tmpfile)))

(defun git-edit-index (operation &rest args)
  (case operation
    ('file-directory-p nil)
    ('file-symlink-p nil)
    ('file-remote-p nil)
    ('vc-registered nil) ; ??
    ('file-writable-p t)
    ('file-regular-p t)
    ('file-exists-p t) ; ??
    ('file-attributes (apply #'git-edit-file-attributes args))
    ('file-modes (apply #'git-edit-file-modes args))
    ('verify-visited-file-modtime
     (apply #'git-edit-verify-visited-file-modtime args))
    ('insert-file-contents (apply #'git-edit-insert-file-contents args))
    ('file-newer-than-file-p
     (message "newer %s %s" (car args) (cadr args))
     nil) ; ??
    ('write-region (apply #'git-edit-write-region args))
    (t
     (unless (memq operation '(substitute-in-file-name
                               expand-file-name
                               directory-file-name
                               file-name-directory
                               file-name-as-directory
                               ;;file-name-sans-directory
                               file-name-nondirectory
                               file-name-sans-versions
                               file-truename
                               make-auto-save-file-name ; ??
                               get-file-buffer
                               ))
       (message "File operation %s" operation))
     (let ((inhibit-file-name-handlers '(git-edit-index))
           (inhibit-file-name-operation operation))
       (apply operation args)))))

(add-to-list 'file-name-handler-alist '("@GITINDEX@/.*\\'" . git-edit-index))
