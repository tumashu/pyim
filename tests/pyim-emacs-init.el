(require 'pyim)
(setq default-input-method "pyim")
(setq pyim-debug t)

(defun pyim-test-find-file (file)
  "Read FILE's content into current buffer."
  (let* ((files (directory-files-recursively default-directory file)))
    (file-truename (car files))))

(setq pyim-dicts (list (list :name "basedict" :file (pyim-test-find-file "pyim-basedict.pyim"))))
(message "pyim-dicts=%s" pyim-dicts)
