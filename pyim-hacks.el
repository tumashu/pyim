;;; pyim-hacks.el --- hacks for pyim    -*- lexical-binding: t; -*-

;; * Header
;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: Feng Shu <tumashu@163.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/pyim
;; Keywords: convenience, Chinese, pinyin, input-method

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * 说明文档                                                              :doc:
;; This file has hacks of pyim

;;; Code:
;; * 代码                                                                 :code:

;; ** Hack for xshell or MobaXTerm.
(defvar pyim-hacks-for-ssh nil
  "Hack of [<no-record> is undefined #402](https://github.com/tumashu/pyim/issues/402)

When user use Xshell or MobaXTerm, error '<no-record> is undefined' will be
exist, this may be not a pyim's bug.  but I do not know how to solve this
problem, so I do this ugly hack, and wait others help ...
1. https://github.com/tumashu/pyim/issues/402
2. https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=bd5c7404195e45f11946b4e0933a1f8b697d8b87x")

(defun pyim-hacks-add-unread-command-events (orig_func key &optional reset)
  "Advice function of `pyim-add-unread-command-events'."
  (if (not pyim-hacks-for-ssh)
      (funcall orig_func key reset)
    (when reset
      (setq unread-command-events nil))
    (setq unread-command-events
          (if (characterp key)
              (cons key unread-command-events)
            (append (mapcan (lambda (e) (list e)))
                    (append key nil))
            unread-command-events))))

(advice-add 'pyim-add-unread-command-events :around #'pyim-hacks-add-unread-command-events)

;; * Footer
(provide 'pyim-hacks)

;;; pyim-hacks.el ends here
