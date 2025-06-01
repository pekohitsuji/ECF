;;; -*- lexical-binding: t; coding: utf-8-unix; mode: emacs-lisp; -*-
;;; my/edeug.el --- edebug tools.

;;; Commentary:
;;
;;    M-x my/edebug--instrumented-functions
;;
;;    Edebug 周りはバージョンによって違いが出やすい
;;    (と ChatGPT が言うとりました) ので
;;    すぐ使い物にならなくなる可能性が高い.
;;    動作確認は Emacs 30.1 のみ.


;;; Code:


(defun my/edebug--instrumented-p (func)
  "関数 FUNC に Edebug がインストゥルメントされていれば t を返す."
  (when-let ((f (symbol-function func)))
    (and (or (eq (type-of f) 'interpreted-function)
             (eq (type-of f) 'byte-code-function))
         (consp (aref f 1))
         (eq 'edebug-enter (caar (aref f 1))))))


(defun my/edebug--instrumented-functions ()
  "Edebug がインストゥルメントされた全ての関数シンボルを返す.
interactive に呼んだときは *Messages* 出力もする."
  (interactive)
  (let ((res nil) (name "my/edebug--instrumented-functions"))
    (mapatoms
     (lambda (s)
       (if (my/edebug--instrumented-p s) (setq res (cons s res)))))
    (if (called-interactively-p)
        (if (null res)
            (message "%s: instrumented function not found" name)
          (dolist (func res) (message "%s: %s" name func))))
    res))


(provide 'my/edebug)

;;; my/edebug.el ends here
