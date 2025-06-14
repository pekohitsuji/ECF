;;; -*- lexical-binding: t; coding: utf-8-unix; mode: emacs-lisp; -*-
;;; ecf.el --- Edebug ChaP Form.

;;; Commentary:
;;
;;      ecf ECF記法で埋め込んだ式をそのまま実行できるマクロ
;;
;;      注意
;;          マクロなので Edebug で ecf 内部の式を追うことはできない
;;

;;; Code:

(defun ecf--open-p (some)
  (and (symbolp some)
       (string-prefix-p "{" (symbol-name some))
       (not (string-suffix-p "}" (symbol-name some)))))


(defun ecf--shut-p (some)
  (and (symbolp some)
       (not (string-prefix-p "{" (symbol-name some)))
       (string-suffix-p "}" (symbol-name some))))


(defun ecf--open-shut-p (some)
  (and (symbolp some)
       (string-prefix-p "{" (symbol-name some))
       (string-suffix-p "}" (symbol-name some))))

(defun ecf--not-shut  () (error "ecf: not shut all"))
(defun ecf--over-shut () (error "ecf: over shut"))


(defun ecf--flat (body:list)
  "BODY:LIST から ECF記法部分を除外したリストを返す."
  (named-let loop ((lst body:list) (res nil) (open 0))
    (cond
     ((null lst) (if (zerop open) (reverse res) (ecf--not-shut)))
     ((eq '=> (car lst)) (loop (cdr lst) res open))
     ((ecf--open-shut-p (car lst)) (loop (cdr lst) res open))
     ((ecf--open-p (car lst))
      (loop (cdr lst) res (+ open 1)))
     ((ecf--shut-p (car lst))
      (if (zerop open) (ecf--over-shut)
        (loop (cdr lst) res (- open 1))))
     ((< 0 open) (loop (cdr lst) res open))
     (t (loop (cdr lst) (cons (car lst) res) open)))))


(defun ecf--deep (body:list)
  "BODY:LIST から **再帰的に** ECF記法部分を除外したリストを返す"
  (seq-map (lambda (x) (if (consp x) (ecf--deep x) x))
    (ecf--flat body:list)))


(defmacro ecf (&rest body)
  "BODY から ECF記法を除外して評価."
  (declare (indent nil) (debug (&rest form)))
  (let ((ecf--body (ecf--deep body))) `(progn ,@ecf--body)))


(defun ecf--edebug ()
  (interactive)
  (ecf {(8 9):dummy} (let ((foo (list 1 2)) {(1 2): NS1})
                       (prog1
                           (reverse foo) {(2 1):NS2}
                           (message "bar") {"bar":NS3}) {(2 1):NS4}) {(2 1):NS5}))

(provide 'ecf)

;;; ecf.el ends here
