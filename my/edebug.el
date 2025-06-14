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


(defun my/edebug--instrument (&optional func:sym)
  "ポイント付近の関数シンボルから、バッファ移動、関数に instrument、戻る."
  (interactive)
  (if (null func:sym)
      (call-interactively 'xref-find-definitions)
    (xref--find-definitions (symbol-name func:sym) nil))
  (eval-defun t) ; instrument !!!
  (call-interactively 'xref-go-back))


(defun my/edebug--instrumented-p (func:sym)
  "FUNC:SYM が Edebug を instrument していれば非nil を返す."
  (let ((cap "my/edebug--instrumented-p"))
    ;; (message "%s: start %s" cap func)
    (if-let ((f (and (fboundp func:sym) (symbol-function func:sym))))
          (cond
           ((autoloadp f) nil)
           ((subrp f) nil)
           (t (let ((g (if (and (macrop f) (listp f) (eq (car f) 'macro))
                           (cdr f) f)))
                (and (or (eq (type-of g) 'interpreted-function)
                         (eq (type-of g) 'byte-code-function))
                     (consp (aref g 1))
                     (eq 'edebug-enter (caar (aref g 1))))))))))


(defun my/edebug--instrumented ()
  "Edebug が instrument された全ての関数シンボルのリストを返す.
interactive に呼んだときは *Messages* 出力もする."
  (interactive)
  (let ((res nil) (name "my/edebug--instrumented"))
    (mapatoms
     (lambda (sym)
       (if (my/edebug--instrumented-p sym) (setq res (cons sym res)))))
    (if (called-interactively-p)
        (if (null res)
            (message "%s: instrumented function not found" name)
          (dolist (sym res) (message "%s: %s" name sym))))
    res))


(defun my/edebug--echo-stream (fmt &rest args)
  (princ (apply 'format fmt args))
  (terpri))


(defun my/edebug--echo-insert (fmt &rest args)
  (insert (apply 'format fmt args))
  (insert "\n"))


(defun my/edebug--echo-table (echo-func &optional func:sym-list)
  "関数シンボルのリスト FUNC:SYM-LIST の要素を調べ、markdown 表を作る.
FUNC:SYM-LIST が省略または nil のとき (my/edebug--instrumented) を使う."
  (let ((func:sym-list (or func:sym-list (my/edebug--instrumented))))
    (funcall echo-func
     "| 名前 | type-of | ファイル | edebug-enter | symbol-function |")
    (funcall echo-func "|:-|:-|:-|:-|:-|")
    (dolist (func:sym func:sym-list)
      (if (not (fboundp func:sym))
          (funcall echo-func "| NOT FBOUNDP %s |  |  |  |  |" func:sym)
        (let* ((width 80) ; 半角を1とする表示幅
               ;;; 省略記号の表示幅
               (ellipsis (string-width (truncate-string-ellipsis)))
               (f (symbol-function func:sym)) ; 関数オブジェクト
               (type (if (macrop f) 'macro (type-of f))) ; タイプ
               (file ; 定義されたファイル名
                (my/tool--omit-path
                 (find-lisp-object-file-name func:sym f)))
               ;;; Edebug を instrument しているか？
               (mark (if (my/edebug--instrumented-p func:sym) 'o 'x))
               ;;; 関数の文字列表現 (byte-code-function に対応)
               (s (escape-control-chars (format "%S" f)))
               ;;; 文字列の切り詰め
               (str (if (<= (string-width s) width)
                        (truncate-string-to-width s width)
                      (concat
                       (truncate-string-to-width s (- width 1 ellipsis))
                       " " (truncate-string-ellipsis))))
               (fmt "| %s | %s | %s | %s | %s |"))
          (funcall echo-func fmt func:sym type file mark str))))))


;;; instrument したい関数等
(setq my/edebug--instrument-list '(minibuffer-complete
                                   display-buffer
                                   completion-in-region
                                   completion-list-mode
                                   display-completion-list
                                   completion-all-completions
                                   completion-pcm--all-completions
                                   with-output-to-temp-buffer))


(defun my/edebug--instrument-list ()
  "変数 my/edebug--instrument--list の関数に Edebug を instrument する."
  (interactive)
  (dolist (func:sym my/edebug--instrument-list)
    (my/edebug--instrument func:sym)))


;;; 拙作 my/buffer パッケージの show マクロ使用
(defun my/edebug--table (prefix)
  "関数の情報を表に出力.

PREFIX なし: sans-control
  display-buffer-alist で制御されてないバッファで stream 出力
  対象となる関数は my/edebug--instrument-list に登録されたもの

PREFIX あり: with-control
  display-buffer-alist で制御されたバッファで insert 出力

  C-u   : 対象となる関数は  my/edebug--instrument-list に登録したもの
  C-u 0 : 対象となる関数は (my/edebug--instrumented) が検知できるもの"
  (interactive "P")
  (let ((sans-control "*TEMP*") (with-control "*TEXT*"))
    (cond
     ((null prefix)
      (with-output-to-temp-buffer sans-control
        (my/edebug--echo-table 'my/edebug--echo-stream
                               my/edebug--instrument-list)))
     ((and (consp prefix) (numberp (car prefix)) (= (car prefix) 4))
      (show with-control 'text-mode
        (my/edebug--echo-table 'my/edebug--echo-insert
                               my/edebug--instrument-list)))
     ((and (numberp prefix) (= prefix 0))
      (show with-control 'text-mode
        (my/edebug--echo-table 'my/edebug--echo-insert)))
     (t (message "my/edebug--table: unknown prefix %S" prefix)))))


(defun my/edebug--dummy-func ()
  "ダミー用関数."
  (message "my/edebug--dummy-function: called"))


(defmacro my/edebug--dummy-macro (&rest body)
  "ダミー用マクロ."
  (declare (debug (body)))
  `(message "my/edebug--dummy-macro: called"))


(defun my/edebug--setup ()
  "Edebug を開始する前にウインドウの整理と必要な instrument の設定をする."
  (interactive)
  (erase-messages '(4)) ; *Messages* バッファをクリア
  (my/edebug--instrument 'my/edebug--dummy-func) ; Edebug ロードのため
  ;; Edebug の評価結果出力をより詳しいものにすり替え.
  (edebug-previous-result-string-builder-ecf)
  ;; ウインドウの整理
  (cond
   ((and (boundp 'mini-mode) mini-mode) ; mini-mode 特化
    (mini-mode-term-action (current-buffer) nil)
    (select-window mini-mode-term)
    (mini-mode-list-action "*Messages*" nil)
    (mini-mode-disp-action "*Messages*" nil)
    (mini-mode-code-action "*Messages*" nil))
   (t (dolist (w (window-list)) ; mini-mode 以外
        (unless (eq (selected-window) w)
          (with-selected-window w (switch-to-buffer "*Messages*"))))))
  ;; ウインドウ情報の出力
  (message "ﾐﾆﾊﾞｯﾌｧ %S %S"
           (minibuffer-window)
           (with-selected-window (minibuffer-window) (current-buffer)))
  (dolist (w (window-list))
    (message "%S %S" w (with-selected-window w (current-buffer))))
  ;; Edebug に instrument
  (my/edebug--instrument-list)
  (call-interactively 'my/edebug--instrumented)
  nil)


;;; Emacs 標準の評価結果出力
(setq edebug-previous-result-string-builder
  (lambda (previous-value)
	(concat "Result: "
		(edebug-safe-prin1-to-string  previous-value)
		(eval-expression-print-format previous-value))))


(defun edebug-previous-result-string-builder-ecf ()
  "より詳しい評価結果出力にすり替える. ECFをキルリングにコピー."
  (interactive)
  (let ((history-alist nil) (counter 0))
    (setq edebug-previous-result-string-builder
      (lambda (previous-value)
        (let ((hit (rassoc previous-value (reverse history-alist)))
              (str (escape-control-chars (format "%S" previous-value))))
          (setq counter (+ 1 counter))
          (setq history-alist
            (cons (cons counter previous-value) history-alist))
          (cond
           ((or (booleanp previous-value)
                (numberp previous-value)
                (not hit))
            (kill-new (format "{%s:%s}" str counter))
            (concat
             (format "RESULT %s: {%s:%s}\n" counter str counter)
             (format "  %s\n-------" str)))
           (hit
            (kill-new (format "{$%s:%s}" (car hit) counter))
            (concat
             (format "RESULT %s: {$%s:%s}\n" counter (car hit) counter)
             (format "  %s\n-------" str))))))))

  ;;; edebug.el LINE: 3733 を tinker
  (defun edebug-compute-previous-result (previous-value)
    "Edebug 公式の関数を上書き."
    (if edebug-unwrap-results
        (setq previous-value
	      (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
      (funcall edebug-previous-result-string-builder previous-value))))


(provide 'my/edebug)

;;; my/edebug.el ends here
