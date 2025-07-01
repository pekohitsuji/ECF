;;; -*- lexical-binding: t; coding: utf-8-unix; mode: emacs-lisp; -*-
;;; ecf.el --- Edebug ChaP Form.

;;; Commentary:
;;
;;      ecf ECF記法を埋め込んだ式をそのまま実行できるマクロ
;;          マクロなので Edebug で ecf 内部の式を追うことはできない
;;
;;      ecf--grep
;;          ECF記法をバッファに出力する Edebug 追跡をした後、
;;          バッファ内の評価値を検索する
;;
;;      ecf--instrument
;;          関数を Edebug に計装する
;;
;;      ecf--instrumented-p
;;          関数が Edebug に計装されているか判定
;;
;;      ecf--instrumented
;;          Edebug に計装された関数の一覧を返す.
;;
;;      ecf--table, ecf--echo-table
;;          関数の状態 (軽装されているか？ 関数の中身) を
;;          Markdown の表で出力する
;;
;;      ecf--init
;;          ECF で記述された Edebug 追跡の前準備
;;
;;      ecf--done
;;          ECF で記述された Edebug 追跡後の片付け

;;; Code:

(require 'edebug)

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


(defun ecf--grep (regex &optional buf-or-name)
  "Edebug の評価結果 (ECF v1.1 形式) の評価値を grep する."
  (interactive "XRegex: ")
  (let ((buf-or-name (or buf-or-name "*ECF*"))
        (ext "| +\\([[:digit:]]+\\) +| +\\(.+\\) +|")
        (res nil))
    (message "ecf--grep: regex=%S" regex)
    (with-current-buffer buf-or-name
      (goto-char (point-min))
      (while (not (eobp))
        (let ((str (buffer-substring-no-properties (pos-bol) (pos-eol))))
          (if (string-match ext str) ; ECF の出力
              (let ((num (match-string 1 str))
                    (val (match-string 2 str)))
                (if (string-match-p regex val) ; 評価値のマッチング
                    (setq res ; 正規表現にマッチしたものを蓄積
                      (cons (cons (string-to-number num) val) res))))))
        (forward-line))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((str (buffer-substring-no-properties (pos-bol) (pos-eol))))
          (if (string-match ext str)
              (let ((num (match-string 1 str))
                    (val (match-string 2 str)))
                (dolist (kv res)
                  (if (string= val (format "$%d" (car kv))) ; 同じ評価値
                      (setq res
                        (cons (cons (string-to-number num) val) res)))))))
        (forward-line))
      (setq res (seq-sort (lambda (a b) (<= (car a) (car b))) res))) ; 番号順
    (when (called-interactively-p)
      (dolist (kv res)
        (message "ecf--grep: %4d %s" (car kv) (cdr kv))))
    res))

(defun ecf--echo-vars (&optional phase)
  "オプション変数等と値を出力. edebug をロードしてなくても使える."
  (interactive)
  (let ((offset (if phase 2 0)))
    (if phase (message "phase: %s" phase))
    (my/message--var offset edebug-active)
    (my/message--var offset edebug-save-windows)
    (my/message--var offset edebug-save-displayed-buffer-points)
    (my/message--var offset edebug-new-definition-function)
    (my/message--var offset edebug-after-instrumentation-function)
    (my/message--var offset edebug-behavior-alist)
    (my/message--var offset edebug-setup-hook)
    (my/message--var offset edebug-all-defs)
    (my/message--var offset edebug-all-forms)
    (my/message--var offset edebug-eval-macro-args)
    (my/message--var offset edebug-initial-mode)
    (my/message--var offset edebug-trace)
    (my/message--var offset edebug-test-coverage)
    (my/message--var offset edebug-unwrap-results)
    (my/message--var offset edebug-global-break-condition)
    (my/message--var offset edebug-on-error)
    (my/message--var offset edebug-on-quit)
    (my/message--var offset debug-on-error)
    (my/message--var offset debug-on-quit)
    (my/message--var offset inhibit-message))
  nil)

;;; before アドバイスしたい実行モードのリスト ************************
(setq ecf--advice-before-list
  '(edebug-step-mode    ; SPC
    edebug-next-mode    ; n
    edebug-forward-sexp ; f
    edebug-step-out     ; o
    edebug-step-in))    ; i


;;; instrument したい関数のリスト ************************************
(setq ecf--instrument-list ; 例: 補完を追う
  '(minibuffer-complete
    completion-in-region
    completion-list-mode
    completion-all-completions
    completion-pcm--all-completions
    with-output-to-temp-buffer
    display-buffer
    display-completion-list))

(defun ecf--instrument-list ()
  "変数 ecf--instrument-list の関数を Edebug に instrument する."
  (interactive)
  (dolist (func:sym ecf--instrument-list)
    (edebug-instrument-function func:sym)))

;;;
;;; ツール機能
;;;
(defun ecf--text-to-name (text)
  "関数等の定義文字列 TEXT から名前を推測."
  (cond
   ((string-match "^[ \t]*(defun[ \t\n]+\\([^ \t]+\\)" text)
    (match-string 1 text))
   ((string-match "^[ \t]*(defmacro[ \t\n]+\\([^ \t]+\\)" text)
    (match-string 1 text))
   ((string-match "^[ \t]*(\\([^ \t]+\\)[ \t\n]+" text)
    (match-string 1 text))))


(defun ecf--instrument (&optional func:sym)
  "ポイント付近の関数シンボルから、関数を instrument する."
  (interactive)
  (if (and func:sym (fboundp 'edebug-instrument-function))
      (edebug-instrument-function func:sym)
    (call-interactively 'xref-find-definitions)
    (eval-defun t) ; instrument !!!
    (call-interactively 'xref-go-back)))


(defun ecf--instrumented-p (func:sym)
  "FUNC:SYM が Edebug に instrument していれば非nil を返す."
  (let ((cap "ecf--instrumented-p"))
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


(defun ecf--instrumented ()
  "Edebug に instrument された全ての関数シンボルのリストを返す.
interactive に呼んだときは *Messages* 出力もする."
  (interactive)
  (let ((res nil) (name "ecf--instrumented"))
    (mapatoms
     (lambda (sym)
       (if (ecf--instrumented-p sym) (setq res (cons sym res)))))
    (if (called-interactively-p)
        (if (null res)
            (message "%s: instrumented function not found" name)
          (dolist (sym res) (message "%s: %s" name sym))))
    res))


(defun ecf--echo-stream (fmt &rest args)
  (princ (apply 'format fmt args))
  (terpri))


(defun ecf--echo-insert (fmt &rest args)
  (insert (apply 'format fmt args))
  (insert "\n"))


(defun ecf--echo-table (&optional echo-func func:sym-list)
  "関数シンボルのリスト FUNC:SYM-LIST の要素を調べ、markdown 表を作る.
FUNC:SYM-LIST が省略または nil のとき (ecf--instrumented) を使う."
  (let ((echo-func     (or echo-func     'ecf--echo-stream))
        (func:sym-list (or func:sym-list (ecf--instrumented))))
    (if (null func:sym-list)
        (message "ecf--echo-table: no functions")
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
                 (mark (if (ecf--instrumented-p func:sym) 'o 'x))
               ;;; 関数の文字列表現 (byte-code-function に対応)
                 (s (escape-control-chars (format "%S" f)))
               ;;; 文字列の切り詰め
                 (str (if (<= (string-width s) width)
                          (truncate-string-to-width s width)
                        (concat
                         (truncate-string-to-width s (- width 1 ellipsis))
                         " " (truncate-string-ellipsis))))
                 (fmt "| %s | %s | %s | %s | %s |"))
            (funcall echo-func fmt func:sym type file mark str)))))))


(defun ecf--table (&optional prefix)
  "関数の情報を表に出力.

| PREFIX | バッファ     | 対象の関数 |
|:-------|:-------------|:-----------|
| なし   | \*Messages\* | リスト変数 ecf--instrument-list |
| C-u    | \*Messages\* | (ecf--instrumented) |
| C-u 0  | \*TEMP\*     | リスト変数 ecf--instrument-list |
| C-u 1  | \*TEMP\*     | (ecf--instrumented) |
| C-u 2  | \*TEXT\*     | リスト変数 ecf--instrument-list |
| C-u 3  | \*TEXT\*     | (ecf--instrumented) |

注意:
  拙作 my/buffer パッケージ show マクロ使用"
  (interactive "P")
  (let ((sans-control "*TEMP*") (with-control "*TEXT*"))
    (cond
     ((null prefix)
      (ecf--echo-table 'message ecf--instrument-list))
     ((and (consp prefix) (numberp (car prefix)) (= (car prefix) 4))
      (ecf--echo-table 'message))
     ((and (numberp prefix) (= prefix 0))
      (with-output-to-temp-buffer sans-control
        (ecf--echo-table 'ecf--echo-stream
                               ecf--instrument-list)))
     ((and (numberp prefix) (= prefix 1))
      (with-output-to-temp-buffer sans-control
        (ecf--echo-table 'ecf--echo-stream)))
     ((and (numberp prefix) (= prefix 2))
      (show with-control 'markdown-mode
        (ecf--echo-table 'ecf--echo-insert
                               ecf--instrument-list)))
     ((and (numberp prefix) (= prefix 3))
      (show with-control 'text-mode
        (ecf--echo-table 'ecf--echo-insert)))
     (t (message "ecf--table: unknown prefix %S" prefix)))))


(defun ecf--ref (num)
  "ECF 番号 NUM から ECF の参照用シンボルを返す."
  (make-symbol (format "$%d" num)))

(defun ecf--ref-p (sym)
  "ECF の参照用シンボルであれば非nil (ECF参照番号) を返す."
  (if (symbolp sym)
      (let ((str (symbol-name sym)))
        (if (string-match "^\\$\\([[:digit:]]+\\)$" str)
            (string-to-number (match-string 1 str))))))


(let ((ecf-alist   nil)  ; 全評価結果の ECF 記録
      (ecf-num       0)  ; 前回出力した ECF 番号
      (ecf-buf "*ECF*")  ; 出力バッファ名
      (funcs       nil)  ; ecf--enter した関数
      (ecf-head    nil)  ; 出力したｿｰｽの先頭ﾎﾟｲﾝﾄ
      (pos-alist   nil)  ; 関数毎に評価のﾎﾟｲﾝﾄ(位置)を記録する
      (code-window nil)  ; ｿｰｽｺｰﾄﾞｳｲﾝﾄﾞｳ
      (code-head   nil)  ; ｿｰｽｺｰﾄﾞﾊﾞｯﾌｧの関数の先頭ﾎﾟｲﾝﾄ
      (code-text   nil)  ; Edebug がｴﾝﾀｰした関数の docstring 抜き文字列
      (len-doc     nil)  ; docstring の長さ
      (depth         0)  ; 評価の深さ
      (counter       0)) ; ECF番号

  (defun ecf--insert ()
    "ECF を抽出したソースに埋め込む."
    (interactive)
    (message "ecf--insert: dive")
    (show ecf-buf 'markdown-mode
      (dolist (pos-ecf (sort pos-alist (lambda (a b) (> (car a) (car b)))))
        (goto-char (car pos-ecf))
        (insert (format " {:%s}" (cdr pos-ecf))))
      (setq pos-alist nil))
    (message "ecf--insert: done"))

  (defun ecf--insert-code ()
    "追っている関数が変わったら、関数のソースを出力"
    (let* ((buf  (window-buffer code-window))
           (file (buffer-file-name buf))
           (base (file-name-nondirectory file))
           (new  (my/treesit--top-text '(4) 'elisp buf))) ; docstring 除去
      (when (or (null code-text)                          ; 初回である
                (not (string= code-text new)))            ; 関数が変わった
        (when (not (null code-text)) (ecf--insert))       ; 初回ではない
          ;;; ソース出力
        (setq code-head (my/treesit--top-pos 'elisp buf))
        (setq len-doc  (- (length (my/treesit--top-text nil 'elisp buf))
                          (length new)))
        (setq code-text new)
        (show ecf-buf 'markdown-mode
          (let ((name (ecf--text-to-name code-text)))
            (goto-char (point-max))
            (insert (format "\n## %s\n\n" name)) ; 関数名
            ;;; HTML 開コードブロック
            ;; (insert "<details open>")
            ;; (insert "<summary>code in ")
            ;; (insert "<a href=\"" file "\">" base "</a>")
            ;; (insert "</summary>\n")
            ;; (insert "<pre><code>")
            ;;; Markdown 開コードブロック
            (insert (format "code in [%s](%s)\n\n" base file))
            (insert "```elisp\n")
            ;;; コード
            (setq ecf-head (point))     ; 位置を記録
            (insert code-text "\n")          ; 定義の出力
            ;;; HTML 閉コードブロック
            ;; (insert "</code></pre>\n")
            ;; (insert "</details>\n\n")
            ;;; Markdown 閉コードブロック
            (insert "```\n\n")
            ;;; 値のヘッダ
            (insert "| num. | value  |\n")
            (insert "|-----:|:-------|\n")))))
    ;;; always
    (setq pos-alist
      (cons (cons (+ (- (point) code-head len-doc) ecf-head) counter)
        pos-alist))
    (show ecf-buf 'markdown-mode
      (goto-char (point-max))
      (let ((x (car ecf-alist)))
        (insert (format "| %4s | %s |\n"
                  (car x)
                  (if (ecf--ref-p (cdr x)) (cdr x)
                    (escape-control-chars (format "%S" (cdr x)))))))))

  (defun ecf--advice-before ()
    "コマンド直前のアドバイス."
    (unless code-window (setq code-window (selected-window)))
    (when (and ecf-alist (< ecf-num (caar ecf-alist)))
      (ecf--insert-code)
      (setq ecf-num (caar ecf-alist))))

  ;;; edebug.el LINE: 3733 を tinker
  (defun edebug-compute-previous-result (previous-value)
    "評価値出力をすり替えた edebug-compute-previous-result."
    (if edebug-unwrap-results
        (setq previous-value (edebug-unwrap* previous-value)))
    (setq edebug-previous-result
      (let ((name "ecf--compute-previous-result-ecf")
            (val (escape-control-chars (format "%S" previous-value)))
            (hit (rassoc previous-value (reverse ecf-alist)))) ; 同じ値?
        
        (setq counter (+ 1 counter))
        ;; (ecf--insert-code)
        (cond
         ((or (not hit) (booleanp previous-value) (numberp  previous-value))
          (setq ecf-alist
            (cons (cons counter previous-value) ecf-alist))
          (format "ECF %s: %s\n" counter
                  (escape-control-chars (format "%S" previous-value))))
         (hit
          (setq ecf-alist
            (cons (cons counter (ecf--ref (car hit))) ecf-alist))
          (format "ECF %s: $%s\n" counter (car hit) counter))))))

  nil)


(defun ecf--init ()
  "Edebug による追跡前の準備. ウインドウ整理や instrument 等をまとめて行う."
  (interactive)
  (setq edebug-on-error t)
  ;; コレをしないと *ECF* が表示されない. 記録はされる.
  (setq edebug-save-windows nil)   ; ウインドウ構成を記録しない
  (setq inhibit-message t)         ; エコーエリアの出力抑制
  (my/window--buffer-tidy-edebug)  ; ウインドウ整理
  (erase-messages '(4))
  (dolist (cmd ecf--advice-before-list) ; before アドバイス追加
    (advice-add cmd :before 'ecf--advice-before))
  (ecf--instrument-list)           ; list に登録した関数を instrument
  (call-interactively 'ecf--instrumented) ; instrument された一覧
  (ecf--echo-vars)                 ; オプション変数を出力
  t)


(defun ecf--done ()
  "Edebug による追跡後の片付け."
  (interactive)
  (call-interactively 'edebug-remove-instrumentation) ; 全 instrument 削除
  (dolist (cmd ecf--advice-before-list)               ; アドバイス削除
    (advice-remove cmd 'ecf--advice-before))
  (ecf--insert)
  (setq inhibit-message nil)                          ; 出力抑制を解除
  (ecf--echo-vars)                                    ; オプション変数を出力
  t)


(provide 'ecf)

;;; ecf.el ends here
