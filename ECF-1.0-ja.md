# Edebug の ちゃっぴー記法 仕様書 v1.0

<details><summary>背景</summary>
Emacs の補完機能で \*Completions\* バッファに補完候補が表示されない
バグに困った私が Edebug を使って調査し、ChatGPT と対話するうちに
「ひとまず用語と記法を定義しよう」となり、勝手に取り決めたものです.
</details>

<details><summary>正式名称</summary>
Edebug の ちゃっぴー記法 (Edebug ChaP Format 略称 ECF)
</details>

## 注意
「ちゃっぴー」とは私が ChatGPT をそう呼んでるだけです.

権威もなければ、広めたい希望もありません.
業界人の真似事みたいな遊びですから、
以下を くれぐれも本気で相手にしないよう願う.

## 目的
Edebug のステップ実行中における 式の評価状態と 
CSP (カレントストップポイント) を静的に視覚表現するための
表記法を定義する.

## 文法定義 (BNF風)
```
<ecf-expr> ::= '(' <symbol> <ecf-arg>* ')'
<ecf-arg>  ::= <symbol> | <literal> | <ecf-expr> | <ecf-evaluated>
<ecf-evaluated> ::= <symbol> '{' <value> '}'
```
| 表記      | 意味                                   |
|:----------|:---------------------------------------|
| `symbol`  | 任意の識別子 (例: `a`, `x`, `foo` 等)  |
| `literal` | リテラル (例: 数値、文字列、リスト 等) |
| `value`   | 評価結果 (例: 数値、関数、文字列 等)   |

## 表記例

| 表記例 | 意味 |
|:-------|:-----|
| `(f a b)` | Edebug ステップ実行対象の式 |
| `(f a {3} b)` | `a` は `3` に評価済、`b` は未評価、CSP は `b` の直前 |
| `(f a {3} b {4})` | `a` は `3`、`b` は `4` に評価済、CSP は `f` の返り値計算の直前 |
| `(f (g x {1}) {5} y)` | `x` は `1`、`g` は `5` を返した. `y` は未評価、CSP は `y` の直前 |

## 注意点
- マクロ展開や副作用を含む場合、評価が左から右に進むことは保証されないので
  関数呼び出しのみに使うのが適切

## 略語、用語まとめ

| 略語 | 意味 |
|:-----|:-----|
| SP   | Stop Point、Edebug のストップポイント |
| BP   | Break Point、Edebug のブレークポイント |
| CSP  | Current Stop Point、最後の `{}` 直後にあるストップポイント |
| SM   | Stop Marker、Edebug が `=>` として表示する. CSP のある行を意味する |
| FEX  | Finished Evaluation Form、評価完了式、`{}` を伴って値が決定した引数 |
| EEX  | Expected Evaluation Form、評価予定式、`{}` を持たない、まだ評価されてない引数 |
| PF   | Parent Form、親式、Edebug ステップ評価中の S式全体 |
