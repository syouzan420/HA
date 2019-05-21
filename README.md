# HA
### 實行方法

- stack をインストール
- 適當なフォルダを作成し 中に移動

```
$ stack new ha
```
- 作成されたフォルダ ha の中へ移動
- package.yaml と stack.yaml を ここのファイルと置き換へる
- ここの imgフォルダを haフォルダ内に配置
- appフォルダ内 の Main.hs を ここのファイルと置き換へる
```
$ stack build
$ stack exec ha-exe
```

### 操作方法
- カーソルの移動 j:下 k:上 h:左 l:右
- 挿入モードへ移行 i
- 初期モードへ戻る ctrl+o
- 初期モードで 削除 x
- 挿入モードで 削除 backspace
- 初期モードで 直前の式を評価 e
- 終了 ESC (glossにもとからある機能)

#### 式とは
-「は」で囲まれた部分
- 現在は 有理數の加減算を實装

#### 數などの表記
1:ひ 2:ふ 3:み 4:よ 5:ゐ 6:む 7:な 8:や 9:こ 0:ろ -:ん \*:た /:そ

#### 整數の例
- ひむな (167)
- ふろろろ (2000)
- んふみ (-23)

#### 有理數の例
- ふそみ (2 / 3) 三分の二
- よたみゐそひむ (4\*35/16)
- ふたみそよたゐそな (2\*3/4\*5/7)

#### 式の例
- は ふみ よ やろ は  -> ひろな
- は よな んころ は -> んよみ
- は ひそみ ふそみ は -> ひ

### その他
- 右上の画像は テストで表示したのを 殘してゐるだけで 特に意味はない
- まだ左端を越へたときの処理を實装してゐない
- 記号と文字の關係は 後に變更するかもしれない
