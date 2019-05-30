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
- 初期モードで カーソルが含まれる式 もしくはカーソルの前にあり一番近い式を評価 e
- 終了 ESC (glossにもとからある機能)

#### 式とは
-「は」から「た」で囲まれた部分
- 現在は 有理數の四則算を實装
- 式の最初に 「まへ」 を入れると その前の式の評価結果が利用できる
- 式に名前をつけるには 「は」のすぐ前に その名前をかく
- 名前のついた式の結果は 別の式の中で使ふことができる

#### 數などの表記
1:ひ 2:ふ 3:み 4:よ 5:ゐ 6:む 7:な 8:や 9:こ 0:ろ -:ん \*:と /:す

#### 整數の例
- ひむな (167)
- ふろろろ (2000)
- んふみ (-23)

#### 有理數の例
- ふすみ (2 / 3) 三分の二
- よとみゐすひむ (4\*35/16)
- ふとみすよとゐすな (2\*3/4\*5/7)

#### 式の例
- は ふみ よ やろ た  -> ひろな
- は よな んころ た -> んよみ
- は ひすみ ふすみ た -> ひ
- は ふ み た は まへ ふとよ た -> ひみ
- これは ひ ふ み た -> これ む
- 上の評価の後ーー  は これ とみ た -> ひや (6\*3=18が算出される)

### その他
- 右上の画像は テストで表示したのを 殘してゐるだけで 特に意味はない
- 記号と文字の關係は 後に變更するかもしれない

### 直近の目標
- 式の中のどこにでも「まへ」を入れられるやうにする (5/25 済)
- 項目の乗算 除算を實装する (ひふ とみ -> みむ) (5/26 済)
- 引數をとる式(函數)を定義し 別の式の中で使へるやうにする (5/30 済)

### 直近の更新
- カーソル移動を改良した(しばらく押すと同じキー入力をする といふエディタなら普通にある動作を實装できたと思ふ)
- カーソルが左端に行つたときのスクロール処理を實装した(5/25)
- 式の評価におけるバグ(「まへ」が入つたときの不具合)を解決した(と思ふ)
- 「まへ」を項に直接含ませられるやうにした (例: まへとみ まへすふ)
- 式に名前をつけ その結果を別の式で利用できるやうにした (5/29)
- 式が引數をとれるやうにした (5/30)
