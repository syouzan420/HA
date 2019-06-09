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
- 「は」から「た」で囲まれた部分
- 現在は 有理數の四則算を實装
- 式の最初に 「まへ」 を入れると その前の式の評価結果が利用できる
- 式に名前をつけるには 「は」のすぐ前に その名前をかく
- 名前のついた式の結果は 別の式の中で使ふことができる

#### 數などの表記
1:ひ 2:ふ 3:み 4:よ 5:ゐ 6:む 7:な 8:や 9:こ 0:ろ -:き +:と \*:を /:す

#### 整數の例
- ひむな (167)
- ふろろろ (2000)
- きふみ (-23)

#### 有理數の例
- ふすみ (2 / 3) 三分の二
- よをみゐすひむ (4\*35/16)
- ふをみすよをゐすな (2\*3/4\*5/7)

#### 式の例
- は ふみ よ やろ た  -> ひろな
- は よな きころ た -> きよみ
- は ひすみ ふすみ た -> ひ
- は ふ み た は まへ ふをよ た -> ひみ
- これは ひ ふ み た -> これ む
- 上の評価の後ーー  は これ をみ た -> ひや (6\*3=18が算出される)

#### 函數の構造
- 函數定義: 「(函數名)は (引數1) (引數2) ... か (式) た」
- 函數定義2: 「(函數名)は (パターン1) か (式1) に (パターン2) か (式2) に ... た」(パターンは引數の組である)
- 函數使用: 「は (引數1) (引數2) ... (函數名) た」

#### 函數定義の例
- これは あれ それ か あれ を それ た (「あれ」と「それ」の積を求める「これ」といふ函數)
- あれは ろ か ろ に それ か それきひ あれ ふ た (引數が「ろ」のときは「ろ」を返し それ以外の數なら そこから「ひ」を引いて同じ函數に渡すといふ再歸が定義されてゐる)
- は む よ これ ひろ た (6×4をして10を加える  結果は 「みよ」(34))


### その他
- 右上の画像は テストで表示したのを 殘してゐるだけで 特に意味はない
- 記号と文字の關係は 後に變更するかもしれない(6/1變更)

### 直近の目標
- 式の中のどこにでも「まへ」を入れられるやうにする (5/25 済)
- 項目の乗算 除算を實装する (ひふ をみ -> みむ) (5/26 済)
- 引數をとる式(函數)を定義し 別の式の中で使へるやうにする (5/30 済)
- パターンマッチ 再歸を實現する (6/6)

### 直近の更新
- カーソル移動を改良した(しばらく押すと同じキー入力をする といふエディタなら普通にある動作を實装できたと思ふ)
- カーソルが左端に行つたときのスクロール処理を實装した(5/25)
- 式の評価におけるバグ(「まへ」が入つたときの不具合)を解決した(と思ふ)
- 「まへ」を項に直接含ませられるやうにした (例: まへとみ まへすふ)
- 式に名前をつけ その結果を別の式で利用できるやうにした (5/29)
- 式が引數をとれるやうにした (5/30)
- 削除時のカーソル移動の不具合や 単独の「す」(除算)が認識されてゐない問題を修正した (5/31)
- むとよ (6+4) やきみ (8-3) のやうな 加算減算のまとまりを表現できるやうにした (6/1)
- 再歸が實現できた (まだ不備があると思ふので これから慎重にコードの検証もしていきたい (6/6)
- 様々な不具合(バグ)を改善した (6/10)
