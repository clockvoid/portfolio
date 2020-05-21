---
title: Hakyllでブログを作ってみた
summary: Hakyllでブログを作ってみたので，経緯，技術選定，辛い部分についてまとめます
github-link: posts/2020-05-20-build-blog-with-hakyll.md
---

# 経緯
最近技術ネタを上げておく場所として，Qiitaのような高尚な場所に上げる前に温めて置く場所が欲しかったのもあり，ブログを開設しようと思っていました．
しかし，普通のはてなブログなどはVimで記事を直接書くこともできない上に，基本的にはオンラインでの作業になり，ブログを書く作業そのものに対してやる気を出せず，続かないとずっと思っていました．

個人的な要求として，

- Vimで直接記事がかける
- markdownで記述できる
  - markdownの文法は個人的に好きなやつが良い
- デプロイはGitのpushコマンドで自動的に行われてほしい
- Source Codeと数式がまともに表示できる
- **無料で使える**

という要求があり，自分でつくろう，ということになりました．

# 技術選定
技術の選定にはある意味で気を使いました．

基本的には上述した要求をクリアできさえすれば良いので，そのへんに転がっている静的サイトジェネレータを使用してGitHubにpushすると適当なCIでビルドしてデプロイすれば良いと思っていました．
しかしながら，私が普段関わっているのはAndroidの開発で，使っている言語はKotlinです（昔はJavaも使ってましたが）．
そのため，型に飼いならされており，型のない言語でのプログラミングがもはやかなりつらすぎる体になってしまいました．

有名な静的サイトジェネレータといえば，最近はGatsbyとかMiddlemanとかJekillとかですが，いずれもJavaScriptとかRubyで開発するものになっています．
はじめはGatsbyで作ろうと思っており，Webの人に色々聞いたらElmのプラグインがあるからそれを使えば良いよ！と言われたので試してみたところ，GatsbyのElmのプラグインが`elm-0.19.0`に依存しており，そのバージョンがMacでもLinuxでもなぜかnpmでインストールできませんでした．

完全に絶望した僕は何故か「Haskell 静的サイトジェネレータ」でググり，適当に出てきたHakyllを選びました（気を使った話はどこいった）．

## 構成
最終的に使っている構成としては，

- Hakyll（静的サイトジェネレータ）
- Lucid（HaskellのHTML EDSL）
- Bulma（CSSフレームワーク）
- GitHub Pages（ホスティング環境）
- GitHub Actions（CI）

のような形になっています．
Lucidで生成したHTMLを自分の好きなところに入れられるような仕組みは自分で構築しています．

詳しくは[GitHub](https://github.com/clockvoid/portfolio)を御覧ください．

# Hakyllの基本的な書き方
ここで，一応Hakyllの基本的な書き方を書いておきます．

Hakyllは基本的に，`hakyll`という関数に`Rules ()`を渡すことによってサイトを作成していきます．
`Rules ()`にはルート情報やテンプレートにインジェクションする文字列などを記述します．例えば，一つの`Rule`は以下のような感じに記述します．

```haskell
index :: Rules ()
index = match indexPattern $ do
    route $ constRoute "index.html"
    compile $ do
        let indexCtx =
                constField "title" "Home" `mappend`
                defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= relativizeUrls
```

`indexPattern`にはテンプレートとして使用するhtmlファイルのパスを`Text`型で指定します．`String`ではないため，`fromRegex`などの`String -> Text`な関数を使用するか．`{-# LANGUAGE OverloadedStrings #-}`
をファイルの先頭に記述します．また，正規表現を使用して複数のテンプレートに対して同時にルールを適用することも可能です．

`rute`では実際に展開されるルートを設定できます．テンプレートそのままで使用したり，拡張子を変えたり，この例のように名前を直接指定することも可能です．

`indexCtx`ではテンプレート中に存在する`$`で囲まれた当該部分を上書きすためなどに使用される`Context`を設定しています．
テンプレートでは，`$test$`などと記述することで，コンテキストに`test`というフィールドが存在した場合に，その部分を上書きしてページを生成することができるようになっています．このテンプレートエンジンは非常に強力で．`if`や`for`を使用して分岐や繰り返しの処理も記述することが可能です．

最後に，テンプレートにコンテキストを適用したり，含まれているURLのパスを調節したりしています．

このようにして作成したいくつかの`Route ()`を`bind`でつなげていき，最終的にできた`Route ()`を`hakyll`関数に渡すことで，静的サイトジェネレータ本体を作成します．

Hakyllはこの説明を見るだけでも非常にクールですが，Pandocに内部的に依存しており，Markdownをコンパイルする機能も有しています．Markdownのコンパイルはたいてい使用するCSSフレームワークに合わなかったりして苦労しますが，PandocのHaskell実装では非常に柔軟なカスタマイズが可能なため，とても便利でした．

例えば，私はCSSフレームワークにBulmaを使用していますが，Bulmaは`h1`タグに対して`is-1`クラスが入ってないとHeader 1として認識してくれません．そこで，

```haskell
toBulmaHeading :: Block -> Block
toBulmaHeading (Header level attrs xs) = Header (level + 2) newAttrs xs
    where
        (identifier, classes, keyvals) = attrs
        newAttrs = (identifier, classes <> ["title", "is-" <> pack (show $ level + 3), "bd-anchor-title"], keyvals)
toBulmaHeading x = x

bulmaHeadingTransform :: Pandoc -> Pandoc
bulmaHeadingTransform = walk toBulmaHeading
```

このような関数を書き，これをPandocのCompilerに設定することで，吐き出すHTMLを変更できるなど，割と簡単にカスタマイズ可能です．

# 辛いところ

基本的な使い方と称してHakyllの素晴らしさを語りましたが，一応辛さも語っておきます．

まず一番つらいのは，コンパイルに使用するリソース量の多さです．
GHCはそもそも遅い気がしますが，Hakyllには大量の依存関係があり（Pandocだけでもかなり大きい），ビルドに非常に時間がかかります．
ビルドに時間とコストが掛かるということは，自動デプロイ用のCI環境にお金をかける必要があるかもしれません．
この話については，別途ブログに記述しようと思っています．

次に辛いのは，情報が割と少ないことです．
こんな構成を好む人は少ないのかもしれません．

辛いところといってもぱっと思い浮かぶのはこのくらいだと思います．

# 最後に
Hakyllとは違ってきますが，実は一番つらかったのはCSSを当てる作業です．

基本的には私はAndroidをいつも書いているので，ConstraintLayoutが恋しくなりました．Flexboxわからん．

また，なにか誤植等ございましたら，右上からこのマークダウンのGitHubのページ飛べますので，PRなどで報告いただけると非常に嬉しいです．
