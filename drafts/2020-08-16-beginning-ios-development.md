---
title: iOS開発を始めるときに読んだ資料
summary: |
  Androidエンジニアの僕がiOS開発を始めたときに読んだ資料をまとめてみます．
---

# プロジェクトのバージョン管理
iOS開発をする上ではXcodeのプロジェクトがバージョン管理した際にコンフリクトしないようにすることが重要です．

このあたりは公式がサポートしてくれないので開発者コミュニティで発達してきた歴史があり，Appleの公式のものはあまり参考にしていません．

- [DroidKaigi 2020のiOS版コード ](https://github.com/DroidKaigi/conference-app-2020/tree/master/ios-bas)
  - 基本的な形はこのレポジトリで学習できます．Kotlin Nativeの設定を除けばあまり攻めた構成にはなっていないので，参考になります．
- [xcconfig-extractor ](https://github.com/toshi0383/xcconfig-extractor)
  - いろいろなところに散らばっている`.xcconfig`をまとめていい感じに管理できるようにするやつです．
  - CocoaPodsを使用している場合，`pod install`をすると`[projectname].xcodeproj`の中の`.xcconfig`ファイルが書き換えられ，謎の差分を生むのでこれの導入は必須です．
- [XcodeGen ](https://github.com/yonaskolb/XcodeGen)
  - ymlファイルから`.xcodeproj`を生成してくれるツールです．これを使うと諸悪の根源`.xcodeproj`を`.gitignore`に入れられるのでコンフリクトを避けられます．
  - [プロジェクトファイルから`project.yml`を生成する ](https://qiita.com/hcrane/items/501100ee9abd5d3b1295)ではもともといじっていたプロジェクトからXcodeGenが使う`project.yml`を生成する方法が紹介されています．
- [チームが幸せなiOSプロジェクトの作り方 ](https://qiita.com/Alex_mht_code/items/bd421275d3fa6ab160c6)
  - ここで使っているツールの導入方法などが書かれています．
- [Swift製コマンドラインツールのパッケージ管理ツール「Mint」のセットアップ&操作方法 ](https://qiita.com/uhooi/items/6a41a623b13f6ef4ddf0)
  - 上で使っているツールは基本的にbrewから手に入りますが，プロジェクトごとの想定しているバージョンの差などを吸収するためにMintを使っていきましょう．

ここではCocoaPodsやCathageなどのツールのリンクは貼っていません．このあたりは人によって使いたいツールの好みが分かれそうでしたので，それぞれ調べてみてください．

# Interface Builder
iOS開発ではAndroidとは違い，Interface Builderと呼ばれるGUIを用いてViewを構築します．
一応コードでViewを書き換えることはできるようですが，あまり推奨されておらず，一般的にはInterface Builderで作ることが多いようです．

Interface Builderそのものの使い方は私自身は友達に教えてもらったため，あまりWebサイトは参考にしていません．GUIの使い方なので，人に直接教わったほうが早いと思います．
ただし，動画で解説されているものがいくつかありますので，YouTubeでの「Interface Builder」の検索結果と「Interface Builder AutoLayout」の検索結果を載せておきます．

- [YouTube - ios interface builder ](https://www.youtube.com/results?search_query=ios+interface+builder)
- [YouTube - ios interface builder autolayout ](https://www.youtube.com/results?search_query=ios+interface+builder+autolayout)

なお，UIKitで開発をするときはAuto Layoutというレイアウト手法がとても大切になってくるようです．iOSは旧来デバイスの画面サイズが基本的にすべて同じでしたが，最近はいろいろな画面サイズが出てきています．それに対応するためにViewを画面サイズに応じて伸び縮みできるように生成する必要があり，それを行えるのがAuto Layoutです．
基本的にはViewに対して大きさと上下左右の基準の制約を貼り，レイアウトを組む手法ですが，これも非常に独特ですので，うまく扱えるようになるには時間がかかるかもしれません．

また，AndroidのViewと同じように作り方によってパフォーマンスも変わるようです．詳細は以下の動画を御覧ください．

- [High Performance Auto Layout ](https://developer.apple.com/videos/play/wwdc2018/220)

# UIKit


