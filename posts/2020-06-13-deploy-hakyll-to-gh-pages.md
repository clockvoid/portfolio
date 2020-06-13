---
title: Hakyllで作ったサイトをGitHub Pagesにデプロイする
summary: |
  HakyllはHaskell製の静的サイトジェネレータです．
  よくWeb上に転がっているCircle CIでのビルドではうまく行かなかったので，GitHub Actionsを使ってデプロイを行う方法を示します．
---

# Circle CIで何が起こったか
私はこのブログをHakyllを使って作っています．HakyllはHaskell製の静的サイトジェネレータですが，初期ビルドに非常に時間を要します．
例えば，私が今使っているMacBook Pro 13inch 2018（Core i7，16GB Memory）では15分ほどを要します．
これは，Hakyllが依存するライブラリが非常に多いということに加え，今回使用したビルドツールのStackでは依存関係がバイナリで降ってこないため，すべてをビルドする必要があるからです．

このようなものをCircle CIのFreeプランでビルドしようとすると，OOMします．
実際に，私のサイトをCircle CIでデプロイしようとした際，OOMに悩まされました．初期ビルドが通らなければ，キャッシュもされず，永遠にビルドが通らない状況に陥ります．

Circle CIのFreeプランではメモリ4GB，2CPUのコンテナが使用できますが，これでは足りないということなのでしょう．

そこで，私はGitHub Actionsに目をつけました．これならば比較的新しい上に無料プランでも一ヶ月あたり2,000時間分の枠が用意されているため，比較的ラフに使用することができるはずです．

# GitHub Actions
[GitHub ActionsのBillingページ](https://help.github.com/en/github/setting-up-and-managing-billing-and-payments-on-github/about-billing-for-github-actions)に行くとフリープランでは一ヶ月あたり500MBのキャッシュ用のストレージが用意されているということになっています．
しかし，搭載しているメモリがどれくらいの量なのか定かではありません．Circle CIでは2GBでも足りなかったみたいなので，殆どのCIの無料版ではビルドは通らないと思っていましたが，試しにGitHub Actionsでビルドしてみたところ，[41分かかって通りました](https://github.com/clockvoid/portfolio/actions/runs/107149520)．41分というと非常に長いように感じますが，この時間の殆どは依存関係の解決に費やされているため，依存関係がキャッシュされてしまえば次からは4〜5分ほどで通るようになります．

ただし，GitHub PagesとGitHub Actionsは両方ともGitHubの製品なのでかなり簡単にデプロイできるような機能が備わっているものかと思いきや，そんなことはありませんでした．

## 今回ビルドするプロジェクトの構成
今回は今閲覧されているブログのビルドを行おうとしています．

このブログのソースコードは[こちら](https://github.com/clockvoid/portfolio)に上がっていますので，そちらも合わせて御覧ください．

さて，このプロジェクトではブログを作っていますが，GitHubのLanguageのところを見ても明る通り，Haskellが98%，CSSが2%という意味のわからない構成になっています．
これには

1. LucidというHaskellの言語内DSLでHTMLを生成するものを使用することでHTMLの記述が0
2. CSSフレームワークとしてBulmaを使用し，テーマの機能などを使用するためににSassを使用した

という理由があります．なお，余談として，実はHakyllにはSassをビルドしてCSSにまとめてデプロイするためのライブラリもあるのですが，こちらは使用せず，素直にnpmを使用しました．
理由としては，Sassのビルドを行うライブラリの依存関係がMac上のNeovimのLanguageClientでは読み込めず，開発が難航してしまうためです．

このような事情があるため，ビルドを行うためには，Stackとnpmが必要になります．

## GitHub Actionsの設定
まず，GitHubのプロジェクトページの一番上にあるタブからActionsタブをクリックします．

![](https://i.imgur.com/eG9gNcCh.png)

Actionタブを開くと，GitHubのレポジトリの主要言語から使用するワークフローの雛形を作ってくれます．とても便利です．

上述したとおり，今回はStackとnpmが必要になるため，GitHub ActionsのActionsとして，以下のものを使用しました．

- `actions/setup-haskell`
- `actions/setup-node`

これらを使用するのみでStackとnpmがそれぞれインストール可能です．

また，今回環境としては`ubuntu-latest`を使用しました．

ここまでの設定は以下のように記述します．

```yaml
name: Build and Deploy

on:
  push:
    branches: [ master ]

jobs:
  build:

    runs-on: ubuntu-latest

    steps:
    - uses: actions/setup-haskell@v1
      with:
        ghc-version: '8.8.2'
        cabal-version: '3.0'
        stack-version: 'latest'
    - uses: actions/setup-node@v1
      with:
        node-version: '13'
```

`name`には適当な名前を指定します．`runs-on`には先程使用すると決めた`ubuntu-latest`を指定します．また，`steps`の初めの方に`uses`で使用するアクションを指定します．アクションには設定項目があることがあり，`with`で指定します．ここではそれぞれバージョンを指定しています．なお，`actions/setup-haskell`に`caba-version`を指定していますが，StackでインストールされるCabalのバージョンが低すぎて今回のプロジェクトをビルドできないので，Cabalは別入れしています．

その他，レポジトリからソースコードを持ってくるために`actions/checkout`とキャッシュするために`actions/cache`を入れます．

```yaml
- uses: actions/checkout@v2
- name: Cache
  uses: actions/cache@v1
  env:
    cache-name: cache-stack
  with:
    path: ~/.stack
    key: ${{ runner.os }}-build-${{ env.cache-name }}-${{ hashFiles('**/*.cabal') }}-${{ hashFiles('**/stack.yaml') }}
    restore-keys: |
      ${{ runner.os }}-build-${{ env.cache-name }}-
      ${{ runner.os }}-build-
      ${{ runner.os }}-
```

cacheの方は`~/.stack`を丸々キャッシュする設定にしています．Stackでは，`~/.stack`というディレクトリにGHCから依存関係をビルドしたバイナリまで入るので，ここをキャッシュすれば十分です．
ただし，このディレクトリはGHCも入る関係から非常に大きくなるため，500MBに収まらない可能性もあり，注意が必要です．

つづいて，依存関係のインストールとブログのビルドまでできるようにする設定を記述します．

```yaml
- name: Install dependencies
  run: | 
    stack setup
    npm install
- name: Build
  run: stack build
- name: Compile site
  run: |
    stack exec html build
    stack exec site build
```

ここではいくつかのステップに分けてdepsのインストールとビルド，ブログのビルドまでを行いましたが，このくらいであれば分ける必要はないかもしれないです．

## デプロイ
ビルドまでできるようになったので，デプロイのための記述をします．

Hakyllではプロジェクトルートに`_site`というディレクトリを作成し，その中にビルドされたブログが構成されるようになっています．

今回は，この`_site`内のファイルを`gh-pages`ブランチにデプロイします．

これを行うためにはまずブログをビルドしてGitの`--orphan`オプションをつけて今までの歴史を削除したブランチを作成しておきます．

```shell
git checkout --orphan gh-pages
```

これを行うと，もとのブランチの`.gitignore`しているものを引き連れてくるため，この状態で`_site`ディレクトリが存在する状態になります．また，`.gitignore`は引き連れてこないため，引き連れてきたファイルはすべて`.gitignore`に入れておきます．

続いて，`_site`ディレクトリ内のファイルをすべてプロジェクトルートに展開します．プロジェクトルートに展開すると，ブログの部分が差分になるため，いつもどおり`git add .`してコミットしてリモートブランチの`gh-pages`ブランチにpushします．

```shell
cp -a _site/* ./
git add .
git commit -m "initial commit"
```

これで`gh-pages`ブランチの準備ができたため，次回からはここで行ったブランチの作成等はせず，後半で行った`_site`ディテクトリ内のファイルをプロジェクトルートに展開し，コミットするのみで良いです．
したがって，最終的なデプロイ用のGitHub Actionsの記述は，下記のようになります．

```yaml
- name: Deploy site
  env:
    GIT_EMAIL: ${{ secrets.GIT_EMAIL }}
    GIT_NAME: ${{ secrets.GIT_NAME }}
  run: |
    git config --local user.email "$GIT_EMAIL"
    git config --local user.name "$GIT_NAME"
    git fetch
    git checkout gh-pages
    cp -a _site/* ./
    git add .
    git commit -m "$GITHUB_REPOSITORY deploy no. $GITHUB_RUN_NUMBER"
- name: Push changes
  uses: ad-m/github-push-action@master
  with:
    github_token: ${{ secrets.GITHUB_TOKEN }}
    branch: "gh-pages"
```

一番下で行っているpushは公式のアクションが見つからなかったため，サードパーティで代用しています．

これでGitHub Actionsを使ってHakyllのブログをGitHub Pagesにデプロイする設定ができました．最終的な構成は，[こちら](https://github.com/clockvoid/portfolio/blob/master/.github/workflows/build-and-deploy.yml)を御覧ください．

# まとめ
1. Haskellのプロジェクトのビルドは時間がかかることがある
2. ビルドに時間がかかるプロジェクトはCIサービスの無料プランではビルドできないことがある
3. GitHub Actionsは無料プランでもかなり柔軟に対応できる
4. GitHub PagesへのHakyllで作ったサイトのデプロイは癖がある

