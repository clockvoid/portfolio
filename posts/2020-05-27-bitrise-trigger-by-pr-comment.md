---
title: GitHubのPRのコメントをトリガーにしてBitriseのビルドを動かす
updated-date: 2020-05-28
github-link: posts/2020-05-27-bitrise-trigger-by-pr-comment.md
summary: |
  BitriseはGUIでワークフローの設定ができるなど，モバイル開発のCIツールとしては非常に優秀です（個人調べ）．
  しかし，GitHubのPRのコメントをトリガーとしてビルドを行うことができません．
  本記事では，この問題をGitHub Actionsを使って強引に解決した話を書きます．
---

# 前提
皆さんはモバイル開発時のCIは何を使っていますか？
僕は最近は主にBitriseを使っています．BitriseはGUIでプロジェクトの設定からワークフローの設定まで行うことができ，手軽です．しかも，ワークフローの中でシェルスクリプトも動かすことができるため，柔軟でもあります．自分でステップを作ることもできます．

ここまで来ると全く死角がないように感じますが，Jenkinsを使っていたときにはできていた，GitHubのPRにコメントをするとビルドやテストをしてくれるアレができません．
要するに，PRのコメントをトリガーに設定することができません．これは2018年の5月に投稿された[こちらのスレッド](https://discuss.bitrise.io/t/add-triggers-based-on-github-pr-labels/5107/10)でも議論されていますが，2020年5月27日現在でも解決していません．スレッドの最後では2020年5月13日に

> Any ETA for this feature? I think its been 2 years.

という投稿がありますが，誰も反応していません．可愛そうです．

僕は今回，Bitriseですべてを行うのではなく，GitHub Actionsを使って無理やりこの問題を解決しようと試みました．
なお，今回はGitHub Actionsを使うことにしましたが，実際にはGitHub ActionsでもPRのコメントをトリガーとしてワークフローを動かすのは面倒くさいです．

実はこの試みには[元ネタ](https://inside.pixiv.blog/kwzr/6190)があります．この元ネタではGitHubのWebHookを通じてGASからBitriseのAPIを叩いていますが，~~これを行うためにはGASが動くサーバが必要となってきます．~~（GASはサーバなしでもWebHookを処理できるようです．詳細は[こちら](https://twitter.com/shiita_0903/status/1265990870437736449)．めちゃくちゃ悲しいですが，@shiita_0903さん，ありがとうございました．）基本的にはGCPでよいのですが，GCPは当方が持っているリソース的に使うことが難しかったので，GitHub Actionsを使うことにしました．GitHub ActionsはGitHubが提供している機能だし簡単だろう，とたかをくくっていたところとても面倒くさかったのでまとめることにしました．

# GitHub ActionsでPRコメントをトリガーとしてワークフローを動かす
まずはGitHub Actionsを使ってPRのコメントをトリガーとして何らかのワークフローを動かすことを考えます．

詳細は省きますが，GitHub Actionsは`on: `キーワードを使用してトリガーを設定することが可能です．基本的にはこのトリガーはORで結合されるため，`on: `に書いた条件のいずれかが適合した場合にワークフローが動きます．

トリガーは[このページ](https://help.github.com/en/actions/reference/events-that-trigger-workflows)に詳しく記載されています．この中で，今回の目的に合致しそうなトリガーとしては
`pull_request_review_comment`がありますが，実際にはこれでは今回の目的としては不足です．このイベントはPRのdiffコメントの投稿を示すもので，diffに対する何らかのコメントを行う必要が出てきてしまい，意味のないdiffのレビューコメントが増えてしまうので運用として適切ではありません．

実際に今回の目的から採用すべきなのは`issue_comment`です．Pull RequestのコメントをトリガーとするのにIssueのコメントのイベントをとってどうするんだ，という感じがしますが，どうやらPull RequestのコメントはIssueコメントと内部的には同じものみたいです．今回はこのイベントのトリガーをキャッチし，`job: `の方でフィルタします．以下にサンプルを示します．

```yaml
on:
  issue_comment:
    type: [ created ]

jobs:
  build:
    if: contains(github.event.comment.html_url, '/pull/') && contains(github.event.comment.body, 'please build')
    runs-on: ubuntu-latest
```

まず，`on: `に`issue_comment`と，`type`として`created`を指定しておきます．これでコメント作成イベントが発火した際にこのワークフローがトリガーされるようになります．
続いて，`build:`ブロックのはじめに`if:`ブロックを書き，ここでPull Requestへのコメント投稿であり，コメントの内容が`please build`となっている場合のみ動くようにフィルタを設定しています．
これが設定されていると，条件に合致しなかった場合は下図のように「This check was skipped」となります．

![](https://i.imgur.com/C12SCqu.png)

なお，`if:`ブロックの中で`github.event.comment.<hoge>`というものが使用されていますが，これはGitHub Actionsのすべてのワークフロー内で使用可能なワークフローの起動条件等を取得できる環境変数です．詳細は[こちら](https://help.github.com/en/actions/reference/context-and-expression-syntax-for-github-actions#contexts)に記載があります．

これでこの下の`steps:`に設定したワークフローがPRのコメントをトリガーにして動くようになりました．

# GitHub ActionsからBitrise APIを叩いてワークフローを開始させる
BitriseのAPIはcurlから叩きます．必要な認証情報は，`APP SLUG`と`BUILD TRIGGER TOKEN`をcurlで使用するので，予めGitHubレポジトリのSecretに設定しておきます．設定の仕方は[こちら](https://help.github.com/en/actions/configuring-and-managing-workflows/creating-and-storing-encrypted-secrets)を御覧ください．それぞれのトークンは下図に示すようにBitriseのCodeタブから確認できます．

![](https://i.imgur.com/XkFsUHg.png)

Bitriseのワークフローをトリガーするためには，

- Headブランチ名
- Baseブランチ名
- BitriseのワークフローID
- Pull Request番号
- レポジトリのURL
- コミットハッシュ

が必要なので，これらのうち，上述したGitHub ActionsのContextから取得できないものは頑張ってワークフロー内で取得する必要があります．

この中だと2つのブランチ名とコミットハッシュはContextから取得することができません．一応`github.head_ref`と`github.base_ref`というContextが存在しますが，これらは`pull_request`イベントで発火した場合のみ使用可能になるため，今回は使用できません．また，コミットハッシュについても`github.sha`というContextが存在しますが，これはワークフローが入っているブランチ（=masterブランチ）の最新のコミットハッシュが入ってきてしまうため，Bitriseが必要とする，PRのブランチの最新のコミットハッシュを取るためには工夫が必要です．

そこで，以下のような環境変数を設定します．

```yaml
- name: get upstream branch
  id: upstreambranch
  run: |
    echo "::set-output name=branchname::$(curl -v -H "Accept: application/vnd.github.sailor-v-preview+json" -u ${{ secrets.PAT }} ${{ github.event.issue.pull_request.url }} | jq '.head.ref' | sed 's/\"//g')"

- name: get base branch
  id: basebranch
  run: |
    echo "::set-output name=branchname::$(curl -v -H "Accept: application/vnd.github.sailor-v-preview+json" -u ${{ secrets.PAT }} ${{ github.event.issue.pull_request.url }} | jq '.base.ref' | sed 's/\"//g')"

- name: get commit hash
  id: commithash
  run: |
    echo "::set-output name=hash::$(curl -v -H "Accept: application/vnd.github.sailor-v-preview+json" -u ${{ secrets.PAT }} ${{ github.event.issue.pull_request.url }} | jq '.head.sha' | sed 's/\"//g')"
```

GitHubのAPIを使用してPull Requestの情報が入っているJsonを受け取り，`jq`コマンドで必要な情報を取得します．使用しているGitHubのAPIについては[こちら](https://developer.github.com/v3/pulls/)を御覧ください．また，`PAT`というSecretを使っていますが，これはPersonal Authorize Tokenです．予め発行してSecretに追加しておいてください．

この環境変数を使用する際には，以下のようにします．

```bash
steps.upstreambranch.outputs.branchname
```

ここではGitHub ActionsのActionのoutput parameterを設定することにより，コマンドで取得した値をとってきています．詳細については[ドキュメント](https://help.github.com/en/actions/reference/workflow-commands-for-github-actions#setting-an-output-parameter)を御覧ください．

続いて，取得したデータを使用し，Bitrise APIを叩きます．

```bash
curl -X POST -H "Content-Type: application/json" \\
-d '{ \\
  "hook_info": { \\
    "type": "bitrise", \\
    "build_trigger_token": "${{ secrets.BUILD_TRIGGER_TOKEN }}" \\
  }, \\
  "build_params": { \\
    "branch": "${{ steps.upstreambranch.outputs.branchname }}", \\
    "workflow_id": "primary", \\
    "branch_dest": "${{ steps.basebranch.outputs.branchname }}", \\
    "pull_request_id": "${{ github.event.issue.number }}", \\
    "pull_request_repository_url": "https://github.com/${{ github.repository }}.git", \\
    "pull_request_merge_branch": "pull/${{ github.event.issue.number }}/merge", \\
    "pull_request_head_branch": "pull/${{ github.event.issue.number }}/head", \\
    "commit_hash": "${{ steps.commithash.outputs.hash }}" \\
  }, \\
  "triggered_by": "curl" \\
}' \\
https://app.bitrise.io/app/${{ secrets.APP_SLUG }}/build/start.json
```

Jsonを送っているので，Jsonの部分だけをわかりやすいようにインデントを入れています．また，上述した`APP SLUG`と`BUILD TRIGGER TOKEN`はそれぞれ`APP_SLUG`と`BUILD_TRIGGER_TOKEN`というSecretにしています．

使用するワークフロー名は`workflow_id`にて指定しています．`primary`を自分が実行したいワークフロー名に変えてください．

これでAPIを叩く事もできるようになりました．あとはすべてをまとめてYamlファイルにするだけです

# 使い方
まずは上述したフローで作成したYamlファイルの全体像を示します．

```yaml
name: CI

on:
  issue_comment:
    type: [ created ]

jobs:
  build:
    if: contains(github.event.comment.html_url, '/pull/') && contains(github.event.comment.body, 'build please')
    runs-on: ubuntu-latest

    steps:
      - name: Dump GitHub context
        env:
          GITHUB_CONTEXT: ${{ toJson(github) }}
        run: echo "$GITHUB_CONTEXT"

      - name: get pullrequest url
        run: |
          echo ${{ github.event.issue.pull_request.url }}

      - name: get upstream branch
        id: upstreambranch
        run: |
          echo "::set-output name=branchname::$(curl -v -H "Accept: application/vnd.github.sailor-v-preview+json" -u ${{ secrets.PAT }} ${{ github.event.issue.pull_request.url }} | jq '.head.ref' | sed 's/\"//g')"

      - name: get base branch
        id: basebranch
        run: |
          echo "::set-output name=branchname::$(curl -v -H "Accept: application/vnd.github.sailor-v-preview+json" -u ${{ secrets.PAT }} ${{ github.event.issue.pull_request.url }} | jq '.base.ref' | sed 's/\"//g')"

      - name: get commit hash
        id: commithash
        run: |
          echo "::set-output name=hash::$(curl -v -H "Accept: application/vnd.github.sailor-v-preview+json" -u ${{ secrets.PAT }} ${{ github.event.issue.pull_request.url }} | jq '.head.sha' | sed 's/\"//g')"

      - name: make bitrise build
        run: |
          curl -X POST -H "Content-Type: application/json" -d '{"hook_info": {"type": "bitrise","build_trigger_token": "${{ secrets.BUILD_TRIGGER_TOKEN }}" }, "build_params": { "branch": "${{ steps.upstreambranch.outputs.branchname }}", "workflow_id": "primary", "branch_dest": "${{ steps.basebranch.outputs.branchname }}", "pull_request_id": "${{ github.event.issue.number }}", "pull_request_repository_url": "https://github.com/${{ github.repository }}.git", "pull_request_merge_branch": "pull/${{ github.event.issue.number }}/merge", "pull_request_head_branch": "pull/${{ github.event.issue.number }}/head", "commit_hash": "${{ steps.commithash.outputs.hash }}" }, "triggered_by": "curl"}' https://app.bitrise.io/app/${{ secrets.APP_SLUG }}/build/start.json
```

このYamlファイルを`.github/workflows/`フォルダに入れ，masterブランチにマージします．masterマージされている状態でなければ，なぜかトリガーが動きませんでした．

masterマージされている状態でPRに設定したコメントを投稿すると，Bitriseのビルドが走ります．

![](https://i.imgur.com/OrB6NrO.png)

# まとめ
全体的に詳しく説明したつもりですが，自分の状況に合わせるためには

- Bitriseの認証情報のためのSecret
- Bitriseワークフロー名
- Personal Authorize Token
- トリガーするキーワード

を変更する必要があります．

正直言ってBitriseにはPRのコメントのトリガーを追加してほしいですし，GitHub Actionsももう少し使い勝手が良かったら嬉しいですが，仕方ないです．

似たような構成で使っている方は是非参考にしていただけると嬉しく思います．

# 参考文献
- [https://github.community/t/trigger-action-via-pull-request-message/17630/12](https://github.community/t/trigger-action-via-pull-request-message/17630/12)

