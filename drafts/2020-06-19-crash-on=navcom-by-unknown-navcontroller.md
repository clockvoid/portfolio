---
title: AndroidXのNavigation Architecuter Componentで出る`destination is unknown for this navcontroller`について
github-link: drafts/2020-06-19-crash-on=navcom-by-unknown-navcontroller.md
summary: |
  AndroidXのNavigation Architecture Componentは画面遷移をかんたんに記述することができるという意味で，非常に優秀なライブラリですが，汎用的に作る必要があるからか，構造的に回避不能なクラッシュを引き起こすことがあります．
  本記事では，NavComを使っていると遭遇する，そういった問題の一つについてNavComのコードを読みながら解説します．
---
