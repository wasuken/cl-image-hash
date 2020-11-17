# ahash memo

[同一画像を判定するためのハッシュ化アルゴリズム - Qiita](https://qiita.com/mamo3gr/items/b93545a0346d8731f03c#average-hash-ahash)

下記の項目はすべて上記のpageのもの。

## 画像を8x8 pixels に縮小する。

* 画像Dataってどうやってbytesにするの...?

opticlのread-png-fileで変換できた。

* 縮小ってどうやるの...?

convertを使います。

## さらにグレースケールに変換する。

*グレースケールって何...?

convertを使います。

## 画素値の平均を求める。

できそう

## 8x8 pixels の各画素に対し、平均値よりも高いか低いかで2値化 (0 or 1) する。

いけそう。

## 2値のシーケンスについて、ラスタスキャン順など何らかの順で一列にし、64bitのハッシュを得る。

* ラスタスキャンって何?

一列はflattenみたいな関数で変換すればいけそう。

## 参考Site

[PNG ファイルフォーマット](https://www.setsuki.com/hsp/ext/png.htm)

[RFC 2083 - File](https://tools.ietf.org/html/rfc2083)

[同一画像を判定するためのハッシュ化アルゴリズム - Qiita](https://qiita.com/mamo3gr/items/b93545a0346d8731f03c#average-hash-ahash)
