# Example 1 - First-order effects

ここでは、[Polysemy](https://hackage.haskell.org/package/polysemy)ライブラリの例にもある`Teletype`エフェクトクラスを例として、
heftia-effectsにおける一階のエフェクトの取り扱い方を説明します。
heftia-effectsにおける一階のエフェクトの扱われ方は、基本的に
Freerそのものであり、一階のエフェクトのみをサポートしている`freer-simple`系のライブラリとほとんど一緒です。

## エフェクトクラスの定義

まず、Teletype用のエフェクトを定義しよう。

```haskell
-- | `Teletype`エフェクトクラス
class Teletype f where
    readTTY :: f String
    writeTTY :: String -> f ()

makeEffectF ''Teletype
```

`makeEffectF`は[`classy-effects-th`](https://github.com/sayo-hs/classy-effects)パッケージの
Template Haskellの関数で、これにより型クラス`Teletype`は自動的にCEP-01からCEP-04までの
すべての[CEPs](https://github.com/sayo-hs/classy-effects/blob/master/CEPs/README.md) (0.1.0) に準拠したエフェクトクラスとなる。

特に、[CEP-02]((https://github.com/sayo-hs/classy-effects/blob/master/CEPs/CEP-02.md))に従い、
以下のようなGADTsが自動的に生成される:

```haskell
-- `makeEffectF`により自動的に生成されるエフェクトクラスのデータ型
data TeletypeI a where
    ReadTTY :: TeletypeI String
    WriteTTY :: String -> TeletypeI ()
```

## インタプリタの実装

次に、この`Teletype`エフェクトクラスのインタプリタを実装しよう。

```haskell
-- | `Teletype`エフェクトクラスを、`getLine`と`putStrLn`の意味で解釈するインタプリタ。
teletypeToIO :: (IO <: Fre r m, Monad m) => Fre (TeletypeI ': r) m ~> Fre r m
teletypeToIO = interpret \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg
```

`Fre`型はいわゆるFreerモナドである。ただしFreerのモナド・トランスフォーマーであり、`m`は下位のモナドだ。
`es`はFreerが受け入れるエフェクトクラスのリストを表す型レベルリストである。
`teletypeToIO`の関数のシグネチャは、`Teletype`のエフェクトを消費して、
`ReadTTY`、`WriteTTY`の各エフェクトを`IO`アクションへと変換することを表現している。
`<:`演算子は`SendIns`型クラスのシノニムであり、これらは`classy-effects-base`より提供され、
左辺の一階エフェクトデータ（インストラクション）を右辺のキャリアへと送信可能であるという制約を表現する。
これはちょうど、サブタイピングのような関係になっている。
送信には`sendIns`メソッドを使う。
`~>`は単に、`type f ~> g = forall x. f x -> g x`と定義されたシノニムである。

また、制約として`Monad m`が必要なことに注意せよ。heftiaそしてclassy-effectsは原理的に、
モナド以外がキャリアとなるエフェクトをも仕組みとしてサポートするよう一般化されている（例えば、Applicativeなエフェクト）。
しかし、`Fre`はモナディックなエフェクト用のFreerであるため、ここでは下位のキャリアは`Monad`に制約されなければならない。
モナディック・エフェクト以外へと一般化した書き方は可能だが（実際heftiaライブラリが提供する関数はこのように一般化されている）、少し冗長であるため、ここでは解説しないものとする。

## エフェクトフルなプログラムの作成

次に、定義したエフェクトを使用するプログラムを書いてみよう。

```haskell
echo :: (Teletype m, Monad m) => m ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _ -> writeTTY i >> echo
```

この`echo`プログラムは、TTYに入力された文字列をオウム返しするループを行う。
ここで、もし空文字列が入力された場合（何も入力せずEnterキーが押された場合）、ループから脱出する。
ここで、`TeletypeI`や`<:`や`Fre`を使用せず、`Teletype`のみを使用していることに注意せよ。
このように、エフェクトデータに関するものを関数のインターフェースに含めないことで、
GADTsに基づかないエフェクトシステム・バックエンド（例えばmtlなど）にもこのプログラムをハンドル
させることが可能になってくる。つまり、特定のエフェクトシステム・バックエンドへの依存という制約が弱められ、
より一般化・多相化されているのだ。
この話の詳細はCEP-01の"Recommendation on Interface Independence from the Effect System Backend"の節を参照のこと。

## エフェクトフル・プログラムとハンドラの合成

エフェクトフルなプログラムである`echo`と、先程定義した`teletypeToIO`インタプリタを組み合わせて、実際に動くmain関数を構成してみよう。

```haskell
main :: IO ()
main = runFreerEffects do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO echo
```

`runFreerEffects`関数は、`Fre`モナドトランスフォーマーを「run」して、下位のモナド（ここでは`IO`モナド）へと落とし込む。

これを実行し、適当に文字列をタイプすると、以下のようになる。
空文字列を入力した所で、プログラムが終了する。

```
Please enter something...
foo↵
foo
bar↵
bar
baz↵
baz
↵
```

## 解釈の改変

次に、`interpose`関数を使ったエフェクトの再解釈について見ていこう。
heftiaライブラリから提供される`reinterpret`, `interpose`, `intercept`系統の関数を使用すると、
エフェクトを途中で別の解釈へと変更することができる。
これはいわゆるプログラムのフックである。

既存のプログラミングの概念で例えると、これは例外の再throwが近い。
ここでは、例外を投げる側がエフェクトフルなプログラムであり、例外を投げることがエフェクトの発行であり、例外ハンドラがエフェクト・ハンドラである。
一度投げられた例外を、一旦catchして、その例外の内容に応じてまた別の例外を生成し、またthrowするのだ。

コンピューターネットワークに詳しい読者のために例えると、これはネットワークを流れるパケットを途中で書き換えるプロクシのようなものである。
ここではクライアントがエフェクトフルなプログラムであり、サーバーがハンドラ（インタプリタ）である。
再解釈系統の関数は、あたかもクライアントからサーバーに送信されるリクエストを書き換えてしまうプロクシのように、
エフェクトフルなプログラムから発行されるエフェクトを、与えられた関数に従って別のエフェクトへと書き換えて再発行する。
この際、元のエフェクトは消費され、書き換え後のエフェクトと同時に重複して発行されることはないことに注意せよ。

以下の関数は、Teletypeエフェクトクラスに関する挙動をフックするものである。
`readTTY`エフェクトについてはそのままの挙動が保たれる。一方、
`writeTTY`エフェクトについては、元の`writeTTY`の挙動を変更し、文字列の末尾にエクスクラメーション・マークを付加する。

```haskell
strong :: (TeletypeI <| es, Monad m) => Fre es m ~> Fre es m
strong =
    interpose \case
        ReadTTY -> readTTY
        WriteTTY msg -> writeTTY $ msg <> "!"
```

`<|`は、エフェクトクラスのリストの所属関係を表現する制約である。
この関数を使って、`main`関数を例えば以下のように変更する:

```haskell
main :: IO ()
main = runFreerEffects do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO $ strong . strong $ echo
```

strongが2回適用されていることに注意せよ。
`interpose`は`f ~> f`形の型をもつ操作であるため、
このように何度でも適用することができる。

実行結果は、以下のようになる。

```
Please enter something...
foo↵
foo!!
bar↵
bar!!
baz↵
baz!!
↵
```

## タグ付きエフェクト

また、（実験的ではあるが）classy-effects及びheftia-effectsではタグ付きのエフェクトをサポートしている。
以下は、ここまでのコードをタグ付けした例である。

```hs
data TTY1

echo :: (Teletype (m @# TTY1), Monad m, Taggable m) => m ()
echo = do
    i <- readTTY & tag @TTY1
    case i of
        "" -> pure ()
        _ -> (writeTTY i & tag @TTY1) >> echo

strong :: (TeletypeI # TTY1 <| es, Monad m) => Fre es m ~> Fre es m
strong =
    interpose @(_ # TTY1) \e -> case getTag e of
        ReadTTY -> readTTY & tag @TTY1
        WriteTTY msg -> writeTTY (msg <> "!") & tag @TTY1

main :: IO ()
main = runFreerEffects $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO . untag @TTY1 . strong . strong $ echo
```

まず、型タグとして`TTY1`を定義している[^1]。

[^1]: もちろんタグとして型レベル文字列を使うことも可能だが、タイプミス時に出るエラーを分かりやすくするためにこのように新たにタグ用にデータ型を定義することを推奨する。

キャリアに対するタグ付けは`@#`演算子で行う。これにより、タグ付きのエフェクトクラスの制約を表現できる。
そして、`tag`関数を使うことでそのスコープ内における
タグ付けされていないエフェクトをタグ付けされた状態でキャリアへ送信できる。

エフェクトクラスデータ型に対しては、一階の場合は`#`演算子でタグ付けできる。次の章で登場する高階エフェクトクラスのときは`##`演算子を使う。

さらに、ハンドル時はタグを外して素の`TeletypeI`に戻すために、`untag @TTY1`を使用している。

## コード全体

コードの全体は以下のようになる。暗黙的に有効になっているGHC拡張が多いことに注意せよ。

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect.Class (sendIns, type (<:), type (~>))
import Control.Effect.Class.Machinery.TH (makeEffectF)
import Control.Effect.Freer (Fre, interpose, interpret, runFreerEffects, type (<|))

class Teletype f where
    readTTY :: f String
    writeTTY :: String -> f ()

makeEffectF ''Teletype

teletypeToIO :: (IO <: Fre es m, Monad m) => Fre (TeletypeI ': es) m ~> Fre es m
teletypeToIO = interpret \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg

echo :: (Teletype m, Monad m) => m ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _ -> writeTTY i >> echo

strong :: (TeletypeI <| es, Monad m) => Fre es m ~> Fre es m
strong =
    interpose \case
        ReadTTY -> readTTY
        WriteTTY msg -> writeTTY $ msg <> "!"

main :: IO ()
main = runFreerEffects $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO $ strong . strong $ echo
```

### タグ付きエフェクトのコード

```hs
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect.Class (Taggable, getTag, sendIns, tag, type (#), type (<:), type (@#), type (~>))
import Control.Effect.Class.Machinery.TH (makeEffectF)
import Control.Effect.Freer (Fre, interpose, interpret, runFreerEffects, untag, type (<|))
import Data.Function ((&))

class Teletype f where
    readTTY :: f String
    writeTTY :: String -> f ()

makeEffectF ''Teletype

teletypeToIO :: (IO <: Fre es m, Monad m) => Fre (TeletypeI ': es) m ~> Fre es m
teletypeToIO = interpret \case
    ReadTTY -> sendIns getLine
    WriteTTY msg -> sendIns $ putStrLn msg

data TTY1

echo :: (Teletype (m @# TTY1), Monad m, Taggable m) => m ()
echo = do
    i <- readTTY & tag @TTY1
    case i of
        "" -> pure ()
        _ -> (writeTTY i & tag @TTY1) >> echo

strong :: (TeletypeI # TTY1 <| es, Monad m) => Fre es m ~> Fre es m
strong =
    interpose @(_ # TTY1) \e -> case getTag e of
        ReadTTY -> readTTY & tag @TTY1
        WriteTTY msg -> writeTTY (msg <> "!") & tag @TTY1

main :: IO ()
main = runFreerEffects $ do
    sendIns $ putStrLn "Please enter something..."
    teletypeToIO . untag @TTY1 . strong . strong $ echo
```
