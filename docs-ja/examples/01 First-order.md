# ごあいさつ
やあみんな！まずは、Heftiaエフェクトシステムライブラリに興味を持ってくれてありがとう。

エフェクトシステムそして代数的エフェクトは、これからの未来のプログラミングパラダイムとして最近注目されてきているね（要出典）。
この解説シリーズでは、HaskellとHeftiaライブラリを使って、君をプログラミングパラダイムの未来へとご招待だ。

DSLを作って、込み入った裏方の面倒な仕事は全部ハンドラ側に押し付けて、プログラミングをめちゃくちゃに快適に、コードをめちゃくちゃに読みやすくしちゃおう！
さらに、堅苦しかった制御構造を、自在に操ってしまおう！

え？まるでgotoの悪夢の再来みたいだって？ハハハ！
大丈夫、型の力で守られてるから、ひどいことにはならないよ！すべては完全に整合的だ、保証しよう！もっとも保証するのは僕じゃなくて、型についてのたくさんの数式めいた理論とGHCコンパイラだけどね！:)

これで君も、みんなより一足先にエフェクトマスターだ！
それじゃ、楽しんでいこう！

# Example 1 - First-order effects

ここでは、[Polysemy](https://hackage.haskell.org/package/polysemy)ライブラリの例にも使用されている`Teletype`エフェクトクラスを例として、
Heftiaにおける一階のエフェクトの取り扱い方を説明します。
基本的にこれはFreer Effectsそのものであり、一階のエフェクトのみをサポートしている`freer-simple`系のライブラリとあまり変わりません。
そのため、Extensible Effectsを既に使ったことがある読者は、本記事では既存のライブラリとの構文の違いを軽く把握しておくだけでよいでしょう。

## エフェクトを定義する

まず、Teletype用のエフェクトを定義しよう。
Teletypeエフェクトとは、仮想端末から文字列を受け取ったり、逆に表示したりするようなものだ。

```haskell
import Data.Effect.TH (makeEffectF)

-- | `Teletype`エフェクトクラス
data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]
```

`makeEffectF`は[`data-effects-th`](https://github.com/sayo-hs/data-effects)パッケージの
Template Haskellの関数で、これによりデータ型`Teletype`を一階のエフェクト型として扱えるようになる。

具体的には、例えば以下のような関数が自動生成される。

```haskell
import Control.Effect (type (<:))

readTTY :: Teletype <: f => f String
readTTY = sendIns $ ReadTTY :: Teletype String

writeTTY :: Teletype <: f => String -> f ()
writeTTY x = sendIns $ WriteTTY x :: Teletype ()
```

ここで`sendIns`は、多相化されたエフェクトフルプログラムの*キャリア*（carrier. 通常はモナド）である`f`へとエフェクトを送り込む関数だ。
`<:`は、`Teletype`エフェクト型のエフェクトをキャリア`f`へと送り込めることを表す制約だ。
これにより、`ReadTTY`エフェクトを投げたいときに毎回`sendIns ReadTTY`と書かなくても良くなる。

## インタプリタの実装

次に、この`Teletype`エフェクト型のインタプリタ（ハンドラ）を実装しよう。

```haskell
import Data.Hefty.Extensible (ForallHFunctor, type (<|))
import Control.Effect (type (~>))
import Control.Effect.ExtensibleFinal (type (:!!))
import Control.Effect.Hefty (interpretRec)
import Control.Monad.IO.Class (liftIO)

-- | `Teletype`エフェクト型を、`getLine`と`putStrLn`の意味で解釈するインタプリタ。
teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg
```

まず、この関数はTeletypeエフェクトをハンドリングするものだ。
`~>`、`:!!`、`':`、`<|`は型レベルの演算子で、優先度は`~>` < `:!!` < `':`なので、`(eh :!! (LTeletype ': r)) ~> (eh :!! r)`というふうに結合する。
型シグネチャの`~>`は、その左側のエフェクト集合 (`eh :!! LTeletype ': r`) で表されたエフェクトフルプログラムを、右側 (`eh :!! r`) のものに変換するということを表している。
左側に対して右側は`LTeletype`が消えているね？これはつまり`Teletype`をハンドリングして、エフェクト集合から消してやるということだ。
`interpretRec`は、エフェクトのパターンマッチをする所だ。
`ReadTTY`のエフェクトが投げられてきたら`getLine`に、`WriteTTY`のエフェクトが投げられていたら`putStrLn`に処理を移譲する。
これによって、単なる型シグネチャだけのインターフェースに過ぎなかった`Teletype`に、初めて標準入出力を読み書きするという意味が割り当てられる。

各演算子の意味を具体的に説明しよう。

まず、`f ~> g`と書くと、これは`forall x. f x -> g x`と書いたのと同じことになる。
エフェクトハンドラではこの形の型が頻出するので、この型シノニムは重要だ。
展開すると、

```haskell
teletypeToIO :: (IO <| r, ForallHFunctor eh) => (eh :!! LTeletype ': r) a -> (eh :!! r) a
```

ということだ。

次に、`:!!`。これが、Heftiaで中心的な役割を果たすものだ。
これはエフェクトフルなプログラムのモナドを表す。いわゆる`Freer`というやつだ。
`:!!`の左側に高階エフェクト型の型レベルリストを、右側に一階エフェクト型の型レベルリストを書く。
ただし注意として、いろいろな都合上、右側の一階エフェクトリストに要素として`Teletype`と直接書くことはできない。
代わりに、`L`を付けた`LTeletype`と書く必要がある。これは`makeEffectF`関数で自動生成されるもので、`LiftIns Teletype`と書いても同じ意味になる。

`':`は実はHaskellが標準に用意しているもので、型レベルのリストの`:`（cons演算子）だ。

`<|`は、さっきの`<:`に似ているけど、`<:`はキャリア型に対する制約なのに対して、こっちはエフェクト型の型レベルリストに対する制約だ。
これはつまり、型レベルリストの変数`r`の中に要素として`IO`モナドが存在しているということを表している。

最後に`ForallHFunctor`だが、この制約があることで`interpretRec`関数などの末尾に`Rec`が付く関数が使えるようになる。
あとの章に出てくるが、`Rec`の付かない関数もある。`Rec`を使用するほうがハンドラがより一般的になるので、使えるときは使うのが好ましい。
具体的には、`Rec`の付かない`interpret`を使うと、高階エフェクトをすべてハンドルした後にしかそのハンドラは使えなくなる。
すなわち、先程は多相化されていた`eh`だが、代わりに空のエフェクトリスト`'[]`しか受け付けなくなる:

```haskell
teletypeToIO :: IO <| r => '[] :!! LTeletype ': r ~> '[] :!! r
teletypeToIO = interpret \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg
```

`Rec`が使えないケースについての説明は後の章でまた。

## エフェクトフルなプログラムの作成

次に、定義したエフェクトを使用するプログラムを書いてみよう。

```haskell
echo :: (Teletype <: m, Monad m) => m ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _ -> writeTTY i >> echo
```

この`echo`プログラムは、TTYに入力された文字列をオウム返しするループを行う。
ここで、もし空文字列が入力された場合（何も入力せずEnterキーが押された場合）、ループから脱出する。
型シグネチャにおいて`:!!`や`<|`を使用せずに`<:`のみを使用していることに気づくかもしれない。
このように型シグネチャにおいてキャリアの具体的な形を指定しないことで、最も一般的な形でエフェクトフルプログラムを書けるようになる。
いま制約として掛かっているのは、`m`がモナドであり、`Teletype`のエフェクトをそこへ送信できるということだけだ。
このことを「キャリアを多相化する」と呼ぶことができる。これによる効用は後の章で解説することになるだろう。

## エフェクトフルプログラムをハンドラへ接続する

エフェクトフルなプログラムである`echo`と、先程定義した`teletypeToIO`インタプリタを組み合わせて、実際に動くmain関数を構成してみよう。

```haskell
import Control.Effect.ExtensibleFinal (runEff)

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO echo
```

`runEff`関数は、ハンドリングを繰り返して`'[] :!! '[F]`の形にまで縮小されたエフェクトフルプログラムについて、
最後に残った唯一のエフェクト型`F`をキャリアと見なして（ここでは`F`=`IO`モナド）、Heftiaの層である`:!!`を「脱皮」して単純な`f`だけにする操作だ:

```haskell
runEff :: '[] :!! '[f] ~> f
```

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
`reinterpret`, `interpose`, `intercept`系統の関数を使用すると、
エフェクトを途中で別の解釈へと変更することができる。
これはいわゆるプログラムのフックである。

あるいは、これは例外の再throwが近い。
ここでは、例外を投げるのがエフェクトフルなプログラムであり、例外を投げることがエフェクトの発行であり、例外ハンドラがエフェクトハンドラに相当している。
一度投げられた例外を、一旦catchして、その例外の内容に応じてまた別の例外を生成し、またthrowするのだ。

コンピューターネットワークに詳しい読者のために例えると、これはネットワークを流れるパケットを途中で書き換えるプロクシのようなものである。
ここではクライアントがエフェクトフルなプログラムであり、サーバーがハンドラ（インタプリタ）である。
再解釈系統の関数は、あたかもクライアントからサーバーに送信されるリクエストを書き換えてしまうプロクシのように、
エフェクトフルなプログラムから発行されるエフェクトを、与えられた関数に従って別のエフェクトへと書き換えて再発行する。
この際、元のエフェクトは消費され、書き換え後のエフェクトと同時に重複して発行されることはないことに注意しよう。

以下の関数は、Teletypeエフェクトクラスに関する挙動をフックするものである。
`readTTY`エフェクトについてはそのままの挙動が保たれる。一方、
`writeTTY`エフェクトについては、元の`writeTTY`の挙動を変更し、文字列の末尾にエクスクラメーション・マークを付加する。

```haskell
strong :: (Teletype <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec \case
        ReadTTY -> readTTY
        WriteTTY msg -> writeTTY $ msg <> "!"
```

この関数を使って、`main`関数を例えば以下のように変更する:

```haskell
main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO $ strong . strong $ echo
```

strongが2回適用されていることに注意せよ。
`interpose`は`F ~> F`形の型をもつ操作であるため、
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

## タグ付きエフェクト型

Heftia（一般にExtensible Effects）では、エフェクト集合を表現するために、代わりに型レベルのリストを使う。
このため、同じエフェクト型をリスト内に複数回同時に出現させることもできる。

Extensible Effectsにおいて、エフェクトリスト内の要素であるエフェクト型は、おのおのがあたかもエフェクトを受け止める容器のように振る舞う。
リスト内にエフェクト型が重複した状態の下でその型のエフェクトを投げると、リスト内で最も左（先頭側）にあるエフェクト型の容器にエフェクトは入る。
あるいは先程のサーバー・クライアントのメタファーで言うと、リストのインデックス番号がサーバーのアドレスで、
それぞれでハンドラたるサーバーのインスタンスが別々に起動しているイメージだ。

例えば、`'[] :!! '[A, B, C, D, D, D, E]`というエフェクトリストの下で`D`型のエフェクトを投げると、インデックスが3番（0オリジン）の`D`にエフェクトは送られる。
最も左側（アドレスが若い方）に送信されるというルールは、振る舞いが暗黙的なので、注意していないと間違ったところにエフェクトを送ってしまうかもしれない。
さらに、4番や5番の`D`へエフェクトを送りたいという場合は、どうすればよいだろう？

そこで、エフェクト型には互いの識別のためのタグを別途付けることができる。これにより、リスト内に重複がある場合でも、ごちゃごちゃに混線してしまうことはないだろう。

タグは、`'[] :!! '[A, B, C, D # "one", D # "two", D # "three", E]`のように書く。
型レベル文字列以外にも任意の型レベルのものがタグとして使える。

以下は、ここまでのコードをタグ付けした例だ。
ただ、ここでは複数のTeletypeが出現することはないので、タグ付けするメリットは特にない。

```haskell
import Data.Effect.Tag (Tag (unTag), type (#))
import Control.Effect.Hefty (untagEff)

echo :: (Teletype # "tty1" <: m, Monad m) => m ()
echo = do
    i <- readTTY' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY' @"tty1" i >> echo

strong :: (Teletype # "tty1" <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec @(_ # "tty1") \e -> case unTag e of
        ReadTTY -> readTTY' @"tty1"
        WriteTTY msg -> writeTTY' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . untagEff @"tty1" . strong . strong $ echo

```

タグ付きエフェクト型の送信には、`makeEffect`系THが生成するアポストロフィの付いたバージョンの関数を使う。

`untagEff`関数は、エフェクトリストの先頭のタグ付けされたエフェクト型のタグを剥がす:

```haskell
untagEff :: eh :!! LiftIns (e # tag) ': r ~> eh :!! LiftIns e ': r
```

`LiftIns`は先程と同様、都合上のおまじないので無視してよい。

## コード全体

コードの全体は以下のようになる。
既に暗黙的に有効になっているGHC拡張やプラグインはREADME.mdのInstallationを見てほしい:
[README.md](https://github.com/sayo-hs/heftia?tab=readme-ov-file#installation)

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect (type (<:), type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect.Hefty (interposeRec, interpretRec)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.TH (makeEffectF)
import Data.Hefty.Extensible (ForallHFunctor, type (<|))

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

echo :: (Teletype <: m, Monad m) => m ()
echo = do
    i <- readTTY
    case i of
        "" -> pure ()
        _ -> writeTTY i >> echo

strong :: (Teletype <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec \case
        ReadTTY -> readTTY
        WriteTTY msg -> writeTTY $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO $ strong . strong $ echo
```

### タグ付きエフェクトのコード

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect (type (<:), type (~>))
import Control.Effect.ExtensibleChurch (runEff, type (:!!))
import Control.Effect.Hefty (interposeRec, interpretRec, untagEff)
import Control.Monad.IO.Class (liftIO)
import Data.Effect.TH (makeEffectF)
import Data.Effect.Tag (Tag (unTag), type (#))
import Data.Hefty.Extensible (ForallHFunctor, type (<|))

data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]

teletypeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTeletype ': r ~> eh :!! r
teletypeToIO = interpretRec \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg

echo :: (Teletype # "tty1" <: m, Monad m) => m ()
echo = do
    i <- readTTY' @"tty1"
    case i of
        "" -> pure ()
        _ -> writeTTY' @"tty1" i >> echo

strong :: (Teletype # "tty1" <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    interposeRec @(_ # "tty1") \e -> case unTag e of
        ReadTTY -> readTTY' @"tty1"
        WriteTTY msg -> writeTTY' @"tty1" $ msg <> "!"

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO . untagEff @"tty1" . strong . strong $ echo
```
