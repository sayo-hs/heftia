# ごあいさつ
まずは、Heftiaエフェクトシステムライブラリに興味を持ってくれてありがとう。

エフェクトシステムそして代数的エフェクトは、これからの未来のプログラミングパラダイムとして最近注目されてきているね（要出典）。この解説シリーズでは、HaskellとHeftiaライブラリを使って、君をプログラミングパラダイムの未来へとご招待だ。

DSLを作って、込み入った裏方の面倒な仕事は全部ハンドラ側に押し付けて、プログラミングをめちゃくちゃに快適に、コードをめちゃくちゃに読みやすくしちゃおう！さらに、堅苦しかった制御構造を、自在に操ってしまおう！

え？まるでgotoの悪夢の再来みたいだって？ハハハ！大丈夫、型の力で守られてるから、ひどいことにはならないよ！すべては完全に整合的だ、保証しよう！もっとも保証するのは僕じゃなくて、型についてのたくさんの数式めいた理論とGHCコンパイラだけどね！:)

これで君も、みんなより一足先にエフェクトマスターだ！それじゃ、楽しんでいこう！

# Example 1 - First-order effects

本記事では、同じHaskellのエフェクトシステムライブラリである[Polysemy](https://hackage.haskell.org/package/polysemy)ライブラリの説明に使用されている`Teletype`エフェクト型を例として、Heftiaにおける一階のエフェクトの使い方を説明します。基本的にHeftiaは一階のエフェクトに関してはFreer Effectsと呼ばれる方式そのものであり、一階のエフェクトのみをサポートしているfreer-simple系のライブラリとあまり変わりません。そのため、Extensible Effectsを既に使ったことがある読者は、Part 1 では既存のライブラリとの違い（特に高階エフェクトと一階エフェクトのリストが分かれている点）を把握するつもりで読むと良いでしょう。Part 2 からは、既存のライブラリとはかなり使い方の異なる高階エフェクトについて説明します。

以降、常体で解説を行います。

## 準備
まず、Heftiaをインストールしよう。
以下のインストール手順は、[README.mdのInstallation](https://github.com/sayo-hs/heftia?tab=readme-ov-file#installation)を和訳したものだ。

1. cabalのローカルのパッケージデータベースを更新する。
    ```console
    $ cabal update
    ```
2. `heftia-effects ^>= 0.2`, `ghc-typelits-knownnat ^>= 0.7` をbuild dependenciesに追加する。
   必要に応じて、[ghc-typelits-knownnat](https://hackage.haskell.org/package/ghc-typelits-knownnat) プラグイン、`GHC2021`、そして次の言語拡張を有効にする:

    * `LambdaCase`
    * `DerivingStrategies`
    * `DataKinds`
    * `TypeFamilies`
    * `BlockArguments`
    * `FunctionalDependencies`
    * `RecordWildCards`
    * `DefaultSignatures`
    * `PatternSynonyms`
    * `TemplateHaskell`
    * `PartialTypeSignatures`
    * `AllowAmbiguousTypes`

.cabalの例:

```
...
    build-depends:
        ...
        heftia-effects ^>= 0.2,
        ghc-typelits-knownnat ^>= 0.7,

    default-language: GHC2021

    default-extensions:
        ...
        LambdaCase,
        DerivingStrategies,
        DataKinds,
        TypeFamilies,
        BlockArguments,
        FunctionalDependencies,
        RecordWildCards,
        DefaultSignatures,
        PatternSynonyms,
        TemplateHaskell,
        PartialTypeSignatures,
        AllowAmbiguousTypes

    ghc-options: ... -fplugin GHC.TypeLits.KnownNat.Solver
...
```

このライブラリはGHC 9.2.8で動作確認をしている。
（もし動作しない例を見つけた場合、GitHubのissueにてご報告ください。）

## エフェクトを定義する

まず、Teletype用のエフェクトを定義しよう。
Teletypeエフェクト型とは、仮想端末から文字列を受け取ったり、逆に表示したりするようなものだ。
`ReadTTY`は引数がなく、端末に入力された文字列を戻り値として返す。
`WriteTTY`は端末に表示する文字列を引数として取り、戻り値は空だ。

```haskell
import Data.Effect.TH (makeEffectF)

-- | `Teletype`エフェクト型
data Teletype a where
    ReadTTY :: Teletype String
    WriteTTY :: String -> Teletype ()

makeEffectF [''Teletype]
```

`makeEffectF`は[`data-effects-th`](https://github.com/sayo-hs/data-effects)パッケージのTemplate Haskellの関数で、これによりデータ型`Teletype`を一階のエフェクト型として扱えるようになる。

具体的には、例えば以下のような関数が自動生成される。

```haskell
import Control.Effect (type (<:), sendIns)

readTTY :: Teletype <: f => f String
readTTY = sendIns $ ReadTTY :: Teletype String

writeTTY :: Teletype <: f => String -> f ()
writeTTY x = sendIns $ WriteTTY x :: Teletype ()
```

ここで`sendIns`は、多相化されたエフェクトフルプログラムの*キャリア*（carrier. 通常はモナド）である`f`へとエフェクトを送り込む関数だ。`<:`は、`Teletype`エフェクト型のエフェクトをキャリア`f`へと送り込めることを表す制約だ。この自動生成により、`ReadTTY`エフェクトを投げたいときに毎回`sendIns ReadTTY`と書かなくても良くなる。

## インタプリタの実装

次に、このTeletypeエフェクト型のインタプリタ（ハンドラ）を実装しよう。

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

この関数はTeletypeのエフェクトをハンドリングするものだ。

`~>`、`:!!`、`':`、`<|`は型レベルの演算子で、`(eh :!! (LTeletype ': r)) ~> (eh :!! r)`というふうに結合する。

型シグネチャの`~>`は、その左側のエフェクト集合 (`eh :!! LTeletype ': r`) で表されたエフェクトフルプログラムを、右側 (`eh :!! r`) のものに変換するということを表している。左側に対して右側は`LTeletype`が消えていることがわかる。これはつまり、`Teletype`をハンドリングして、エフェクト集合から消してやるということを意味する。

`interpretRec`は、エフェクトのパターンマッチをする所だ。`ReadTTY`のエフェクトが投げられてきたら`getLine`に、`WriteTTY`のエフェクトが投げられていたら`putStrLn`に処理を移譲する。これによって、単なる型シグネチャだけのインターフェースに過ぎなかった`Teletype`に初めて、標準入出力を読み書きするという意味が割り当てられる。

各演算子の意味を具体的に説明しよう。

まず、`f ~> g`と書くと、これは`forall x. f x -> g x`と書いたのと同じことになる。なお、この演算子は`->`よりも強く結合する。エフェクトハンドラではこの形の型が頻出するので、この型シノニムは重要だ。展開すると、

```haskell
teletypeToIO :: (IO <| r, ForallHFunctor eh) => (eh :!! LTeletype ': r) a -> (eh :!! r) a
```

ということだ。

次に、`:!!`。これが、Heftiaで中心的な役割を果たすものだ。
これはエフェクトフルなプログラムのモナドを表す。`:!!`の左側に高階エフェクト型の型レベルリストを、右側に一階エフェクト型の型レベルリストを書く。ただし注意として、いろいろな都合上、右側の一階エフェクトリストに要素として`Teletype`と直接書くことはできない。代わりに、`L`を付けた`LTeletype`と書く必要がある。これは`makeEffectF`関数で自動生成されるもので、`LiftIns Teletype`と書いても同じ意味になる。

`':`は実はHaskellが標準に用意しているもので、型レベルのリストの`:`（cons演算子）だ。

`<|`は、さっきの`<:`に似ているが、`<:`はキャリア型に対する制約なのに対して、こちらはエフェクト型の型レベルリストに対する制約だ。これはつまり、型レベルリストの変数`r`の中に要素として`IO`モナドが存在しているということを表している。

最後に`ForallHFunctor`だが、この制約があることで`interpretRec`関数などの末尾に`Rec`が付く関数が使えるようになる。あとの章に出てくるが、`Rec`の付かない関数もある。`Rec`を使用するほうがハンドラがより一般的になるので、使えるときは使うのが好ましい。具体的には、`Rec`の付かない`interpret`を使うと、高階エフェクトをすべて解釈し切った後にしかそのハンドラは使えなくなる。すなわち、先程は多相化されていた`eh`だが、代わりに空のエフェクトリスト`'[]`しか受け付けなくなる:

```haskell
teletypeToIO :: IO <| r => '[] :!! LTeletype ': r ~> '[] :!! r
teletypeToIO = interpret \case
    ReadTTY -> liftIO getLine
    WriteTTY msg -> liftIO $ putStrLn msg
```

`Rec`が使えないケースについての説明はPart 2で行う。

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

型シグネチャにおいて`:!!`や`<|`を使用せずに`<:`のみを使用していることに気づくかもしれない。このように型シグネチャにおいてキャリアの具体的な形を指定しないことで、最も一般的な形でエフェクトフルプログラムを書けるようになる。いま制約として掛かっているのは、`m`がモナドであり、`Teletype`のエフェクトをそこへ送信できるということだけだ。このことを「キャリアを多相化する」と呼ぶ。これによる効用は後に応用編にて解説できればと思う。

## エフェクトフルプログラムをハンドラへ接続する

エフェクトフルなプログラムである`echo`と、先程定義した`teletypeToIO`インタプリタを組み合わせて、実際に動くmain関数を構成してみよう。

```haskell
import Control.Effect.ExtensibleFinal (runEff)

main :: IO ()
main = runEff do
    liftIO $ putStrLn "Please enter something..."
    teletypeToIO echo
```

`runEff`関数は、ハンドリングを繰り返して`'[] :!! '[f]`の形にまで縮小されたエフェクトフルプログラムについて、最後に残った唯一のエフェクト型`f`をキャリアと見なして（ここでは`f`=`IO`モナド）、Heftiaの層である`:!!`を「脱皮」して単純な`f`だけにする操作だ:

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
`interpose`系統の関数を使用すると、エフェクトを途中で別の解釈へと変更することができる。これはいわゆるプログラムのフックである。

あるいは、これは例外の再throwが近い。ここでは、例外がエフェクトであり、例外が湧いてくるコールスタックの深いところがエフェクトフルなプログラムであり、例外を投げることがエフェクトの送信であり、コールスタックの浅い所にある例外ハンドラがエフェクトハンドラに相当している。一度投げられた例外を、一旦catchして、その例外の内容に応じてまた別の例外を生成し、またthrowするのだ。

コンピューターネットワークに詳しい読者のために例えると、これはネットワークを流れるリクエストを中継するプロキシのようなものである。この例えにおいて、クライアントがエフェクトフルなプログラムであり、サーバーがハンドラ（インタプリタ）であり、エフェクトがリクエストであり、エフェクトの戻り値はレスポンスだ。再解釈系統の関数は、エフェクトフルなプログラムから送信されるエフェクトをその途中で受け取り、そのエフェクトの内容に応じた別のエフェクトフルプログラムを実行する。このプログラムにおいて、再度同じエフェクトを引数の内容だけ変えて再送信すると、あたかもクライアントからサーバーに送信されるリクエストを書き換えるプロキシのように動作することになる。

以下の関数は、Teletypeエフェクト型に関する挙動をフックするものである。
`readTTY`エフェクトについてはそのままの挙動が保たれる。一方、`writeTTY`エフェクトについては、元の`writeTTY`の挙動を変更し、文字列の末尾にエクスクラメーションマークを付加する。

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

strongが2回適用されていることに注意してほしい。`interpose`は`F ~> F`形の型をもつ操作であるため、このように何度でも適用することができる。

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

なお、このようにエフェクトの引数の内容を書き換えるだけなら、より高速な`transform`/`translate`/`rewrite`系関数を使用することが推奨される。今回の場合、`strong`関数は

```haskell
import Control.Effect.Hefty (rewrite)

strong :: (Teletype <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
strong =
    rewrite \case
        ReadTTY -> ReadTTY
        WriteTTY msg -> WriteTTY $ msg <> "!"
```

と書き直すことができる。`interpose`系を使う場合との違いは、複数回に跨るような手続き的な処理へと再解釈できない点だ。例えば、受け取った一つのエフェクトを複数のエフェクトへと複製することはできない。

## タグ付きエフェクト型

Heftiaは、*Extensible Effects* (EE) という方式に属するものだ。EEでは、エフェクト集合を表現するために型レベルのリストを使うことがある。この場合、同じエフェクト型をリスト内に複数回に出現させることもできる。

EEにおいて、エフェクトリスト内の要素であるエフェクト型は、おのおのがあたかもエフェクトを受け止める容器のように振る舞う。リスト内にエフェクト型が重複した状態の下でその型のエフェクトを投げると、リスト内で最も左（先頭側）にあるエフェクト型の容器にエフェクトは入る。あるいは先程のサーバー・クライアントのメタファーで言うと、リストのインデックス番号がサーバーのアドレスで、それぞれでハンドラたるサーバーのインスタンスが別々に起動しているイメージだ。このように、「エフェクトが入るところ」というイメージを込めて、エフェクトリスト内の要素ないしインデックスのことを（エフェクトの）**スロット**と呼び表すことにする。

例えば、`'[] :!! '[A, B, C, D, D, D, E]`というエフェクトリストの下で`D`型のエフェクトを投げると、インデックスが3番（0オリジン）の`D`にエフェクトは送られる。最も左側（アドレスが若い方）に送信されるというルールは、振る舞いが暗黙的なので、注意していないと間違ったところにエフェクトを送ってしまうかもしれない。さらに、4番や5番の`D`へエフェクトを送りたいという場合は、どうすればよいだろう？

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
import Data.Effect (LiftIns)

untagEff :: eh :!! LiftIns (e # tag) ': r ~> eh :!! LiftIns e ': r
```

`LiftIns`は先程と同様、都合上のものなので無視してよい。

## エフェクトの活用方法
ここまでで、Heftiaによるエフェクトの使用例を見てきた。
ここで着目するべきは、インタプリタである`teletypeToIO`とエフェクトフルプログラムである`echo`が定義として分離されている点だ。

このエコープログラムをCLIではなく何かGUIに組み込みたいとなったとき、TeletypeエフェクトをGUIに対する操作に変換する`teletypeToGUI`ハンドラを後から書くことで、`echo`側を一切修正せずに目的を達成できる。

さらに、`echo`を書いている最中は、`readTTY`の裏でどのような処理が動くかについて一切気にする必要がない。これはつまり、ドメインロジック（ビジネスロジック）と、低水準で実装都合的でハードウェア的なインフラシステムとの間で分離ができているということを意味する。ドメインロジックとインフラシステムの間が、エフェクトという抽象化層、インターフェースによって接続されているのである。

このような分離は、現代において重要性が十分に認識されているので、既に実現するさまざまな方法がある。その中でもエフェクトパラダイムを使う利点は、**ドメインロジックを表現するコードの可読性、合成可能性、モジュラー性**だ。

十分に高度なEEにおいては、エフェクトを定義することはすなわちDSL（ドメイン特化言語）を定義することと等しい意味をもつ。そこで書かれるエフェクトフルプログラムは、ドメインロジックを表現するのに必要最小限な記述に絞られる。それは**まさにそのドメイン専用に設計された言語を書いている**という体験を可能にする。分離を実現するための従来のアドホックなデザインパターンによるコード上のノイズは、最小限に抑えられる。

さらに、エフェクトフルプログラムは合成可能だ。複数のエフェクトフルプログラムは、エフェクトシステムにより、単に従来の手続き型言語を書くようにするだけで、自然にエフェクトの集合が合成される。
先程、EEではエフェクト集合を表現するために型レベルのリストを使うと述べた一方で、`<:`を使用した形式で型シグネチャが書かれたエフェクトフルプログラムの例を [エフェクトフルなプログラムの作成](#%E3%82%A8%E3%83%95%E3%82%A7%E3%82%AF%E3%83%88%E3%83%95%E3%83%AB%E3%81%AA%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%A0%E3%81%AE%E4%BD%9C%E6%88%90) に示した。`<:`や`<|`を使用した制約でエフェクト集合を表現する場合、エフェクト集合の異なるエフェクトフルプログラム同士は自然に合成される。EEではエフェクト集合を表現するために、型レベルリストを使う他にこのようにHaskellの制約が暗黙的にANDで合成されていくのを利用することで、エフェクトの型システムをエミュレートできる。

最後に、モジュラー性。これは合成可能性の一種とも言える。エフェクトフルプログラムは、インタプリタによってエフェクトを解釈し、エフェクト集合を段階的に縮小していくことができる。つまり、あるソースコードの箇所で定義されたエフェクトフルプログラムである
```haskell
prog :: ('[] :!! '[A,B,C]) a
```
を、まず別の箇所でその中の一つのエフェクト型`A`を解釈してそれを
```haskell
prog' :: ('[] :!! '[B,C]) a
```
と定義して、それをさらに別の箇所で今度は別のエフェクト型`C`を解釈してそれを
```haskell
prog'' :: ('[] :!! '[B]) a
```
として…というようなことが可能というわけだ（解釈するエフェクト型はリストの先頭に限られない。リストは自由に並び替えることができる）。さらに、ある箇所で定義されたプログラムを別の箇所で容易にフックできる利点は言うまでもない[^1]。

[^1]: アスペクト指向と呼ばれるパラダイムと利点を共有している。

エフェクトパラダイムの欠点としては、先に述べた利点を実現するためには、高度な型システムとミニマルな言語構文・糖衣構文が要請される点である。これにより、主流の言語においてEEをライブラリとして実現するのは困難であり、単に同じ原理 (いわゆるFreer) を実現するように移植するだけでは十分な利便性は得られない。EEがHaskellにおいて盛んであるのは、単純にHaskellに実験的な文化のコミュニティが根付いているからという理由だけでなく、必要な要請を強く満たしていることがあるだろう。

## まとめ
* 一階エフェクトの定義には`makeEffectF`を使う
* エフェクトハンドラを作るには`interpret`系の関数を使う
* エフェクトのフックには`intepose`系の関数を使う
* タグを付けることでリスト内で重複したエフェクト型をわかりやすく識別可能にできる
*

## コード全体

コードの全体は以下のようになる。タグ付きエフェクトのコードは[GitHub](https://github.com/sayo-hs/heftia/blob/9639e3aeb3fcfd641f19b7cdccc10e686e6fca5d/heftia-effects/Example/Teletype/Main.hs)にある。

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
