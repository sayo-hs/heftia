# Example 3 - Delimited Continuation

ここでは、heftia-effectsにおいて限定継続を容易に扱うことができることを、例を使って示します。

## `fork`エフェクト

以下で定義する`fork`エフェクトは、制御構造を非決定性計算的に分枝させるものだ。

非決定性計算と言えば、リストモナドだ。
リストモナドで`do { x <- [1..4]; ... }`とやるように、このエフェクトは制御の流れを分枝させ、パラレルワールドを作り出す。

Posixプログラミングに詳しい読者は、これはプロセスを分裂させるいわゆる`fork()`関数と同じものだと思ってもらって良い。
ただし、Posixの`fork`関数は制御構造を2つに分裂させるが、こちらは任意の個数だけ分裂させる。

```hs
type ForkID = Int

class Fork f where
    fork :: f ForkID

makeEffectF ''Fork

runForkSingle :: Monad m => Fre (ForkI ': r) m ~> Fre r m
runForkSingle = interpret \Fork -> pure 0
```

戻り値の`ForkID`は、分裂後に自分がどの分枝世界にいるかを表すIDだ。
`runForkSingle`は単純に、`fork`エフェクトが投げられても分枝せず、戻り値は`0`のみを返すインタプリタだ。

## `delimitFork`エフェクト

以下の`delimitFork`エフェクトは、分枝の範囲をスコープで区切って限定するための高階エフェクトだ。

```hs
class DelimitFork f where
    delimitFork :: Monoid w => f w -> f w

makeEffectH ''DelimitFork
```

Posixの`fork()`との違いは、あちらはプロセス終了まで永遠と分枝したままだが、
こちらは分枝の範囲が限定されているということだ。

`delimitFork`が導入するスコープを抜けるタイミングで、スコープ内で発生した`fork`による分枝はすべて収束し、戻り値は`Monoid`に沿って合成される。

さて、ここで限定継続の出番だ。
限定継続が何かの説明はより良い説明が他所にあるはずなのでここでは行わないが、
「限定」というのはつまり分枝がスコープ内の中で収まっていて、その外側では分枝は継続しないという話だ、
ということだけ伝えておこう。

以下のelaboratorは、スコープに対応した限定継続を取り出すことで、`fork`エフェクトが投げられたタイミングで実際に制御構造を`numberOfFork`個に分枝させる。

```hs
applyDelimitFork :: (ForkI <| es, Monad m) => Int -> Elaborator DelimitForkS (Fre es m)
applyDelimitFork numberOfFork (DelimitFork m) =
    m & interposeK pure \k Fork -> do
        r <- mapM k [1 .. numberOfFork]
        pure $ mconcat r
```

ここで、`Elaborator`は単に`type Elaborator e f = e f ~> f`で定義される型シノニムだ。

限定継続の取り出しには、例えば`interposeK`関数を使う。他にも`K`系統の関数はいくつかあるので、用途に応じて使い分けよう。
ここでは取り出された限定継続`k`を`1`から`numberOfFork`にかけて呼び出し、最後に各々の継続の結果を`mconcat`で集計している。

実行例を見てみよう。

```hs
main :: IO ()
main =
    runFreerEffects
        . runForkSingle
        . runElaborate @_ @HeftiaChurchT @SumUnionH (applyDelimitFork 4 |+: absurdUnionH)
        $ do
            sendIns . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- delimitFork do
                fid1 <- fork
                fid2 <- fork
                sendIns $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (fid1, fid2)
                pure $ show (fid1, fid2)
            sendIns $ putStrLn $ "scope exited. result: " ++ s

{- 実行結果:
[out of scope] 0
[delimited continuation of `fork`] Fork ID: (1,1)
[delimited continuation of `fork`] Fork ID: (1,2)
[delimited continuation of `fork`] Fork ID: (1,3)
[delimited continuation of `fork`] Fork ID: (1,4)
[delimited continuation of `fork`] Fork ID: (2,1)
[delimited continuation of `fork`] Fork ID: (2,2)
[delimited continuation of `fork`] Fork ID: (2,3)
[delimited continuation of `fork`] Fork ID: (2,4)
[delimited continuation of `fork`] Fork ID: (3,1)
[delimited continuation of `fork`] Fork ID: (3,2)
[delimited continuation of `fork`] Fork ID: (3,3)
[delimited continuation of `fork`] Fork ID: (3,4)
[delimited continuation of `fork`] Fork ID: (4,1)
[delimited continuation of `fork`] Fork ID: (4,2)
[delimited continuation of `fork`] Fork ID: (4,3)
[delimited continuation of `fork`] Fork ID: (4,4)
scope exited. result: (1,1)(1,2)(1,3)(1,4)(2,1)(2,2)(2,3)(2,4)(3,1)(3,2)(3,3)(3,4)(4,1)(4,2)(4,3)(4,4)
-}
```

まず、`delimitFork`のスコープの外では`applyDelimitFork`の影響を受けず、単に`runForkSingle`の挙動が反映される。
`delimitFork`のスコープ内ではリストモナドの要領で、おのおのの`fork`について`1`から`4`のすべてのパターンが戻ってきて、計算が分枝して進んでいる。
`delimitFork`のスコープを抜けると分枝は収束し、スコープから返却された`(fid1,fid2)`という形式の文字列が
すべて`Monoid`により結合されて結果として得られている。

---

このように、heftia-effectsでは限定継続を容易に扱うことができる。
その上で、どのように限定継続を使うか（あるいは使わないか）をelaborationにより柔軟に（モジュラーに）変更することができる。
これにより、非決定性計算はもちろん、非同期バックエンドを変更可能なAsyncエフェクトといった並行計算への応用など、
色々と面白いエフェクトを作ることが可能になるだろう。

Haskell上でEffect Systemを実現するライブラリは数多くあるが、
有名な「Algebraic Effects and Handlers」で可能なこと（例えば限定継続の取り出しとそれを用いたモジュラーなエフェクトのハンドリング）をほぼフルでエミュレートできるのは、
筆者の知る限りではいまのところHefty Algebrasに基づくこの方式のみのはずである。

## コード全体

```hs
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect.Class (sendIns, type (~>))
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Freer (Fre, interposeK, interpret, runFreerEffects, type (<|))
import Control.Effect.Heftia (Elaborator, runElaborate)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Function ((&))
import Data.Hefty.Sum (SumUnionH)
import Data.Hefty.Union (UnionH (absurdUnionH, (|+:)))

type ForkID = Int

class Fork f where
    fork :: f ForkID

makeEffectF ''Fork

runForkSingle :: Monad m => Fre (ForkI ': r) m ~> Fre r m
runForkSingle = interpret \Fork -> pure 0

class DelimitFork f where
    delimitFork :: Monoid w => f w -> f w

makeEffectH ''DelimitFork

applyDelimitFork :: (ForkI <| es, Monad m) => Int -> Elaborator DelimitForkS (Fre es m)
applyDelimitFork numberOfFork (DelimitFork m) =
    m & interposeK pure \k Fork -> do
        r <- mapM k [1 .. numberOfFork]
        pure $ mconcat r

main :: IO ()
main =
    runFreerEffects
        . runForkSingle
        . runElaborate @_ @HeftiaChurchT @SumUnionH (applyDelimitFork 4 |+: absurdUnionH)
        $ do
            sendIns . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- delimitFork do
                fid1 <- fork
                fid2 <- fork
                sendIns $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (fid1, fid2)
                pure $ show (fid1, fid2)
            sendIns $ putStrLn $ "scope exited. result: " ++ s
```
