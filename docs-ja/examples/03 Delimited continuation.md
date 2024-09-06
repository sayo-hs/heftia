# Example 3 - Delimited continuation

ここでは、Heftiaにおける一階及び高階エフェクトを用いた限定継続の操作について説明します。

ここまでで出てきた使い方は、既存のエフェクトシステムライブラリで実現可能な部分と被るところがありました。しかし本パートで説明すること[^1]は、既存のものでは実現が困難であり、完全に新規な技術だと思われます。

[^1]: 高階エフェクトを組み合わせたモジュラーな限定継続の制御

以降、常体で解説を行います。

## `Fork`エフェクト

以下で定義する`Fork`エフェクトは、制御構造を非決定性計算的に分枝させるものだ。

非決定性計算と言えば、リストモナドだ。リストモナドで`do { x <- [1..4]; ... }`とやるように、このエフェクトは制御の流れを分枝させ、あたかもパラレルワールドでプログラムが並行に動いているように見える。

Posixプログラミングに詳しい読者は、これはプロセスを分裂させるいわゆる`fork()`関数と同じものだと思ってもらって良い。ただし、Posixの`fork`関数は制御構造を2つに分裂させるが、こちらは任意の個数だけ分裂させる。

```haskell
type ForkID = Int

data Fork a where
    Fork :: Fork ForkID
makeEffectF [''Fork]

runForkSingle :: ForallHFunctor eh => eh :!! LFork ': r ~> eh :!! r
runForkSingle = interpretRec \Fork -> pure 0
```

戻り値の`ForkID`は、分裂後に自分がどの分枝世界にいるかを表すIDだ。`runForkSingle`は単純に、`fork`エフェクトが投げられても分枝せず、戻り値は`0`のみを返すインタプリタだ。

## `ResetFork`エフェクト

以下の`ResetFork`エフェクトは、分枝の範囲をスコープで区切って限定するための高階エフェクトだ。

```haskell
data ResetFork f a where
    ResetFork :: Monoid w => f w -> ResetFork f w
makeEffectH [''ResetFork]
```

Posixの`fork()`との違いは、あちらはプロセス終了まで永遠と分枝したままだが、
こちらは分枝の範囲が限定されているということだ。

`RestFork`が導入するスコープを抜けるタイミングで、スコープ内で発生した`Fork`による分枝はすべて収束し、戻り値は`Monoid`に沿って合成される。

さて、ここで限定継続の出番だ。「限定」というのはつまり分枝がスコープ内の中で収まっていて、その外側では分枝は継続しないということだ。

以下のelaboration射は、スコープに対応した限定継続を取り出すことで、`Fork`エフェクトが投げられたタイミングで実際に制御構造を`numberOfFork`個に分枝させる。

```haskell
applyResetFork :: Fork <| r => Int -> Elab ResetFork ('[] :!! r)
applyResetFork numberOfFork (ResetFork m) =
    m & interposeK pure \resume Fork -> do
        r <- mapM resume [1 .. numberOfFork]
        pure $ mconcat r
```

限定継続の取り出しには`interposeK`関数を使う。他にも`K`系統の関数はいくつかあるので、用途に応じて使い分けよう。ここでは取り出された限定継続`resume`を`1`から`numberOfFork`にかけて呼び出し、最後に各々の継続の結果を`mconcat`で集計している。

実行例を見てみよう。

```haskell
main :: IO ()
main =
    runEff
        . runForkSingle
        . interpretRecH (applyResetFork 4)
        $ do
            liftIO . putStrLn . (("[out of scope] " ++) . show) =<< fork
            s <- resetFork do
                fid1 <- fork
                fid2 <- fork
                liftIO $ putStrLn $ "[delimited continuation of `fork`] Fork ID: " ++ show (fid1, fid2)
                pure $ show (fid1, fid2)
            liftIO $ putStrLn $ "scope exited. result: " ++ s
```

実行結果:
```console
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
```

まず、`resetFork`のスコープの外では`applyResetFork`の影響を受けず、単に`runForkSingle`の挙動が反映される。`resetFork`のスコープ内ではリストモナドの要領で、おのおのの`fork`について`1`から`4`のすべてのパターンが戻ってきて、計算が分枝して進んでいる。`resetFork`のスコープを抜けると分枝は収束し、スコープから返却された`(fid1,fid2)`という形式の文字列がすべて`Monoid`により結合されて結果として得られている。

単にこれと同じ動作をする`fork`関数を作るだけなら、リストモナドや継続モナドを使うことで今までも可能であった。本ライブラリが真に可能にするのは、例えば`applyResetFork`とは別の`fork`戦略をelaboratorとして実装するなどして、**どのようにforkするかを差し替えできる**ということだ。例えば、スレッドを使って並行に実行するような`applyResetForkByThread`なるelaboratorを実装できるはずだ。さらにPart 2で説明したようにして、高階なフックを用いて挙動をモジュラーに変更することができ、それらは合成可能だ。

# おわりに

このように、Heftiaでは限定継続を容易に扱うことができる。その上で、どのように限定継続を使うか（あるいは使わないか）をelaborationにより柔軟に（モジュラーに）変更することができる。これにより、非決定性計算はもちろん、非同期バックエンドを変更可能なAsyncエフェクトといった並行計算への応用など、色々と面白いエフェクト・DSLを作ることが可能になるだろう。

Haskell上でEffect Systemを実現するライブラリは数多くあるが、いわゆる「Algebraic Effects and Handlers」で可能なこと（限定継続の取り出しとそれを用いたモジュラーな高階エフェクトのハンドリング）をほぼフルでエミュレートできるのは、Hefty Algebrasに基づくElaboration方式のみのようだ。

本パートの例で使用したコードの全体は[heftia-effects/Example/Continuation/](https://github.com/sayo-hs/heftia/blob/v0.3.0/heftia-effects/Example/Continuation/Main.hs)にある。

