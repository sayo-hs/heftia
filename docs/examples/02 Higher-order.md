# Example 2 - Logging

ここでは、ログ出力のエフェクトを通じて、heftia-effectsにおける高階のエフェクトの取り扱い方を説明します。
一階エフェクトはほとんど既存のFreerに基づくライブラリと同様であったのに対して、
heftia-effectsにおける高階エフェクトの扱われ方は、[Hefty Algebras (Casper et, al. 2023)](https://dl.acm.org/doi/10.1145/3571255)
に基づく、まったく新しいものです。

## ログ出力のエフェクトクラス

まず、ログ出力の一階エフェクトクラスは、例えば以下のように定義できる:

```haskell
class Log f where
    log :: LogLevel -> Text -> f ()

makeEffectF ''Log
```

これに対するインタプリタとしては、例えばこうだ:

```haskell
logToIO ::
    (IO <: Fre r m, Ask LogLevel (Fre r m), Monad m) =>
    Fre (LogI ': r) m ~> Fre r m
logToIO =
    interpret \(Log level msg) -> do
        currentLevel <- ask
        when (level <= currentLevel) do
            sendIns $ T.putStrLn msg
```

`Ask`エフェクトクラスは`Reader`エフェクトクラスのサブクラスで、いわゆる`Reader`モナドの`ask`関数のエフェクト版である。
このインタプリタは、現在のログレベルを`Reader`の環境から取得し、ログレベルに応じてフィルターされたログを標準出力する[^1]。

[^1]: `LogLevel`データ型は`loglevel`パッケージ、`Text`データ型は`text`パッケージより。

以上は実用的なログ出力エフェクトの例だが、簡単のために以降はログレベルの要素を排して、単に

```haskell
class Log f where
    log :: Text -> f ()

makeEffectF ''Log

logToIO :: (IO <: Fre r m, Monad m) => Fre (LogI ': r) m ~> Fre r m
logToIO = interpret \(Log msg) -> sendIns $ T.putStrLn msg
```

という定義で話を進める。

## 時刻取得エフェクト

まず以下のようにして、現在時刻を取得するエフェクトを導入できる[^2]。

```haskell
class Time f where
    currentTime :: f UTCTime

makeEffectF ''Time

timeToIO :: (IO <: Fre r m, Monad m) => Fre (TimeI ': r) m ~> Fre r m
timeToIO = interpret \case
    CurrentTime -> sendIns getCurrentTime
```

[^2]: `getCurrentTime`は`time`パッケージより。

そして、以下のようにして、ログに現在時刻を付加する再解釈関数を作ることができる。

```haskell
logWithTime :: (LogI <| es, Time (Fre es m), Monad m) => Fre es m ~> Fre es m
logWithTime = interpose \(Log msg) -> do
    t <- currentTime
    log $ "[" <> T.pack (show t) <> "] " <> msg
```

試してみよう。

```haskell
main :: IO ()
main = runFreerEffects
    . logToIO
    . timeToIO
    . logWithTime
    $ do
        log "foo"
        log "bar"
        log "baz"
        log "baz"
        log "qux"
        log "quux"

{- 実行結果:
[2023-09-14 06:35:14.709968653 UTC] foo
[2023-09-14 06:35:14.710160875 UTC] bar
[2023-09-14 06:35:14.710175171 UTC] baz
[2023-09-14 06:35:14.710188216 UTC] baz
[2023-09-14 06:35:14.710200429 UTC] qux
[2023-09-14 06:35:14.71021154 UTC] quux
-}
```

## ログのスコープ化
さて、ここからは高階エフェクトの例を見ていこう。

まず、ログを出力するプログラムをスコープで区切って、ひとまとまりのブロックを表現する、
次のような高階エフェクトクラスを導入してみよう:

```haskell
-- | An effect that introduces a scope that represents a chunk of logs.
class LogChunk f where
    logChunk :: f a -> f a

makeEffectH ''LogChunk
```

新たに登場した`makeEffectH`は、高階エフェクトクラス用の自動導出THだ。

特に何もしない、スコープ内のログをそのまま出力する高階な解釈関数を書いてみよう:

```haskell
-- | Output logs in log chunks as they are.
passthroughLogChunk ::
    (Monad m, HFunctor (SumH r)) =>
    Hef (LogChunkS ': r) m ~> Hef r m
passthroughLogChunk = interpretH \(LogChunk m) -> m
```

すると、このエフェクトは例えば、次のようにして使える:

```haskell
main :: IO ()
main = runFreerEffects
    . logToIO
    . timeToIO
    . logWithTime
    . elaborated
    . passthroughLogChunk
    $ do
        log "foo"
        log "bar"
        log "baz"
        log "qux"

        sendIns $ putStrLn "------"

        logChunk do
            log "hoge"
            log "piyo"
            log "fuga"
            log "hogera"

        sendIns $ putStrLn "------"

        log "quux"
        log "foobar"


{- 実行結果:
[2023-09-14 07:50:25.808109772 UTC] foo
[2023-09-14 07:50:25.808203669 UTC] bar
[2023-09-14 07:50:25.808218386 UTC] baz
[2023-09-14 07:50:25.808232673 UTC] qux
------
[2023-09-14 07:50:25.808254394 UTC] hoge
[2023-09-14 07:50:25.808267879 UTC] piyo
[2023-09-14 07:50:25.808280974 UTC] fuga
[2023-09-14 07:50:25.808293527 UTC] hogera
------
[2023-09-14 07:50:25.808311922 UTC] quux
[2023-09-14 07:50:25.808325617 UTC] foobar
-}
```

### Heftiaにおける原則

`passthroughLogChunk`にて使われているものについて説明しよう。

`interpretH`は高階版の`interpret`だ。
また、ここで、関数の型が少し珍しいことになっている。

まず、制約の`HFunctor (SumH ...)`だが、これはheftia-effectsにおいて至る所で必要になる、
エフェクトクラス・リストに掛かる制約だ。
関数を書いてて`No instance for (HFunctor ...)`が出たら、関数の制約にこれを追加しよう。

そして`Hef`だが、これは`Fre` (Freer)に対する高階版のモナドトランスフォーマ、その名も**Heftia**である。
Heftiaはhefty algebraとco-Yonedaの合成であり、高階エフェクトを取り扱うために本ライブラリが新たに導入するものだ。

heftia-effectsでは、高階エフェクトを取り扱うために、Heftiaトランスフォーマを使う仕組みになっている。
そして、一階エフェクトのキャリア（すなわちFreer）は、基本的に高階エフェクトのキャリア（すなわちHeftia）の下位のキャリアとなる。

hefty algebraの提唱するエフェクトの取り扱われ方においては原則的に、
一階エフェクトへと自由にアクセスする（一階エフェクトの解釈・再解釈を好きに行う）ためには、まず「上に覆いかぶさっている」高階エフェクトのハンドリングをすべて終えなければならない。

高階エフェクトをハンドルすることは、一階エフェクトに対しては*interpret*と呼ばれる一方、しばしば*elaborate*と呼ばれる。
本ライブラリでは高階エフェクトに対するハンドルに関する命名は、*elaborate*、ないし*interpretH*といった一階側の相当物にHを付けることで行われている。

この用語を使って原則を言い換えるとつまり、「まず`elaborate`せよ、そうすれば`interpret`できるようになる」ということである。

制限が強いと思われるかもしれない。しかし、これは論文中で述べられていることだが、
この制限は第一に、エフェクトのハンドリングにおけるセマンティクスの正常性の保護のために重要である。
これにより、ハンドリングの結果の予測性が向上し、シンプルで直感的なものになる。
第二に、このhefty algebraに基づく方法は、制限と引き換えに自由を引き出す。
これについては後述する。

さらに、本ライブラリでは、Heftiaトランスフォーマが上に覆いかぶさっている状態でも、
多くの状況で適用可能な、下位のキャリア（典型的にはFreer）にアクセスして操作を施すための手段をいくつか提供する。
これはいわゆる`hoist`系の関数である。
しかし、これは挙動が不安定になることがあるという副作用があり、使用は慎重を期す。
将来的には非推奨になるかもしれない。

---

話を戻そう。

この`main`関数内で、`elaborated`は、すべての高階エフェクトのハンドリングがすべて完了し、
高階エフェクトクラスのリストが空になった（`Hef '[]`の形になった）タイミングで、
`Hef`トランスフォーマをrunして下位のキャリア（ここでは`Fre '[TImeI, LogI, IO]`）へと落とし込むための関数だ[^3]。

[^3]: 一階側にもこれに相当する、`interpreted`という関数が存在する。

### スコープの操作

以下は、スコープ内でログが`n`回以上投げられた場合、`n`回以降は省略し、
省略されたことをログに出すという挙動への再解釈を行う、インターポーズ系の関数である。

```haskell
-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk ::
    forall es es' m.
    (LogChunkS <<| es, LogI <| es', Monad m, HFunctor (SumH es)) =>
    Int ->
    Hef es (Fre es' m) ~> Hef es (Fre es' m)
limitLogChunk n =
    hoistHeftiaEffects (evalState 0)
        . interposeLogChunk
        . hoistHeftiaEffects raise
  where
    interposeLogChunk ::
        Hef es (Fre (StateI Int ': es') m)
            ~> Hef es (Fre (StateI Int ': es') m)
    interposeLogChunk = interposeH \(LogChunk a) ->
        logChunk $
            a & interposeIns \(Log msg) -> liftLowerH do
                count <- get
                when (count <= n) do
                    if count == n
                        then log "LOG OMITTED..."
                        else log msg
                    modify @Int (+ 1)
```

まず、`<<|`演算子は`<|`の高階版である。
制約は、高階側では`LogChunk`エフェクトクラスが、一階側では`Log`エフェクトクラスが
取り扱われていることを表している。

`hoistHeftiaEffects`は前述した`hoist`系の関数で、上に被さっている`Hef es`を貫通して一階の`Fre es' m`を操作するための関数である。
ここではまず、`raise`関数により`Fre es' m`を`Fre (StateI Int ': es') m`へと変形し、`Int`状態についての状態エフェクトクラスの取り扱いを導入している。
そして、`interposeLogChunk`における状態エフェクトを伴ったインターポーズを終えたら、最後に`evalState 0`により
状態エフェクトを初期値`0`でハンドルし、`Fre (StateI Int ': es') m`を `Fre es' m`へと戻している。

`interposeIns`もまた`hoist`系の関数で、これはHeftiaを貫通して一つ下位のFreerをインターポーズするための関数である。
`liftLowerH`はHeftiaトランスフォーマ用の`lift`関数である[^4]。

[^4]: `Fre`用には`liftLower`関数がある。

---

`limitLogChunk`関数を使うと、次のようにスコープ内のログの数が制限される。
`limitLogChunk 2`を間に挟んだ部分以外に変更はない。

```haskell
main :: IO ()
main = runFreerEffects
    . logToIO
    . timeToIO
    . logWithTime
    . elaborated
    . passthroughLogChunk
    . limitLogChunk 2
    $ do
        log "foo"
        log "bar"
        log "baz"
        log "qux"

        sendIns $ putStrLn "------"

        logChunk do
            log "hoge"
            log "piyo"
            log "fuga"
            log "hogera"

        sendIns $ putStrLn "------"

        log "quux"
        log "foobar"

{- 実行結果:
[2023-09-14 07:52:35.763686223 UTC] foo
[2023-09-14 07:52:35.763782454 UTC] bar
[2023-09-14 07:52:35.763799185 UTC] baz
[2023-09-14 07:52:35.76381255 UTC] qux
------
[2023-09-14 07:52:35.763850271 UTC] hoge
[2023-09-14 07:52:35.763870098 UTC] piyo
[2023-09-14 07:52:35.763887561 UTC] LOG OMITTED...
------
[2023-09-14 07:52:35.763916635 UTC] quux
[2023-09-14 07:52:35.763931614 UTC] foobar
-}
```
