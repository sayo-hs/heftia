# Example 2 - Higher-order effects

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

[^2]: `getCurrentTime`, `UTCTIme`は`time`パッケージより。

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
main =
    runFreerEffects
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
特に、以下のようなGADTsを生成する:
```hs
data LogChunkS f a where
    LogChunk :: f a -> LogChunkS f a
```

特に何もしない、スコープ内のログをそのまま出力する高階な解釈関数を書いてみよう:

```haskell
-- | Output logs in log chunks as they are.
passthroughLogChunk ::
    (Monad m, ForallHFunctor r) =>
    Hef (LogChunkS ': r) m ~> Hef r m
passthroughLogChunk = interpretH \(LogChunk m) -> m
```

すると、この`logChunk`エフェクトは例えば、次のようにして使える:

```haskell
logs :: (LogChunk m, Log m, IO <: m, Monad m) => m ()
logs =
    logChunk do
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

main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . elaborated
        . passthroughLogChunk
        $ logs

{- 実行結果:
[2023-09-14 11:22:52.125513923 UTC] foo
[2023-09-14 11:22:52.125611386 UTC] bar
[2023-09-14 11:22:52.125627817 UTC] baz
[2023-09-14 11:22:52.125641813 UTC] qux
------
[2023-09-14 11:22:52.125663835 UTC] hoge
[2023-09-14 11:22:52.125678823 UTC] piyo
[2023-09-14 11:22:52.125691336 UTC] fuga
[2023-09-14 11:22:52.125705543 UTC] hogera
------
[2023-09-14 11:22:52.125724589 UTC] quux
[2023-09-14 11:22:52.125739317 UTC] foobar
-}
```

### Heftiaにおける原則

`passthroughLogChunk`にて使われているものについて説明しよう。

`interpretH`は高階版の`interpret`だ。
またここで、関数の型が少し珍しいことになっている。

まず、制約の`ForallHFunctor ...`だが、これはheftia-effectsにおいて至る所で必要になる、
エフェクトクラス・リストに掛かる制約だ。
関数を書いていて`Could not deduce (Forall HFunctor ...)`が出たら、関数の制約にこれを追加しよう。

また、`Could not deduce KnownNat`といったエラーは、制約を関数に追加せずとも
`ghc-typelits-knownnat`プラグインを導入することで回避できるだろう。

そして`Hef`だが、これは`Fre` (Freer)に対する高階版、その名も**Heftia**（のモナドトランスフォーマー）である。
FreerがFreeモナドとco-Yonedaの合成であるように、
Heftiaはhefty treeとco-Yoneda（の高階版）の合成であり、高階エフェクトの取り扱いのために本ライブラリが新たに導入するものだ。

heftia-effectsでは、高階エフェクトを取り扱うために、Heftiaトランスフォーマを使う仕組みになっている。
そして、一階エフェクトのキャリア（すなわちFreer）は、基本的に高階エフェクトのキャリア（すなわちHeftia）の下位のキャリアに
配置されることになる。

hefty algebraの提唱するエフェクトの取り扱われ方においては原則的に、
一階エフェクトへと自由にアクセスする（一階エフェクトの解釈・再解釈を好きに行う）ためには、まず「上に覆いかぶさっている」高階エフェクトのハンドリングをすべて終えなければならない。

一階エフェクトをハンドルすることは*interpret*と呼ぶ一方、高階エフェクトをハンドルすることはしばしば*elaborate*と呼ばれる。
本ライブラリでは高階エフェクトに対するハンドルに関する命名は、*elaborate*、ないし*interpretH*のように一階側の相当物にHを付けることで行われている。

この用語を使って原則を言い換えるとつまり、「まず`elaborate`せよ、そうすれば`interpret`できるようになる」ということである。

制限が強いと思われるかもしれない。しかし、これは論文中で述べられていることだが、
この制限は第一に、エフェクトのハンドリングにおけるセマンティクスの正常性の保護のために重要である。
これにより、ハンドリングの結果の予測性が向上し、シンプルで直感的なものになる。
第二に、このhefty algebraに基づく方法は、制限と引き換えに自由を引き出す。
これについては次章で述べる。

さらに、本ライブラリでは、Heftiaトランスフォーマが上に覆いかぶさっている状態でも、
特定の状況で使用可能な、下位のキャリア（典型的にはFreer）にアクセスして操作を施すための手段を提供する。
これはいわゆる`hoist`系の関数である。
ただし後述するように、これの使用には注意が必要である。

---

話を戻そう。

この`main`関数内で、`elaborated`は、すべての高階エフェクトのハンドリングがすべて完了し、
高階エフェクトクラスのリストが空になった（`Hef '[]`の形になった）タイミングで、
`Hef`トランスフォーマをrunして下位のキャリア（ここでは`Fre '[TImeI, LogI, IO]`）へと落とし込むための関数だ[^3]。

[^3]: 一階側にもこれに相当する、`interpreted`という関数が存在する。

### スコープの操作

さて、`logChunk`エフェクトを使って、何か面白いことをしてみよう。

#### ログの出力回数の制限

以下は、スコープ内でログが`n`回以上投げられた場合、`n`回以降は省略し、
省略されたことをログに出すという挙動への再解釈を行うための関数である。

```haskell
-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk ::
    forall m.
    (LogChunk m, Log m, Monad m) =>
    Int ->
    LogChunkS (Fre '[LogI] m) ~> m
limitLogChunk n (LogChunk a) =
    logChunk
        . interpreted
        . evalState 0
        . interpretLog
        . flipFreer
        . raise
        $ a
  where
    interpretLog :: Fre '[LogI, StateI Int] m ~> Fre '[StateI Int] m
    interpretLog =
        interpret \(Log msg) -> do
            count <- get
            when (count <= n) do
                liftLower $
                    if count == n
                        then log "LOG OMITTED..."
                        else log msg

                modify @Int (+ 1)
```

まず、引数で受け取った`logChunk`のスコープを表すアクション
```hs
    a :: Fre '[LogI] m
```
は、関数`raise`によって
```hs
    raise a :: Fre '[StateI Int, LogI] m
```
へと変形される。これにより、状態エフェクトを扱えるようになる。
関数`raise`は、任意のエフェクトクラスをエフェクトクラスリストの先頭に挿入する関数だ。
そして、Freerのエフェクトクラスリストを並び替える関数`flipFreer`により
```hs
    flipFreer (raise a) :: Fre '[LogI, StateI Int] m
```
と変形される。
ここからがメインの処理で、`interpretLog`関数により、スコープ内で投げられるすべてのログを解釈する。
ログが投げられるたびに状態エフェクトが保持している値をインクリメントし、現在のカウントに応じてログを出力したりしなかったり、
省略されたことを表すログを出したりする。
最後に、`evalState`でカウンタの初期値を0として状態エフェクトをハンドルする。
また、`logChunk`エフェクトは`limitLogChunk`の解釈を通じて消費されてしまい、このままではスコープの情報は消失してしまうため、
引き続きスコープに応じたさらなるフックを可能にするために、最後に全体に`logChunk`を適用することで、スコープの情報を保持している。

また、`interpretLog`関数内で使用されている`liftLower`は、Freerトランスフォーマ用の`lift`関数である。
Heftiaトランスフォーマー用には`liftLowerH`がある。

---

`limitLogChunk`関数を使うと、次のようにスコープ内のログの数が制限される。
ここで、`limitLogChunk`は`runElaborate`関数と組み合わせて使う必要がある。

```haskell
main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . elaborated
        . passthroughLogChunk
        . interpreted
        . interpret (\(Log m) -> log m)
        . runElaborate @_ @HeftiaChurchT @SumUnionH
            (liftLower . limitLogChunk 2 |+: absurdUnionH)
        $ logs

{- 実行結果:
[2023-09-15 09:08:46.157032474 UTC] foo
[2023-09-15 09:08:46.15713674 UTC] bar
[2023-09-15 09:08:46.157159723 UTC] LOG OMITTED...
------
[2023-09-15 09:08:46.157204818 UTC] hoge
[2023-09-15 09:08:46.157224835 UTC] piyo
[2023-09-15 09:08:46.157245805 UTC] LOG OMITTED...
------
-}
```

ここで、`runElaborate`の引数の型に合わせるために`liftLower`を使用している。
`interpret (\(Log m) -> log m)`は、`logChunk`で囲われていないスコープ外において投げられたログを処理するものである。
この例ではすべて`logChunk`内であるため関係なく、単に型を合わせるためのものである。
また、`interpreted . logToIO`が追加されており、
既存のHeftiaの階層の上に新たにHeftiaとFreerの階層が乗っかった形になっている。
このように、heftia-effectsのトランスフォーマー・スタックは一般に、HeftiaとFreerがミルフィーユのような層を成す形となる。
この層の構造こそが、高階エフェクトと一階エフェクトの、制御-被制御の関係を型レベルで表現したものであり、
高階エフェクトを健全なセマンティクスの下で取り扱うためのガードレールの役割を担う。

ちなみに、`limitLogChunk`と同時に他の高階エフェクトクラスもelaborateしたい場合は、
```hs
    f1 |+: f2 |+: ... |+: fn |+: absurdUnionH
```
のように、`|+:`をリストの`:`演算子のように使うことで可能である。

`runElaborate`の型適用は、今現在のところ正しく型が推論されるために必要なものだが、
この構文の冗長さは将来のバージョンで改善される予定である。

#### ログをファイルに保存

次の例に移ろう。

まず準備として、ディレクトリ作成操作、ファイル書き込み操作を表現するエフェクトクラスを定義しよう。
```hs
class FileSystem f where
    mkdir :: FilePath -> f ()
    writeFS :: FilePath -> String -> f ()

makeEffectF ''FileSystem

runDummyFS :: (IO <: Fre r m, Monad m) => Fre (FileSystemI ': r) m ~> Fre r m
runDummyFS = interpret \case
    Mkdir path -> sendIns $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteFS path content -> sendIns $ putStrLn $ "<runDummyFS> writeFS " <> path <> " : " <> content
```

このインタプリタはダミーで、操作のエフェクトが投げられたら単にその旨を出力するだけのものだ[^4]。

[^4]: もちろん、実際にIOを行うインタプリタを書くことは容易である。

そして以下は、`logChunk`エフェクトのスコープに入るたびに、
その瞬間の時刻の名前のディレクトリを再帰的に作成し、
スコープ内において投げられるログをそのディレクトリに保存するように
`logChunk`の挙動を変更する関数である。

```haskell
-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk ::
    forall es es' m.
    ( LogChunkS <<| es
    , LogI <| es'
    , FileSystem (Fre es' m)
    , Time (Fre es' m)
    , Monad m
    , ForallHFunctor es
    ) =>
    Hef (LogChunkS ': es) (Fre (LogI ': es') m) ~> Hef es (Fre (LogI ': es') m)
saveLogChunk =
    interpretReader ("./log_chunks/" :: FilePath)
        . hoistHeftiaEffects flipFreer
        . interpretLogChunk
        . hoistHeftiaEffects flipFreer
        . flipHeftia
        . liftReader
  where
    interpretLogChunk ::
        Hef (LogChunkS ': LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es') m)
            ~> Hef (LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es') m)
    interpretLogChunk =
        interpretH \(LogChunk a) -> logChunk do
            chunkBeginAt <- currentTime & raise2 & liftLowerH
            local @FilePath (++ iso8601Show chunkBeginAt ++ "/") do
                newLogChunkDir <- ask & liftLowerH
                mkdir newLogChunkDir & raise2 & liftLowerH
                a & hoistInterpose \(Log msg) -> do
                    logAt <- currentTime & raise2
                    saveDir <- ask
                    log msg & raise2
                    writeFS (saveDir ++ iso8601Show logAt ++ ".log") (show msg) & raise2
```

まず、`<<|`型レベル演算子は`<|`の高階版である。
制約は、高階側では`LogChunk`エフェクトクラスが、一階側では`Log`エフェクトクラスが
取り扱われていることを表している。

`flipHeftia`は`flipFreer`のHeftia版だ。
`liftReader`は、`Hef ... (Fre ... m)`を`Hef (LocalS ': ...) (Fre (AskI ': ...) m)`の形へとリフトする関数だ。

そして`hoistHeftiaEffects`が、前述のhoist系の関数だ。
これを使うと、上に被さっている`Hef`を貫通して、下位のキャリアになっている一階の`Fre`を操作することができる。

`raise2`は`(raise . raise)`と等価だ。

全体の流れはこうだ:

まず、`liftReader`関数や`flip`系の関数を使って、
```haskell
Hef (LogChunkS ': es) (Fre (LogI ': es') m)
```
を
```haskell
Hef (LogChunkS ': LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es')
```
へと変形する。
追加された`Reader`系のエフェクトクラスは、今現在のスコープに対応したディレクトリのパスを保持する。

`interpretLogChunk`は、エフェクトクラスリスト先頭の`LogChunkS`を解釈し、`es`内の`LogChunkS`へと再送信する。
再解釈されたスコープ内では、現在時刻の名前のディレクトリを作成し、そしてログが投げられるたびにその時刻の名前のファイルを作成し、ログの内容を書き込む。
最後に、`Reader`のエフェクトを初期ディレクトリのパスを`"./log_chunks/"`としてハンドルしている。

---

以上の関数を使うと、例えば以下のようになる:

```hs
main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . runDummyFS
        . interpret (\(Log m) -> log m)
        . elaborated
        . passthroughLogChunk
        . saveLogChunk
        $ logs

{- 実行結果:
<runDummyFS> mkdir ./log_chunks/2023-09-15T09:43:52.199981569Z/
[2023-09-15 09:43:52.200115099 UTC] foo
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200107896Z.log : "foo"
[2023-09-15 09:43:52.200169902 UTC] bar
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200163049Z.log : "bar"
[2023-09-15 09:43:52.200219275 UTC] baz
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200211871Z.log : "baz"
[2023-09-15 09:43:52.200267285 UTC] qux
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200260312Z.log : "qux"
------
<runDummyFS> mkdir ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200335544Z/
[2023-09-15 09:43:52.200394645 UTC] hoge
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200335544Z/2023-09-15T09:43:52.200385077Z.log : "hoge"
[2023-09-15 09:43:52.200468874 UTC] piyo
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200335544Z/2023-09-15T09:43:52.200457022Z.log : "piyo"
[2023-09-15 09:43:52.200545648 UTC] fuga
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200335544Z/2023-09-15T09:43:52.200534888Z.log : "fuga"
[2023-09-15 09:43:52.200628704 UTC] hogera
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200335544Z/2023-09-15T09:43:52.200616501Z.log : "hogera"
------
[2023-09-15 09:43:52.200724204 UTC] quux
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200713073Z.log : "quux"
[2023-09-15 09:43:52.200794005 UTC] foobar
<runDummyFS> writeFS ./log_chunks/2023-09-15T09:43:52.199981569Z/2023-09-15T09:43:52.200784397Z.log : "foobar"
-}
```

スコープに入るたびに再帰的にディレクトリが作成され、そのスコープに対応したディレクトリにログファイルが保存されるという挙動
が実現されている。

---

さらに、この`saveLogChunk`と先程の`limitLogChunk`を組み合わせることも、もちろん可能だ。
このとき、合成の順番によって振る舞いが変わる。
`limitLogChunk`が先に適用されるようにすると:

```hs
main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . runDummyFS
        . interpret (\(Log m) -> log m)
        . elaborated
        . passthroughLogChunk
        . saveLogChunk
        . interpreted
        . interpret (\(Log m) -> log m)
        . runElaborate @_ @HeftiaChurchT @SumUnionH
            (liftLower . limitLogChunk 2 |+: absurdUnionH)
        $ logs

{- 実行結果:
<runDummyFS> mkdir ./log_chunks/2023-09-15T10:11:39.696369733Z/
[2023-09-15 10:11:39.696510378 UTC] foo
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696502403Z.log : "foo"
[2023-09-15 10:11:39.696573617 UTC] bar
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696565822Z.log : "bar"
[2023-09-15 10:11:39.696633649 UTC] LOG OMITTED...
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696626346Z.log : "LOG OMITTED..."
------
<runDummyFS> mkdir ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696700715Z/
[2023-09-15 10:11:39.696753224 UTC] hoge
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696700715Z/2023-09-15T10:11:39.696743917Z.log : "hoge"
[2023-09-15 10:11:39.696820531 UTC] piyo
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696700715Z/2023-09-15T10:11:39.696810502Z.log : "piyo"
[2023-09-15 10:11:39.696880012 UTC] LOG OMITTED...
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:11:39.696369733Z/2023-09-15T10:11:39.696700715Z/2023-09-15T10:11:39.696872378Z.log : "LOG OMITTED..."
------
-}
```

ファイルへの保存にも制限が適用される。
逆に、`saveLogChunk`を先に適用すると

```hs
main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . runDummyFS
        . elaborated
        . passthroughLogChunk
        . interpreted
        . interpret (\(Log m) -> log m)
        . runElaborate @_ @HeftiaChurchT @SumUnionH
            (liftLower . limitLogChunk 2 |+: absurdUnionH)
        . hoistHeftiaEffects (interpret \(Log m) -> log m)
        . saveLogChunk
        $ logs

{- 実行結果:
<runDummyFS> mkdir ./log_chunks/2023-09-15T10:19:16.000887165Z/
[2023-09-15 10:19:16.00101224 UTC] foo
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.000999395Z.log : "foo"
[2023-09-15 10:19:16.001072643 UTC] bar
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001062464Z.log : "bar"
[2023-09-15 10:19:16.001123529 UTC] LOG OMITTED...
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.00111353Z.log : "baz"
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001168804Z.log : "qux"
------
<runDummyFS> mkdir ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001221984Z/
[2023-09-15 10:19:16.001268862 UTC] hoge
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001221984Z/2023-09-15T10:19:16.001256298Z.log : "hoge"
[2023-09-15 10:19:16.001327252 UTC] piyo
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001221984Z/2023-09-15T10:19:16.001314748Z.log : "piyo"
[2023-09-15 10:19:16.001384669 UTC] LOG OMITTED...
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001221984Z/2023-09-15T10:19:16.001373448Z.log : "fuga"
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001221984Z/2023-09-15T10:19:16.001431868Z.log : "hogera"
------
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001487723Z.log : "quux"
<runDummyFS> writeFS ./log_chunks/2023-09-15T10:19:16.000887165Z/2023-09-15T10:19:16.001522258Z.log : "foobar"
-}
```

制限前の生のログがファイルへ出力される。

型を合わせるためにいくつかの小細工が必要なことに注意してほしい。
本ライブラリはあたかもRust言語のように、「型によるガードレール」という側面が大きいため、学習曲線が急な傾向にある。
Haskellに慣れた読者にとって、これはそれほど高い壁ではないであろうことを信じている。

## 高階エフェクト取り扱いの際の諸注意

高階エフェクトを扱う際には、いくつかの落とし穴がある。

### hoist系関数の非安全性
先程、Heftiaの層を貫通してFreerの層を操作することができるhoist系の関数の存在について述べた。
これらの関数の使用の際には注意が必要である。
hoistの際に関数に渡す自然変換`phi :: f ~> g`がmonad morphismでない場合、すなわち以下の法則を満たさない場合、操作はill-behavedとなる。

* 法則1

    ```hs
    forall m f. phi $ do { x <- m; f x } = do { x <- phi m; phi (f x) }
    ```

* 法則2

    ```hs
    forall x. phi (return x) = return x
    ```

monad morphismについての詳細は[mmorphパッケージのドキュメント](https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html)を参照してほしい。

ここまでの例においてhoist系関数が使用されている部分で、例えば`interpretReader`はこの法則を満たすため、問題が発生しなかった。
一方、この法則を満たさない変換の例として `evalState` (`interpretState`) がある。

以下は、`saveLogChunk`と同様の形式で書かれた`limitLogChunk`関数である。

```hs
limitLogChunkBroken ::
    forall es es' m.
    ( LogChunkS <<| es
    , LogI <| es'
    , Monad m
    , ForallHFunctor es
    ) =>
    Int ->
    Hef (LogChunkS ': es) (Fre (LogI ': es') m) ~> Hef es (Fre (LogI ': es') m)
limitLogChunkBroken n =
    hoistHeftiaEffects (evalState 0 . flipFreer)
        . interpretLogChunk
        . hoistHeftiaEffects (flipFreer . raise)
  where
    interpretLogChunk ::
        Hef (LogChunkS ': es) (Fre (LogI ': StateI Int ': es') m)
            ~> Hef es (Fre (LogI ': StateI Int ': es') m)
    interpretLogChunk =
        interpretH \(LogChunk a) ->
            logChunk $
                ($ a) $
                    hoistHeftiaEffects $ reinterpret \(Log msg) -> do
                        count <- get
                        when (count <= n) do
                            raise $
                                if count == n
                                    then log "LOG OMITTED..."
                                    else log msg
                            modify @Int (+ 1)
```

これを使用すると以下のようになり、期待した動作は得られない。

```hs
[2023-09-15 10:38:36.149857247 UTC] foo
[2023-09-15 10:38:36.149962565 UTC] bar
[2023-09-15 10:38:36.149988965 UTC] LOG OMITTED...
------
[2023-09-15 10:38:36.150036474 UTC] hoge
[2023-09-15 10:38:36.150063014 UTC] piyo
[2023-09-15 10:38:36.150090225 UTC] LOG OMITTED...
------
[2023-09-15 10:38:36.150133356 UTC] quux
[2023-09-15 10:38:36.15016217 UTC] foobar
```

途中までは良いが、`quux`以降は本来出力されないべきである。
カウンタの状態を見てみると:
```hs
...

                        when (count <= n) do
                            raise $
                                if count == n
                                    then log "LOG OMITTED..."
                                    else log $ "<" <> T.pack (show count) <> "> " <> msg
                            modify @Int (+ 1)

...

{- 実行結果:
[2023-09-15 10:51:45.798360938 UTC] <0> foo
[2023-09-15 10:51:45.798464993 UTC] <1> bar
[2023-09-15 10:51:45.798520367 UTC] LOG OMITTED...
------
[2023-09-15 10:51:45.798570251 UTC] <0> hoge
[2023-09-15 10:51:45.798599095 UTC] <1> piyo
[2023-09-15 10:51:45.798625705 UTC] LOG OMITTED...
------
[2023-09-15 10:51:45.798669577 UTC] <0> quux
[2023-09-15 10:51:45.79870274 UTC] <1> foobar
-}
```

カウンタの状態がリセットされてしまっている。
この挙動は、hoistにおいて使用されている`evalState`が法則1を満たさないために起こっていると考えられる。

注意が必要なのは、Freerから下位のキャリアを操作する場合でも同様である（おそらく）。

将来のバージョンでは、この非安全性は修正される予定である。
おおまかな方針としては、例えばmonad morphismな自然変換を表す以下のような型クラス`MonadMorph`を導入し、
自然変換を単に型シノニム`type f ~> g = forall x. f x -> g x`と定義するのではなく、
自然変換を表現するデータ型をいくつか用意し、法則を満たすものに限り`MonadMorph`のインスタンスとなるようにする。
```hs
class MonadMorph f g a | a -> f g where
    morph :: a -> (forall x. f x -> g x)
```
そして、すべてのモナドに関するhoist系関数には`MonadMorph`を制約として持たせる。
これにより、well-typedで安全にhoist系関数が扱えるようになるはずである。
今のところは、法則を満たしていることを確認しつつ注意して使うしかないが、次のバージョンをお待ちいただきたい。

ここまでの事実はすなわち、
一般にhoist系の操作は「まず先にelaborate、interpretはそのあと」という原則に抵触しているため、
法則なしには安全でない（制約付きで安全に可能である）、という話として解釈できる。
このことは、セマンティクス保護のために原則が重要であるということの説得力を増すと言えるだろう。

### エフェクトの干渉

`saveLogChunk`関数において、`LogChunk`や`Log`を`interpret`する代わりに、`interpose`を使って書くこともできる:

```hs
saveLogChunk' ::
    forall es es' m.
    ( LogChunkS <<| es
    , LogI <| es'
    , FileSystem (Fre es' m)
    , Time (Fre es' m)
    , Monad m
    , ForallHFunctor es
    ) =>
    Hef es (Fre es' m) ~> Hef es (Fre es' m)
saveLogChunk' =
    interpretReader ("./log_chunks/" :: FilePath)
        . interposeLogChunk
        . liftReader
  where
    interposeLogChunk ::
        Hef (LocalS FilePath ': es) (Fre (AskI FilePath ': es') m)
            ~> Hef (LocalS FilePath ': es) (Fre (AskI FilePath ': es') m)
    interposeLogChunk =
        interposeH \(LogChunk a) -> logChunk do
            chunkBeginAt <- currentTime & raise & liftLowerH
            local @FilePath (++ iso8601Show chunkBeginAt ++ "/") do
                newLogChunkDir <- ask & liftLowerH
                mkdir newLogChunkDir & raise & liftLowerH
                a & hoistInterpose \(Log msg) -> do
                    logAt <- currentTime & raise
                    saveDir <- ask
                    log msg
                    writeFS (saveDir ++ iso8601Show logAt ++ ".log") (show msg) & raise
```

これを使うと以下のようになる:

```hs
main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . runDummyFS
        . elaborated
        . passthroughLogChunk
        . saveLogChunk
        $ logs

{- 実行結果:
<runDummyFS> mkdir ./log_chunks/2023-09-15T12:06:00.261237746Z/
[2023-09-15 12:06:00.261369062 UTC] foo
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261363221Z.log : "foo"
[2023-09-15 12:06:00.261418806 UTC] bar
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261414117Z.log : "bar"
[2023-09-15 12:06:00.26146344 UTC] baz
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261458851Z.log : "baz"
[2023-09-15 12:06:00.261505278 UTC] qux
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.26150068Z.log : "qux"
------
<runDummyFS> mkdir ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/
<runDummyFS> mkdir ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/
[2023-09-15 12:06:00.261663496 UTC] hoge
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261657214Z.log : "hoge"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261649079Z.log : "hoge"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261639501Z.log : "hoge"
[2023-09-15 12:06:00.26178842 UTC] piyo
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261781367Z.log : "piyo"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261773342Z.log : "piyo"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261763664Z.log : "piyo"
[2023-09-15 12:06:00.261912223 UTC] fuga
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261903236Z.log : "fuga"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261895542Z.log : "fuga"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.261887987Z.log : "fuga"
[2023-09-15 12:06:00.262032469 UTC] hogera
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.262026257Z.log : "hogera"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.262018402Z.log : "hogera"
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.261555944Z/2023-09-15T12:06:00.261587653Z/2023-09-15T12:06:00.262009656Z.log : "hogera"
------
[2023-09-15 12:06:00.262157594 UTC] quux
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.262152575Z.log : "quux"
[2023-09-15 12:06:00.262200054 UTC] foobar
<runDummyFS> writeFS ./log_chunks/2023-09-15T12:06:00.261237746Z/2023-09-15T12:06:00.262194834Z.log : "foobar"
-}
```

スコープが深くなるにつれて、その深さに応じた回数だけ処理が重複している。
`logChunk`は、そのスコープの深さ分だけディレクトリを重複して作成し、
`log`はログをその深さ分だけ重複してファイルを保存する。

高階エフェクトのelaborationの際、このように`interpose`（そしておそらく`reinterpret`も）は、素朴に使用すると、
エフェクトを同一階層内で干渉させてしまい、結果の予想が難しくなる。
場合によっては、うまいことこの干渉動作を活用してコードを書くことができるかもしれない。
しかし大抵は、干渉の結果をコードから予想することは難しいだろうし、そうなると可読性に支障が出てくる。
エフェクトの階層を細かく区切り、再解釈においては`interpret`を使うなどして解釈元の階層と別階層へエフェクトを送信するのが、混乱が避けられてよいだろう。
もちろん`interpose`を使う場合でもこれは可能だ。`raise`や`liftLower`/`liftLowerH`を上手に使い、解釈元の階層にエフェクトが送られないようにしよう。

エフェクトの干渉は、前述のhoist系操作の非安全性とは異なり、別に何か危険があるわけではないと考えられる。
単に話が幾分難しくなるというだけで、hoist系のように何か根本的な法則が破れて結果がめちゃくちゃになってしまうということは、おそらくない。

`limitLogChunk`に関しては、おそらくその形式によって、解釈元と同一の階層に再度送信するように書くことは不可能なはずだ。
`runElaborate`と組み合わせるこの形式のelaborationは、原則にもっとも忠実で、故に型により振る舞いの良さ・セマンティクスの予測可能性が保証されていると言って良いだろう。
`saveLogChunk`の形式は、`runElaborate`と組み合わせる方法と比べて、elaborationを適用する側の柔軟性が高い。
runElaborateの方式においては、`|+:`によって組み合わされたelaboratorたちは、elaborationにおいて「一斉に・並列的に」実行され、相互作用することができない。
一方で`saveLogChunk`の形式は、他のelaboratorと相互作用する形で組み合わせることができる。
故に一般に、後者の形で書けるのであれば、そうしたほうがよい。
ただしその柔軟性の分だけ、elaborator実装の際のエフェクトの取り扱いはチャレンジングなものとなるだろう。
つまり、前者の形式のほうが保守的である。

## コード全体

参考までに、`limitLogChunk`、`saveLogChunk`の順番でelaboratorを合成した際のコードの全体を掲載する。

```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Effect.Class (SendIns (sendIns), type (<:), type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor)
import Control.Effect.Class.Machinery.TH (makeEffectF, makeEffectH)
import Control.Effect.Class.Reader (Ask (ask), AskI, Local (local), LocalS)
import Control.Effect.Class.State (State (get), StateI, modify)
import Control.Effect.Freer (
    Fre,
    flipFreer,
    interpose,
    interpret,
    interpreted,
    liftLower,
    raise,
    raise2,
    runFreerEffects,
    type (<|),
 )
import Control.Effect.Handler.Heftia.Reader (interpretReader, liftReader)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Heftia (
    Hef,
    elaborated,
    flipHeftia,
    hoistHeftiaEffects,
    hoistInterpose,
    interpretH,
    liftLowerH,
    runElaborate,
    type (<<|),
 )
import Control.Monad (when)
import Control.Monad.Trans.Heftia.Church (HeftiaChurchT)
import Data.Function ((&))
import Data.Hefty.Sum (SumH, SumUnionH)
import Data.Hefty.Union (absurdUnionH, (|+:))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Prelude hiding (log)

class Log f where
    log :: Text -> f ()

makeEffectF ''Log

logToIO :: (IO <: Fre r m, Monad m) => Fre (LogI ': r) m ~> Fre r m
logToIO = interpret \(Log msg) -> sendIns $ T.putStrLn msg

class Time f where
    currentTime :: f UTCTime

makeEffectF ''Time

timeToIO :: (IO <: Fre r m, Monad m) => Fre (TimeI ': r) m ~> Fre r m
timeToIO = interpret \case
    CurrentTime -> sendIns getCurrentTime

logWithTime :: (LogI <| es, Time (Fre es m), Monad m) => Fre es m ~> Fre es m
logWithTime = interpose \(Log msg) -> do
    t <- currentTime
    log $ "[" <> T.pack (show t) <> "] " <> msg

-- | An effect that introduces a scope that represents a chunk of logs.
class LogChunk f where
    logChunk :: f a -> f a

makeEffectH ''LogChunk

-- | Output logs in log chunks as they are.
passthroughLogChunk ::
    (Monad m, ForallHFunctor r) =>
    Hef (LogChunkS ': r) m ~> Hef r m
passthroughLogChunk = interpretH \(LogChunk m) -> m

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk ::
    forall m.
    (LogChunk m, Log m, Monad m) =>
    Int ->
    LogChunkS (Fre '[LogI] m) ~> m
limitLogChunk n (LogChunk a) =
    logChunk
        . interpreted
        . evalState 0
        . interpretLog
        . flipFreer
        . raise
        $ a
  where
    interpretLog :: Fre '[LogI, StateI Int] m ~> Fre '[StateI Int] m
    interpretLog =
        interpret \(Log msg) -> do
            count <- get
            when (count <= n) do
                liftLower $
                    if count == n
                        then log "LOG OMITTED..."
                        else log msg

                modify @Int (+ 1)

class FileSystem f where
    mkdir :: FilePath -> f ()
    writeFS :: FilePath -> String -> f ()

makeEffectF ''FileSystem

runDummyFS :: (IO <: Fre r m, Monad m) => Fre (FileSystemI ': r) m ~> Fre r m
runDummyFS = interpret \case
    Mkdir path -> sendIns $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteFS path content -> sendIns $ putStrLn $ "<runDummyFS> writeFS " <> path <> " : " <> content

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk ::
    forall es es' m.
    ( LogChunkS <<| es
    , LogI <| es'
    , FileSystem (Fre es' m)
    , Time (Fre es' m)
    , Monad m
    , ForallHFunctor es
    ) =>
    Hef (LogChunkS ': es) (Fre (LogI ': es') m) ~> Hef es (Fre (LogI ': es') m)
saveLogChunk =
    interpretReader ("./log_chunks/" :: FilePath)
        . hoistHeftiaEffects flipFreer
        . interpretLogChunk
        . hoistHeftiaEffects flipFreer
        . flipHeftia
        . liftReader
  where
    interpretLogChunk ::
        Hef (LogChunkS ': LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es') m)
            ~> Hef (LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es') m)
    interpretLogChunk =
        interpretH \(LogChunk a) -> logChunk do
            chunkBeginAt <- currentTime & raise2 & liftLowerH
            local @FilePath (++ iso8601Show chunkBeginAt ++ "/") do
                newLogChunkDir <- ask & liftLowerH
                mkdir newLogChunkDir & raise2 & liftLowerH
                a & hoistInterpose \(Log msg) -> do
                    logAt <- currentTime & raise2
                    saveDir <- ask
                    log msg & raise2
                    writeFS (saveDir ++ iso8601Show logAt ++ ".log") (show msg) & raise2

logs :: (LogChunk m, Log m, IO <: m, Monad m) => m ()
logs =
    logChunk do
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

main :: IO ()
main =
    runFreerEffects
        . logToIO
        . timeToIO
        . logWithTime
        . runDummyFS
        . interpret (\(Log m) -> log m)
        . elaborated
        . passthroughLogChunk
        . saveLogChunk
        . interpreted
        . interpret (\(Log m) -> log m)
        . runElaborate @_ @HeftiaChurchT @SumUnionH
            (liftLower . limitLogChunk 2 |+: absurdUnionH)
        $ logs

{- Execution result:
<runDummyFS> mkdir ./log_chunks/2023-09-15T13:08:58.061694233Z/
[2023-09-15 13:08:58.061839747 UTC] foo
<runDummyFS> writeFS ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.061832603Z.log : "foo"
[2023-09-15 13:08:58.061906122 UTC] bar
<runDummyFS> writeFS ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.061898517Z.log : "bar"
[2023-09-15 13:08:58.06196369 UTC] LOG OMITTED...
<runDummyFS> writeFS ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.061956957Z.log : "LOG OMITTED..."
------
<runDummyFS> mkdir ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.06204337Z/
[2023-09-15 13:08:58.062120395 UTC] hoge
<runDummyFS> writeFS ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.06204337Z/2023-09-15T13:08:58.062108332Z.log : "hoge"
[2023-09-15 13:08:58.062202208 UTC] piyo
<runDummyFS> writeFS ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.06204337Z/2023-09-15T13:08:58.062190025Z.log : "piyo"
[2023-09-15 13:08:58.062287759 UTC] LOG OMITTED...
<runDummyFS> writeFS ./log_chunks/2023-09-15T13:08:58.061694233Z/2023-09-15T13:08:58.06204337Z/2023-09-15T13:08:58.062269415Z.log : "LOG OMITTED..."
------
-}
```
