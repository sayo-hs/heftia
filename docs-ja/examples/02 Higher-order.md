# Example 2 - Higher-order effects

ここでは、ロギングのためのエフェクトを通じて、heftia-effectsにおける高階のエフェクトの取り扱い方を説明します。一階エフェクトはほとんど既存のFreerに基づくライブラリと同様であったのに対して、heftia-effectsにおける高階エフェクトの扱われ方は**Elaboration方式**といい、[Hefty Algebras (Casper et, al. 2023)](https://dl.acm.org/doi/10.1145/3571255)に基づく、まったく新しいものです。

以降、常体で解説を行います。

# 準備
このパートでは、外部のライブラリとして[`time ^>= 1.11.1`](https://hackage.haskell.org/package/time), [`text ^>= 1.2.5`](https://hackage.haskell.org/package/text)を使用する。

# ログ出力のエフェクトクラス

まず、復習も兼ねて必要な一階エフェクトを定義していこう。
ロギングを行う一階エフェクトは、例えば以下のように定義できる:

```haskell
import Data.Text (Text)

data Log a where
    Logging :: Text -> Log ()
makeEffectF [''Log]
```

これに対するインタプリタとして例えば、単にメッセージをそのまま標準出力に表示するようにする:

```haskell
logToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LLog ': r ~> eh :!! r
logToIO = interpretRec \(Logging msg) -> liftIO $ T.putStrLn msg
```

なお、ログレベルに関しては省略。

# 時刻取得エフェクト

次に、以下のようにして現在時刻を取得するエフェクトを導入できる。

```haskell
import Data.Time (UTCTime, getCurrentTime)

data Time a where
    CurrentTime :: Time UTCTime
makeEffectF [''Time]

timeToIO :: (IO <| r, ForallHFunctor eh) => eh :!! LTime ': r ~> eh :!! r
timeToIO = interpretRec \CurrentTime -> liftIO getCurrentTime
```

そして、以下のようにしてログに現在時刻を付加する再解釈関数を作ることができる。

```haskell
import Data.Time.Format (defaultTimeLocale, formatTime)

logWithTime :: (Log <| ef, Time <| ef, ForallHFunctor eh) => eh :!! ef ~> eh :!! ef
logWithTime = interposeRec \(Logging msg) -> do
    t <- currentTime
    logging $ "[" <> iso8601 t <> "] " <> msg

iso8601 :: UTCTime -> Text
iso8601 t = T.take 23 (T.pack $ formatTime defaultTimeLocale "%FT%T.%q" t) <> "Z"
```

試してみよう。

```haskell
main :: IO ()
main =
    runEff . logToIO . timeToIO . logWithTime $ do
        logging "foo"
        logging "bar"
        logging "baz"
        logging "baz"
        logging "qux"
        logging "quux"
```

実行結果:
```console
[2024-08-30T15:19:42.568Z] foo
[2024-08-30T15:19:42.568Z] bar
[2024-08-30T15:19:42.568Z] baz
[2024-08-30T15:19:42.568Z] baz
[2024-08-30T15:19:42.568Z] qux
[2024-08-30T15:19:42.569Z] quux
```

ここまでは Part 1 で説明した通りだ。

# ログのスコープ化
さて、ここからは高階エフェクトの例を見ていこう。

ログを出力するプログラムをスコープで区切って、名前の付いたひとまとまりのブロックを表現する、次のような高階エフェクトを導入してみよう。

```haskell
import Data.Effect.TH (makeEffectH)

-- | An effect that introduces a scope that represents a chunk of logs.
data LogChunk f (a :: Type) where
    LogChunk ::
        -- | chunk name
        Text ->
        -- | scope
        f a ->
        LogChunk f a
makeEffectH [''LogChunk]
```

このように、新たに型変数としてキャリアである`f :: Type -> Type`を追加する形式で定義する。新たに登場した`makeEffectH`は、高階エフェクト型のための自動導出THだ。この場合、`a :: Type`のようにして型変数のカインドの曖昧さを解消しないと`makeEffectH`がエラーになることに注意。

まず、特に何もせず、名前の情報も利用しない、スコープ内のログをそのまま保持してスコープの構造のみを捨てる高階な解釈関数を書いてみよう。

```haskell
import Control.Effect.Hefty (interpretRecH)

-- | Ignore chunk names.
runLogChunk :: ForallHFunctor eh => LogChunk ': eh :!! ef ~> eh :!! ef
runLogChunk = interpretRecH \(LogChunk _ m) -> m
```

`interpretRecH`は、`interpretRec`の高階版だ。
すると、この`LogChunk`エフェクトは例えば、次のようにして使える:

```haskell
logExample :: (LogChunk <<: m, Log <: m, MonadIO m) => m ()
logExample = do
    logging "out of chunk scope 1"
    logging "out of chunk scope 2"
    logging "out of chunk scope 3"
    logging "out of chunk scope 4"

    liftIO $ putStrLn "------"

    logChunk "scope1" do
        logging "in scope1 1"
        logging "in scope1 2"
        logging "in scope1 3"
        logging "in scope1 4"

        liftIO $ putStrLn "------"

        logChunk "scope2" do
            logging "in scope2 1"
            logging "in scope2 2"
            logging "in scope2 3"
            logging "in scope2 4"

        liftIO $ putStrLn "------"

        logging "in scope1 5"
        logging "in scope1 6"

main :: IO ()
main = runEff . logToIO . timeToIO . logWithTime . runLogChunk $ logExample
```

実行結果:
```
[2024-08-30T15:39:17.645Z] out of chunk scope 1
[2024-08-30T15:39:17.645Z] out of chunk scope 2
[2024-08-30T15:39:17.645Z] out of chunk scope 3
[2024-08-30T15:39:17.645Z] out of chunk scope 4
------
[2024-08-30T15:39:17.645Z] in scope1 1
[2024-08-30T15:39:17.645Z] in scope1 2
[2024-08-30T15:39:17.645Z] in scope1 3
[2024-08-30T15:39:17.646Z] in scope1 4
------
[2024-08-30T15:39:17.646Z] in scope2 1
[2024-08-30T15:39:17.646Z] in scope2 2
[2024-08-30T15:39:17.646Z] in scope2 3
[2024-08-30T15:39:17.646Z] in scope2 4
------
[2024-08-30T15:39:17.646Z] in scope1 5
[2024-08-30T15:39:17.646Z] in scope1 6
```

一階エフェクトを解釈することを*ハンドル/ハンドリング*と呼ぶのに対して、高階エフェクトを解釈することはここでは**elaborate/elaboration**と呼ぶ。これは方式の名前でもある。Heftiaは、高階エフェクトの仕組みに最新の、*Hefty AlgebraによるElaboration方式*をベースとしている。Elaborationには、
* 高階エフェクトというブロック（塊）を、より細かで具体的な一階エフェクトへと展開する
* 木構造を広げて平坦化する
というニュアンスが込められていると思われる。

さて、`LogChunk`エフェクトを使って、何か面白いことをしてみよう。

## ログをファイルに保存

まず準備として、ディレクトリ作成操作、ファイル書き込み操作を表現するエフェクトを定義しよう。
```haskell
data FileSystem a where
    Mkdir :: FilePath -> FileSystem ()
    WriteToFile :: FilePath -> String -> FileSystem ()
makeEffectF [''FileSystem]

runDummyFS :: (IO <| r, ForallHFunctor eh) => eh :!! LFileSystem ': r ~> eh :!! r
runDummyFS = interpretRec \case
    Mkdir path ->
        liftIO $ putStrLn $ "<runDummyFS> mkdir " <> path
    WriteToFile path content ->
        liftIO $ putStrLn $ "<runDummyFS> writeToFile " <> path <> " : " <> T.unpack content
```

このハンドラはダミーで、操作のエフェクトが投げられたら単にその旨を出力するだけのものだ[^1]。

[^1]: もちろん、実際にIOを行うインタプリタを書くことは容易である。

そして以下は、`LogChunk`エフェクトのスコープに入るたびに、その瞬間の時刻の名前のディレクトリを再帰的に作成し、スコープ内において投げられるログをそのディレクトリに保存するように`LogChunk`及び`Logging`をフックする関数である。

```haskell
import Control.Effect.Hefty (raise, raiseH, interposeRec, interposeRecH)
import Data.Effect.Reader (LAsk, Local, ask, local)
import Control.Effect.Handler.Heftia.Reader (runReader)

-- | Create directories according to the log-chunk structure and save one log in one file.
saveLogChunk ::
    forall eh ef.
    (LogChunk <<| eh, Log <| ef, FileSystem <| ef, Time <| ef, ForallHFunctor eh) =>
    eh :!! ef ~> eh :!! ef
saveLogChunk =
    raise >>> raiseH
        >>> hookCreateDirectory
        >>> hookWriteFile
        >>> runReader @FilePath "./log/"
  where
    hookCreateDirectory
        , hookWriteFile ::
            (Local FilePath ': eh :!! LAsk FilePath ': ef)
                ~> (Local FilePath ': eh :!! LAsk FilePath ': ef)
    hookCreateDirectory =
        interposeRecH \(LogChunk chunkName a) -> logChunk chunkName do
            chunkBeginAt <- currentTime
            let dirName = T.unpack $ iso8601 chunkBeginAt <> "-" <> chunkName
            local @FilePath (++ dirName ++ "/") do
                logChunkPath <- ask
                mkdir logChunkPath
                a

    hookWriteFile =
        interposeRec \(Logging msg) -> do
            logChunkPath <- ask
            logAt <- currentTime
            writeToFile (T.unpack $ T.pack logChunkPath <> iso8601 logAt <> ".log") msg
            logging msg
```

まず、`<<|`型レベル演算子は`<|`の高階版である。制約は、高階側では`LogChunk`エフェクト型が、一階側では`FileSystem`,`Time`,`Log`エフェクト型がリスト内に入っていることを表している。

`raise`は、一階エフェクトリストの先頭に新たに任意のエフェクトを追加するものだ:
```haskell
raise :: ForallHFunctor eh => eh :!! ef ~> eh :!! e ': ef
```
`raiseH`は、`raise`の高階エフェクト版である:
```haskell
raiseH :: (ForallHFunctor eh => eh :!! ef ~> e ': eh :!! ef
```
エフェクトリストからエフェクトを消去するためには解釈することが必要だが、単に追加するだけなら無制限に可能だ。

そして`>>>`演算子は関数合成演算子`.`の向きを反転させたもので、これはHaskellの標準で用意されているものだ。

全体の流れはこうだ:

まず、`raise`,`raiseH`により、入力で受けたエフェクトフルプログラムである
```haskell
eh :!! LLog ': ef
```
を
```haskell
Local FilePath ': eh :!! LAsk FilePath ': ef
```
へと変換する。

ここで`Local`は`Reader`系エフェクトの高階な操作に属する`local`エフェクト、`Ask`は一階な`ask`エフェクトに対応するエフェクト型だ。このようにHeftiaでは、従来のEEでは区別されていなかった一階エフェクトと高階エフェクトが異なるエフェクト型へと分割される。これが従来のEEと決定的に異なる点であり、一階エフェクトと高階エフェクトの特性の違い、すなわちそれぞれに対して可能な操作の違いを筋良く扱うための秘訣である。一階エフェクトに対して可能なことが、高階エフェクトが絡むと不可能になったりするのだ。これについては後述する。

リストに追加された`Reader`系のエフェクト型は、今現在のスコープに対応したディレクトリのパスを文脈（環境値）として保持する。

さて、これでディレクトリのパスを文脈として保持できるようになったので、`hookCreateDirectory`関数を用いてLogChunkのフックに入る。

`interposeRecH`は`interposeRec`の高階版だ。interposeに際して、何もしないとそのエフェクトは消えてしまう。つまり今回の場合、何もしなければ`logChunk`の構造は`interposeRecH`により失われてしまうのだ。そこで、再度`logChunk`で囲むことで、このフックの後でもログチャンクの構造を失うことなく、ログチャンクの構造を利用した他のフックを繰り返し行うことができる。

新たな解釈の内部では、`Time`エフェクトから現在時刻を取得し、それとチャンク名を組み合わせた名前のディレクトリを作成しつつ、`local`エフェクトで環境値を新たなディレクトリへと移す。

次に、`hookWriteFile`関数にて、ログチャンク内部のすべてのログエフェクトに対して、ファイル書き込み操作をフックする。その際のファイル名はそのときの時刻だ。`hookCreateDirectory`関数を通して既に環境値はLogChunkの構造に従ったディレクトリパスに設定されているので、現在の、いわばこの文脈での"カレントディレクトリ"[^2]は`ask`で取得できる。

[^2]: 実際のOSのプロセスの状態の意味でのカレントディレクトリではないことに注意

最後に、`Reader`のエフェクトを初期ディレクトリのパスを`"./log/"`としてハンドルしている。

---

以上の関数を使うと、例えば以下のようになる:

```haskell
import Data.Free.Sum (type (+))
import Control.Effect.ExtensibleFinal (type (!!))

runApp :: LogChunk !! FileSystem + Time + Log + IO ~> IO
runApp =
    runLogChunk
        >>> runDummyFS
        >>> logWithTime
        >>> timeToIO
        >>> logToIO
        >>> runEff

main :: IO ()
main = runApp . saveLogChunk $ logExample
```

実行結果:
```console
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.209Z.log : out of chunk scope 1
[2024-08-31T11:40:19.209Z] out of chunk scope 1
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.209Z.log : out of chunk scope 2
[2024-08-31T11:40:19.209Z] out of chunk scope 2
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.209Z.log : out of chunk scope 3
[2024-08-31T11:40:19.209Z] out of chunk scope 3
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.209Z.log : out of chunk scope 4
[2024-08-31T11:40:19.209Z] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-08-31T11:40:19.210Z-scope1/
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z.log : in scope1 1
[2024-08-31T11:40:19.210Z] in scope1 1
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z.log : in scope1 2
[2024-08-31T11:40:19.210Z] in scope1 2
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z.log : in scope1 3
[2024-08-31T11:40:19.210Z] in scope1 3
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z.log : in scope1 4
[2024-08-31T11:40:19.210Z] in scope1 4
------
<runDummyFS> mkdir ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z-scope2/
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z-scope2/2024-08-31T11:40:19.210Z.log : in scope2 1
[2024-08-31T11:40:19.210Z] in scope2 1
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z-scope2/2024-08-31T11:40:19.210Z.log : in scope2 2
[2024-08-31T11:40:19.210Z] in scope2 2
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z-scope2/2024-08-31T11:40:19.210Z.log : in scope2 3
[2024-08-31T11:40:19.210Z] in scope2 3
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z-scope2/2024-08-31T11:40:19.210Z.log : in scope2 4
[2024-08-31T11:40:19.210Z] in scope2 4
------
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z.log : in scope1 5
[2024-08-31T11:40:19.210Z] in scope1 5
<runDummyFS> writeToFile ./log/2024-08-31T11:40:19.210Z-scope1/2024-08-31T11:40:19.210Z.log : in scope1 6
[2024-08-31T11:40:19.210Z] in scope1 6
```

エフェクトたちを`IO`へと解釈する系の処理は`runApp`関数としてまとめた。ここの型シグネチャにおいて出てきている`!!`や`+`は、`:!!`が型レベルリストを使うのに対する代替の記法で、`eh`や`ef`や`r`といった多相化されたリストの型変数が出現しない場合このように簡潔に書くことができる。
```haskell
(LogChunk !! (FileSystem + Time + Log + IO)) ~> IO
```
のように結合する。高階エフェクトには`+`の代わりに`:+:`を使う。

結果は、スコープに入るたびに再帰的にディレクトリが作成され、そのスコープに対応したディレクトリにログファイルが保存されるという挙動が実現されている。

## ログの出力回数の制限

次の例に移ろう。
以下は、スコープ内でログが`n`回以上投げられた場合、`n`回以降は省略し、省略されたことをログに出すという挙動への再解釈を行うフックである。

```haskell
import Control.Effect.Hefty (raiseUnder)
import Data.Effect.State (get, modify)
import Control.Effect.Handler.Heftia.State (evalState)
import Control.Effect.Hefty (Elab)

-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk :: Log <| ef => Int -> '[LogChunk] :!! LLog ': ef ~> '[LogChunk] :!! LLog ': ef
limitLogChunk n = reinterpretRecH $ elabLimitLogChunk n

elabLimitLogChunk :: Log <| ef => Int -> Elab LogChunk ('[LogChunk] :!! LLog ': ef)
elabLimitLogChunk n (LogChunk name a) =
    logChunk name do
        raise . raiseH $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog :: Log <| ef => '[] :!! LLog ': ef ~> '[] :!! ef
    limitLog a' =
        evalState @Int 0 $
            raiseUnder a' & interpretRec \(Logging msg) -> do
                count <- get
                when (count < n) do
                    logging msg
                    when (count == n - 1) do
                        logging "Subsequent logs are omitted..."

                    modify @Int (+ 1)
```

`State`エフェクトは状態付きの計算を実現する。

また、`raiseUnder`は、エフェクトリストの先頭`e1`のひとつ下に新たな任意のエフェクト型`e2`を挿入する:
```haskell
raiseUnder :: ForallHFunctor eh => eh :!! e1 ': ef ~> eh :!! e1 ': e2 ef
```

`Elab`は単に
```haskell
type Elab e f = e f ~> f
```
という型シノニムである。これは後述する*Elaboration射*を表す。

`reinterpretRecH`は、基本は単なる`interpretRecH`だが、入力の先頭のエフェクト型`e1`を新しいエフェクト型`e2`へと変換するというようなニュアンスをもち、そのために`raiseUnderH`が挿入される:
```haskell
reinterpretRecH ::
    (HFunctor e1, HFunctor e2, ForallHFunctor eh) =>
    (e1 (e2 ': eh :!! ef) ~> e2 ': eh :!! ef) ->
    e1 ': eh :!! ef ~> e2 ': eh :!! ef
reinterpretRecH i = interpretRecH i . raiseUnderH
```

内部では、ログが投げられるたびに状態エフェクトが保持している値をインクリメントし、現在のカウントに応じてログを出力したりしなかったり、省略されたことを表すログを出したりする。そして、`evalState`でカウンタの初期値を0として状態エフェクトをハンドルする。
また先程と同様、フック後もLogChunkの構造を残すために、全体を`logChunk`で包んでいる。

ここで、`Logging`エフェクトに対してフックを掛けるために`interposeRec`ではなく`interpretRec`を使用していることに注意してほしい。これは、フック元の`Logging`エフェクトが入る*スロット*とフック先の`Logging`エフェクトが入る*スロット*を分けることで、**エフェクトの干渉**を防ぐためである。*エフェクトの干渉*については後述する。スロットについては[Part 1 の タグ付きエフェクト型の節](https://zenn.dev/articles/9061f0121f3cf5#%E3%82%BF%E3%82%B0%E4%BB%98%E3%81%8D%E3%82%A8%E3%83%95%E3%82%A7%E3%82%AF%E3%83%88%E5%9E%8B)を参照。

---

`limitLogChunk`関数を使うと、次のようにスコープ内のログの数が制限される。LogChunkスコープ外のログについてはそのままになる。

```haskell
import Control.Effect.Hefty (subsume)

main :: IO ()
main = runApp . subsume . limitLogChunk 2 $ logExample
```

```console
[2024-08-31T12:05:57.856Z] out of chunk scope 1
[2024-08-31T12:05:57.856Z] out of chunk scope 2
[2024-08-31T12:05:57.856Z] out of chunk scope 3
[2024-08-31T12:05:57.856Z] out of chunk scope 4
------
[2024-08-31T12:05:57.856Z] in scope1 1
[2024-08-31T12:05:57.856Z] in scope1 2
[2024-08-31T12:05:57.856Z] Subsequent logs are omitted...
------
[2024-08-31T12:05:57.856Z] in scope2 1
[2024-08-31T12:05:57.856Z] in scope2 2
[2024-08-31T12:05:57.856Z] Subsequent logs are omitted...
------
```

ここで`subsume`は先頭のエフェクトをそれよりも下位へと送信する関数だ:
```haskell
subsume :: (e <| ef, ForallHFunctor eh) => eh :!! LiftIns e ': ef ~> eh :!! ef
subsume = interpretRec sendIns
```
これは、フック元の`Log`のスロットがフック後もまだ残っており、LogChunkスコープ外にあるログがそこにまだ入っているため、それらをすべて下位に存在するフック先の`Log`スロットへと送り切るためである。

ここまでで重要なのは、**`State`エフェクトのハンドル、そして一般にエフェクトに対して*状態付きの解釈*を行うときは、基本的に高階エフェクトは既にすべて解釈済みで、高階エフェクトリストは空になっていなければならない**ということである。これが先に述べた、一階エフェクトと高階エフェクトの可能な操作の違いのことだ。ある高階エフェクトを未elaboratedの状態で残しつつ、その他の一階/高階エフェクトを状態付きで解釈することは不可能である。これは、状態付き解釈の場合のみは唯一、モジュラー性に限界があるということだ。状態付きな解釈を行う際は、まず最初にすべての高階エフェクトを一斉に、同時にelaborateしなければならない。これは`interpretAllH`系関数と`|+:`演算子を使うことで可能だ。これについては今後のパートで説明できればと思う。

解釈が状態付きでない場合、高階エフェクトが未elaboratedの状態でも解釈が可能だ。`interpretRec`のように、後ろに`Rec`と付いている関数はこのためにある。`Rec`はRecursively（再帰的）の意味である。高階エフェクトを含んでいるエフェクトフルプログラムは構造として、高階エフェクトのスコープの中にさらに高階エフェクトを含むことができるという木構造を持つ。この木のノードは高階エフェクトであり、エッジはその引数のうちキャリア型変数`f`を含んでいるものたちである。高階エフェクトを含むプログラムが木構造を成すというのは、先程の`logExample`を見るとわかるだろう。`Rec`系関数はこの木構造に対して、一番深い所から浅い所に向かって順番に解釈やフック等の変換操作を適用する。

## フックの合成

さらに、この`saveLogChunk`と先程の`limitLogChunk`を組み合わせることができる。すなわち、フック（そして一般にelaboration）は合成可能で、モジュラーだ。

ここで、合成の順番によって振る舞いが変わる。`limitLogChunk`が先に適用されるようにすると

```haskell
main :: IO ()
main = runApp . saveLogChunk . subsume . limitLogChunk 2 $ logExample
```

実行結果:
```console
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.140Z.log : out of chunk scope 1
[2024-08-31T14:04:04.141Z] out of chunk scope 1
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z.log : out of chunk scope 2
[2024-08-31T14:04:04.141Z] out of chunk scope 2
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z.log : out of chunk scope 3
[2024-08-31T14:04:04.141Z] out of chunk scope 3
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z.log : out of chunk scope 4
[2024-08-31T14:04:04.141Z] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-08-31T14:04:04.141Z-scope1/
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z-scope1/2024-08-31T14:04:04.141Z.log : in scope1 1
[2024-08-31T14:04:04.141Z] in scope1 1
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z-scope1/2024-08-31T14:04:04.141Z.log : in scope1 2
[2024-08-31T14:04:04.141Z] in scope1 2
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z-scope1/2024-08-31T14:04:04.141Z.log : Subsequent logs are omitted...
[2024-08-31T14:04:04.141Z] Subsequent logs are omitted...
------
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z-scope1/2024-08-31T14:04:04.141Z.log : in scope2 1
[2024-08-31T14:04:04.141Z] in scope2 1
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z-scope1/2024-08-31T14:04:04.141Z.log : in scope2 2
[2024-08-31T14:04:04.141Z] in scope2 2
<runDummyFS> writeToFile ./log/2024-08-31T14:04:04.141Z-scope1/2024-08-31T14:04:04.141Z.log : Subsequent logs are omitted...
[2024-08-31T14:04:04.141Z] Subsequent logs are omitted...
------
```

ファイルへの保存にも制限が適用される。

逆に、`saveLogChunk`を先に適用すると
```haskell
main :: IO ()
main = runApp . subsume . limitLogChunk 2 . saveLogChunk $ logExample
```

実行結果:
```
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z.log : out of chunk scope 1
[2024-08-31T14:05:03.211Z] out of chunk scope 1
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z.log : out of chunk scope 2
[2024-08-31T14:05:03.211Z] out of chunk scope 2
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z.log : out of chunk scope 3
[2024-08-31T14:05:03.211Z] out of chunk scope 3
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z.log : out of chunk scope 4
[2024-08-31T14:05:03.211Z] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-08-31T14:05:03.211Z-scope1/
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.211Z.log : in scope1 1
[2024-08-31T14:05:03.211Z] in scope1 1
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.211Z.log : in scope1 2
[2024-08-31T14:05:03.211Z] in scope1 2
[2024-08-31T14:05:03.211Z] Subsequent logs are omitted...
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z.log : in scope1 3
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z.log : in scope1 4
------
<runDummyFS> mkdir ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z-scope2/
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z-scope2/2024-08-31T14:05:03.212Z.log : in scope2 1
[2024-08-31T14:05:03.212Z] in scope2 1
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z-scope2/2024-08-31T14:05:03.212Z.log : in scope2 2
[2024-08-31T14:05:03.212Z] in scope2 2
[2024-08-31T14:05:03.212Z] Subsequent logs are omitted...
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z-scope2/2024-08-31T14:05:03.212Z.log : in scope2 3
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z-scope2/2024-08-31T14:05:03.212Z.log : in scope2 4
------
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z.log : in scope1 5
<runDummyFS> writeToFile ./log/2024-08-31T14:05:03.211Z-scope1/2024-08-31T14:05:03.212Z.log : in scope1 6
```

制限前の生のログがファイルへ出力される。

`subsume`と`limitLogChunk`の間で`saveLogChunk`を適用したらどうなるか？
```haskell
main :: IO ()
main = runApp . subsume . saveLogChunk . limitLogChunk 2 $ logExample
```

```console
<runDummyFS> writeToFile ./log/2024-08-31T14:05:40.969Z.log : out of chunk scope 1
[2024-08-31T14:05:40.969Z] out of chunk scope 1
<runDummyFS> writeToFile ./log/2024-08-31T14:05:40.969Z.log : out of chunk scope 2
[2024-08-31T14:05:40.970Z] out of chunk scope 2
<runDummyFS> writeToFile ./log/2024-08-31T14:05:40.970Z.log : out of chunk scope 3
[2024-08-31T14:05:40.970Z] out of chunk scope 3
<runDummyFS> writeToFile ./log/2024-08-31T14:05:40.970Z.log : out of chunk scope 4
[2024-08-31T14:05:40.970Z] out of chunk scope 4
------
<runDummyFS> mkdir ./log/2024-08-31T14:05:40.970Z-scope1/
[2024-08-31T14:05:40.970Z] in scope1 1
[2024-08-31T14:05:40.970Z] in scope1 2
[2024-08-31T14:05:40.970Z] Subsequent logs are omitted...
------
[2024-08-31T14:05:40.970Z] in scope2 1
[2024-08-31T14:05:40.970Z] in scope2 2
[2024-08-31T14:05:40.970Z] Subsequent
```

先程述べたように、`subsume`によりフック先の`Log`スロットへまとめられる前の、リスト先頭のフック元の`Log`スロットには、チャンク外にあるログが保持されているのだった。`<|`演算子を制約とする`interpose`系関数は、リストのもっとも上位の該当するエフェクトに対してフックを適用する。したがって、この未処理のチャンク外ログがファイルへの保存処理の対象となるのだ。

# 高階エフェクト取り扱いの際の注意

高階エフェクトを扱う際には、エフェクトの干渉に注意する必要がある。

先程の`limitLogChunk`関数において、`Log`のスロットを分けて`Log`を`interpretRec`する代わりに、スロットを分けずに`interposeRec`を使って書くこともできる:

```haskell
-- | Limit the number of logs in a log chunk to the first @n@ logs.
limitLogChunk :: Log <| ef => Int -> '[LogChunk] :!! ef ~> '[LogChunk] :!! ef
limitLogChunk n = reinterpretRecH $ elabLimitLogChunk n

elabLimitLogChunk :: Log <| ef => Int -> Elab LogChunk ('[LogChunk] :!! ef)
elabLimitLogChunk n (LogChunk name a) =
    logChunk name do
        raiseH $ limitLog $ runLogChunk $ limitLogChunk n a
  where
    limitLog :: Log <| ef => '[] :!! ef ~> '[] :!! ef
    limitLog a' =
        evalState @Int 0 $
            raise a' & interposeRec \(Logging msg) -> do
                count <- get
                when (count < n) do
                    logging msg
                    when (count == n - 1) do
                        logging "Subsequent logs are omitted..."

                    modify @Int (+ 1)
```

これを使うと以下のようになる。`subsume`は付ける必要がなくなる。

```haskell
main :: IO ()
main = runApp . limitLogChunk 2 $ logExample
```

実行結果:
```console
[2024-08-31T14:15:14.169Z] out of chunk scope 1
[2024-08-31T14:15:14.169Z] out of chunk scope 2
[2024-08-31T14:15:14.169Z] out of chunk scope 3
[2024-08-31T14:15:14.169Z] out of chunk scope 4
------
[2024-08-31T14:15:14.169Z] in scope1 1
[2024-08-31T14:15:14.169Z] in scope1 2
[2024-08-31T14:15:14.169Z] Subsequent logs are omitted...
------
------
```

`scope1`については正常だが、`scope2`のログが最初から消えてしまっている。これは、`interposeRec`の再帰的な動作を考えると理解できる。

フックは木の深いところから行われるので、まず`scope2`のレベルのものが再解釈され、
```haskell
logging "in scope2 1"
logging "in scope2 2"
logging "in scope2 3"
logging "in scope2 4"
```
が
```haskell
logging "in scope2 1"
logging "in scope2 2"
logging "Subsequent logs are omitted..."
```
へと変換される。ここまでは問題ない。

問題は、次にこれが`scope1`のレベルに登ったときである。いま、`scope1`のレベルは
```haskell
logging "in scope1 1"
logging "in scope1 2"
logging "in scope1 3"
logging "in scope1 4"

liftIO $ putStrLn "------"

logChunk "scope2" do
    logging "in scope2 1"
    logging "in scope2 2"
    logging "Subsequent logs are omitted..."

liftIO $ putStrLn "------"

logging "in scope1 5"
logging "in scope1 6"
```
のようになっている。いま、この中の`logging`はすべて同一のスロットに入っており、区別されることがない。この状態でフックが行われるとどうなるか？
```haskell
logging "in scope1 1"
logging "in scope1 2"
logging "Subsequent logs are omitted..."

liftIO $ putStrLn "------"

logChunk "scope2" do
    pure ()

liftIO $ putStrLn "------"
```
そう、`scope1`のレベルのログだけでなく、先程の`scope2`のフック結果のログも区別されず省略の対象になってしまうのだ。これを防ぐためには、深い階層からのフック結果がさらなるフックの対象にされてしまうことを回避する必要がある。このために、スロットを分ける必要があったのだ。

高階エフェクトの解釈やフックの際、スロットを分けずに素朴に操作を行おうとすると、このようにエフェクトを同一スロット内で干渉させてしまい、結果の予想が難しくなることがある。場合によっては、この干渉動作をうまいこと活用してコードを書くことができるかもしれない。しかし大抵は、干渉の結果をコードから予想することは難しいだろうし、可読性に支障が出てくる。エフェクトのスロットを細かく区切り、再解釈においては`interpret`を使うなどして解釈元の階層と別スロットへエフェクトを送信するようにするのが、混乱が避けられてよいだろう。もちろん`interpose`を使う場合でもスロットを分けることは可能だ。`raise`系関数をうまく使い、フック元・再解釈元のスロットにエフェクトが送られないようにしよう。また今回は使わなかったが、エフェクトタグをうまく使おう。

# Heftiaにおける用語

ここで一旦、基本的な概念についてまとめつつ、応用的な概念をいくつか導入する。

* エフェクト (effect)
    エフェクトフルプログラムから送出され、インタプリタにより解釈されるデータ。また、それを作り出すコンストラクタ。

* エフェクト型 (effect type)
    いくつかの関連するエフェクトを直和でまとめたもの。エフェクト集合の要素として現れる。エフェクトは、エフェクト型のコンストラクタ。

* エフェクトフルプログラム (effectful program)
    複数のエフェクトを特定の制御構造の形式に従って並べたもの。制御構造の形式にはモナドの他にアプリカティブなどがある。モナドな形式のエフェクトフルプログラムはモナディックプログラム (monadic program) 、アプリカティブな形式のものはアプリカティブプログラム (applicative program) と呼ぶ。

* キャリア (carrier)
    制御構造の形式を表現する型クラス (`Monad`, `Applicative`等) のインスタンスとなる、カインドが`Type -> Type`のデータ型（型構築子）。例えば、`IO`モナドはキャリアである。Heftiaが使用するキャリアはコード中では通常`(:!!)`演算子で表される。多相化する場合、型変数`f`で表す。モナドの場合は特に`m`。

* エフェクトリスト
    エフェクト集合を型レベルリストとしてHaskell内で表現したもの。

* エフェクト制約
    エフェクト集合を`<:`, `<|`等を用いた制約としてHaskell内で表現したもの。

* 解釈 (interpret/interpretation)、
    エフェクトインタフェースに従ったデータであるエフェクトを、別のエフェクトフルプログラムへと変換すること。単なるデータであるエフェクトに、振る舞いとしての意味付けを行う。

* インタプリタ (interpreter/解釈器)
    与えられた解釈の下で、入力されたエフェクトフルプログラムが保持しているすべてのエフェクトに対してその解釈を適用し、別のエフェクトフルプログラムへと変換し出力するようなHaskell上の関数。

* 解釈関数 (interpreting function)
    `interpret`, `interpretRec`, `interpretRecH`等の、解釈をしたり解釈器を作ったりするためにHeftiaライブラリ上で用意された関数のこと。

* 一階エフェクト
    引数にエフェクトフルプログラムを含まないもの。より厳密には、引数部の型$A$中にキャリアの型変数`f`（モナドの場合`m`）が出現しないもの。

* 高階エフェクト
    一階エフェクトでないエフェクト。

* ハンドル (handle) 、ハンドリング (handling) 、ハンドラ (handler)
    一階エフェクトを解釈すること。またその解釈器。

* elaborate/elaboration/elaborator
    高階エフェクトを解釈すること。またその解釈器。

* 解釈射/ハンドリング射/elaboration射 (interpretation/handling/elaboration morphism)
    解釈関数に与える`i :: e ~> f`ないし`i :: e f ~> f`の形式の関数のこと。ここで`e`はエフェクト型で`f`はキャリア。例えば前述の中の`elabLimitLogChunk n`はelaboration射である。

また、次のものは発展的な概念であり、読み飛ばしても問題ない。

* エフェクトインタフェース (effect interface)
    エフェクトの引数と戻り値の型と、エフェクトの名前の組のこと。エフェクトの名前が$N$、引数の数が$n$、$i$番目の引数の型が$A_i$、戻り値の型が$R$のとき、組$(N,\{A_i\}_{i=1,2,...,n},R)$のこと。ドメインロジック（エフェクトフルプログラム側）とインフラシステム（インタプリタ側）の間の橋渡しをするインタフェース。抽象化層。これを定めることで、DSL（ドメイン特化言語）を構築することができる。

    例えば
    * Loggingエフェクトのインタフェースは$N=$`Logging`, $A_1=$`Text`, $R=$`()`
    * LogChunkエフェクトのインタフェースは$N=$`LogChunk`, $A_1=$`Text`, $A_2=$`f a`, $R=$`a`
        * ここで`a`は型変数で、`f`は特別にキャリアを表す

* 状態付き解釈 / 無状態解釈 (stateful/stateless interpretation)
    エフェクトの解釈がある意味で状態に依存するとき、その解釈は状態付き解釈であるという。具体的には、解釈において*解釈先キャリアのモナド性を利用する*とき、状態付きである。

    Heftiaの解釈関数には様々なバリエーションがあるが、究極的には以下の操作の形へと還元される:

    ```haskell
    interpretWhole :: (eh f ~> f) -> (ef ~> f) -> '[eh] :!! '[ef] ~> f
    ```

    すなわち、解釈先である`:!!`キャリアが保持しているエフェクトをすべて別の解釈先キャリア`f`へと変換することで、`:!!`キャリア全体を`f`へと変換する操作だ。
    ここで、解釈先キャリア`f`は任意に取ることができる。解釈において、この`f`がモナドであることに依存しているとき、すなわち`f`がモナド以外に取り得ないときに限り、その操作は状態付き解釈である。
    例えば、Heftiaの`evalState`は内部で`f = StateT s ('[] :!! ef)`としてこの`interpretWhole`に相当する関数を使用している。`StateT`モナドトランスフォーマーの代わりに何か他のキャリアで代用することを試みようとしても（例えば継続モナドなど）、いずれにせよ`f`はモナドでなければならない。なぜなら、状態`s`を保持し、それに応じて挙動を逐次動的に変更する必要があるからである。したがって、`evalState`は状態付き解釈である。

# Elaboration方式のメンタルモデル

*Hefty AlgebraのElaboration方式*においては原則的に、一階エフェクトへと自由にアクセスする（一階エフェクトの解釈・再解釈を好きに行う）ためには、高階エフェクトの解釈をすべて終えなければならない。つまり、「まずelaborateせよ、そうすればハンドルできるようになる」ということである。**一階エフェクトの上に高階エフェクトが覆い被さっているイメージ**を持ってほしい。解釈という行為は、覆いを取り払う操作のイメージだ。

この制限は第一に、エフェクトの解釈における挙動（意味論）の正常性の保護のために重要である。これにより、解釈後の結果の予測性が向上し、シンプルで直感的なものになる。既存のエフェクトライブラリにおける不自然な意味論[^4][^5]は、この原則に違反した操作を試みているのが原因と考えることができる。第二に、この原則は制限と引き換えに*限定継続*を操作できるという自由を引き出す。これについては次のパートで述べる。

[^4]: [Polysemy #246 \<bug\> \<weird semantics\> : Choice of functorial state for runNonDet](https://github.com/polysemy-research/polysemy/issues/246)
[^5]: [The effect semantics zoo](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md)

そして、この制限は部分的に緩めることができる。すなわち無状態な解釈に限り、`(Forall)HFunctor`と`*Rec`系関数により制限は回避できる。これはイメージ的には、覆い被さっている高階エフェクトに穴を開ける感じだ。

つまるところ、高階エフェクトは考えることが増えるので、一階エフェクトで同じことが実現できるのであればそうしたようがシンプルでよい。しかし一方で、高階エフェクトを使わなければ実現できない便利なこともある。そのときは、躊躇なく高階エフェクトの恩恵に預かることにしよう。

---

パートの例で使用したコードの全体は[GitHub](TODO)にある。
