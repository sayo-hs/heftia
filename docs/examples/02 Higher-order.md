# Example 2 - Higher-order effects

In this section, we explain how to handle higher-order effects in `heftia-effects` using the effect of log output. While first-order effects were quite similar to most existing libraries based on Freer, the treatment of higher-order effects in `heftia-effects` is entirely new and based on [Hefty Algebras (Casper et al., 2023)](https://dl.acm.org/doi/10.1145/3571255).

## Log output effect class

First, the first-order effect class for log output can be defined as follows:

```haskell
class Log f where
    log :: LogLevel -> Text -> f ()

makeEffectF ''Log
```

An interpreter for this could be:

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

The `Ask` effect class is a subclass of the `Reader` effect class, which can be described as the effect version of the `ask` function of the so-called `Reader` monad. This interpreter fetches the current log level from the `Reader` environment and outputs the logs filtered by the log level[^1].

[^1]: The `LogLevel` data type is from the `loglevel` package, and the `Text` data type is from the `text` package.

Although this is a practical example of a log output effect, for simplicity, we'll continue our discussion using the following definition:

```haskell
class Log f where
    log :: Text -> f ()

makeEffectF ''Log

logToIO :: (IO <: Fre r m, Monad m) => Fre (LogI ': r) m ~> Fre r m
logToIO = interpret \(Log msg) -> sendIns $ T.putStrLn msg
```

## Timestamp retrieval effect

We can introduce an effect to retrieve the current timestamp as follows[^2]:

```haskell
class Time f where
    currentTime :: f UTCTime

makeEffectF ''Time

timeToIO :: (IO <: Fre r m, Monad m) => Fre (TimeI ': r) m ~> Fre r m
timeToIO = interpret \case
    CurrentTime -> sendIns getCurrentTime
```

[^2]: `getCurrentTime` and `UTCTime` are from the `time` package.

Then, you can create a re-interpretation function that adds the current timestamp to the log as follows:

```haskell
logWithTime :: (LogI <| es, Time (Fre es m), Monad m) => Fre es m ~> Fre es m
logWithTime = interpose \(Log msg) -> do
    t <- currentTime
    log $ "[" <> T.pack (show t) <> "] " <> msg
```

Let's give it a try.

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

{- Execution result:
[2023-09-14 06:35:14.709968653 UTC] foo
[2023-09-14 06:35:14.710160875 UTC] bar
[2023-09-14 06:35:14.710175171 UTC] baz
[2023-09-14 06:35:14.710188216 UTC] baz
[2023-09-14 06:35:14.710200429 UTC] qux
[2023-09-14 06:35:14.71021154 UTC] quux
-}
```

## Scoping of logs
Now, let's look at an example of a higher-order effect.

First, let's introduce a higher-order effect class that delimits the program that outputs logs with a scope, representing a block of logs:

```haskell
-- | An effect that introduces a scope that represents a chunk of logs.
class LogChunk f where
    logChunk :: f a -> f a

makeEffectH ''LogChunk
```

The newly introduced `makeEffectH` is an auto-derivation TH for higher-order effect classes. In particular, it generates the following GADT:

```hs
data LogChunkS f a where
    LogChunk :: f a -> LogChunkS f a
```

Let's try writing a higher-order interpretation function that simply outputs the logs in the log chunks as they are:

```haskell
-- | Output logs in log chunks as they are.
passthroughLogChunk ::
    (Monad m, ForallHFunctor r) =>
    Hef (LogChunkS ': r) m ~> Hef r m
passthroughLogChunk = interpretH \(LogChunk m) -> m
```

Then, you can use this `logChunk` effect like this:

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

{- Execution result:
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

### Principles in Heftia

Let's explain what's used in `passthroughLogChunk`.

`interpretH` is a higher-order version of `interpret`.
Also here, the function type has a slightly unusual structure.

First, the constraint `ForallHFunctor ...` is a constraint on the effect class list in `heftia-effects`, required throughout the library. If you encounter a `Could not deduce (Forall HFunctor ...)` error while writing a function, add this to the function's constraints.

Additionally, errors like `Could not deduce KnownNat` can be avoided by introducing the `ghc-typelits-knownnat` plugin without adding constraints to the function.

Then, there's `Hef`, which is a higher-order version of `Fre` (Freer) and is (a monad transformer) called **Heftia**. Just as Freer is a combination of the Free monad and co-Yoneda, Heftia is a combination of a hefty tree and a higher-order co-Yoneda. This is introduced by this library specifically for handling higher-order effects.

In `heftia-effects`, the system uses the Heftia transformer to handle higher-order effects. And the carriers for first-order effects (namely, Freer) are usually placed as subordinate carriers under the carriers for higher-order effects (namely, Heftia).

In the approach of handling effects as proposed by hefty algebra, to freely access (interpret and reinterpret) first-order effects, one must first complete the handling of "overlaying" higher-order effects.

Handling first-order effects is called *interpret*, while handling higher-order effects is often termed *elaborate*. In this library, naming related to handling higher-order effects is done by adding an H to its first-order counterpart, such as *interpretH*, or *elaborate*.

Using this terminology, the principle can be rephrased as: "First, `elaborate`, then you can `interpret`".

This restriction might seem strong. However, as described in the paper, this restriction is essential, first and foremost, for preserving the semantics of effect handling. It enhances the predictability of handling results, making them straightforward and intuitive. Secondly, methods based on this hefty algebra extract freedom in exchange for these restrictions. This will be elaborated in the next section (Example 3 - Delimited Continuation).

Moreover, this library provides means to access and operate on subordinate carriers (typically Freer) even when the Heftia transformer is overlaying, using the so-called `hoist` series of functions. However, caution is required when using this, as will be discussed later.

---

Let's get back to the point.

Within this `main` function, `elaborated` is a function that runs the `Hef` transformer when all higher-order effect handling is completed, and the higher-order effect class list becomes empty (taking the form `Hef '[]`), dropping it down to the subordinate carrier (in this case, `Fre '[TimeI, LogI, IO]`) [^3].

[^3]: There's also a corresponding function on the first-order side called `interpreted`.

### Manipulating Scopes

Now, using the `logChunk` effect, let's do something interesting.

#### Limiting the Number of Log Outputs

Below is a function that reinterprets the behavior to omit logs after the `n`-th log within a scope and logs the fact that they were omitted.

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

First, the action that represents the scope of `logChunk` received as an argument
```hs
    a :: Fre '[LogI] m
```
is transformed by the `raise` function into
```hs
    raise a :: Fre '[StateI Int, LogI] m
```
With this, it becomes possible to handle state effects. The `raise` function is one that inserts any effect class at the beginning of the effect class list. And by the `flipFreer` function which rearranges the effect class list of Freer, it's transformed into
```hs
    flipFreer (raise a) :: Fre '[LogI, StateI Int] m
```
The main process starts from here, where the `interpretLog` function interprets all logs that can be thrown within the scope. Each time a log is thrown, the value held by the state effect is incremented, and depending on the current count, logs may or may not be output, or logs indicating that something has been omitted might be output. Finally, `evalState` handles the state effect with an initial counter value of 0. Also, the `logChunk` effect gets consumed through the interpretation of `limitLogChunk`, and as it stands, the information of the scope would be lost. Therefore, in order to enable further hooks according to the scope, the `logChunk` is applied to the entire process at the end to retain the information of the scope.

Additionally, the `liftLower` used inside the `interpretLog` function is a `lift` function for the Freer transformer. For the Heftia transformer, there is `liftLowerH`.

---

When you use the `limitLogChunk` function, the number of logs in the scope is limited as follows. Note that you need to combine `limitLogChunk` with the `runElaborate` function.

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

{- Execution result:
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

Here's the English translation:

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

{- Execution result:
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

In this case, `liftLower` is used to match the type of the argument for `runElaborate`.
`interpret (\(Log m) -> log m)` is for handling logs that are thrown outside the scope that isn't surrounded by `logChunk`. In this example, everything is within `logChunk`, so it's merely for type matching. Moreover, `interpreted . logToIO` has been added, and on top of the existing Heftia hierarchy, a new layer of Heftia and Freer is formed. Typically, the transformer stack of heftia-effects takes on a form where Heftia and Freer create layers similar to a mille-feuille pastry. This layered structure embodies the controlled-controlling relationship between first-order effects and higher-order effects at the type level. It serves as a guardrail for handling higher-order effects under sound semantics.

By the way, if you want to elaborate other higher-order effect classes at the same time as `limitLogChunk`, you can use `|+:` like the `:` operator of lists, as shown by:

```hs
    f1 |+: f2 |+: ... |+: fn |+: absurdUnionH
```

The type application of `runElaborate` is currently necessary for the type to be correctly inferred. However, this syntactical verbosity is expected to be improved in future versions.

#### Save Logs to a File

Let's move on to the next example.

First, as a preparation, let's define effect classes representing directory creation and file writing operations.
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

This interpreter is a dummy. It simply outputs when an operation's effect is thrown[^4].

[^4]: Of course, writing an interpreter that actually performs IO is easy.

Next, the following is a function that changes the behavior of `logChunk` such that, every time you enter the scope of a `logChunk` effect, it recursively creates a directory named after the current timestamp and saves the logs thrown within that scope into that directory.

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

First, the `<<|` type-level operator is a higher-order version of `<|`. The constraint indicates that the higher-order side deals with the `LogChunk` effect class and the first-order side deals with the `Log` effect class.

`flipHeftia` is the Heftia version of `flipFreer`. `liftReader` is a function that lifts `Hef ... (Fre ... m)` to the form of `Hef (LocalS ': ...) (Fre (AskI ': ...) m)`.

Then `hoistHeftiaEffects` is the aforementioned hoist family of functions. With this, you can penetrate the overlaying `Hef` and manipulate the underlying first-order `Fre`.

`raise2` is equivalent to `(raise . raise)`.

Here's the overall flow:

Using the `liftReader` function and the flip series of functions,
```haskell
Hef (LogChunkS ': es) (Fre (LogI ': es') m)
```
is transformed into
```haskell
Hef (LogChunkS ': LocalS FilePath ': es) (Fre (LogI ': AskI FilePath ': es')
```
The added `Reader` effect classes store the path of the directory corresponding to the current scope.

`interpretLogChunk` interprets the `LogChunkS` at the head of the effect class list and retransmits it to `es`. Within the reinterpreted scope, it creates a directory named after the current time, and every time a log is thrown, it creates a file named after that time and writes the contents of the log. Finally, it handles the `Reader` effect with the initial directory path set to "./log_chunks/".

---

Using the above functions, the behavior is as follows:

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

{- Execution result:
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

Each time it enters a scope, directories are recursively created, and log files are saved in the directory corresponding to that scope.

---

Furthermore, of course, it's possible to combine this `saveLogChunk` with the earlier `limitLogChunk`. In this case, the behavior changes depending on the order of composition. If `limitLogChunk` is applied first:

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

{- Execution result:
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

The limit is also applied to saving files.

On the other hand, when applying `saveLogChunk` first:

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

{- Execution result:
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

The raw logs before the limit are outputted to the file.

Note the need for some tricks to match the types. This library has a significant aspect of "guardrails by type," much like the Rust language, which means it tends to have a steep learning curve. I believe that for readers familiar with Haskell, this won't be too high a barrier.

Here is the English translation of the provided text:

## Precautions When Handling Higher-Order Effects

There are some pitfalls when dealing with higher-order effects.

### Unsafety of hoist-like functions
Earlier, I mentioned the existence of hoist-like functions that can penetrate the Heftia layer and operate on the Freer layer. Care is needed when using these functions. If the natural transformation `phi :: f ~> g` passed to the hoist is not a monad morphism, that is, it does not satisfy the following laws, the operation becomes ill-behaved.

* Law 1

    ```hs
    forall m f. phi $ do { x <- m; f x } = do { x <- phi m; phi (f x) }
    ```

* Law 2

    ```hs
    forall x. phi (return x) = return x
    ```

For more details on monad morphism, refer to the [mmorph package documentation](https://hackage.haskell.org/package/mmorph-1.2.0/docs/Control-Monad-Morph.html).

In the examples provided so far, there was no problem because functions like `interpretReader` that used hoist-like functions satisfied these laws. On the other hand, `evalState` (or `interpretState`) serves as an example of a transformation that doesn't satisfy these laws.

Below is the `limitLogChunk` function, written in the same format as `saveLogChunk`.

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

When using this, the result is as follows, and the expected behavior is not achieved:

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

The output should not include the messages after "quux". Looking at the state of the counter:

```hs
...

                        when (count <= n) do
                            raise $
                                if count == n
                                    then log "LOG OMITTED..."
                                    else log $ "<" <> T.pack (show count) <> "> " <> msg
                            modify @Int (+ 1)

...

{- Execution result:
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

The state of the counter has been reset. This behavior is believed to be due to the `evalState` used in hoist not satisfying Law 1.

Caution is also needed when operating on a lower carrier from Freer (probably).

In future versions, this unsafety is expected to be fixed. The general idea is to introduce a type class like `MonadMorph` that represents monad morphisms:
```hs
class MonadMorph f g a | a -> f g where
    morph :: a -> (forall x. f x -> g x)
```
Instead of simply defining natural transformations as type synonyms, a few data types representing natural transformations would be introduced, becoming instances of `MonadMorph` only if they satisfy the laws. All hoist-like functions related to monads will then have `MonadMorph` as a constraint. This should ensure that hoist-like functions can be used safely and are well-typed. For now, we have to use them carefully while verifying that they satisfy the laws, but the next version is eagerly awaited.

This all means that, in general, hoist-like operations violate the principle of "elaborate first, then interpret", so they're not safe without the laws (though they can be safe with constraints). This underscores the importance of the principle for preserving semantics. This fact is likely to strengthen the argument for the importance of the principle.

### Interference of Effects

Within the `saveLogChunk` function, instead of interpreting `LogChunk` and `Log` using `interpret`, we can use `interpose` as follows:

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

When used, the behavior becomes:

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

{- Execution result:
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

As the scope deepens, the number of duplicate processing increases accordingly. The `logChunk` creates directories repeatedly based on the depth of its scope, and `log` saves files multiple times depending on that same depth.

When elaborating higher-order effects, naively using `interpose` (and probably `reinterpret` as well) can lead to interference of effects at the same level. This makes the outcome hard to predict. In certain cases, one might cleverly leverage this interfering behavior in their code. However, in most cases, it would be challenging to anticipate the interference outcome from the code, compromising readability. It's preferable to delineate the hierarchy of effects finely. For reinterpretation, using `interpret` can help in dispatching effects to different levels, avoiding confusion. Even when using `interpose`, one can achieve this by adeptly employing `raise`, `liftLower`, and `liftLowerH` to ensure effects aren't sent to the original level.

Interference of effects, unlike the previously mentioned unsafety of hoist operations, is not necessarily considered dangerous. It just complicates matters. Unlike the hoist operations, it's unlikely to fundamentally violate any rules, leading to chaotic outcomes.

Regarding `limitLogChunk`, it is likely impossible to write it in a way that sends it back to the same level as the original source based on its format.
The elaboration of this format combined with `runElaborate` is most faithful to the principles, and therefore it's safe to say that good behavior and predictability of semantics by type are guaranteed.
The format of `saveLogChunk` has greater flexibility in applying elaboration compared to the method combined with `runElaborate`.
In the approach of `runElaborate`, the elaborators combined using `|+:` are executed "simultaneously and in parallel" during elaboration, and cannot interact with each other.
On the other hand, the format of `saveLogChunk` can be combined in a way that interacts with other elaborators.
Therefore, in general, if it can be written in the latter format, it is better to do so.
However, this flexibility makes handling the effects during the elaborator implementation more challenging.
In other words, the former format is more conservative.

## Entire Code

For reference, here is the entire code when the elaborators are combined in the order of `limitLogChunk` and `saveLogChunk`.

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
