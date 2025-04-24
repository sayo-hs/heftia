{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Heftia is an extensible effects library that generalizes "Algebraic Effects and
Handlers" to higher-order effects, providing users with maximum flexibility and
delivering standard and reasonable speed.
In its generalization, the focus is on ensuring predictable results based on
simple, consistent semantics, while preserving soundness.

= Basic Usage #basic-usage#

The following is an example of defining, using, and interpreting the first-order
effect @Log@ for logging and the higher-order effect @Span@ for representing
named spans in a program.

@
{\-# LANGUAGE AllowAmbiguousTypes #-\}
{\-# LANGUAGE TemplateHaskell #-\}

import "Control.Monad.Hefty"
import Prelude hiding (log, span)

data Log :: t'Effect' where
    Log :: String -> Log f ()
'makeEffectF' ''Log

data Span :: t'Effect' where
    Span :: String -> f a -> Span f a
'makeEffectH' ''Span

runLog :: (@t'Emb'@ 'IO' t'Data.Effect.OpenUnion.:>' es) => 'Eff' (Log ': es) t'Control.Effect.~>' 'Eff' es
runLog = 'interpret' \\(Log msg) -> liftIO $ putStrLn $ "[LOG] " <> msg

runSpan :: (@t'Emb'@ 'IO' t'Data.Effect.OpenUnion.:>' es) => 'Eff' (Span ': es) t'Control.Effect.~>' 'Eff' es
runSpan = 'interpret' \\(Span name m) -> do
    'liftIO' $ 'putStrLn' $ "[Start span '" <> name <> "']"
    r <- m
    'liftIO' $ 'putStrLn' $ "[End span '" <> name <> "']"
    'pure' r

prog :: 'IO' ()
prog = 'runEff' . runLog . runSpan $ do
    span "example program" do
        log "foo"

        span "greeting" do
            log "hello"
            log "world"

        log "bar"

>>> prog
[Start span \'example program\']
[LOG] foo
[Start span \'greeting\']
[LOG] hello
[LOG] world
[End span \'greeting\']
[LOG] bar
[End span \'example program\']
@


* When defining effects, you use the Template Haskell functions 'makeEffectF' and 'makeEffectH'.

= Algebraic Handler #algebraic-handler#

An interpreter function that realizes features related to the continuation in algebraic effects.

It is a function that takes two arguments: an effectful operation and a continuation, which is the continuation of the computation from that operation, and returns the computation up to the end of the computation being interpreted.

By ignoring the continuation argument, it allows for global escapes like the 'Data.Effect.Except.Throw' effect.

@
[runThrow](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-Except.html#v:runThrow) :: ('FOEs' es) => 'Eff' (@t'Data.Effect.Except.Throw'@ e ': es) a -> 'Eff' es ('Either' e a)
runThrow = 'interpretBy' ('pure' '.' 'Right') handleThrow

[handleThrow](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-Except.html#v:handleThrow) :: 'Applicative' g => 'AlgHandler' (@t'Data.Effect.Except.Throw'@ e) f g ('Either' e a)
handleThrow (@v'Data.Effect.Except.Throw'@ e) _ = 'pure' $ 'Left' e
@

Here, @handleThrow@ is the algebraic handler for the t'Data.Effect.Except.Throw' effect.

By calling the continuation argument multiple times, it allows for non-deterministic computations like the "Data.Effect.NonDet" effect.

@
[runNonDet](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-NonDet.html#v:runNonDet)
    :: ('Alternative' f)
    => 'Eff' (@t'Data.Effect.NonDet.Choose'@ ': t'Data.Effect.NonDet.Empty' ': es) a
    -> 'Eff' es (f a)
runNonDet =
    'interpretsBy'
        ('pure' . 'pure')
        $ (\\@v'Data.Effect.NonDet.Choose'@ k -> 'liftA2' ('<|>') (k 'False') (k 'True'))
            '!:' (\\@v'Data.Effect.NonDet.Empty'@ _ -> 'pure' 'empty')
            '!:' 'nil'
@

The function passed as the second argument to 'interpretBy'\/'interpretsBy' is the algebraic handler.

Additionally, what is passed as the first argument to 'interpretBy'\/'interpretsBy' is called a /value handler/.
This extends the continuation in the computation being interpreted.

We shall call the state of computation that emerges through algebraic interpretation and behaves according to [continuation-based semantics](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md)
a \"algebraic state\".

= Naming Rules for Interpretation Functions #naming-rules-for-interpretation-functions#

* Functions may additionally have @With@ or @By@ at the end of their names.

    * These provide functionality equivalent to "Algebraic Effects and Handlers," meaning they offer access to delimited continuations during interpretation.

    * Functions in the @By@ family take two arguments: a value handler and a algebraic effect handler. They are the most generalized form.

    * Functions in the @With@ family omit the value handler and take only the effect interpreter as an argument.

    * The difference between @interpretBy ret f m@ and @interpretWith f m >>= ret@ is that, during interpretation,
        the delimited continuation passed as the second argument @k@ to @f@ in the former extends up to when @ret@ finishes,
        whereas in the latter, it only goes until @m@ finishes (just before @ret@), so @ret@ is not included in @k@.

    * Functions without @With@ or @By@ cannot manipulate continuations;
        therefore, you cannot maintain internal state or perform behaviors like
        global escapes or non-deterministic computations during interpretation.

= Semantics of effects #semantics-of-effects#

Consider the following example.

@
data SomeEff :: Effect where
    SomeAction :: SomeEff m a
'makeEffectF' ''SomeEff

-- | Throws an exception when \'SomeAction\' is encountered
runSomeEff :: (@t'Data.Effect.Except.Throw'@ String t'Data.Effect.OpenUnion.:>' es) => 'Eff' (SomeEff ': es) t'Control.Effect.~>' 'Eff' es
runSomeEff = 'interpret' \\SomeAction -> v'Data.Effect.Except.throw' "not caught"

-- | Catches the exception if \'someAction\' results in one
action :: (SomeEff t'Data.Effect.OpenUnion.:>' es, t'Data.Effect.Except.Catch' String t'Data.Effect.OpenUnion.:>' es, t'Data.Effect.Except.Throw' String t'Data.Effect.OpenUnion.:>' es) => Eff es String
action = someAction \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"

prog1 :: IO ()
prog1 = 'runPure' . runThrow . runCatch . runSomeEff $ action

>>> prog1
Right "caught"

prog2 :: IO ()
prog2 = 'runPure' . runThrow . runSomeEff . runCatch $ action

>>> prog2
Left "not caught"
@

When applying @runCatch@ after @runSomeEff@ in @prog1@, the exception is caught, but in the reverse order, it is not caught.
We will now explain this behavior to understand it.

In Heftia, the behavior of higher-order effects is based on reduction semantics—that is, term rewriting semantics similar to those in "Algebraic Effects and Handlers."
By properly understanding and becoming familiar with this semantics, users can quickly and easily predict execution results.

Let's revisit the definition of @runCatch@:

@
[runCatch](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-Except.html#v:runCatch) :: (@t'Data.Effect.Except.Throw'@ e `@t'Data.Effect.OpenUnion.In'@` es, 'FOEs' es) => 'Eff' (@t'Data.Effect.Except.Catch'@ e ': es) t'Control.Effect.~>' 'Eff' es
runCatch = 'interpret' handleCatch

[handleCatch](https://hackage.haskell.org/package/heftia-effects-0.6.0.0/docs/Control-Monad-Hefty-Except.html#v:handleCatch) :: (@t'Data.Effect.Except.Throw'@ e `@t'Data.Effect.OpenUnion.In'@` es, 'FOEs' es) => t'Data.Effect.Except.Catch' e t'Control.Monad.Hefty.~~>' 'Eff' es
handleCatch (@v'Data.Effect.Except.Catch'@ action hdl) = action & 'interposeWith' \\(@v'Data.Effect.Except.Throw'@ e) _ -> hdl e
@

When @runCatch@ encounters code like @... (action \`catch\` hdl) ...@ in the program, it rewrites that part to @... ('interposeWith' (\\(@v'Data.Effect.Except.Throw'@ e) _ -> hdl e) action) ...@.
In general, functions like 'interpret' and 'interpose' behave this way—they recursively rewrite the target higher-order effects according to the given handler.
Rewriting proceeds from the deepest scope toward the outer scopes.

The same applies to first-order effects. Handling an effect means rewriting the effects that appear in the program.

With this in mind, let's follow the rewriting step by step.

Looking at @prog1@.
First, when @runSomeEff@ is applied to @action@:

@
    runSomeEff action
 =  'interpret' (\\SomeAction -> v'Data.Effect.Except.throw' "not caught") $ someAction \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"
==> v'Data.Effect.Except.throw' "not caught" \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"
@

The program is rewritten into a program like the above.

Next, when @runCatch@ is applied to this, it evaluates to:

@
    runCatch $ v'Data.Effect.Except.throw' "not caught" \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"
==> 'interposeWith' (\\(@v'Data.Effect.Except.Throw'@ e) _ -> 'pure' "caught") $ v'Data.Effect.Except.throw' "not caught"
==> 'pure' "caught"
@

In this way, the exception is caught.

On the other hand, in @prog2@, when @runCatch@ is applied to @action@:

@
    runCatch action
 =  runCatch $ someAction \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"
==> 'interposeWith' (\\(@v'Data.Effect.Except.Throw'@ e) _ -> 'pure' "caught") $ someAction
@

At this point, since there is no v'Data.Effect.Except.throw' in the computation
that is the target of 'interposeWith' (only @someAction@ appears, which is not
 v'Data.Effect.Except.throw'!), 'interposeWith' does nothing because there is no
 v'Data.Effect.Except.throw' to rewrite:

@
==> someAction
@

Therefore, when @runSomeEff@ is applied:

@
    runSomeEff someAction
==> v'Data.Effect.Except.throw' "not caught"
@

Thus, the exception remains as is.

In other words, in @prog2@, at the point of @runCatch@, /it is impossible for @runCatch@ to know that @someAction@ will later be rewritten into @throw@/.
__Interpreters decide what to do based only on the current state of the program's rewriting__. They do not change the result based on any other information.

This is all there is to the reduction semantics of algebraic effects.

== Independence from IO Semantics #independence-from-io-semantics#

As seen in the initial example with logs and spans, 'IO' operations are embedded as effects.
Not limited to 'IO', any monad can be embedded as an effect.

Embedded 'IO' can be viewed as instruction scripts, and to avoid confusion when using Heftia, it should be regarded as such.
Rather than thinking "Haskell represents side effects via a type-level tag called 'IO'", it's better to think:

* Haskell is a purely functional language where you cannot write anything other than pure functions.
* 'IO' is just an opaque algebraic data type whose definition you cannot see, but no different from others.
* The runtime system treats the value @main@ as a sequence of instructions to be executed on the CPU.
* Programming with side effects in Haskell is meta-programming where you write a pure function program that outputs 'IO'-typed instruction scripts.

In fact, the semantics of effects in Heftia are completely isolated from the level of 'IO'.
Considerations at the 'IO' level, such as "asynchronous exceptions might be thrown",
"what is the current state of exception masking", or
"this state/environment value is local and not shared between threads", have no influence on effect interpretation and need not be considered.
 'IO' is just a data type representing programs with side effects, and we are merely meta-programming it.
The consistent semantics of algebraic effects prevent leaks of abstraction from the 'IO' level.

This is a significant difference from 'IO'-fused effect system libraries like [effectful](https://hackage.haskell.org/package/effectful) and [cleff](https://hackage.haskell.org/package/cleff).

= Interpreting Multiple Effects Simultaneously #interpreting-multiple-effects-simultaneously#

For example, consider a situation where you want to use multiple t'Data.Effect.Except.Catch' effects simultaneously.
The following is a case where both @String@ and @Int@ appear as exception types:

@
prog :: 'Eff' '[@t'Data.Effect.Except.Catch'@ String, @t'Data.Effect.Except.Catch'@ Int, @t'Data.Effect.Except.Throw'@ String, @t'Data.Effect.Except.Throw'@ Int] ()
@

In this case, you may get stuck trying to use @runCatch@.
This is because @runCatch@ has the following type signature:

@
runCatch :: (@t'Data.Effect.Except.Throw'@ e `@t'Data.Effect.OpenUnion.In'@` es, 'FOEs' es) => 'Eff' (@t'Data.Effect.Except.Catch'@ e ': es) t'Control.Effect.~>' 'Eff' es
@

You cannot write @runCatch . runCatch@. It requires the higher-order effects to be exhausted after interpretation:


>     runCatch . runCatch $ prog
>                ^^^^^^^^
>
>   • No instance for ‘Data.Effect.FirstOrder (Catch Int)’
>       arising from a use of ‘runCatch’
>   • In the second argument of ‘(.)’, namely ‘runCatch’
>     In the first argument of ‘($)’, namely ‘runCatch . runCatch’
>     In the expression: runCatch . runCatch $ prog

In situations like this, where you want to perform algebraic interpretation on multiple higher-order effects simultaneously,
you generally cannot reduce the higher-order effect list step by step or via multi-staging.
Instead, you need to interpret /all of them at once simultaneously/.

This is possible by 'interprets' family and pattern matching on the open union using the '!:' operator.

@
prog' :: 'Eff' '[@t'Data.Effect.Except.Throw'@ String, @t'Data.Effect.Except.Throw'@ Int] ()
prog' = 'interprets' (handleCatch '!:' handleCatch '!:' 'nil') prog
@
-}
module Control.Monad.Hefty (
    -- * Basics
    Eff,
    Freer (Op, Val),
    type ($),
    type ($$),
    AlgHandler,
    type (~>),
    type (~~>),
    FOEs,
    PolyHFunctor,
    PolyHFunctors,
    type (:>),
    type In,
    type Has,
    type (++),
    (!:),
    (!++),
    nil,
    perform,
    perform',
    perform'',
    send,
    sendAt,
    sendFor,
    emb,

    -- * Interpreting effects

    -- ** Running t`Eff`
    runEff,
    runPure,

    -- ** Standard functions
    interpret,
    interprets,
    interpretWith,
    interpretBy,
    interpretsBy,

    -- ** Reinterpretation functions
    reinterpret,
    reinterprets,
    reinterpretBy,
    reinterpretsBy,
    reinterpretWith,

    -- ** Interposition functions
    interpose,
    interposeOn,
    interposeIn,
    interposeBy,
    interposeOnBy,
    interposeInBy,
    interposeWith,
    interposeOnWith,
    interposeInWith,
    interposeFor,
    interposeForWith,
    interposeForBy,

    -- ** Transformation to monads
    iterAllEff,

    -- ** Utilities
    stateless,
    interpretAll,

    -- ** Ad-hoc stateful interpretation

    -- | Theses entities provides an ad-hoc specialized version to accelerate interpretations that have a
    -- single state type @s@, especially for effects like t'Data.Effect.State.State' or
    --  [@Writer@]("Data.Effect.Writer").
    StateHandler,

    -- *** Interpretation functions
    interpretStateBy,
    reinterpretStateBy,

    -- *** Interposition functions
    interposeStateBy,
    interposeStateInBy,
    interposeStateForBy,

    -- * Transforming effects

    -- ** Rewriting effectful operations
    transform,
    translate,
    translateOn,
    translateIn,
    translateFor,
    rewrite,
    rewriteOn,
    rewriteIn,
    rewriteFor,

    -- ** Manipulating the effect list (without rewriting effectful operations)

    -- *** Insertion functions
    raise,
    raises,
    raisesUnder,
    raiseUnder,
    Suffix,
    SuffixUnder,
    onlyFOEs,
    WeakenHOEs,
    RemoveHOEs,
    raisePrefix,
    raiseSuffix,
    raisePrefix1,
    subsume,
    subsumeUnder,

    -- *** Manipulating Tags
    tag,
    untag,

    -- * Misc
    KnownOrder,
    Type,
    liftIO,
    module Data.Effect,
    module Data.Effect.Tag,
    module Data.Effect.TH,
    module Data.Effect.HFunctor.TH,
    module Control.Effect,
) where

import Control.Effect hiding (Eff)
import Control.Effect.Interpret (interposeIn, interposeOn, interprets, reinterprets)
import Control.Effect.Transform (
    onlyFOEs,
    raise,
    raisePrefix,
    raisePrefix1,
    raiseSuffix,
    raiseUnder,
    raises,
    raisesUnder,
    rewrite,
    rewriteFor,
    rewriteIn,
    rewriteOn,
    subsume,
    subsumeUnder,
    tag,
    transform,
    translate,
    translateFor,
    translateIn,
    translateOn,
    untag,
 )
import Control.Monad.Hefty.Interpret (
    interpose,
    interposeBy,
    interposeFor,
    interposeForBy,
    interposeForWith,
    interposeInBy,
    interposeInWith,
    interposeOnBy,
    interposeOnWith,
    interposeWith,
    interpret,
    interpretAll,
    interpretBy,
    interpretWith,
    interpretsBy,
    iterAllEff,
    reinterpret,
    reinterpretBy,
    reinterpretWith,
    reinterpretsBy,
    runEff,
    runPure,
    stateless,
 )
import Control.Monad.Hefty.Interpret.State (
    StateHandler,
    interposeStateBy,
    interposeStateForBy,
    interposeStateInBy,
    interpretStateBy,
    reinterpretStateBy,
 )
import Control.Monad.Hefty.Types (
    AlgHandler,
    Eff,
    Freer (..),
 )
import Control.Monad.IO.Class (liftIO)
import Data.Effect
import Data.Effect.HFunctor.TH
import Data.Effect.OpenUnion (FOEs, Has, In, KnownOrder, PolyHFunctors, RemoveHOEs, Suffix, SuffixUnder, WeakenHOEs, nil, (!++), (!:), (:>), type (++))
import Data.Effect.TH
import Data.Effect.Tag
import Data.Kind (Type)
