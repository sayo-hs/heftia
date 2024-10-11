{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Heftia is an extensible effects library that generalizes "Algebraic Effects and
Handlers" to higher-order effects, providing users with maximum flexibility and
delivering standard and reasonable speed.
In its generalization, the focus is on ensuring predictable results based on
simple, consistent semantics, while preserving soundness.

= Basic Usage

The following is an example of defining, using, and interpreting the first-order
effect @Log@ for logging and the higher-order effect @Span@ for representing
named spans in a program.

@
{\-# LANGUAGE AllowAmbiguousTypes #-\}
{\-# LANGUAGE TemplateHaskell #-\}

import "Control.Monad.Hefty"
import Prelude hiding (log, span)

data Log a where
    Log :: String -> Log ()
'makeEffectF' [''Log]

data Span m (a :: Type) where
    Span :: String -> m a -> Span m a
'makeEffectH' [''Span]

runLog :: ('IO' t'Data.Effect.OpenUnion.<|' ef) => 'Eff' eh (Log ': ef) t'Control.Effect.~>' 'Eff' eh ef
runLog = 'interpret' \\(Log msg) -> liftIO $ putStrLn $ "[LOG] " <> msg

runSpan :: ('IO' t'Data.Effect.OpenUnion.<|' ef) => 'Eff' (Span ': eh) ef t'Control.Effect.~>' 'Eff' eh ef
runSpan = 'interpretH' \\(Span name m) -> do
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
* The first 'Eff' type parameter is a type-level list of higher-order effects, the second is for first-order effects.

= Glossary

[Handler]: Interpreter for first-order effects.

[Elaborator]:
    Interpreter for higher-order effects.

    Elaboration is generally performed by editing first-order (or higher-order) effectful operations within the computation held by the higher-order effect being elaborated.

    @
    [runCatch](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-Except.html#v:runCatch) :: (@t'Data.Effect.Except.Throw'@ e t'Data.Effect.OpenUnion.<|' ef) => 'Eff' '[@t'Data.Effect.Except.Catch'@ e] ef t'Control.Effect.~>' 'Eff' '[] ef
    runCatch = 'interpretH' elabCatch

    [elabCatch](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-Except.html#v:elabCatch) :: (@t'Data.Effect.Except.Throw'@ e t'Data.Effect.OpenUnion.<|' ef) => t'Data.Effect.Except.Catch' e t'Control.Monad.Hefty.~~>' 'Eff' '[] ef
    elabCatch (@v'Data.Effect.Except.Catch'@ action hdl) = action & 'interposeWith' \\(@v'Data.Effect.Except.Throw'@ e) _ -> hdl e
    @

    Here, @elabCatch@ is the elaborator for the t'Data.Effect.Except.Catch' effect.

[Interpretation \/ Handling \/ Elaboration]:
    The act of performing interpretation, or the process thereof.

    Also, an /interpreter function/ refers to a function represented by a natural transformation t'Control.Effect.~>' or of type 'Interpreter', that is, one that takes an effectful operation as an argument.
    On the other hand, when we say /interpretation function/, we mean a function of the form @'Eff' eh ef ~> 'Eff' eh' ef'@, that is, one that takes the 'Eff' monad as an argument.
    In the previous example, @elabCatch@ is the /interpreter function/ for the t'Data.Effect.Except.Catch' effect, and @runCatch@ is the /interpretation function/ for the t'Data.Effect.Except.Catch' effect.

    The interpretation function may also be called an /interpreter/.

[Continuational stateful interpreter]:
    An interpreter function that realizes features related to the continuation in algebraic effects.

    It is a function that takes two arguments: an effectful operation and a continuation, which is the continuation of the computation from that operation, and returns the computation up to the end of the computation being interpreted.

    By ignoring the continuation argument, it allows for global escapes like the 'Data.Effect.Except.Throw' effect.

    @
    [runThrow](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-Except.html#v:runThrow) :: 'Eff' '[] (@t'Data.Effect.Except.Throw'@ e ': r) a -> 'Eff' '[] r ('Either' e a)
    runThrow = 'interpretBy' ('pure' '.' 'Right') handleThrow

    [handleThrow](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-Except.html#v:handleThrow) :: 'Interpreter' (@t'Data.Effect.Except.Throw'@ e) ('Eff' '[] r) ('Either' e a)
    handleThrow (@v'Data.Effect.Except.Throw'@ e) _ = 'pure' $ 'Left' e
    @

    Here, @handleThrow@ is the continuational stateful handler for the t'Data.Effect.Except.Throw' effect.

    By calling the continuation argument multiple times, it allows for non-deterministic computations like the "Data.Effect.NonDet" effect.

    @
    [runNonDet](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-NonDet.html#v:runNonDet)
        :: forall f ef a
        . ('Alternative' f)
        => 'Eff' '[] (@t'Data.Effect.NonDet.Choose'@ ': t'Data.Effect.NonDet.Empty' ': ef) a
        -> 'Eff' '[] ef (f a)
    runNonDet =
        'bundleN' \@2
            '>>>' 'interpretBy'
                ('pure' . 'pure')
                ( (\\@v'Data.Effect.NonDet.Choose'@ k -> 'liftA2' ('<|>') (k 'False') (k 'True'))
                    '!+' (\\@v'Data.Effect.NonDet.Empty'@ _ -> 'pure' 'empty')
                    '!+' 'nil'
                )
    @

    The function passed as the second argument to 'interpretBy' is the continuational stateful handler.

    Additionally, what is passed as the first argument to 'interpretBy' is called a /value handler/.
    This extends the continuation in the computation being interpreted.

[Continuational state]:
    The state of the computation that appears through interpretation, behaving based on [continuation-based semantics](https://github.com/lexi-lambda/eff/blob/master/notes/semantics-zoo.md).

= Naming Rules for Interpretation Functions

* Functions with an @H@, such as 'interpretH', are for higher-order effects, while those without are for first-order effects.

    @
    'interpret' :: e t'Control.Effect.~>' 'Eff' eh ef -> 'Eff' (e ': eh) ef ~> 'Eff' eh ef
    'interpretH' :: e ('Eff' eh ef) t'Control.Effect.~>' 'Eff' eh ef -> 'Eff' (e ': eh) ef t'Control.Effect.~>' 'Eff' eh ef
    @

    Note: t'Control.Effect.~>' binds more tightly than @->@.

* Functions may additionally have @With@ or @By@ at the end of their names.

    * These provide functionality equivalent to "Algebraic Effects and Handlers," meaning they offer access to delimited continuations during interpretation.

    * Functions in the @By@ family take two arguments: a value handler and a continuational stateful effect interpreter. They are the most generalized form.

    * Functions in the @With@ family omit the value handler and take only the effect interpreter as an argument.

    * The difference between @interpretBy ret f m@ and @interpretWith f m >>= ret@ is that, during interpretation,
        the delimited continuation passed as the second argument @k@ to @f@ in the former extends up to when @ret@ finishes,
        whereas in the latter, it only goes until @m@ finishes (just before @ret@), so @ret@ is not included in @k@.

    * Functions without @With@ or @By@ cannot manipulate continuations;
        therefore, you cannot maintain internal state or perform behaviors like
        global escapes or non-deterministic computations during interpretation.

* Functions that perform recursive continuational stateful interpretation have @Rec@ additionally added.

    * Non-recursive continuational stateful interpretation functions like 'interpretWith' cannot be used unless the higher-order effects are empty:

        @
        'interpretWith' :: e t'Control.Effect.~>' 'Eff' '[] ef -> 'Eff' '[] (e ': ef) t'Control.Effect.~>' 'Eff' '[] ef
        @

    * The @Rec@ versions can be used even when @eh@ is not empty.

        @
        'interpretRecWith' :: e t'Control.Effect.~>' 'Eff' eh ef -> 'Eff' eh (e ': ef) t'Control.Effect.~>' 'Eff' eh ef
        @

    * When using this type of function, pay attention to their /reset semantics/. This is discussed later.

    * In principle, they cannot take value handlers, so there is no combination with @By@.

Function names combine the above three attributes.
Examples of complex combinations include 'interpretHBy' and 'interpretRecHWith'.

= Semantics of effects

Consider the following example.

@
data SomeEff a where
    SomeAction :: SomeEff String
'makeEffectF' [''SomeEff]

-- | Throws an exception when \'SomeAction\' is encountered
runSomeEff :: (@t'Data.Effect.Except.Throw'@ String t'Data.Effect.OpenUnion.<|' ef) => 'Eff' eh (SomeEff ': ef) ~> 'Eff' eh ef
runSomeEff = 'interpret' \\SomeAction -> v'Data.Effect.Except.throw' "not caught"

-- | Catches the exception if \'someAction\' results in one
action :: (SomeEff t'Data.Effect.OpenUnion.<|' ef, t'Data.Effect.Except.Catch' String t'Data.Effect.OpenUnion.<<|' eh, t'Data.Effect.Except.Throw' String '<|' ef) => Eff eh ef
action = someAction \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"

prog1 :: IO ()
prog1 = 'runPure' . runThrow @String . runCatch @String . runSomeEff $ action

>>> prog1
Right "caught"

prog2 :: IO ()
prog2 = 'runPure' . runThrow @String . runSomeEff . runCatch @String $ action

>>> prog2
Left "not caught"
@

When applying @runCatch@ after @runSomeEff@ in @prog1@, the exception is caught, but in the reverse order, it is not caught.
We will now explain this behavior to understand it.

In Heftia, the behavior of higher-order effects is based on reduction semantics—that is, term rewriting semantics similar to those in "Algebraic Effects and Handlers."
By properly understanding and becoming familiar with this semantics, users can quickly and easily predict execution results.

Let's revisit the definition of @runCatch@:

@
[runCatch](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-Except.html#v:runCatch) :: (@t'Data.Effect.Except.Throw'@ e t'Data.Effect.OpenUnion.<|' ef) => 'Eff' '[@t'Data.Effect.Except.Catch'@ e] ef t'Control.Effect.~>' 'Eff' '[] ef
runCatch = 'interpretH' elabCatch

[elabCatch](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-Except.html#v:elabCatch) :: (@t'Data.Effect.Except.Throw'@ e t'Data.Effect.OpenUnion.<|' ef) => t'Data.Effect.Except.Catch' e t'Control.Monad.Hefty.~~>' 'Eff' '[] ef
elabCatch (@v'Data.Effect.Except.Catch'@ action hdl) = action & 'interposeWith' \\(@v'Data.Effect.Except.Throw'@ e) _ -> hdl e
@

When @runCatch@ encounters code like @... (action \`catch\` hdl) ...@ in the program, it rewrites that part to @... ('interposeWith' (\\(@v'Data.Effect.Except.Throw'@ e) _ -> hdl e) action) ...@.
In general, functions like 'interpretH' and 'interposeH' behave this way—they recursively rewrite the target higher-order effects according to the given elaborator.
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
    runCatch \@String $ v'Data.Effect.Except.throw' "not caught" \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"
==> 'interposeWith' (\\(@v'Data.Effect.Except.Throw'@ e) _ -> 'pure' "caught") $ v'Data.Effect.Except.throw' "not caught"
==> 'pure' "caught"
@

In this way, the exception is caught.

On the other hand, in @prog2@, when @runCatch@ is applied to @action@:

@
    runCatch \@String action
 =  runCatch \@String $ someAction \`@v'Data.Effect.Except.catch'@\` \\(_ :: String) -> 'pure' "caught"
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

== Independence from IO Semantics

As seen in the initial example with logs and spans, 'IO' operations are embedded as effects.
Not limited to 'IO', any monad can be embedded as an effect.

Embedded 'IO' can be viewed as instruction scripts, and to avoid confusion when using Heftia, it should be regarded as such.
Rather than thinking "Haskell represents side effects via a type-level tag called 'IO'", it's better to think:

* Haskell is a purely functional language where you cannot write anything other than pure functions.
* 'IO' is just an opaque algebraic data type whose definition you cannot see, no different from others.
* The runtime system treats the value @main@ as a sequence of instructions to be executed on the CPU.
* Programming with side effects in Haskell is meta-programming where you write a pure function program that outputs 'IO' type instruction scripts.

In fact, the semantics of effects in Heftia are completely isolated from the level of 'IO'.
Considerations at the 'IO' level, such as "asynchronous exceptions might be thrown",
"what is the current state of exception masking", or
"this state/environment value is local and not shared between threads", have no influence on effect interpretation and need not be considered.
 'IO' is just a data type representing programs with side effects, and we are merely meta-programming it.
The consistent semantics of algebraic effects prevent leaks of abstraction from the 'IO' level.

This is a significant difference from 'IO'-fused effect system libraries like [effectful](https://hackage.haskell.org/package/effectful) and [cleff](https://hackage.haskell.org/package/cleff).

= Reset Semantics in Recursive Continuational Stateful Interpretation

When performing recursive continuational stateful interpretation, that is, when using functions with @Rec@, it's necessary to understand their semantics.
If you are not using @Rec@ functions, you don't need to pay particular attention to this section.

[@runStateRec@](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-State.html#v:runStateRec) is a variant of
 [@runState@](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-State.html#v:runState),
a handler for the @State@ effect that can be used even when higher-order effects are unelaborated:

@
[@runStateRec@](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-State.html#v:runStateRec) :: 'Eff' eh (@t'Data.Effect.State.State'@ s ': ef) t'Control.Effect.~>' 'Eff' eh ef
[@runState@](https://hackage.haskell.org/package/heftia-effects-0.4.0.0/docs/Control-Monad-Hefty-State.html#v:runState) :: 'Eff' '[] (@t'Data.Effect.State.State'@ s ': ef) t'Control.Effect.~>' 'Eff' '[] ef
@

@runStateRec@ uses @Rec@ functions internally. When a function uses @Rec@ functions internally, it's best to reflect that in its naming.

Now, if you perform @runStateRec@ before elaborating higher-order effects, the following occurs.
Note that we are using the @Log@ and @Span@ effects introduced in the first example.

@
import Prelude hiding (log, span)

prog :: IO ()
prog = 'runEff' do
    runLog . runSpan . runStateRec \@[Int] [] $ do

        v'Data.Effect.State.modify' \@[Int] (++ [1])
        log . show =<< v'Data.Effect.State.get' \@[Int]

        span \"A\" do
            v'Data.Effect.State.modify' \@[Int] (++ [2])
            log . show =<< v'Data.Effect.State.get' \@[Int]

        v'Data.Effect.State.modify' \@[Int] (++ [3])
        log . show =<< v'Data.Effect.State.get' \@[Int]

    'pure' ()

>>> prog
[LOG] [1]
[Start span 'A']
[LOG] [1,2]
[End span 'A']
[LOG] [1,3]
@

After exiting span @A@, the added @2@ has disappeared. As shown, state changes within the scope may not be preserved after exiting the scope.

This is a fundamental limitation of state preservation.
When attempting to perform continuational stateful interpretation of an effect,
if there are unelaborated higher-order effects remaining, resets of this continuational state occur for each scope of those higher-order effects.
For higher-order effects that have already been elaborated and removed from the list at that point, there is naturally no impact.

This is simply because @runStateRec@ (generally all @Rec@ functions) recursively applies @runState@ to the scopes of unelaborated higher-order effects.
Interpretation occurs independently for each scope, and the state is not carried over.

From the perspective of @shift/reset@ delimited continuations, this phenomenon can be seen as @reset@s being inserted at the scopes of unelaborated higher-order effects.

Whether this behavior occurs can be determined in advance.
This reset behavior occurs only when using @Rec@ functions and when the program passed to that function performs unelaborated higher-order effects internally.
The reset behavior occurs locally only at the points where those effects are performed.
Whether an unelaborated higher-order effect @e@ is performed internally can generally be determined by looking at the type signature of the effectful program.
Given an effectful program @p@, if its type signature includes @e@ in the list of higher-order effects,
or if the constraint part includes @e t'Data.Effect.OpenUnion.<<|' eh@ or @e t'Control.Effect.<<:' m@, then you know that higher-order effect is being used.
If not, @e@ cannot be performed internally in the function @p@.

If you do not desire this reset behavior, you can avoid it by elaborating all higher-order effects first and emptying them when performing continuational stateful interpretation without using @Rec@ functions:

@
import Prelude hiding (log, span)

prog :: IO ()
prog = 'runEff' do
    runLog . runState \@[Int] [] . runSpan $ do

        v'Data.Effect.State.modify' \@[Int] (++ [1])
        log . show =<< v'Data.Effect.State.get' \@[Int]

        span \"A\" do
            v'Data.Effect.State.modify' \@[Int] (++ [2])
            log . show =<< v'Data.Effect.State.get' \@[Int]

        v'Data.Effect.State.modify' \@[Int] (++ [3])
        log . show =<< v'Data.Effect.State.get' \@[Int]

    'pure' ()

>>> prog
[LOG] [1]
[Start span 'A']
[LOG] [1,2]
[Ene span 'A']
[LOG] [1,2,3]
@

= Interpreting Multiple Effects Simultaneously

For example, consider a situation where you want to use multiple t'Data.Effect.Except.Catch' effects simultaneously.
The following is a case where both @String@ and @Int@ appear as exception types:

@
prog :: 'Eff' '[@t'Data.Effect.Except.Catch'@ String, @t'Data.Effect.Except.Catch'@ Int] '[@t'Data.Effect.Except.Throw'@ String, @t'Data.Effect.Except.Throw'@ Int] ()
@

In this case, you may get stuck trying to use @runCatch@.
This is because @runCatch@ has the following type signature:

@
runCatch :: (@t'Data.Effect.Except.Throw'@ e t'Data.Effect.OpenUnion.<|' ef) => 'Eff' '[@t'Data.Effect.Except.Catch'@ e] ef t'Control.Effect.~>' 'Eff' '[] ef
@

You cannot write @runCatch \@Int . runCatch \@String@. It requires the higher-order effects to be empty after interpretation:


>runCatch @String . runCatch @Int $ prog
>                   ^^^^^^^^^^^^^
>
>• Couldn't match type: '[]
>                 with: '[Catch String]
>  Expected: Eff '[Catch Int] ef x -> Eff '[Catch String] ef x
>    Actual: Eff '[Catch Int] ef x -> Eff '[] ef x
>• In the second argument of ‘(.)’, namely ‘runCatch @Int’
>  In the first argument of ‘($)’, namely
>    ‘runCatch @String . runCatch @Int’
>  In the expression: runCatch @String . runCatch @Int $ prog

In situations like this, where you want to perform continuational stateful elaboration on multiple higher-order effects simultaneously,
you generally cannot reduce the higher-order effect list step by step or via multi-staging.
Instead, you need to elaborate /all of them at once simultaneously/.

This is possible by pattern matching on the open union of higher-order effects using the '!!+' operator.

@
prog' :: 'Eff' '[] '[@t'Data.Effect.Except.Throw'@ String, @t'Data.Effect.Except.Throw'@ Int] ()
prog' = 'interpretH' (elabCatch \@String '!!+' elabCatch \@Int '!!+' 'nilH') . 'bundleAllH' $ prog
@

'bundleAllH' collects the entire list of higher-order effects into a single higher-order effect using an open union.

Similarly, this can be done for first-order effects using '!+', 'nil', and 'bundleAll'.
-}
module Control.Monad.Hefty (
    -- * Basics
    Eff (Op, Val),
    type (:!!),
    type (!!),
    type (+),
    type (:+:),
    type ($),
    type ($$),
    Interpreter,
    Elaborator,
    type (~~>),
    send,
    sendH,
    send0,
    send0H,
    sendN,
    sendNH,
    sendUnion,
    sendUnionBy,
    sendUnionH,
    sendUnionHBy,

    -- * Interpreting effects

    -- ** Running t`Eff`
    runEff,
    runPure,

    -- ** Standard functions

    -- *** For first-order effects
    interpret,
    interpretWith,
    interpretBy,
    interpretRecWith,

    -- *** For higher-order effects
    interpretH,
    interpretHWith,
    interpretHBy,
    interpretRecHWith,

    -- ** Reinterpretation functions

    -- *** For first-order effects
    reinterpret,
    reinterpretN,
    reinterpretNWith,
    reinterpretBy,
    reinterpretNBy,
    reinterpretRecWith,
    reinterpretRecNWith,

    -- *** For higher-order effects
    reinterpretH,
    reinterpretNH,
    reinterpretHWith,
    reinterpretNHWith,
    reinterpretHBy,
    reinterpretNHBy,
    reinterpretRecHWith,
    reinterpretRecNHWith,

    -- ** Interposition functions

    -- *** For first-order effects
    interpose,
    interposeWith,
    interposeBy,
    interposeRecWith,

    -- *** For higher-order effects
    interposeH,
    interposeRecHWith,

    -- ** Transformation to monads
    iterEffBy,
    iterEffHBy,
    iterEffRecH,
    iterEffRecHWith,
    iterEffRecHFWith,
    iterEffHFBy,
    iterAllEffHFBy,

    -- ** Utilities
    stateless,

    -- ** Ad-hoc stateful interpretation

    -- | Theses entities provides an ad-hoc specialized version to accelerate interpretations that have a
    -- single state type @s@, especially for effects like t'Data.Effect.State.State' or
    --  [@Writer@]("Data.Effect.Writer").
    StateElaborator,
    StateInterpreter,

    -- *** Interpretation functions
    interpretStateBy,
    reinterpretStateBy,
    interpretStateRecWith,
    reinterpretStateRecWith,

    -- *** Interposition functions
    interposeStateBy,

    -- *** Transformation to monads
    iterStateAllEffHFBy,

    -- * Transforming effects

    -- ** Rewriting effectful operations
    transform,
    transformH,
    translate,
    translateH,
    rewrite,
    rewriteH,
    transEff,
    transEffH,
    transEffHF,

    -- ** Manipulating the effect list (without rewriting effectful operations)

    -- *** Insertion functions
    raise,
    raises,
    raiseN,
    raiseUnder,
    raisesUnder,
    raiseNUnder,
    raiseH,
    raisesH,
    raiseNH,
    raiseUnderH,
    raiseNUnderH,

    -- *** Merging functions
    subsume,
    subsumes,
    subsumeN,
    subsumeUnder,
    subsumesUnder,
    subsumeNUnder,
    subsumeH,
    subsumesH,
    subsumeNH,
    subsumeUnderH,
    subsumeNUnderH,

    -- ** Bundling functions
    bundle,
    bundleN,
    unbundle,
    unbundleN,
    bundleUnder,
    unbundleUnder,
    bundleAll,
    unbundleAll,
    bundleH,
    unbundleH,
    bundleUnderH,
    unbundleUnderH,
    bundleAllH,
    unbundleAllH,

    -- *** Manipulating Tags & Keys
    tag,
    untag,
    retag,
    tagH,
    untagH,
    retagH,
    unkey,
    rekey,
    unkeyH,
    rekeyH,

    -- * Misc
    HFunctor,
    ReaderKey,
    WriterKey,
    StateKey,
    ErrorKey,
    Type,
    liftIO,
    module Data.Effect.OpenUnion,
    module Data.Effect,
    module Data.Effect.TH,
    module Control.Effect,
) where

import Control.Monad.Hefty.Interpret (
    interpose,
    interposeBy,
    interposeH,
    interposeRecHWith,
    interposeRecWith,
    interposeWith,
    interpret,
    interpretBy,
    interpretH,
    interpretHBy,
    interpretHWith,
    interpretRecHWith,
    interpretRecWith,
    interpretWith,
    iterAllEffHFBy,
    iterEffBy,
    iterEffHBy,
    iterEffHFBy,
    iterEffRecH,
    iterEffRecHFWith,
    iterEffRecHWith,
    reinterpret,
    reinterpretBy,
    reinterpretH,
    reinterpretHBy,
    reinterpretHWith,
    reinterpretN,
    reinterpretNBy,
    reinterpretNH,
    reinterpretNHBy,
    reinterpretNHWith,
    reinterpretNWith,
    reinterpretRecHWith,
    reinterpretRecNHWith,
    reinterpretRecNWith,
    reinterpretRecWith,
    runEff,
    runPure,
    stateless,
 )

import Control.Monad.Hefty.Interpret.State (
    StateElaborator,
    StateInterpreter,
    interposeStateBy,
    interpretStateBy,
    interpretStateRecWith,
    iterStateAllEffHFBy,
    reinterpretStateBy,
    reinterpretStateRecWith,
 )
import Control.Monad.Hefty.Transform (
    bundle,
    bundleAll,
    bundleAllH,
    bundleH,
    bundleN,
    bundleUnder,
    bundleUnderH,
    raise,
    raiseH,
    raiseN,
    raiseNH,
    raiseNUnder,
    raiseNUnderH,
    raiseUnder,
    raiseUnderH,
    raises,
    raisesH,
    raisesUnder,
    rekey,
    rekeyH,
    retag,
    retagH,
    rewrite,
    rewriteH,
    subsume,
    subsumeH,
    subsumeN,
    subsumeNH,
    subsumeNUnder,
    subsumeNUnderH,
    subsumeUnder,
    subsumeUnderH,
    subsumes,
    subsumesH,
    subsumesUnder,
    tag,
    tagH,
    transEff,
    transEffH,
    transEffHF,
    transform,
    transformH,
    translate,
    translateH,
    unbundle,
    unbundleAll,
    unbundleAllH,
    unbundleH,
    unbundleN,
    unbundleUnder,
    unbundleUnderH,
    unkey,
    unkeyH,
    untag,
    untagH,
 )

import Control.Effect
import Control.Monad.Hefty.Types (
    Eff (..),
    Elaborator,
    ErrorKey,
    Interpreter,
    ReaderKey,
    StateKey,
    WriterKey,
    send,
    send0,
    send0H,
    sendH,
    sendN,
    sendNH,
    sendUnion,
    sendUnionBy,
    sendUnionH,
    sendUnionHBy,
    type (!!),
    type ($),
    type ($$),
    type (:!!),
    type (~~>),
 )
import Control.Monad.IO.Class (liftIO)
import Data.Effect
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.OpenUnion
import Data.Effect.OpenUnion.Sum (type (:+:))
import Data.Effect.TH
import Data.Kind (Type)
