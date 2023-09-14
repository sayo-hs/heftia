# Example 2 - Logging

```haskell
class Log f where
    log :: LogLevel -> Text -> f ()
makeEffectF ''Log
```

