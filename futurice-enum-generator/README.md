# futurice-enum-generator

From

```tex
\begin{code}
module Futurice.App.Checklist.Types.TaskTag (
    TaskTag (..),
    _TaskTag,
    taskTagToText,
    taskTagFromText,
    ) where
\end{code}

\imports

\begin{code}
data TaskTag
    = GithubTask    -- ^ This task relates to Github
    | PlanmillTask  -- ^ This task relates to Planmill

instance ToHtml TaskTag where
    toHtmlRaw = toHtml
    toHtml GithubTask   = "GitHub"
    toHtml PlanmillTask = "PlanMill"
\end{code}

\enum{TaskTag}{"github","planmill"}{ToHtml}
```

with

```
ghc-options: -pgmL futu-enum-gen
build-tool-depends: futurice-enum-generator:futu-enum-gen
```

we get a lot of stuff...

*Note:* we don't generate a lot of stuff, it's all relying on `generics-sop`.
