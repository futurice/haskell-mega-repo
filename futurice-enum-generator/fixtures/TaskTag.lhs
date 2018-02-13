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
