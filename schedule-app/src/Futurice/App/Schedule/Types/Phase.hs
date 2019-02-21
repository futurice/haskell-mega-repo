{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Futurice.App.Schedule.Types.Phase where

data Phase = Input     -- ^User input
           | Executing -- ^Command that needs executing (for example sending emails) TODO: remove this?
           | Done      -- ^Command is done

type family Phased (phase :: Phase) a b c where
    Phased 'Input a b c     = a
    Phased 'Executing a b c = b
    Phased 'Done a b c      = c
