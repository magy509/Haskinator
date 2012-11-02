module Imperativo where

newtype Imperativo s a = Imperativo (s -> (a, s))