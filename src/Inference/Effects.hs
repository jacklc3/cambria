module Inference.Effects where

import qualified Data.Map as Map

import Types

effectOps :: EffectsType -> Map.Map Op Arity
effectOps (Closed m) = m
effectOps (Open m _) = m

addEffectOps :: Map.Map Op Arity -> EffectsType -> EffectsType
addEffectOps ops (Closed m) = Closed (ops <> m)
addEffectOps ops (Open m u) = Open (ops <> m) u

nullifyEffects :: EffectsType -> EffectsType
nullifyEffects (Closed _) = Closed mempty
nullifyEffects (Open _ e) = Open mempty e

