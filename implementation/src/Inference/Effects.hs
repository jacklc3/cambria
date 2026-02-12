module Inference.Effects where

import qualified Data.Map as Map

import Types

closed :: EffectsType
closed = Closed mempty

effectOps :: EffectsType -> Map.Map Op Arity
effectOps (Closed m) = m
effectOps (Open m _) = m

closeEffects :: EffectsType -> EffectsType
closeEffects (Closed m) = Closed m
closeEffects (Open m _) = Closed m

effectUnion :: EffectsType -> EffectsType -> EffectsType
effectUnion (Closed m1) (Closed m2) = Closed (m1 <> m2)
effectUnion (Closed m1) (Open m2 r) = Open (m1 <> m2) r
effectUnion (Open m1 r) (Closed m2) = Open (m1 <> m2) r
effectUnion (Open m1 _) (Open m2 r) = Open (m1 <> m2) r

effectDiff :: EffectsType -> EffectsType -> EffectsType
effectDiff (Closed m1) (Closed m2) = Closed (m1 Map.\\ m2)
effectDiff (Open m1 r) (Closed m2) = Open (m1 Map.\\ m2) r
effectDiff (Open m1 r) (Open m2 _) = Open (m1 Map.\\ m2) r
effectDiff (Closed m1) (Open m2 _) = Closed (m1 Map.\\ m2)

addEffectOps :: Map.Map Op Arity -> EffectsType -> EffectsType
addEffectOps ops (Closed m) = Closed (ops <> m)
addEffectOps ops (Open m u) = Open (ops <> m) u
