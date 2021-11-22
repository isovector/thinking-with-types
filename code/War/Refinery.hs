{-# LANGUAGE DerivingStrategies #-}

module War.Refinery where

import GHC.Generics

data ProofStateT x' x e s m a
  = Subgoal
      a
      (x' -> ProofStateT x' x e s m a)
  | Effect
      (m (ProofStateT x' x e s m a))
  | Stateful
      (s -> (s, ProofStateT x' x e s m a))
  | Alt
      (ProofStateT x' x e s m a)
      (ProofStateT x' x e s m a)
  | Interleave
      (ProofStateT x' x e s m a)
      (ProofStateT x' x e s m a)
  | Commit
      (ProofStateT x' x e s m a)
      (ProofStateT x' x e s m a)
  | Empty
  | Failure
      e
      (x' -> ProofStateT x' x e s m a)
  | Handle
      (ProofStateT x' x e s m a)
      (e -> m e)
  | Axiom x
  deriving stock (Generic, Functor)

