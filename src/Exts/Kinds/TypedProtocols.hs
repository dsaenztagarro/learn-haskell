{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Exts.Kinds.TypedProtocols
-- Stage       : 06-Kinds  (capstone — see docs/ROADMAP.md)
-- Source      : Tweag / typed-protocols library and lectures (Cardano network)
-- Prereqs     : Exts.Kinds.ClosedTypeFamily, Exts.GADT.HeterogeneousList,
--               Exts.GADT.CommandRunner
--
-- == Concept
-- A typed state machine: a kind whose inhabitants are the protocol
-- states (@StIdle@, @StBusy@, @StDone@) plus a GADT indexed by those
-- states so that the type system rules out illegal transitions. The
-- "ping-pong" example here is the simplest non-trivial protocol: only
-- @Idle -> Busy -> Idle@ or @Idle -> Done@ are reachable.
module Exts.Kinds.TypedProtocols where

-- Start with the protocol states, as types of a common kind

data PingPong where   -- This is a kind
  StIdle :: PingPong  -- These are types
  StBusy :: PingPong
  StDone :: PingPong

-- Define messages between the states

class Protocol ps where
--             ^ at this point `ps` is just a type

  data Message ps (st :: ps) (st' :: ps)  -- data family declaration
  --               ^ from     ^ to   ^ when declaring an associated data family
  --                                   things shift to the type level, and here
  --                                   `ps` acts as a _kind_.
  --           ^ `ps` acts as a _type_
  --
  -- That dual role of `ps` is totally allowed. GHC (with Data.Kinds) promotes
  -- types to kinds where necessary.
  --
  -- `st` and `st'` are type parameter that live in the kind `ps`

  data ClientHasAgency (st :: ps)
  data ServerHasAgency (st :: ps)
  data NobodyHasAgency (st :: ps)

instance Protocol PingPong where        -- Protocol class

  data Message PingPong st st' where
    MsgPing :: Message PingPong StIdle StBusy
    MsgPong :: Message PingPong StBusy StIdle
    MsgDone :: Message PingPong StIdle StDone
  -- ^ You are both:
  --   (1) Defining the associated data type `Message PingPong from to`
  --   (2) Using GADT syntax to give it constructors that are constrained to
  --       specific transitions.

  -- Label states with peer agency:

  data ClientHasAgency st where       -- more associated data families
    TokIdle :: ClientHasAgency StIdle
  -- ^ `ClientHasAgency StIdle` is inhabited

  data ServerHasAgency st where
    TokBusy :: ServerHasAgency StBusy

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency StDone


-- This definition is only used as promoted types and kinds, never as values.

data PeerRole = AsClient | AsServer

data Peer ps (pr :: PeerRole) (st :: ps) m a where
  Effect :: m (Peer ps pr st m a) -> Peer ps pr st m a

  -- We can terminate with a result, but only in a terminal protocol state.
  -- Require _evidence_ to show we are in a terminal state.

  Done :: NobodyHasAgency st -- require evidence
       -> a
       -> Peer ps pr st m a

  -- We can send a message, but only in a state where we have agency!
  -- Require evidence to show we are in a state we have agency

  Yield :: WeHaveAgency pr st -- require evidence
        -> Message ps st st'
        -> Peer ps pr st' m a
        -> Peer ps pr st m a

type WeHaveAgency (pr :: PeerRole) st = PeerHasAgency pr st

data PeerHasAgency (pr :: PeerRole) (st :: ps) where
  ClientAgency :: ClientHasAgency st -> PeerHasAgency AsClient st
  ServerAgency :: ServerHasAgency st -> PeerHasAgency AsServer st

-- pingPongClient :: Int -> Peer PingPong AsClient StIdle m ()
-- pingPongServer ::        Peer PingPong AsServer StIdle m Int
--                                                        ^ the type of any final result once the peer terminates
--                                                     ^ the monad in which the peer operates
--                                              ^ the current protocol state
--                                     ^ the client/server role
--                            ^ the protocol itself




{-
Examples:

Effect $ do
  ...           -- actions in the monad
  return $ ...  -- another Peer value

Yield (ClientAgency TokIdle)
      MsgDone
      (Done TokDone result)  -- provide evidence

Yield (ClientAgency TokIdle) MsgPing $ ... -- provide evidence
-}
