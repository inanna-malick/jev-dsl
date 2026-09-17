-- | Typed records for TypeSafe's Jev. Two fronts over one core:
--
-- * "Jev.Operators" — for agent use and review: anonymous type-indexed
--   packets, type-level disjunctions, typed handler lists.
-- * @Jev.Records@ — for human use and review: declared records with
--   ordinary sums and enums (designed in @docs/records-dsl.md@; not yet
--   implemented).
--
-- This module re-exports the shared vocabulary: errors, the model, the
-- policy types, and the response envelope accessors are the same on both
-- fronts. "Jev.Core" is the polymorphic core for another JSON value type.
module Jev
  ( Model (..), jevLatest
  , PrepError (..), DecodeError (..), Rejection (..), ValidationIssue (..), JevError (..)
  , Policy (..), lenient, Doubt (..)
  , Presence (..)
  ) where

import Jev.Aeson ()
import Jev.Core
