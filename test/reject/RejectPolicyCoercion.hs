{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- The nominal tag records which validated policy ran; it cannot be relabelled.
import Data.Coerce (coerce)
import Jev.Core

bad :: Policy Lenient -> Policy Strict
bad = coerce
