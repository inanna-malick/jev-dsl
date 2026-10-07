{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- The nominal policy tag is an authority boundary.
import Data.Coerce (coerce)
import Jev.Core

bad :: Policy Lenient -> Policy Strict
bad = coerce
