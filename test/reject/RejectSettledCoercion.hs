{-# LANGUAGE DataKinds #-}

-- The settled policy tag must survive representation coercion.
import Data.Coerce (coerce)
import Jev.Core

bad :: Settled Lenient String -> Settled Strict String
bad = coerce
