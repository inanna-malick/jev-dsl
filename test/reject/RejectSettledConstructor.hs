{-# LANGUAGE DataKinds #-}

-- Consumers can inspect a settled value only through settledValue.
import Jev.Core

bad :: Settled Lenient String
bad = Settled "unchecked"
