module ISurreal where

import           ISet (ISet)
import qualified ISet as I

data ISurreal = ISurreal (ISet ISurreal) (ISet ISurreal)

zero :: ISurreal
zero = ISurreal (I.empty) (I.empty)

one :: ISurreal
one = ISurreal (I.fromList [zero]) (I.empty)

minusone :: ISurreal
minusone = ISurreal (I.empty) (I.fromList [zero])

--omega :: ISurreal
--omega = ISurreal I.integers I.empty
