-- | Control Backlight brightness natively on intel notebooks.
-- You need intel_reg prog.
-- 
--    $ sudo chown root:users ~/.cabal/bin/lightctl
--    $ sudo chmod +s ~/.cabal/bin/lightctl

import Control.Concurrent
import System.Environment
import System.Process

main = do
  args <- getArgs
  let values = case map (limit.read) args of
                 [from, to] | from <= to -> [from..to] 
                 [from, to] -> reverse [to..from]
                 values -> values
  mapM_ lightup values

lightup n = do
  threadDelay 1000
  callProcess "intel_reg" ["write", "0x48254", show n]

limit :: Integer -> Integer
limit n | n < 0 = 0
limit n | n >= 4529 = 4529
limit n = n

