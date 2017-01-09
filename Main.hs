module Main where
import Control.Concurrent
import System.Environment

main = do
	args <- getArgs
	let values = case map (limit.read) args of
		[from, to] | from <= to -> [from..to] 
		[from, to] -> reverse [to..from]
		values -> values
	mapM_ lightup values

lightup n = do
	threadDelay 1000
	writeFile path (show n)

limit :: Integer -> Integer
limit n | n < 0 = 0
limit n | n >= 4529 = 4529
limit n = n

path = "/sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-LVDS-1/intel_backlight/brightness"
