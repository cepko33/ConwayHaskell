import Graphics.HGL

updateLive x y grid
	| neigh < 2 = 0
	| neigh > 3 = 0
	| otherwise = 1
	where 
	num = gp x y grid
	neigh = numNeigh x y grid

updateDead x y grid = if (numNeigh x y grid) == 3 then 1 else 0

gp :: Integer -> Integer -> [[Integer]] -> Integer
gp x y grid
	| x < 0 || y < 0  || x == (fromIntegral . length) grid || y == (fromIntegral . length) grid = 0
	|otherwise = ((grid !! (fromIntegral x)) !! (fromIntegral y))

numNeigh :: Integer -> Integer -> [[Integer]] -> Integer
numNeigh x y grid = sum [gp (x+n) (y+m) grid | n<-[-1..1], m<-[-1..1], not $ (n == 0 && m == 0)]

testGrid = [[0,0,0,1,0],[0,1,0,0,0],[0,0,1,0,0],[0,0,1,0,0],[0,0,0,0,0]]

-- 00000
-- 10000
-- 00110
-- 01000
-- 00000
--becomes
-- 00000
-- 00000
-- 01100
-- 00100
-- 00000

updateGridCol x y grid
	| y == (fromIntegral . length) grid = []
	| gp x y grid == 1 = updateLive x y grid: updateGridCol x (y+1) grid
	| gp x y grid == 0 = updateDead x y grid: updateGridCol x (y+1) grid

updateGrid x grid
	| x == (fromIntegral . length) grid = []
	| otherwise = updateGridCol x 0 grid:updateGrid (x+1) grid


main :: IO ()
main = runGraphics $
	withWindow_ "Hello World Window" (300, 200) $ \ w -> do
	drawInWindow w $ text (100, 100) "Hello World"
	drawInWindow w $ ellipse (100, 80) (200, 180)
	getKey w
