import FRP.Helm
import qualified FRP.Helm.Mouse as Mouse
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Window as Window

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

beacon = [[0,0,1,1,0],[0,0,0,1,0],[1,0,0,0,0],[1,1,0,0,0],[0,0,0,0,0]]
toad = [[0,1,0,0,0],[0,1,1,0,0],[0,1,1,0,0],[0,0,1,0,0],[0,0,0,0,0]]
genGrid x y = [[0 | n<-[1..x]] | m<-[1..y]]


updateGridRow x y grid
	| x == (fromIntegral . length) grid = []
	| gp x y grid == 1 = updateLive x y grid: updateGridRow (x+1) y grid
	| gp x y grid == 0 = updateDead x y grid: updateGridRow (x+1) y grid

updateGrid y grid
	| y == (fromIntegral . length) grid = []
	| otherwise = updateGridRow 0 y grid:updateGrid (y+1) grid

genRender siz grid = gr 0 siz grid
	where
	gr _ _ [] = []
	gr x siz (c:cs) = genRender' x 0 siz c ++ gr (x+1) siz cs

genRender' _ _ _ [] = []
genRender' x y siz (0:rest) = genRender' x (y+1) siz rest
genRender' x y siz (1:rest) = newRect:genRender' x (y+1) siz rest
	where 
	coord = (siz*x + siz/2, siz*y + siz/2) 
	newRect = (move coord $ filled red $ rect siz siz)	

step :: Time -> [[Integer]] -> [[Integer]]
step dt grid = updateGrid 0 grid

fi = fromIntegral

render :: [[Integer]] -> (Int,Int) -> Element
render grid (w,h) = collage w h (genRender 30 grid)

main :: IO ()
main = run defaultConfig $ render <~ foldp step beacon (Time.delay $ Time.fps 20)  ~~ Window.dimensions 


