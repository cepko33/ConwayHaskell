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
genGrid x = [[0 | n<-[1..x]] | m<-[1..x]]


updateGridCol x y grid
	| y == (fromIntegral . length) grid = []
	| gp x y grid == 1 = updateLive x y grid: updateGridCol x (y+1) grid
	| gp x y grid == 0 = updateDead x y grid: updateGridCol x (y+1) grid

updateGrid x grid
	| x == (fromIntegral . length) grid = []
	| otherwise = updateGridCol x 0 grid:updateGrid (x+1) grid

genRender siz grid = genRenderW 0 siz grid

genRenderW _ _ [] = []
genRenderW x siz grid = genRender' x 0 siz (head grid) ++ genRenderW (x+1) siz (tail grid)

genRender' _ _ _ [] = []
genRender' x y siz (0:rest) = genRender' x (y+1) siz rest
genRender' x y siz (1:rest) = (move (siz*x + siz/2, siz*y + siz/2) $ filled red $ rect siz siz):genRender' x (y+1) siz rest

step :: Time -> [[Integer]] -> [[Integer]]
step dt True grid = updateGrid 0 grid

fi = fromIntegral

render :: [[Integer]] -> (Int,Int) -> Element
render grid (w,h) = collage w h (genRender 20 grid)

main :: IO ()
main = run defaultConfig $ render <~ foldp step toad ( (Time.delay $ Time.fps 2)  ~~ Window.dimensions 


