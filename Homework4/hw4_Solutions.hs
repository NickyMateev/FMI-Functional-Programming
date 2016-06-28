main = do
  print (minDistance [(1,1,1), (10, 1, 1), (5,1,1)])
  print (maximize ([(\ x -> x * x * x), (\ x -> x + 1)]) (0.5))
  print (maximize ([(\ x -> x * x * x), (\ x -> x + 1)]) (-2))

-- Problem 1
type Point = (Double, Double, Double)

minDistance :: [Point] -> Double
minDistance pts = minimum (allDistances pts)

allDistances :: [Point] -> [Double]
allDistances []  = []
allDistances pts = (minDistanceH (head pts) (tail pts)) ++ (allDistances (tail pts))

minDistanceH :: Point -> [Point] -> [Double]
minDistanceH pt []  = []
minDistanceH pt pts = (d pt (head pts)) : (minDistanceH pt (tail pts))

d :: Point -> Point -> Double
d (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2) + (z1 - z2)*(z1 - z2))

-- Problem 2
maximize :: [(Double -> Double)] -> (Double -> Double)
maximize fs = (\x -> (maximizeH fs x))

maximizeH :: [(Double -> Double)] -> Double -> Double
maximizeH fs x
  | length fs == 1    = (head fs) x
  | otherwise         = maxAbs (head fs x) (maximizeH (tail fs) x)

maxAbs :: Double -> Double -> Double
maxAbs x y
  | (abs x) > (abs y) = x
  | otherwise         = y
