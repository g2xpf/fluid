import Data.List
f :: Double -> Double
f r = m * (315 / (64 * pi)) * ((h ^ 2 - r ^ 2) ^ 3) / h ^ 9
  where h = 0.02
        m = 0.00020543

main :: IO ()
main = do
  let v0 = [-0.01, -0.00999..0.01] :: [Double]
      v1 = map f v0
      v2 = zip (map (/ 0.02) v0) v1
      makePare (x, y) = show x ++ ", " ++ (show y)
      v3 = concat $ intersperse "\n" $ map makePare v2
  writeFile "output.log" v3
  
      
