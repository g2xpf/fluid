import Data.List
f :: Double -> Double
f r = m * (315 / (64 * pi)) * ((h ^ 2 - r ^ 2) ^ 3) / h ^ 9
  where h = 0.02
        m = 0.00020543

getRho1 :: Double -> Double
getRho1 rho = k * (rho - rho0)
  where k = 1.0
        rho0 = 82.66

getRho2 :: Double -> Double
getRho2 rho = c * c * rho / gamma * (((rho / rho0) ** gamma) - 1.0)
  where c = 0.075
        gamma = 7.0
        rho0 = 82.66
main :: IO ()
main = do
  let v0 = [-0.02, -0.01999..0.02] :: [Double]
      v1 = map (getRho1 . (* 5.3) . f) v0
      v1' = map (getRho2 . (* 5.3) . f) v0
      v2 = zip (map (/ 0.02) v0) v1
      v2' = zip (map (/ 0.02) v0) v1'
      makePare (x, y) = show x ++ ", " ++ (show y)
      v3 = concat $ intersperse "\n" $ map makePare v2
      v3' = concat $ intersperse "\n" $ map makePare v2'
  writeFile "output1.log" v3
  writeFile "output2.log" v3'
  
      
