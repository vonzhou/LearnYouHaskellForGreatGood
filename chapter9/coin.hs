import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen 
      (thirdCoin, newGen'') = random newGen' 
  in (firstCoin, secondCoin, thirdCoin )


--main = putStrLn $ show $ threeCoins (mkStdGen 8)
--
finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen = 
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in (value:restOfList, finalGen)

