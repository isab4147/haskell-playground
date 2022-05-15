module Ex9a where
import Text.Read

partWeight :: (Integral a) => a -> Maybe a -> a
partWeight pw (Just w) = pw + w
partWeight pw _ = pw

weightToFuel :: (Integral a) => a -> a -> a
weightToFuel weight fuel | newFuel == 0 = fuel
                         | otherwise = weightToFuel (weight + newFuel) (fuel + newFuel)
                         where
                           capacity = if fuel /= 0
                                      then (fuel + 2) * 3
                                      else 0
                           newFuel = max 0 $ (weight - capacity) `div` 3 - 2

main :: IO ()
main = do
  fileLines <- readFile "componentes.txt"
  rawWeightList <- return $ lines fileLines
  weightList <- return $ map readMaybe rawWeightList
  partialWeight <- return $ foldl partWeight 0 weightList
  totalFuel <- return $ weightToFuel partialWeight 0

  print totalFuel
