module Ex9a where
import Text.Read

weightToFuel :: (Integral a) => a -> Maybe a -> a
weightToFuel pw (Just w) = pw + max 0 (w `div` 3 - 2)
weightToFuel pw _ = pw

main :: IO ()
main = do
  fileLines <- readFile "componentes.txt"
  rawWeightList <- return $ lines fileLines
  weightList <- return $ map readMaybe rawWeightList
  totalFuel <- return $ foldl weightToFuel 0 weightList

  print totalFuel
