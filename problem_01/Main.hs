import Data.Foldable (foldl')
import System.Environment (getArgs)

findSumImpl :: [Int] -> [Int] -> Int -> [Int]
findSumImpl [] _ _ = []
findSumImpl (_:y:xs) [] target = findSumImpl (y:xs) xs target
findSumImpl _ [] _ = []
findSumImpl a@(x:_) (y:ys) target
    | x + y == target = [x,y]
    | otherwise = findSumImpl a ys target

findTwoSum :: [Int] -> Int -> [Int]
findTwoSum [] _ = []
findTwoSum a@(_:xs) target = findSumImpl a xs target

findThreeSum :: [Int] -> Int -> [Int]
findThreeSum [] _ = []
findThreeSum (x:xs) target  = case findTwoSum xs (target-x) of
    [] -> findThreeSum xs target
    ys -> x:ys

product' :: (Foldable a, Num b) => a b -> b
product' = foldl' (*) 1

main :: IO ()
main = do
    path <- head <$> getArgs
    ls  <- (map read . lines <$> readFile path) :: IO [Int]
    let answer_one = product' $ findTwoSum ls 2020
    let answer_two = product' $ findThreeSum ls 2020
    print answer_one
    print answer_two