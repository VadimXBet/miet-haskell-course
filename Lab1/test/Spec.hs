import FirstSteps
import Lists
import Luhn
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "first steps" $ do
        -- Можно вложить глубже: describe "xor" do $ ... чтобы дать названия отдельным тестам
        it "xor" $ do
            xor True True `shouldBe` False
            xor True False `shouldBe` True
            xor False True `shouldBe` True
            xor False False `shouldBe` False
        it "max3" $ do
            max3 1 3 2 `shouldBe` 3
            max3 5 2 5 `shouldBe` 5
        it "median3" $ do
            median3 1 5 91  `shouldBe` 5
            median3 2 3 4  `shouldBe` 3
            median3 0 11 9  `shouldBe` 9
        it "rbgToCmyk" $ do
            rbgToCmyk RGB {red = 255, green = 255, blue = 255} `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 255, green = 0, blue = 0}     `shouldBe` CMYK {cyan = 0.0, magenta = 1.0, yellow = 1.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 255, blue = 0}     `shouldBe` CMYK {cyan = 1.0, magenta = 0.0, yellow = 1.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 0, blue = 255}     `shouldBe` CMYK {cyan = 1.0, magenta = 1.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 255, green = 255, blue = 0}   `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 1.0, black = 0.0}
            rbgToCmyk RGB {red = 255, green = 0, blue = 255}   `shouldBe` CMYK {cyan = 0.0, magenta = 1.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 255, blue = 255}   `shouldBe` CMYK {cyan = 1.0, magenta = 0.0, yellow = 0.0, black = 0.0}
            rbgToCmyk RGB {red = 0, green = 0, blue = 0}       `shouldBe` CMYK {cyan = 0.0, magenta = 0.0, yellow = 0.0, black = 1.0}
            rbgToCmyk RGB {red = 10, green = 20, blue = 30}    `shouldBe` CMYK 
              {cyan = 0.666666666666667, magenta = 0.333333333333334, yellow = 0.0, black = 0.8823529411764706}
        it "geomProgression" $ do
            geomProgression 3.0 2.0 2 `shouldBe` 12.0
            geomProgression 1.0 3.0 3 `shouldBe` 27.0
            geomProgression 256.0 0.5 4 `shouldBe` 16.0
            geomProgression 1.0 1.0 16 `shouldBe` 1.0
        it "coprime" $ do
            coprime 10 15 `shouldBe` False
            coprime 12 35 `shouldBe` True
            coprime 31 21 `shouldBe` True
            coprime 5 10 `shouldBe` False
            coprime 30 15 `shouldBe` False
            coprime 100 100 `shouldBe` False
            coprime 0 1 `shouldBe` False
            coprime 0 4 `shouldBe` False
            coprime 12 0 `shouldBe` False
            coprime 0 0 `shouldBe` False
    describe "lists" $ do
        it "distance" $ do
            distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` sqrt 2.0
            distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) `shouldBe` 1.0
            distance (Point [3, 0, 0, 0]) (Point [0, 0, 4, 0]) `shouldBe` 5
            distance (Point []) (Point []) `shouldBe` 0
        it "intersect" $ do
            intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] `shouldBe` [2, 4]
            intersect [1, 2, 4, 6] [3, 5, 7] `shouldBe` []
            intersect [] [] `shouldBe` []
            intersect [1, 1, 1, 1] [2, 2, 2, 2, 2] `shouldBe` []
            intersect [0, 2, 3, 4, 6] [1, 3, 5, 7, 9] `shouldBe` [3]
            intersect [2, 3, 5, 8] [] `shouldBe` []
            intersect [] [1, 10, 100] `shouldBe` []
        it "zipN" $ do
            zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
            zipN [[1, 2, 3], [4, 5], [6]] `shouldBe` [[1, 4, 6], [2, 5], [3]]
            zipN [[1], [2, 2], [3, 3, 3]] `shouldBe` [[1,2,3],[2,3],[3]]
            zipN [] `shouldBe` []
            zipN [[]] `shouldBe` []
        it "find" $ do
            find (> 0) [-1, 2, -3, 4] `shouldBe` Just 2
            find (odd) [-1, 1, -3, 4] `shouldBe` Just (-1)
            find (even) [-1, 1, -3, 5] `shouldBe` Nothing
            find (> 0) [-1, -2, -3] `shouldBe` Nothing
        it "findLast" $ do
            findLast (> 0) [-1, 2, -3, 4] `shouldBe` Just 4
            findLast (< 0) [-1, 2, -3, 4] `shouldBe` Just (-3)
            findLast (odd) [-1, 1, -3, 4] `shouldBe` Just (-3)
            findLast (even) [-1, 1, -3, 5] `shouldBe` Nothing
        it "mapFuncs" $ do
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
            mapFuncs [\x -> sqrt (-x), abs] (-4) `shouldBe` [2.0,4.0]
            mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 `shouldBe` [9, 4, 0]
        it "satisfiesAll" $ do
            satisfiesAll [even, \x -> x `rem` 5 == 0] 10 `shouldBe` True
            satisfiesAll [] 4 `shouldBe` True
        it "tailNel" $ do
            tailNel (NEL 1 [2,3]) `shouldBe` NEL 2 [3]
            tailNel (NEL 1 [2]) `shouldBe` NEL 2 []
        it "lastNel" $ do
            lastNel (NEL 1 [2,3]) `shouldBe` NEL 3 []
            lastNel (NEL 1 [2]) `shouldBe` NEL 2 []
        it "zipNel" $ do
            zipNel (NEL 1 [2,3]) (NEL 1 [2,3]) `shouldBe` NEL (1,1) [(2,2),(3,3)] 
            zipNel (NEL 1 []) (NEL 1 []) `shouldBe` NEL (1,1) []
            zipNel (NEL 1 [2]) (NEL 1 [3]) `shouldBe` NEL (1,1) [(2,3)] 
        it "listToNel" $ do
            listToNel [1,2,3] `shouldBe` (NEL 1 [2,3])
            listToNel [1] `shouldBe` (NEL 1 [])
        it "nelToList" $ do
            nelToList (NEL 1 [2,3]) `shouldBe` [1, 2, 3]
            nelToList (NEL 1 []) `shouldBe` [1]
    describe "luhn" $ do
        it "numberDivider" $ do
            numberDivider 1324 4 `shouldBe` [4, 2, 3, 1]
            numberDivider 2 1 `shouldBe` [2]
            numberDivider 1324223366559900 16 `shouldBe` [0, 0, 9, 9, 5, 5, 6, 6, 3, 3, 2, 2, 4, 2, 3, 1]
        it "readNumber" $ do 
            readNumber "1324" `shouldBe` [4, 2, 3, 1]
            readNumber "1324223366559900" `shouldBe` [0, 0, 9, 9, 5, 5, 6, 6, 3, 3, 2, 2, 4, 2, 3, 1]
            readNumber "" `shouldBe` []
        it "doubleNumbers" $ do 
            doubleNumbers [4, 2, 3, 1] `shouldBe` [4, 4, 3, 2]
            doubleNumbers [2] `shouldBe` [2]
            doubleNumbers [4, 5, 6, 1, 2, 6, 1, 2, 1, 2, 3, 4, 5, 4, 6, 4] `shouldBe` [4, 1, 6, 2, 2, 3, 1, 4, 1, 4, 3, 8, 5, 8, 6, 8]
        it "answer" $ do 
            answer [8, 5, 3, 1, 4, 6, 2, 2, 2, 2, 6, 4, 1, 4, 3, 4] `shouldBe` False
            answer [8, 5, 3, 1, 4, 6, 2, 2, 2, 2, 6, 4, 1, 4, 3, 7] `shouldBe` True
        it "isLuhnValid" $ do 
            isLuhnValid 4561261212345467 `shouldBe` True
            isLuhnValid 4561261212345464 `shouldBe` False