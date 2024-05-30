import Poly
import SimpleLang
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "poly" $ do
        it "applyPoly" $ do
            applyPoly (P [1, 2, 1]) 0 `shouldBe` 1
            applyPoly (P [1, 1, 1, 1]) 1 `shouldBe` 4
            applyPoly (P [1, 2, 3, 4]) 5 `shouldBe` 586
        it "+" $ do 
            (P [1, 2, 1]) + (P [2, -2, 3]) `shouldBe` (P [3, 0, 4])
            (P [1, 1, 1]) + (P []) `shouldBe` (P [1, 1, 1])
        it "*" $ do
            (P [1, 2, 1]) * (P [2, -2, 3]) `shouldBe` (P [2, 2, 1, 4, 3])
            (P [1, 2]) * (P [2, 3, 1]) `shouldBe` (P [2, 7, 7, 2])
        it "negate" $ do 
            negate (P [1, 2, 1]) `shouldBe` P [-1, -2, -1]
            negate (P [0, 0, 0, -1]) `shouldBe` P [0, 0, 0, 1]
        it "(==)" $ do 
            ((P [1, 2, 1]) == (P [2, -2, 3])) `shouldBe` False
            ((P [1, 2, 1]) == (P [1, 2, 1, 0, 0])) `shouldBe` True
            ((P [1, 0, 3, 1]) == (P [1, 0, 3, 1])) `shouldBe` True
        it "show" $ do 
            show (P [1, 2, 1]) `shouldBe` "x^2 + 2 * x + 1"
            show (P [0, 0, 3, 4]) `shouldBe` "4 * x^3 + 3 * x^2"
        it "nderiv" $ do 
            nderiv 3 (P [2, 2, 1, 4, 3]) `shouldBe` (P [24, 72])
            nderiv 2 (P [2, 7, 7, 2]) `shouldBe` (P [14, 12])

        it "newton" $ do 
            newton (P [-1, 0, 1]) 2 0.001 9 `shouldBe` Just 1.0003048780487804
            newton (P [-3, 4, 0, 1]) 0.7 0.001 15 `shouldBe` Just 0.6735930845595811
            newton (P [2, -2, 0, 1]) 0 0.001 20 `shouldBe` Nothing

    describe "simpleLang" $ do
        -- включите тесты на работу 
        it "empty" $ do
            empty "a" `shouldBe` 0
        it "extend" $ do  
            extend empty "a" 113 "a" `shouldBe` 113
        it "eval" $ do  
            eval (extend empty "a" 7) (Op (Var "a") Eql (Val 25)) `shouldBe` 0
            eval (extend empty "a" 1) (Op (Var "a") Plus (Val 3)) `shouldBe` 4
        it "desugar" $ do  
            desugar (Incr "a") `shouldBe` DAssign "a" (Op (Var "a") Plus (Val 1))
            desugar (Assign "a" (Val 1)) `shouldBe` DAssign "a" (Val 1)
        it "programms" $ do  
            runSimpler empty (DAssign "A" (Val 10)) "A" `shouldBe` 10
            SimpleLang.run empty (Incr "A") "A" `shouldBe` 1
            ((SimpleLang.run (extend empty "In" 4) factorial) "Out") `shouldBe` 24
            ((SimpleLang.run (extend empty "In" 4) fibonacci) "Out") `shouldBe` 5
            ((SimpleLang.run (extend empty "A" 100) squareRoot) "B") `shouldBe` 10
