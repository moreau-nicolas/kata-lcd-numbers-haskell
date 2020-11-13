module Lcd.LcdSpec where

import Lcd (lcdNumber, height, width)

import Test.Hspec
import Control.Category ((>>>))

infix 1 `shouldBeShownAs`
shouldBeShownAs :: (HasCallStack, Show a) => a -> String -> Expectation
actual `shouldBeShownAs` expected = show actual `shouldBe` expected

spec :: Spec
spec = do
    describe "LCD numbers" $ do
        describe "showing an LCD Number" $ do
            let lcd = lcdNumber
            it "should show 0" $ do
                lcd 0 `shouldBeShownAs`
                    " _ \n" ++
                    "| |\n" ++
                    "|_|\n"
            it "should show 1" $ do
                lcd 1 `shouldBeShownAs`
                    "   \n" ++
                    "  |\n" ++
                    "  |\n"
            it "should show 2" $ do
                lcd 2 `shouldBeShownAs`
                    " _ \n" ++
                    " _|\n" ++
                    "|_ \n"
            it "should show 3" $ do
                lcd 3 `shouldBeShownAs`
                    " _ \n" ++
                    " _|\n" ++
                    " _|\n"
            it "should show 4" $ do
                lcd 4 `shouldBeShownAs`
                    "   \n" ++
                    "|_|\n" ++
                    "  |\n"
            it "should show 5" $ do
                lcd 5 `shouldBeShownAs`
                    " _ \n" ++
                    "|_ \n" ++
                    " _|\n"
            it "should show 6" $ do
                lcd 6 `shouldBeShownAs`
                    " _ \n" ++
                    "|_ \n" ++
                    "|_|\n"
            it "should show 7" $ do
                lcd 7 `shouldBeShownAs`
                    " _ \n" ++
                    "  |\n" ++
                    "  |\n"
            it "should show 8" $ do
                lcd 8 `shouldBeShownAs`
                    " _ \n" ++
                    "|_|\n" ++
                    "|_|\n"
            it "should show 9" $ do
                lcd 9 `shouldBeShownAs`
                    " _ \n" ++
                    "|_|\n" ++
                    " _|\n"
            it "should show 10" $ do
                lcd 10 `shouldBeShownAs`
                    "    _ \n" ++
                    "  || |\n" ++
                    "  ||_|\n"
            it "should show 23" $ do
                lcd 23 `shouldBeShownAs`
                    " _  _ \n" ++
                    " _| _|\n" ++
                    "|_  _|\n"
            it "should show 456" $ do
                lcd 456 `shouldBeShownAs`
                    "    _  _ \n" ++
                    "|_||_ |_ \n" ++
                    "  | _||_|\n"
            it "should show 1234567890" $ do
                lcd 1234567890 `shouldBeShownAs`
                    "    _  _     _  _  _  _  _  _ \n" ++
                    "  | _| _||_||_ |_   ||_||_|| |\n" ++
                    "  ||_  _|  | _||_|  ||_| _||_|\n"
        describe "scaling an LCD Number" $ do
            it "should scale 0 horizontally" $ do
                let lcd = lcdNumber >>> width 2
                lcd 0 `shouldBeShownAs`
                    " __ \n" ++
                    "|  |\n" ++
                    "|__|\n"
            it "should scale 9 horizontally" $ do
                let lcd = lcdNumber >>> width 3
                lcd 9 `shouldBeShownAs`
                    " ___ \n" ++
                    "|___|\n" ++
                    " ___|\n"
            it "should scale 0 vertically" $ do
                let lcd = lcdNumber >>> height 2
                lcd 0 `shouldBeShownAs`
                    " _ \n" ++
                    "| |\n" ++
                    "| |\n" ++
                    "| |\n" ++
                    "|_|\n"
            it "should scale 9 vertically" $ do
                let lcd = lcdNumber >>> height 3
                lcd 9 `shouldBeShownAs`
                    " _ \n" ++
                    "| |\n" ++
                    "| |\n" ++
                    "|_|\n" ++
                    "  |\n" ++
                    "  |\n" ++
                    " _|\n"
            it "should scale 456 both vertically and horizontally" $ do
                let lcd = lcdNumber >>> height 2 >>> width 3
                lcd 456 `shouldBeShownAs`
                    "      ___  ___ \n" ++
                    "|   ||    |    \n" ++
                    "|___||___ |___ \n" ++
                    "    |    ||   |\n" ++
                    "    | ___||___|\n"
            it "should scale 789 both vertically and horizontally" $ do
                let lcd = lcdNumber >>> width 2 >>> height 3
                lcd 789 `shouldBeShownAs`
                    " __  __  __ \n" ++
                    "   ||  ||  |\n" ++
                    "   ||  ||  |\n" ++
                    "   ||__||__|\n" ++
                    "   ||  |   |\n" ++
                    "   ||  |   |\n" ++
                    "   ||__| __|\n"
            it "should scale 1234567890 both vertically and horizontally" $ do
                let lcd = lcdNumber >>> width 3 >>> height 2
                lcd 1234567890 `shouldBeShownAs`
                    "      ___  ___       ___  ___  ___  ___  ___  ___ \n" ++
                    "    |    |    ||   ||    |        ||   ||   ||   |\n" ++
                    "    | ___| ___||___||___ |___     ||___||___||   |\n" ++
                    "    ||        |    |    ||   |    ||   |    ||   |\n" ++
                    "    ||___  ___|    | ___||___|    ||___| ___||___|\n"
