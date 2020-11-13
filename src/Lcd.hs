module Lcd
    ( lcdNumber
    , width
    , height
    ) where

import Data.List (transpose)
import Data.Function ((&))

newtype Width = Width Int
newtype Height = Height Int
type Scale = (Width, Height)
data LcdNumber = LcdNumber Scale Int

lcdNumber :: Int -> LcdNumber
lcdNumber = LcdNumber (Width 1, Height 1)

width :: Int -> LcdNumber -> LcdNumber
width w (LcdNumber (_,h) n) = LcdNumber (Width w, h) n

height :: Int -> LcdNumber -> LcdNumber
height h (LcdNumber (w,_) n) = LcdNumber (w, Height h) n

instance Show LcdNumber where
    show (LcdNumber s n) = decimalDigits n & map (scale s . lcdDigit) & transpose & map concat & unlines

type DecimalDigit = Int

decimalDigits :: Int -> [DecimalDigit]
decimalDigits x | x >= 0 && x < 10 = [x]
                | x >= 10 = decimalDigits (x `quot` 10) ++ decimalDigits (x `rem` 10)

type LcdDigit = [String]

lcdDigit :: Int -> LcdDigit
lcdDigit x | x >= 0 && x < 10 = (lcdDigits !! x)

lcdDigits :: [LcdDigit]
lcdDigits = transpose [ [" _ ", "   ",  " _ ", " _ ", "   ", " _ ", " _ ", " _ ", " _ ", " _ "]
                      , ["| |", "  |",  " _|", " _|", "|_|", "|_ ", "|_ ", "  |", "|_|", "|_|"]
                      , ["|_|", "  |",  "|_ ", " _|", "  |", " _|", "|_|", "  |", "|_|", " _|"] ]

scale :: Scale -> LcdDigit -> LcdDigit
scale (Width w, Height h) [ [top_left, top, top_right]
                          , [left, center, right]
                          , [bottom_left, bottom, bottom_right]
                          ] = [ [top_left] ++ replicate w top ++ [top_right] ] ++
                              replicate (h - 1) ([left] ++ replicate w ' ' ++ [right]) ++
                              [ [left] ++ replicate w center ++ [right] ] ++
                              replicate (h - 1) ([bottom_left] ++ replicate w ' ' ++ [bottom_right]) ++
                              [ [bottom_left] ++ replicate w bottom ++ [bottom_right] ]

