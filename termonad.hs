{-# LANGUAGE OverloadedStrings #-}

module Main where

import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig, FontSize(FontSizePoints), Option(Set)
  , ShowScrollbar(ShowScrollbarAlways), defaultConfigOptions, defaultFontConfig
  , defaultTMConfig, fontConfig, fontFamily, fontSize, options, showScrollbar
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, addColourExtension, createColour
  , createColourExtension, cursorFgColour, cursorBgColour
  , defaultColourConfig, Palette(ExtendedPalette)
  , palette, List8, unsafeMkList8
  , foregroundColour, backgroundColour
  )
import Numeric (readHex)


-- | Converts a hex string to colour.
makeColour :: [Char] -> AlphaColour Double
makeColour str = let uncurry3 f [r,g,b] = f r g b
                 in uncurry3 createColour (colourFromStr str)
                    where colourFromStr (x:xs) = let rgb     = [ take 2 xs
                                                               , take 2 $ drop 2 xs
                                                               , take 2 $ drop 4 xs
                                                               ]
                                                     dec hex = [(fst . head . readHex) hex]
                                                 in rgb >>= dec


-- | Converts a list of hex strings to the List8 of colours.
makeColourTable :: [[Char]] -> List8 (AlphaColour Double)
makeColourTable xs = unsafeMkList8 $
                     (\colourStr -> [makeColour colourStr]) =<< xs


-- | This sets the foreground colour of the cursor in the terminal.
cursFgColour :: AlphaColour Double
cursFgColour = makeColour "#33305E"


-- | This sets the background colour of the cursor in the terminal.
cursBgColour :: AlphaColour Double
cursBgColour = makeColour "#DFF8FD"


-- | This sets the foreground colour of the terminal.
termFgColour :: AlphaColour Double
termFgColour = cursBgColour


-- | This sets the background colour of the terminal.
termBgColour :: AlphaColour Double
termBgColour = makeColour "#20141A"


-- | This sets normal ANSI colours.
paletteNormal :: List8 (AlphaColour Double)
paletteNormal =
  makeColourTable [ "#36212D" -- 0
                  , "#698B78" -- 1
                  , "#038E7C" -- 2
                  , "#DB954C" -- 3
                  , "#2B7A89" -- 4
                  , "#C2708D" -- 5
                  , "#128588" -- 6
                  , "#A7BABE" -- 7
                  ]


-- | This sets bright ANSI colours.
paletteBright :: List8 (AlphaColour Double)
paletteBright =
  makeColourTable [ "#553346" -- 0
                  , "#78C49A" -- 1
                  , "#01BBA3" -- 2
                  , "#FFC24F" -- 3
                  , "#2CA6BD" -- 4
                  , "#FF7DB1" -- 5
                  , "#0DB6BA" -- 6
                  , "#DFF8FD" -- 7
                  ]


-- | This sets the colours used for the terminal.
colConf :: ColourConfig (AlphaColour Double)
colConf =
  defaultColourConfig
    { cursorBgColour = Set cursBgColour
    , cursorFgColour = Set cursFgColour
    , backgroundColour = Set termBgColour
    , foregroundColour = Set termFgColour
    , palette = ExtendedPalette paletteNormal paletteBright
    }

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Iosevka Term Slab"
    , fontSize = FontSizePoints 13
    }

main :: IO ()
main = do
  colExt <- createColourExtension colConf
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig = fontConf
                  -- Make sure the scrollbar is always visible.
                , showScrollbar = ShowScrollbarAlways
                }
          }
        `addColourExtension` colExt
  defaultMain termonadConf
