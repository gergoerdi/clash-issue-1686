{-# LANGUAGE NumericUnderscores #-}
module Hardware.Compucolor2.Video
    ( Dom40
    , TextWidth
    , TextHeight
    , FontWidth
    , FontHeight
    , VidAddr
    , plotRom
    ) where

import Clash.Prelude
import Hardware.Compucolor2.Video.Plot
import qualified Language.Haskell.TH.Syntax as TH

type TextWidth = 64
type TextHeight = 32
type TextSize = TextWidth * TextHeight
type VidSize = TextSize * 2
type VidAddr = Index VidSize

-- | 40 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom40", vPeriod = hzToPeriod 40_000_000}

plotRom
    :: (HiddenClockResetEnable dom)
    => Signal dom (Unsigned 8)
    -> Signal dom (Index FontHeight)
    -> Signal dom (Unsigned 8)
plotRom char row = stretchRow <$> col1 <*> col2
  where
    (char2, char1) = unbundle . fmap bitCoerce $ char

    col1 = rom $(TH.lift plots) $ toAddr <$> char1 <*> row
    col2 = rom $(TH.lift plots) $ toAddr <$> char2 <*> row

    toAddr :: Unsigned 4 -> Index 8 -> Unsigned (4 + CLog 2 (FontHeight `Div` 2))
    toAddr char row = bitCoerce (char, row')
      where
        (row', _) = bitCoerce @_ @(Index (FontHeight `Div` 2), Bit) row
