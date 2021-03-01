module Board where

import Clash.Prelude
import Clash.Annotations.TH
import Lib

simEntity
    :: "CLK_40MHZ"    ::: Clock Dom40
    -> "VID_READ"     ::: Signal Dom40 (Maybe (Unsigned 8))
    -> ( "VID_ADDR"   ::: Signal Dom40 (Maybe (Bool, VidAddr))
       , "VID_WRITE"  ::: Signal Dom40 (Maybe (Unsigned 8))
       )
simEntity clk rd = (pure Nothing, rd)

makeTopEntity 'simEntity
