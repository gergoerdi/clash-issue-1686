{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.Compucolor2
    ( simEntity
    , mainBoard
    , simBoard

    , VidAddr
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Compucolor2.TMS5501 as TMS5501
import Hardware.Compucolor2.CRT5027 as CRT5027
import Hardware.Compucolor2.Keyboard
import Hardware.Compucolor2.Video
import Hardware.Compucolor2.FloppyDrive
import Hardware.Intel8080.CPU

import RetroClash.Utils
import RetroClash.Memory
import RetroClash.Barbies

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom (Maybe (Bool, VidAddr))
       , Signal dom (Maybe (Unsigned 8))
       )
mainBoard vidRead = (vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    pause = pure False
    interruptRequest = pure False

    (dataIn, (vidAddr, vidWrite)) =
        $(memoryMap @(Either (Unsigned 8) (Unsigned 16)) [|_addrOut|] [|_dataOut|] $ do
            rom <- romFromFile (SNat @0x4000) [|"_build/v678.rom.bin"|]
            ram <- ram0 (SNat @0x4000)
            (vid, vidAddr, vidWrite) <- conduit @(Bool, VidAddr) [|vidRead|]

            matchRight $ do
                from 0x0000 $ connect rom
                from 0x6000 $ tag True $ connect vid
                from 0x7000 $ tag False $ connect vid
                from 0x8000 $ connect ram

            return (vidAddr, vidWrite))

simBoard
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => "VID_READ"    ::: Signal dom (Maybe (Unsigned 8))
    -> ( "VID_ADDR"  ::: Signal dom (Maybe (Bool, VidAddr))
       , "VID_WRITE" ::: Signal dom (Maybe (Unsigned 8))
       )
simBoard vidRead = (vidAddr, vidWrite)
  where
    (vidAddr, vidWrite) = mainBoard vidRead

simEntity
    :: "CLK_40MHZ"    ::: Clock Dom40
    -> "VID_READ"     ::: Signal Dom40 (Maybe (Unsigned 8))
    -> ( "VID_ADDR"   ::: Signal Dom40 (Maybe (Bool, VidAddr))
       , "VID_WRITE"  ::: Signal Dom40 (Maybe (Unsigned 8))
       )
simEntity = withResetEnableGen simBoard

makeTopEntity 'simEntity
