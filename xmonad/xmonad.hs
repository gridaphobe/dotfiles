import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig
import           XMonad.Util.Run

import           System.IO


main = do
    xmproc <- spawnPipe "xmobar -x 0"
    -- xmproc' <- spawnPipe "xmobar -x 1"
    xmonad $ ewmh desktopConfig
        { modMask = mod4Mask
        , terminal = "urxvtc -e fish"
        , manageHook = myManageHook <+> manageHook desktopConfig
        , layoutHook = smartBorders $ avoidStruts $ layoutHook desktopConfig
        , logHook = dynamicLogWithPP $ xmobarPP
                    { ppOutput = \o -> hPutStrLn xmproc o -- >> hPutStrLn xmproc' o
                    , ppTitle = xmobarColor "green" "" . shorten 50
                    }
        , workspaces = ["1:web", "2:dev", "3:mail", "4:irc"] ++ map show [5..9]
        }

myManageHook = composeAll
    [ className =? "gnus" --> doShift "3:mail"
    , className =? "erc"  --> doShift "4:irc"
    , isFullscreen --> doFullFloat
    , manageDocks
    ]
