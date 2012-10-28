import           XMonad
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Util.EZConfig

main = xmonad =<< xmobar myConfig

myConfig = ewmh desktopConfig
    { modMask = mod4Mask
    , terminal = "urxvtc -e fish"
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = avoidStruts  $  layoutHook desktopConfig
    , workspaces = ["1:web", "2:dev", "3:mail", "4:irc"] ++ map show [5..9]
    }

myManageHook = composeAll
    [ className =? "gnus" --> doShift "3:mail"
    , className =? "erc"  --> doShift "4:irc"
    , manageDocks
    ]
