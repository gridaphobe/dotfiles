{-# LANGUAGE OverloadedStrings #-}

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Config.Desktop
import           XMonad.Config.Gnome
import           XMonad.Config.Kde
import           XMonad.Config.Xfce
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.WindowProperties (getProp32s)


import qualified Codec.Binary.UTF8.String     as UTF8
import           Data.Ratio                   ((%))
import qualified DBus                         as D
import qualified DBus.Client                  as D
import           System.IO

myBaseConfig = xfceConfig

myManageHook = composeAll
    [ appName =? "gnus" --> doShift "3:mail"
    , appName =? "erc"  --> doShift "4:irc"
    , kdeOverride --> doFloat
    , isFullscreen --> doFullFloat
    , className =? "Xfce4-notifyd" --> doF W.focusDown <+> doF copyToAll
    , className =? "Xfrun4" --> doFullFloat
    , manageDocks
    ] <+> manageHook myBaseConfig

myLayoutHook = smartBorders $ avoidStruts $ onWorkspace "4:irc" ircLayout $
               baseLayout
  where
    baseLayout = layoutHook myBaseConfig
    ircLayout = Grid

main = do
    xmonad =<< xmobar (myBaseConfig
        { modMask = mod4Mask
        , terminal = "urxvtc -e fish"
        , manageHook = (className =? "krunner") >>= return . not --> myManageHook
        , layoutHook = myLayoutHook
        , workspaces = ["1:web", "2:dev", "3:mail", "4:irc"] ++ map show [5..9]
        , startupHook = setWMName "LG3D"
        } `additionalKeysP`
        [ ("M-p", spawn "dmenu_run -fn '-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1'")
        ])

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt
