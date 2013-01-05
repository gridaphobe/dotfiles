{-# LANGUAGE OverloadedStrings #-}

import           XMonad
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.Warp
import           XMonad.Actions.WindowBringer
import           XMonad.Actions.WindowNavigation
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
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ToggleLayouts
import           XMonad.Prompt
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.WindowProperties (getProp32s)


import qualified Codec.Binary.UTF8.String     as UTF8
import           Data.List                    (isPrefixOf)
import           Data.Ratio                   ((%))
import           System.IO


myBaseConfig = desktopConfig


myManageHook = composeAll
    [ appName =? "gnus" --> doShift "3:mail"
    , appName `startsWith` "erc"  --> doShift "4:irc"
    , kdeOverride --> doFloat
    , isFullscreen --> doFullFloat
    , className =? "knotify4" --> doFloat
    , className =? "Kmix" --> doIgnore
    , className =? "Xfce4-notifyd" --> doF W.focusDown <+> doF copyToAll
    , className =? "Xfrun4" --> doFullFloat
    , manageDocks
    ] <+> manageHook myBaseConfig


myLayoutHook = smartBorders
             $ spacing 2
             $ avoidStruts
             $ onWorkspace "4:irc" ircLayout
             $ baseLayout
  where
    baseLayout = toggleLayouts Full
               $ tiled ||| Mirror tiled
    tiled = ResizableTall 1 (3/100) (1/2) []
        --layoutHook myBaseConfig
    ircLayout = Grid

startsWith :: Query String -> String -> Query Bool
q `startsWith` x = fmap (x `isPrefixOf`) q


myFont = "-*-terminus-medium-*-*-*-14-*-*-*-*-*-iso10646-1"


myKeys =
    [ ("M-p",   spawn $ "dmenu_run -fn '" ++ myFont ++ "'")
    , ("M-f",   sendMessage $ Toggle "Full")
    , ("M-S-b", banishScreen LowerLeft)
    , ("M-o",   sendMessage Expand)
    , ("M-S-o", sendMessage MirrorExpand)
    , ("M-u",   sendMessage Shrink)
    , ("M-S-u", sendMessage MirrorShrink)
    , ("M-;",   xmonadPrompt myXPConfig)
    , ("M-g",   windowPromptGoto myXPConfig)
      -- , ("M-g", gotoMenu)
    -- , ("M-<L>", sendMessage $ Go L)
    -- , ("M-<D>", sendMessage $ Go D)
    -- , ("M-<R>", sendMessage $ Go R)
    -- , ("M-<U>", sendMessage $ Go U)
    ]


myXPConfig =
    defaultXPConfig
      { font = myFont
      , position = Top
      }

myConfig =
    myBaseConfig
      { modMask = mod4Mask
      , terminal = "urxvtc -e fish"
      , manageHook = (className =? "krunner") >>= return . not --> myManageHook
      , layoutHook = myLayoutHook
      , workspaces = ["1:web", "2:dev", "3:mail", "4:irc"] ++ map show [5..9]
      , borderWidth = 2
      , normalBorderColor = "#3f3f3f"
      , focusedBorderColor = "#ee0000"
      , startupHook = setWMName "LG3D"
      } `additionalKeysP` myKeys


main = do
    spawn "tint2"
    config <- withWindowNavigation (xK_i, xK_j, xK_k, xK_l) $ myConfig
    xmonad =<< xmobar config

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt
