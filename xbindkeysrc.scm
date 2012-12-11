;;; Media keys

(xbindkey '(XF86AudioRaiseVolume) "pactl set-sink-volume -- 0 +5%")
(xbindkey '(XF86AudioLowerVolume) "pactl set-sink-volume -- 0 -5%")
(xbindkey '(XF86AudioMute) "~/bin/toggle-mute")
;; (xbindkey '(XF86MonBrightnessUp) "light 10+")
;; (xbindkey '(XF86MonBrightnessDown) "light 10-")

;; (xbindkey '(mod4 s) "single-monitor")
;; (xbindkey '(mod4 d) "dual-monitor")
