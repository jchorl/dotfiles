import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO

import qualified XMonad.StackSet as W

myStartupHook :: X ()
myStartupHook = do
	spawn "nm-applet"
	spawn "xloadimage -onroot -fullscreen ~/Pictures/Wallpapers/original.jpg"
	spawn "xset s off"
myWorkspaces = ["1:web", "2:work", "3", "4:music", "5:skype", "6", "7", "8", "9"]
myManageHook = composeAll [
		className =? "Rhythmbox" --> doShift "4:music",
		className =? "Skype"	 --> doShift "5:skype",
                className =? "Gimp"      --> doFloat,
                isFullscreen             --> doFullFloat
               ]
myKeys = 
        [ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
        ++
        [ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
	++
	[ ("M-c", spawn "google-chrome"),
	("M-x", spawn "env PULSE_LATENCY_MSEC=30 skype"),
	("M-m", spawn "rhythmbox"),
	("M-e", spawn "eclipse"),
	("M-n", spawn "nautilus"),
	("M-;", spawn "rhythmbox-client --play-pause"),
	("M-y", spawn "rhythmbox-client --next"),
	("M-t", spawn "rhythmbox-client --previous"),
--	("<XF86AudioMute>", spawn "amixer -q set Master toggle"),
--	("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 2dB+"),
--	("<XF86AudioLowerVolume>", spawn "amixer -q set Master 2dB-"),
	("<XF86AudioMute>", spawn "pacmd set-sink-mute 0 $(if [ $(pacmd dump |grep set-sink-mute | cut -f 3 -d' ') = 'no' ]; then echo yes; else echo no; fi;)"),
	("<XF86AudioRaiseVolume>", spawn "pacmd set-sink-volume 0 $(printf '0x%x' $(( $(pacmd dump|grep set-sink-volume|cut -f3 -d' ') + 0xf00)) )"),
	("<XF86AudioLowerVolume>", spawn "pacmd set-sink-volume 0 $(printf '0x%x' $(( $(pacmd dump|grep set-sink-volume|cut -f3 -d' ') - 0xf00)) )"),
	("M-S-l", spawn "echo -n mem > /sys/power/state"),
	("M-S-s", spawn "dbus-send --system --print-reply --dest=\"org.freedesktop.ConsoleKit\" '\'/org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop"),
	("M-S-r", spawn "dbus-send --system --print-reply --dest=\"org.freedesktop.ConsoleKit\" '\'/org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart") ]

main = do xmproc <- spawnPipe "xmobar"
          xmonad $ defaultConfig {
		    modMask     = mod4Mask,
                    workspaces  = myWorkspaces,
		    terminal    = "urxvt",
		    startupHook = myStartupHook, -- <+> startupHook defaultConfig,
                    manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig,
                    layoutHook  = avoidStruts $ smartBorders $ layoutHook defaultConfig ||| Grid,
		    handleEventHook = fullscreenEventHook,
                    logHook     = dynamicLogWithPP $ xmobarPP {
                        ppOutput = hPutStrLn xmproc,
	                ppTitle = xmobarColor "green" "" . shorten 100
                     }
          } `additionalKeysP` myKeys
