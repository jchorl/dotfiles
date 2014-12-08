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
myStartupHook = return()
myWorkspaces = ["1:web", "2:work", "3", "4:music", "5", "6", "7", "8", "9"]
myManageHook = composeAll [
	className =? "Rhythmbox" --> doShift "4:music",
	isFullscreen             --> doFullFloat
	]
myKeys = 
	[ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
	++
	[ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
	++
	[ ("M-c", spawn "google-chrome"),
	("M-m", spawn "rhythmbox"),
	("M-n", spawn "nautilus"),
	("M-;", spawn "rhythmbox-client --play-pause"),
	("M-y", spawn "rhythmbox-client --next"),
	("M-t", spawn "rhythmbox-client --previous"),
	("M-S-s", spawn "dbus-send --system --print-reply --dest=\"org.freedesktop.ConsoleKit\" '\'/org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop"),
	("M-S-l", spawn "dbus-send --system --print-reply --dest=\"org.freedesktop.UPower\" '\'/org/freedesktop/UPower org.freedesktop.UPower.Suspend"),
	("M-S-r", spawn "dbus-send --system --print-reply --dest=\"org.freedesktop.ConsoleKit\" '\'/org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart") ]

main = do xmproc <- spawnPipe "xmobar"
	  xmonad $ defaultConfig {
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
