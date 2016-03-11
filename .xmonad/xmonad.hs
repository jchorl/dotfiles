import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import Control.Monad (liftM2)
import System.IO
import qualified XMonad.StackSet as W

myWorkspaces = ["1:web", "2:work", "3", "4:music", "5", "6", "7", "8", "9"]
myManageHook = composeAll [
	title =? "ncmpcpp" --> viewShift "4:music",
	isFullscreen             --> doFullFloat
	]
    where viewShift = doF . liftM2 (.) W.greedyView W.shift
myKeys =
	[ ("M-" ++ ws, windows $ W.greedyView ws) | ws <- myWorkspaces ]
        ++
	[ ("M-S-" ++ ws, windows $ W.shift ws) | ws <- myWorkspaces ]
        ++
        [ ("M-c", spawn "ulimit -v 3072000; chromium"),
	("M-m", spawn "xterm -e ncmpcpp"),
        ("M-;", spawn "mpc toggle"),
        ("M-y", spawn "mpc next"),
        ("M-t", spawn "mpc prev"),
	("M-s", spawn "systemctl suspend"),
	("M-S-s", spawn "systemctl poweroff"),
	("M-S-r", spawn "systemctl reboot") ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
        workspaces  = myWorkspaces,
        terminal    = "xterm",
        manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
        layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig,
        logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        },
        modMask = mod4Mask     -- Rebind Mod to the Windows key
    } `additionalKeysP` myKeys
