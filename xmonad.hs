import XMonad

import XMonad.Actions.Warp
import qualified XMonad.Actions.Search as S
import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Scratchpad

import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed

import Data.Maybe(fromMaybe)

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.AppLauncher as AL

import qualified XMonad.StackSet as W

import System.IO
import Control.Monad



-- Set up the two basic bits: modifier key and terminal
myModMask = mod4Mask
myTerminal = "urxvt"
-- shortcut for constructing the command for opening a program in the terminal
myTerminalEx cmd = myTerminal ++ " -e " ++ cmd
inTerm cmd = spawn $ myTerminalEx cmd



-- I only use two layouts: the regular tiling layout, and the tabbed layout
myLayout = avoidStruts $ smartBorders $ myTiled ||| myTabbed
    where
        myTiled = named "Tall" (ResizableTall 1 (3/100) (1/2) [])
	myTabbed = named "Tabs" (simpleTabbed)



-- Manage hook allowing for automatic dzen gap handling and scratchpad handling
myManageHook = composeAll $ [manageDocks, myScratchpadManageHook, manageHook defaultConfig] 



-- XPConfig styles the prompts in XMonad.Prompt
myXPConfig = defaultXPConfig { position = Top }



-- Configure the dzen statusbar:
--  How dzen should be run, and how the loghook pretty printer should work
dzenStatusBar = "dzen2 -fn -*-cure-*-*-*-*-*-*-*-*-*-*-*-* -h 11 -ta l -w 150 -x 0 -tw 100 -p" 
myPP h = defaultPP { ppOutput = hPutStrLn h
                   , ppSep    = "  |  "
                   , ppLayout = id 
		   , ppSort = return scratchpadFilterOutWorkspace
	           , ppTitle  = (\_->"") }



-- Scratchpad is a popup terminal that you can banish and call back
--  Here we configure its default window geometry
myScratchpadManageHook = scratchpadManageHook (W.RationalRect 0.2 0.2 0.5 0.5)



-- Key bindings, readably formatted for additionalKeysP in EZConfig
myKeyMapping = [ 
	       -- Launcher: 
	       ("M-p", spawn "dmenu")

               -- Terminal: Either scratchpad, persistent terminal, screen, conf, volume, wifi, seedbox login
	       , ("M-S-x", spawn myTerminal) 
	       , ("M-x", scratchpadSpawnActionTerminal myTerminal) 
	       , ("M-u", inTerm "screen -U -d -R") 
	       , ("M-S-u", inTerm "vim ~/dotfiles/xmonad.hs") 
	       , ("M-v", inTerm "alsamixer") 
	       , ("M-w", inTerm "sudo wifi-select wlan0") 
	       , ("M-S-s", inTerm "ssh -t goliath@jupiterbrains -p24 screen -r")

	       -- Prompts: Search, manpages, web browse, or edit 
	       , ("M-g", S.promptSearch myXPConfig S.google) 
	       , ("M-S-g", S.selectSearch S.google)
	       , ("M-d", AL.launchApp myXPConfig $ "surfraw ")
	       , ("M-<F1>", manPrompt myXPConfig) 
	       , ("M-f", AL.launchApp myXPConfig $ "vimprobable2") 
	       , ("M-e", AL.launchApp myXPConfig $ myTerminalEx "vim") 
	       , ("M-S-e", AL.launchApp myXPConfig $ myTerminalEx "sudo vim") 
	       -- Tags:
	       , ("M-t", tagPrompt myXPConfig (\s -> focusUpTaggedGlobal s))  --raise
	       , ("M-S-t", tagPrompt myXPConfig (\s -> withFocused (addTag s)))

	       -- Refresh statusbar if I want
	       , ("M-r", spawn "/bin/bash ~/scripts/go_status.sh")
	       -- Sort unsorted downloads
	       , ("M-S-d", spawn "python ~/scripts/sort-downloaded.py")

	       -- Window management
	       , ("M-o", windows W.focusMaster) 
	       , ("M-S-o", windows W.swapMaster) 
	       , ("M-i", sendMessage NextLayout) 
	       , ("M-s", withFocused $ windows . W.sink) 
	       , ("M-c", kill) 
	       , ("M-m", banish LowerRight) -- ratpoison-like cursor banish
	       ]



-- Fix the settings
myXMonadConfig h = defaultConfig
    { modMask = myModMask
    , terminal = myTerminal
    , layoutHook = myLayout
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#00FF00"
    , manageHook = myManageHook
    , logHook = dynamicLogWithPP $ myPP h
    } `additionalKeysP` myKeyMapping
    


-- Spawn the pipe that reads in to the dzen statusbar, then apply these settings
main = do
h <- spawnPipe dzenStatusBar
xmonad $ myXMonadConfig h



