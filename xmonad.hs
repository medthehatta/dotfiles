import XMonad

import XMonad.Actions.Warp
import qualified XMonad.Actions.Search as S
import XMonad.Actions.TagWindows

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

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



-- Layouts
myLayout = avoidStruts $ smartBorders $ myTiled ||| myTabbed
    where
        myTiled = named "Tall" (ResizableTall 1 (3/100) (1/2) [])
	myTabbed = named "Tabs" (simpleTabbed)



-- Manage hook allowing for automatic dzen gap handling 
myManageHook = composeAll $ [manageDocks, manageHook defaultConfig] 



-- XPConfig styles the prompts in XMonad.Prompt
myXPConfig = defaultXPConfig { position = Top }



-- Configure the dzen statusbar:
--  How dzen should be run, and how the loghook pretty printer should work
dzenStatusBar = "dzen2 -fn -*-cure-*-*-*-*-*-*-*-*-*-*-*-* -h 11 -ta l -w 150 -x 0 -tw 100 -p" 
myPP h = defaultPP { ppOutput = hPutStrLn h
                   , ppSep    = "  |  "
                   , ppLayout = id 
	           , ppTitle  = (\_->"") }




-- Set up the workspace names:
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]




-- Key bindings, readably formatted for additionalKeysP in EZConfig
myKeyMapping = [ 
	       -- Launcher: 
	       ("M-S-p", AL.launchApp myXPConfig $ myTerminalEx "screen -U -d -R -S ")
	       , ("M-v", spawn "rox ~")
	       , ("M-S-v", spawn "rox ~/Downloads")

               -- Terminal: Either scratchpad, persistent terminal, screen, conf, wifi, seedbox login
	       , ("M-S-x", spawn myTerminal) 
	       , ("M-x", inTerm "screen -S scratchpad -U -d -R") 
	       , ("M-u", inTerm "screen -c ~/dotfiles/general-scrc -S general -U -d -R") 
	       , ("M-S-u", inTerm "vim ~/dotfiles/xmonad.hs") 
	       , ("M-w", inTerm "sudo wifi-select wlan0") 
	       , ("M-S-w", inTerm "sudo netcfg -a")

	       -- Prompts: Search, manpages, web browse, or edit 
	       , ("M-g", S.promptSearch myXPConfig S.google) 
	       , ("M-S-g", S.selectSearch S.google)
	       , ("M-<F1>", manPrompt myXPConfig) 
	       , ("M-d", AL.launchApp myXPConfig $ "surfraw -browser=chromium")
	       , ("M-f", AL.launchApp myXPConfig $ "chromium") 
	       , ("M-S-f", AL.launchApp myXPConfig $ "chromium --incognito") 
	       , ("M-e", AL.launchApp myXPConfig $ myTerminalEx "vim") 
	       , ("M-S-e", AL.launchApp myXPConfig $ myTerminalEx "sudo vim") 
	       -- Tags:
	       , ("M-t", tagPrompt myXPConfig (\s -> focusUpTaggedGlobal s))  --raise
	       , ("M-S-t", tagPrompt myXPConfig (\s -> withFocused (addTag s)))

	       -- Refresh statusbar if I want
	       , ("M-r", spawn "/bin/bash ~/scripts/go_status.sh")
	       , ("M-S-r", inTerm "sudo ntpdate tick.ucla.edu")

	       -- XMMS2
	       , ("M-b l", spawn "nyxmms2 next")
	       , ("M-b h", spawn "nyxmms2 prev")
	       , ("M-b j", spawn "nyxmms2 toggle")
	       , ("M-b k", spawn "nyxmms2 stop")
	       , ("M-b d", inTerm "/usr/bin/python2 ~/scripts/gsdl.py")

	       -- Window management
	       , ("M-o", windows W.focusMaster) 
	       , ("M-S-o", windows W.swapMaster) 
	       , ("M-i", sendMessage NextLayout) 
	       , ("M-s", withFocused $ windows . W.sink) 
	       , ("M-c", kill) 
	       , ("M-m", banish LowerRight) -- ratpoison-like cursor banish
	       ]
	       ++
	       [ (prefixMasks ++ "M-" ++ [key], action tag)
	       		| (tag, key) <- zip myWorkspaces "123456789"
			, (prefixMasks, action) <- [ ("", windows . W.view)
						   , ("S-", windows . W.shift) ]
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



