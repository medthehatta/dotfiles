import XMonad

import XMonad.Actions.Warp
import qualified XMonad.Actions.Search as S

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)

import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.ResizableTile

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL

import qualified XMonad.StackSet as W

import System.IO
import Control.Monad



-- Set up the basic bits: modifier key, terminal
myModMask = mod4Mask
myTerminal = "urxvt"
-- shortcut for constructing the command for opening a program in the terminal
-- (vanilla term)
myTerminalEx cmd = myTerminal ++ " -e " ++ cmd 
inTerm cmd = spawn $ myTerminalEx cmd
-- (named screen)
myNamedScreenEx name cmd = myTerminalEx "screen -U -d -R -S " ++ name ++ " " ++ cmd
inNamedScreen name cmd = spawn $ myNamedScreenEx name cmd 



-- Layouts
myLayout = avoidStruts $ smartBorders $ myTiled ||| myTabbed ||| myFull
    where
        myTiled = named "Tall" (ResizableTall 1 (3/100) (1/2) [])
        myTabbed = named "Tabs" (simpleTabbed)
        myFull = named "Full" (Full)



-- Manage hook allowing for automatic dzen gap handling 
myManageHook = composeAll
  [ resource =? "bashrun2-run-dialog" --> doFloat 
  , manageDocks 
  ] <+> manageHook defaultConfig



-- XPConfig styles the prompts in XMonad.Prompt
myXPConfig = defaultXPConfig { position = Top }



-- Configure the dzen statusbar:
--  How dzen should be run, and how the loghook pretty printer should work
dzenStatusBar = "dzen2 -fn -*-cure-*-*-*-*-*-*-*-*-*-*-*-* -h 11 -ta l -w 150 -x 0 -tw 100 -p" 
myPP h = defaultPP { ppOutput = hPutStrLn h
                   , ppSep    = "  |  "
                   , ppLayout = id 
      	           , ppTitle  = (\_->"") }



-- Key bindings, readably formatted for additionalKeysP in EZConfig
myKeyMapping = [ 
          -- File Browser:
          ("M-v", spawn "rox ~")
          , ("M-S-v", spawn "rox ~/Downloads")

          -- Terminal: Either scratchpad, persistent terminal, screen, conf, wifi
          , ("M-x", inNamedScreen "scratchpad" "")
          , ("M-d", spawn myTerminal)
          , ("M-u", spawn "bashrun2")
          , ("M-S-p", AL.launchApp myXPConfig $ "~/scripts/term_in_dir.sh")
          , ("M-S-u", inTerm "vim ~/dotfiles/xmonad.hs") 
          , ("M-w", inTerm "sudo wifi-select") 

          -- Some random launchers
          , ("M-p m", spawn "mendeleydesktop --force-bundled-qt")
          , ("M-p l", spawn "slock")
          , ("M-S-f f", spawn "chromium --incognito")
          , ("M-S-f p", spawn "~/scripts/proxy-browse.sh http://scholar.google.com") 
          , ("M-e n", spawn "~/scripts/mknotes.sh ~/ref/notes")
          , ("M-S-e n", inTerm "~/scripts/catnotes.sh ~/ref/notes")
          , ("M-C-e n", inTerm "vim ~/ref/notes/$(ls --sort=time ~/ref/notes | head -n1)")
          , ("M-e r", spawn "~/scripts/mknotes.sh ~/re/notes")
          , ("M-S-e r", inTerm "~/scripts/catnotes.sh ~/re/notes")
          , ("M-C-e r", inTerm "vim ~/re/notes/$(ls --sort=time ~/re/notes | head -n1)")
          , ("M-e t", inTerm "vim `mktemp` ")

          -- Prompts: Search, web browse, or edit 
          , ("M-g", S.promptSearch myXPConfig S.google) 
          , ("M-S-g", S.selectSearch S.google)
          , ("M-f", AL.launchApp myXPConfig $ "chromium") 
          , ("M-e e", AL.launchApp myXPConfig $ myTerminalEx "vim ")
          , ("M-S-e e", AL.launchApp myXPConfig $ myTerminalEx "sudo vim -u /home/med/.vimrc")

          -- Refresh statusbar if I want
          , ("M-r", spawn "/bin/bash ~/scripts/go_status.sh")

          -- XMMS2
          , ("M-b l", spawn "nyxmms2 next")
          , ("M-b h", spawn "nyxmms2 prev")
          , ("M-b j", spawn "nyxmms2 toggle")
          , ("M-b k", spawn "nyxmms2 stop")

          -- Window management
          , ("M-o", windows W.focusMaster) 
          , ("M-S-o", windows W.swapMaster) 
          , ("M-i", sendMessage NextLayout) 
          , ("M-s", withFocused $ windows . W.sink) 
          , ("M-c", kill) 
          , ("M-m", banish LowerRight) -- ratpoison-like cursor banish
          , ("M-p s", sendMessage ToggleStruts) -- toggle struts
	       ] ++
         [ (shiftMask ++ "M-" ++ key, action key) | key <- (map show [1..9]), (shiftMask, action) <- [ ("", windows . W.view), ("S-", windows . W.shift) ] ]



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


