import XMonad

import XMonad.Actions.Warp
import XMonad.Actions.CycleWS
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
        myTabbed = named "Tabs" (simpleTabbedAlways)
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
dzenStatusBar = "dzen2 -fn cure -h 11 -ta l -w 200 -x 0 -tw 200 -p" 
myPP h = defaultPP { ppOutput = hPutStrLn h
                   , ppSep    = "  |  "
                   , ppLayout = id 
      	           , ppTitle  = (\_->"") }



-- Key bindings, readably formatted for additionalKeysP in EZConfig
myKeyMapping = [ 
          -- File Browser:
          ("M-v", spawn "rox ~")
          , ("M-S-v d", spawn "rox ~/Downloads")
          , ("M-S-v v", spawn "rox /media/usb")

          -- Terminal: Either scratchpad, persistent terminal, screen, conf, wifi
          , ("M-x", inTerm "tmux new -As scratch")
          , ("M-d", spawn myTerminal)
          , ("M-u", spawn "bashrun2")
          , ("M-S-p", AL.launchApp myXPConfig $ "~/scripts/term_in_dir.sh")
          , ("M-S-u", inTerm "vim ~/dotfiles/xmonad.hs") 
          , ("M-S-w", inTerm "sudo wifi-select") 

          -- Some random launchers
          , ("M-p m", spawn "mendeleydesktop --force-bundled-qt")
          , ("M-p l", spawn "slock")
          , ("M-p i", inTerm "~/scripts/ssh_to_mancer.sh")
          , ("M-S-f f", spawn "chromium --incognito")
          , ("M-S-f p", spawn "~/scripts/proxy-browse.sh http://scholar.google.com") 
          , ("M-p w", spawn "virtualbox --startvm WinXP")

          -- Take notes: either regular notes, or research notes (which I haven't done in ages :-p)
          , ("M-e n", spawn "~/scripts/mknotes.sh ~/ref/notes")
          , ("M-S-e n", inTerm "~/scripts/catnotes.sh ~/ref/notes")
          , ("M-C-e n", inTerm "vim ~/ref/notes/$(ls --sort=time ~/ref/notes | head -n1)")
          , ("M-e r", spawn "~/scripts/mknotes.sh ~/re/notes")
          , ("M-S-e r", inTerm "~/scripts/catnotes.sh ~/re/notes")
          , ("M-C-e r", inTerm "vim ~/re/notes/$(ls --sort=time ~/re/notes | head -n1)")

          -- Prompts: Search, web browse, or edit 
          , ("M-g", S.promptSearch myXPConfig S.google) 
          , ("M-f", AL.launchApp myXPConfig $ "chromium") 
          , ("M-e e", AL.launchApp myXPConfig $ myTerminalEx "vim ")

          -- Refresh statusbar if I want
          , ("M-r", spawn "/bin/bash ~/scripts/go_status.sh")

          -- Window management
          , ("M-o", windows W.focusMaster) 
          , ("M-S-o", windows W.swapMaster) 
          , ("M-i", sendMessage NextLayout) 
          , ("M-n", nextScreen)
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


