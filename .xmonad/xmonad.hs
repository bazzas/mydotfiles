------------------------------------------------------------------------
-- import
------------------------------------------------------------------------

import XMonad hiding ( (|||) ) -- jump to layout
import XMonad.Layout.LayoutCombinators (JumpToLayout(..), (|||)) -- jump to layout
import XMonad.Config.Desktop
import System.Exit
import qualified XMonad.StackSet as W
import Control.Monad (liftM2)

-- data
import Data.Char (isSpace)
import Data.List
import Data.Monoid
import Data.Maybe (isJust)
import Data.Ratio ((%)) -- for video
import qualified Data.Map as M

-- system
import System.IO (hPutStrLn) -- for xmobar

-- util
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare
import XMonad.Util.FixedWorkspaces -- custom lib

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.EwmhDesktops -- to show workspaces in application switchers
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat, doRectFloat)
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicProperty

-- actions
import XMonad.Actions.CopyWindow -- for dwm window style tagging
import XMonad.Actions.UpdatePointer -- update mouse postion
import XMonad.Actions.CycleWS

-- layout
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.GridVariants
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.PerWorkspace

-- regex
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

------------------------------------------------------------------------
-- variables
------------------------------------------------------------------------

myModMask = mod4Mask -- Sets modkey to super/windows key
myModKey = "super"
myTerminal = "st" -- Sets default terminal
myBorderWidth = 2 -- Sets border width for windows
myNormalBorderColor = "#7c6f64"
myFocusedBorderColor = "#d65d0e"
myppCurrent = "#cb4b16"
myppVisible = "#cb4b16"
myppHidden = "#268bd2"
myppHiddenNoWindows = "#93A1A1"
myppTitle = "#FDF6E3"
myppUrgent = "#DC322F"


myWorkspaces :: [String]
myWorkspaces = show <$> [1..9] ++ [0]

-- myWorkspaceScreens :: String -> Int
-- myWorkspaceScreens ws = abs $ mod (read ws :: Int) 2 - 1 -- all odd numbers are shown on 0 monitor

myWorkspaceScreens ws
    | head ws >= '7' = 1
    | head ws == '0' = 1
    | head ws == '2' = 1
    | otherwise = 0 -- 1, 3-6 is on left monitor

windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


------------------------------------------------------------------------
-- LogHook to make workspaces in xmobar clickable
------------------------------------------------------------------------
-- myWorkspaces :: [String]
-- myWorkspaces =
--     "1: \xf02d " :
--     "2: \xf269 " :
--     "3" :
--     "4: \xf07b " :
--     "5" :
--     "6: \xf09b " :
--     "7" :
--     "8: \xf269 " :
--     "9: \xf1d8 " :
--     "0: \xf025 " :
--     []
showWorkspace :: String -> String
showWorkspace "1" = "1: \xf1c9" -- text editor
showWorkspace "2" = "2: \xf269" -- firefox
showWorkspace "3" = "3: \xf09b" -- git
showWorkspace "4" = "4: \xf07b" -- files manager
showWorkspace "7" = "7: \xf03d" -- video conference
showWorkspace "9" = "9: \xf1d8" -- telegram (\xf198 - slack)
showWorkspace ws = ws

myLogHook xmproc0 xmproc1  = do
    dynamicLogWithPP xmobarPP {
        ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
        , ppCurrent = xmobarColor myppCurrent "" . clickWorkspace "[" "] "
        , ppVisible = xmobarColor myppVisible "" . showWorkspace
        , ppHidden = xmobarColor myppHidden "" . clickWorkspace "" " "
        , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows "" . clickWorkspace "" " "
        , ppTitle = xmobarColor  myppTitle "" . shorten 80
        , ppSep =  "<fc=#586E75> | </fc>"
        , ppUrgent = xmobarColor  myppUrgent "" . clickWorkspace "!" "!"
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        , ppSort = fmap (namedScratchpadFilterOutWorkspace.) (ppSort def)
        , ppExtras  = [windowCount]
    } >> updatePointer (0.25, 0.25) (0.25, 0.25)
    where
        clickWorkspace a b ws = "<action=xdotool key " ++ myModKey ++ "+" ++ show index ++ ">" ++ a ++ showWorkspace ws ++ b ++ "</action>" where
            wsIdxToString Nothing = "1"
            wsIdxToString (Just n) = show $ mod (n+1) $ (length myWorkspaces) -- if you use 10 ws and 10 is bind to 0 remove the +1
            index = wsIdxToString (elemIndex ws myWorkspaces)

------------------------------------------------------------------------
-- desktop notifications -- dunst package required
------------------------------------------------------------------------

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook = do
      spawnOnce "firefox"
      spawnOnce "subl"
      -- set layout switcher on caps lock
      spawnOnce "setxkbmap -layout 'us,ru' -option 'grp:caps_toggle'"
      --By default xmonad doesn't set a particular X cursor, which usually means the default X cursor will be used by
      -- the system. To set your own custom cursor, use the xsetroot program
      spawnOnce "xsetroot -cursor_name left_ptr"
      -- restore wallpapers
      spawnOnce "nitrogen --restore &"
      spawnOnce "compton &"
      -- tray icons
      spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 20 &"
      spawnOnce "/usr/bin/bash -c 'sleep 5; /usr/bin/xmodmap /home/serhii/.Xmodmap'" -- delay the execution so the xmodmap changes are not overwritten by setxkbmap.


------------------------------------------------------------------------
-- layout
------------------------------------------------------------------------

myLayout = onWorkspace "2" (avoidStruts (full ||| tiled) ||| full) $ avoidStruts (tiled ||| full ||| grid) ||| full
  where
     -- full
     full = renamed [Replace "Full"]
          $ noBorders (Full)

     -- tiled
     tiled = renamed [Replace "Tall"]
           $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True
           $ ResizableTall 1 (3/100) (1/2) []
    -- myTiled
     myTiled = renamed [Replace "Tall"]
           $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True
           $ ResizableTall 1 (3/100) (4/5) []

     -- grid
     grid = renamed [Replace "Grid"]
          $ spacingRaw True (Border 10 0 10 0) True (Border 0 10 0 10) True
          $ Grid (16/10)

     -- bsp
     bsp = renamed [Replace "BSP"]
         $ emptyBSP

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

myManageHook = insertPosition Below Newer <+> (composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [isFullscreen --> doRectFloat (W.RationalRect 0 0 1 1)]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 0) | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 1) | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 6) | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo (myWorkspaces !! 8) | x <- my9Shifts]
    ]) <+> namedScratchpadManageHook myScratchpads
    -- [className =? "Firefox" --> doShift ( myWorkspaces !! 1 )
    -- , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
    -- , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat -- firefox pip
    -- , className =? "Sublime_text"     --> doShift ( myWorkspaces !! 2 )
    -- , resource  =? "desktop_window" --> doIgnore
    -- , resource  =? "kdesktop"       --> doIgnore
    -- , isFullscreen --> doFullFloat
    -- ]
    where
        doShiftAndGo = doF . liftM2 (.) W.view W.shift
        myRFloats = ["Dialog", "Tookkit"]
        myIgnores = ["desktop_window", "kdesktop"]
        my1Shifts = ["Sublime_text"]
        my2Shifts = ["Firefox", "Chromium-browser", "Vivaldi-stable"]
        my7Shifts = ["Microsoft Teams - Preview"]
        my9Shifts = ["TelegramDesktop", "Slack"]

-----------------------------------------------------------------------
-- Handle event hook
-----------------------------------------------------------------------
myHandleEventHook :: Event -> X All
myHandleEventHook = dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
    where floating  = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
------------------------------------------------------------------------

myKeys =
    -- [("M-" ++ m ++ k, windows $ f i)
    --     | (i, k) <- zip (myWorkspaces) (map show [1 :: Int ..])
    --     , (f, m) <- [(W.view, ""), (W.shift, "S-"), (copy, "S-C-")]]
    -- ++
    [("S-C-a", windows copyToAll)   -- copy window to all workspaces
     , ("S-C-z", killAllOtherCopies)  -- kill copies of window on other workspaces
     , ("M-a", sendMessage MirrorExpand)
     , ("M-z", sendMessage MirrorShrink)
     , ("M-s", sendMessage ToggleStruts)
     , ("M-f", sendMessage $ JumpToLayout "Full")
     , ("M-t", sendMessage $ JumpToLayout "Tall")
     , ("M-g", sendMessage $ JumpToLayout "Grid")
     -- , ("M-b", sendMessage $ JumpToLayout "BSP")
     -- , ("M-d", spawn "rofi -show drun -show-icons -theme dmenu") -- rofi
     , ("M-d", spawn "j4-dmenu-desktop") -- dmenu https://github.com/enkore/j4-dmenu-desktop
     , ("S-M-l", spawn "slock") -- lock the screen
     , ("S-M-t", withFocused $ windows . W.sink) -- flatten floating window to tiled
     , ("S-M-f", withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f) -- full float window
     , ("M-C-<Space>", namedScratchpadAction myScratchpads "terminal")
     , ("M-`", namedScratchpadAction myScratchpads "spotify")
     , ("M-<Return>", spawn myTerminal)
     -- Move focused window to next/prev monitor
     , ("M-S-.", shiftNextScreen)
     , ("M-S-,", shiftPrevScreen)

     -- Move focuse to next/prev monitor
     , ("M-.", nextScreen)
     , ("M-,", prevScreen)
     -- Restart xmonad
     , ("M-q", spawn "killall xmobar;  xmonad --recompile; xmonad --restart")
     -- volume controls
     , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
     , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
     , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")

     -- player controls (Spotify). Need to istall playerctl utility
     , ("<XF86AudioPlay>", spawn "playerctl play-pause")
     , ("<XF86AudioPrev>", spawn "playerctl previous")
     , ("<XF86AudioNext>", spawn "playerctl next")

     -- make a screenshot
     , ("<Print>", spawn "scrot '%Y-%m-%d_$wx$h.png' -e 'mv $f ~/Pictures/Screenshots/'")
     -- kill application
     , ("M-S-q", kill)
     , ("M-S-c", io exitSuccess)
    ]
    ++ [ ("M-" ++ ws,   windows $ fixedView myWorkspaceScreens ws) | ws <-  myWorkspaces]
    ++ [ ("M-S-" ++ ws, windows $ W.shift ws)                     | ws <-  myWorkspaces]

------------------------------------------------------------------------
-- scratchpads
------------------------------------------------------------------------
myScratchpads :: [NamedScratchpad]
myScratchpads = [ NS "spotify" "spotify" (className =? "Spotify") defaultFloating
                , NS "terminal" spawnTerm findTerm manageTerm
                ]
                where
                    spawnTerm  = myTerminal ++ " -n scratchpad" ++ " -e tmux new-session \\; split-window -h \\; split-window -v \\; attach"
                    findTerm   = resource =? "scratchpad"
                    manageTerm = customFloating $ W.RationalRect l t w h
                               where
                                 h = 0.9
                                 w = 0.9
                                 t = 0.95 -h
                                 l = 0.95 -w


------------------------------------------------------------------------
-- main
------------------------------------------------------------------------

main = do
    xmproc0 <- spawnPipe "/usr/bin/xmobar -x 0 /home/serhii/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "/usr/bin/xmobar -x 1 /home/serhii/.config/xmobar/xmobarrc"
    xmonad $ ewmh def
        { manageHook = myManageHook <+> manageDocks
        , startupHook        = myStartupHook
        , layoutHook         = myLayout
        , handleEventHook    = handleEventHook desktopConfig <+> myHandleEventHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , terminal           = myTerminal
        , modMask            = myModMask
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , logHook            = myLogHook xmproc0 xmproc1
        -- , logHook = dynamicLogWithPP xmobarPP
        --                 { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
        --                 , ppCurrent = xmobarColor myppCurrent "" . clickWorkspace "[" "]" -- Current workspace in xmobar
        --                 , ppVisible = xmobarColor myppVisible ""                -- Visible but not current workspace
        --                 , ppHidden = xmobarColor myppHidden "" . clickWorkspace "+" ""   -- Hidden workspaces in xmobar
        --                 , ppHiddenNoWindows = xmobarColor  myppHiddenNoWindows ""        -- Hidden workspaces (no windows)
        --                 , ppTitle = xmobarColor  myppTitle "" . shorten 80     -- Title of active window in xmobar
        --                 , ppSep =  "<fc=#586E75> | </fc>"                     -- Separators in xmobar
        --                 , ppUrgent = xmobarColor  myppUrgent "" . clickWorkspace "!" "!"  -- Urgent workspace
        --                 , ppExtras  = [windowCount]                           -- # of windows current workspace
        --                 , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        --                 } >> updatePointer (0.25, 0.25) (0.25, 0.25)

          }
          `additionalKeysP` myKeys

