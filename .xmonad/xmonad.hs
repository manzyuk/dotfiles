import XMonad
import qualified XMonad.StackSet as S

import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.Reflect             (reflectHoriz)
import XMonad.Layout.NoBorders           (smartBorders)
import XMonad.Layout.Decoration
import XMonad.Layout.PerWorkspace        (onWorkspace)
import XMonad.Layout.NoFrillsDecoration

import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Hooks.DynamicLog    hiding (xmobar)
import XMonad.Hooks.ManageDocks          (avoidStruts, manageDocks)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops         (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run                   (spawnPipe, hPutStrLn)
import XMonad.Util.EZConfig              (additionalKeysP)
import XMonad.Util.NamedWindows          (getName)

import Data.Ord                          (comparing)
import Data.List                         (intercalate, sortBy, isInfixOf)
import Data.Maybe                        (isJust, catMaybes)
import Data.Ratio                        ((%))
import Data.Monoid                       (All(All), mappend)

import Codec.Binary.UTF8.String          (encodeString)
import Control.Monad                     (when, zipWithM_, liftM2)


main = do
  xmobar0 <- xmobar 0 "%StdinReader%}{"       "[Run StdinReader]"
  xmobar1 <- xmobar 1 "%StdinReader%}{%date%" "[Run StdinReader, Run Date \"%a %b %_d, %H:%M\" \"date\" 10]"
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {
               focusFollowsMouse  = False
             , borderWidth        = 1
             , focusedBorderColor = "#386890"
             , normalBorderColor  = "#555555"
             , terminal           = "gnome-terminal"
             , modMask            = mod4Mask
             , logHook            = myLogHook [ pp { ppOutput = hPutStrLn xmobar0 }
                                              , pp { ppOutput = hPutStrLn xmobar1 }
                                              ]
             , workspaces         = myWorkspaces
             , layoutHook         = myLayoutHook
             , manageHook         = myManageHook
             , startupHook        = setWMName "LG3D"
             , handleEventHook    = fullscreenEventHook `mappend` handleEventHook defaultConfig
             }
             `additionalKeysP`
             [ ("M-p", withDmenu "." "dmenu_path" "exec" ["-p", "'Run:'"])
             , ("M-o", withDmenu "$LIBRARY" "ls" "evince" ["-l", "75"])
             , ("M-<Esc>", spawn "gnome-screensaver-command --lock")
             , ("<XF86AudioMute>",        spawn "amixer -q set Master     toggle")
             , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
             , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")
             ]


myWorkspaces = ["1:edit", "2:surf", "3:read", "4:chat", "5:play", "6:misc", "7:misc", "8:misc", "9:misc"]


myLayoutHook = avoidStruts
             $ smartBorders
             $ noFrillsDeco shrinkText myTheme
             $ onWorkspace "4:chat" chatLayout
             $ layoutHook defaultConfig
    where chatLayout = withIM (1/6) (Role "MainWindow")
                     $ reflectHoriz
                     $ withIM (1/5) (Role "buddy_list") Grid


myTheme = defaultTheme {
            activeColor         = "SteelBlue"
          , activeBorderColor   = "#386890"
          , inactiveBorderColor = "#555555"
          , fontName            = "xft:DejaVu Sans-10:bold"
          }


myManageHook = manageDocks <+> manageFloats <+> manageApps
    where manageFloats = composeOne [ isFullscreen -?> doFullFloat
                                    , isDialog     -?> doFloat
                                    ]
          manageApps   = composeAll [ className =? "Emacs"         --> moveTo "1:edit"
                                    , className =? "Iceweasel"     --> moveTo "2:surf"
                                    , className =? "Google-chrome" --> moveTo "2:surf"
                                    , className =? "X-www-browser" --> moveTo "2:surf"
                                    , className =? "GV"            --> moveTo "3:read"
                                    , className =? "XDvi"          --> moveTo "3:read"
                                    , className =? "Xpdf"          --> moveTo "3:read"
                                    , className =? "Evince"        --> moveTo "3:read"
                                    , className =? "Skype"         --> moveTo "4:chat"
                                    , className =? "Pidgin"        --> moveTo "4:chat"
                                    , className =? "Rhythmbox"     --> moveTo "5:play"
                                    ]
          moveTo       = doF . liftM2 (.) S.view S.shift


xmobar screen template commands = spawnPipe . unwords $ options
    where options = [ "xmobar"
                    , "-x"
                    , show screen
                    , "-t"
                    , wrap "'" "'" template
                    , "-c"
                    , wrap "'" "'" commands
                    ]


currentScreenID :: X Int
currentScreenID = (fromIntegral . S.screen . S.current) `fmap` gets windowset


withDmenu :: String -> String -> String -> [String] -> X ()
withDmenu dir src prg opt = do
  screen <- currentScreenID
  let tmp = wrap "`" "`" . unwords $ [ src
                                     , "|"
                                     , "dmenu"
                                     , "-i"
                                     , "-xs"
                                     , show screen
                                     ] ++ opt
      cmd = unwords [ "cd"
                    , dir
                    , "&&"
                    , "tmp=" ++ tmp
                    , "&&"
                    , prg
                    , "\"$tmp\""
                    ]
  io . spawn $ cmd


pp = defaultPP {
       ppHiddenNoWindows = xmobarColor "DimGray"      ""            . pad
     , ppCurrent         = xmobarColor "White"        "#555555"     . pad
     , ppVisible         = pad
     , ppHidden          = pad
     , ppUrgent          = xmobarColor ""             "LightSalmon"       . xmobarStrip
     , ppLayout          = xmobarColor "LightSkyBlue" ""            . pad . iconify
     , ppTitle           = xmobarColor "PaleGreen"    ""            . pad . shorten 200
     , ppWsSep           = ""
     , ppSep             = ""
     , ppOrder           = \(ws:l:_:rest) -> (ws:l:rest)
     }
    where
      iconify l | "Mirror" `isInfixOf` l = "[-]"
                | "Grid"   `isInfixOf` l = "[+]"
                | "Tall"   `isInfixOf` l = "[|]"
                | "Full"   `isInfixOf` l = "[ ]"
      iconify _                          = "[?]"


myLogHook pps = do
  screens <- (sortBy (comparing S.screen) . S.screens) `fmap` gets windowset
  zipWithM_ dynamicLogWithPP' screens pps

-- Extract the focused window from the stack of windows on the given screen.
-- Return Just that window, or Nothing for an empty stack.
focusedWindow = maybe Nothing (return . S.focus) . S.stack . S.workspace

-- The functions dynamicLogWithPP', dynamicLogString', and pprWindowSet' below
-- are similar to their undashed versions, with the difference being that the
-- latter operate on the current screen, whereas the former take the screen to
-- operate on as the first argument.

dynamicLogWithPP' screen pp = dynamicLogString' screen pp >>= io . ppOutput pp

dynamicLogString' screen pp = do

  winset <- gets windowset
  urgents <- readUrgents
  sort' <- ppSort pp

  -- layout description
  let ld = description . S.layout . S.workspace $ screen

  -- workspace list
  let ws = pprWindowSet' screen sort' urgents pp winset

  -- window title
  wt <- maybe (return "") (fmap show . getName) $ focusedWindow screen

  -- run extra loggers, ignoring any that generate errors.
  extras <- mapM (`catchX` return Nothing) $ ppExtras pp

  return $ encodeString . sepBy (ppSep pp) . ppOrder pp $
             [ ws
             , ppLayout pp ld
             , ppTitle  pp wt
             ]
             ++ catMaybes extras


pprWindowSet' screen sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $ S.workspaces s
    where this     = S.tag . S.workspace $ screen
          visibles = map (S.tag . S.workspace) (S.current s : S.visible s)

          fmt w = printer pp (S.tag w)
              where printer | S.tag w == this                                               = ppCurrent
                            | S.tag w `elem` visibles                                       = ppVisible
                            | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents  = \ppC -> ppUrgent ppC . ppHidden ppC
                            | isJust (S.stack w)                                            = ppHidden
                            | otherwise                                                     = ppHiddenNoWindows


sepBy :: String -> [String] -> String
sepBy sep = intercalate sep . filter (not . null)
