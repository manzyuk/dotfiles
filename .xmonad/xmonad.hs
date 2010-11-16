import XMonad
import qualified XMonad.StackSet as S

import XMonad.Layout.NoBorders           (smartBorders)

import XMonad.Hooks.SetWMName            (setWMName)
import XMonad.Hooks.DynamicLog    hiding (xmobar)
import XMonad.Hooks.ManageDocks          (avoidStruts, manageDocks)
import XMonad.Hooks.UrgencyHook          (readUrgents)
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run                   (spawnPipe, hPutStrLn)
import XMonad.Util.Font                  (encodeOutput)
import XMonad.Util.EZConfig              (additionalKeysP)
import XMonad.Util.NamedWindows          (getName)

import Data.List                         (intercalate, sortBy)
import Data.Maybe                        (isJust, catMaybes)
import Data.Monoid                       (All(All), mappend)
import Data.Function                     (on)

import Monad                             (when)
import Control.Monad                     (zipWithM_)

main = do
  xmobar0 <- xmobar 0 "%StdinReader%}{"       "[Run StdinReader]"
  xmobar1 <- xmobar 1 "%StdinReader%}{%date%" "[Run StdinReader, Run Date \"%a %b %_d, %H:%M\" \"date\" 10]"
  xmonad $ defaultConfig {
               focusFollowsMouse = False
             , borderWidth       = 2
             , terminal          = "urxvtc"
             , modMask           = mod4Mask
             , logHook           = myLogHook [ pp { ppOutput = hPutStrLn xmobar0 }
                                             , pp { ppOutput = hPutStrLn xmobar1 }
                                             ]
             , layoutHook        = avoidStruts  $  smartBorders  $  layoutHook defaultConfig
             , manageHook        = manageDocks <+> manageFloats <+> manageHook defaultConfig
             , startupHook       = setWMName "LG3D"
             , handleEventHook   = fullscreenEventHook `mappend` handleEventHook defaultConfig
             }
             `additionalKeysP`
             [ ("M-p", dmenu)
             , ("<XF86AudioMute>",        spawn "amixer -q set Master     toggle")
             , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
             , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")
             ]
    where
      manageFloats = composeOne [ isFullscreen -?> doFullFloat
                                , isDialog     -?> doFloat
                                ]


xmobar screen template commands = spawnPipe . intercalate " " $ options
    where options = [ "xmobar"
                    , "-x"
                    , show screen
                    , "-t"
                    , wrap "'" "'" template
                    , "-c"
                    , wrap "'" "'" commands
                    ]


dmenu = do
  screen <- (fromIntegral . S.screen . S.current) `fmap` gets windowset :: X Int
  io . spawn $ "exe=`dmenu_path | dmenu -i -p \"Run:\" -xs " ++ show screen ++ "` && exec $exe"


pp = defaultPP {
       ppHiddenNoWindows = xmobarColor "#444444"  ""       . pad
     , ppCurrent         = xmobarColor "#ffffff" "#777777" . pad
     , ppVisible         = pad
     , ppHidden          = pad
     , ppLayout          = pad . \x -> case x of
                                         "Tall"        -> "[|]"
                                         "Mirror Tall" -> "[-]"
                                         "Full"        -> "[ ]"
                                         _             -> "[?]"
     , ppTitle           = shorten 200
     , ppWsSep           = ""
     , ppSep             = ""
     }


myLogHook pps = do
  screens <- (sortBy (compare `on` S.screen) . S.screens) `fmap` gets windowset
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
  extras <- sequence $ map (flip catchX (return Nothing)) $ ppExtras pp

  return $ encodeOutput . sepBy (ppSep pp) . ppOrder pp $
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


-- Support for toggling Totem movie player fullscreen, see:
-- http://code.google.com/p/xmonad/issues/detail?id=339


fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ S.float w r
    where r = S.RationalRect 0 0 1 1
tileWin w = windows $ S.sink w


fullscreenEventHook :: Event -> X All
fullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state  <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win


  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add    = 1
      toggle = 2


      -- The ATOM property type for changeProperty
      ptype  = 4


      action = head dat


  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
      fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
      io $ changeProperty32 dpy win state ptype propModeReplace []
      tileWin win


  return $ All True


fullscreenEventHook _ = return $ All True
