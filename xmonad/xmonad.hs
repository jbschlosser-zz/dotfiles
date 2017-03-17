--------------------------------------------------------------------------------
import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

--------------------------------------------------------------------------------
main = do
  spawn "xmobar" -- Start a task bar such as xmobar.

  -- Start xmonad using the main desktop configuration with a few
  -- simple overrides:
  xmonad $ ewmh desktopConfig
    { terminal = "gnome-terminal"
    , focusFollowsMouse = False
    , modMask = mod4Mask -- Use the "Win" key for the mod key
    , manageHook = myManageHook <+> manageHook desktopConfig
    , logHook = dynamicLogString def >>= xmonadPropLog
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    , layoutHook = myLayout
    }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("M-p", shellPrompt myXPConfig)
      , ("M-S-f", spawn "firefox")
      , ("M-S-e", confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-S-r", restartXMonad)
      , ("M-<Return>", spawn "gnome-terminal") -- fix this
      , ("M-S-<Return>", windows W.swapMaster)
      , ("M-<Down>", windows W.focusDown)
      , ("M-<Up>", windows W.focusUp)
      , ("M-S-<Down>", windows W.swapDown)
      , ("M-S-<Up>", windows W.swapUp)
      , ("M-S-q", kill)
      , ("M-q", kill)
      , ("M-t", withFocused $ windows . W.sink)
      , ("M-r", sendMessage NextLayout)
      , ("M-f", sendMessage $ Toggle FULL)
      , ("M-x", sendMessage $ Toggle REFLECTX)
      , ("M-y", sendMessage $ Toggle REFLECTY)
      ]

restartXMonad :: X()
restartXMonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

myLayout = id
    . smartBorders
    . mkToggle (NOBORDERS ?? FULL ?? EOT)
    . mkToggle (single REFLECTX)
    . mkToggle (single REFLECTY)
        $ tiled ||| Mirror tiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:monospace:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
  [ className =? "Pidgin" -?> doFloat
  , className =? "XCalc"  -?> doFloat
  , className =? "mpv"    -?> doFloat
  , isDialog              -?> doCenterFloat

    -- Move transient windows to their parent:
  , transience
  ]
