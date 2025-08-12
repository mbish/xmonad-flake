{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Concurrent
import Control.Monad
import qualified DBus as D
import Options.Applicative
import qualified DBus.Client as D
import Data.Default
import Data.List
import Data.List.Split
import Data.Text (replace, pack, unpack)
import qualified Data.Map as M
import Data.Time.Format
import Data.Time.LocalTime
import Graphics.X11.ExtraTypes.XF86
import System.Directory
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicProjects
import XMonad.Actions.OnScreen
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.DwmStyle
import XMonad.Layout.Maximize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P
import Projects

data Options = Options
  { configFile :: FilePath,
    xmonadRecompile :: Bool,
    xmonadVersion :: Bool,
    xmonadReplace :: Bool,
    xmonadRestart :: Bool
  }
  deriving (Show)

modm :: KeyMask
modm = mod4Mask -- Win key or Super_L
-- myLayout = smartBorders . avoidStruts $ layoutHook defaultConfig $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ tiled ||| Mirror tiled

bg0 = "#282828"

bg1 = "#665c54"

bg2 = "#3c3836"

bg3 = "#665c54"

red = "#fb4934"

blue = "#8ec07c"

dark_blue = "#83a598"

ddark_blue = "#458588"

black = "#282828"

fg = "#ebdbb2"

yellow = "#fabd2f"

projects :: [Project]
projects =
  [ Project
      { projectName = "scratch",
        projectDirectory = "~/",
        projectStartHook = Nothing
      },
    Project
      { projectName = "browser",
        projectDirectory = "~/Download",
        projectStartHook = Just $ do
          spawn customBrowser
      },
    Project
      { projectName = "console",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn startupConsoleInit
      },
    Project
      { projectName = "dev-2",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn customTerm
      },
    Project
      { projectName = "dev-3",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn customTerm
      },
    Project
      { projectName = "mail",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn customMail
      },
    Project
      { projectName = "windows-authproxy",
        projectDirectory = "~/workspace/authproxy",
        projectStartHook = Just $ do spawn "~/bin/winauthproxy"
      },
    Project
      { projectName = "soundControl",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn soundControl
      },
    Project
      { projectName = "obs",
        projectDirectory = "~/Streams",
        projectStartHook = Just $ do spawn screenRecording
      },
    Project
      { projectName = "workChat",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn workChat
      },
    Project
      { projectName = "radio",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn customRadio
      },
    Project
      { projectName = "gimp",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn imageEditing
      },
    Project
      { projectName = "todo",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn todoList
      },
    Project
      { projectName = "socialChat",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn socialChat
      },
    Project
      { projectName = "remmina",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn remoteAccess
      },
    Project
      { projectName = "videoChat",
        projectDirectory = "~/",
        projectStartHook = Nothing
      },
    Project
      { projectName = "..",
        projectDirectory = "~/",
        projectStartHook = Nothing
      },
    Project
      { projectName = "sms",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn customSms
      },
    Project
      { projectName = "flashcards",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn customFlashcards
      },
    Project
      { projectName = "virtualbox",
        projectDirectory = "~/",
        projectStartHook = Just $ do spawn customHypervisor
      }
  ]

scratchpads :: [NamedScratchpad]
scratchpads =
  [ 
    NS "Calculator" customCalculator (title =? "Calculator") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5),
    NS "PythonInterpreter" (popupTerm "pythoninterp" customPython) (title =? "pythoninterp") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5),
    NS "NodeInterpreter" (popupTerm "nodeinterp" customNode) (title =? "nodeinterp") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5),
    NS "HaskellInterpreter" (popupTerm "haskellinterp" customGHCI) (title =? "haskellinterp") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5),
    NS "PopupBrowser" popupBrowser (className =? popupBrowserClass) (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5),
    NS "ShellScratch" (popupTerm "shellscratch" "zsh") (title =? "shellscratch") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5)
  ]

myPrompt :: XPConfig
myPrompt =
  def
    { position = Bottom,
      font = "xft:DejaVu Sans Condensed-8"
    }

mPrompt :: XPConfig
mPrompt =
  def
    { position = Bottom,
      font = "xft:DejaVu Sans Condensed-8",
      alwaysHighlight = True
    }

tmuxinatorProjects :: IO [FilePath]
tmuxinatorProjects = do 
  configDir <- getXdgDirectory XdgConfig "tmuxinator"
  System.Directory.listDirectory configDir

data MuxPrompt = MuxPrompt

instance XPrompt MuxPrompt where
  showXPrompt MuxPrompt = "Tmuxinator project: "
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

muxPrompt :: XPConfig -> X ()
muxPrompt c = do
  mc <- io tmuxinatorProjects
  mkXPrompt MuxPrompt c (mkComplFunFromList' c (map (\x -> head $ splitOn "." x) mc)) mux

mux :: [Char] -> X ()
mux = (spawn . tmuxinatorCommand)

data TodoPrompt = TodoPrompt

instance XPrompt TodoPrompt where
  showXPrompt TodoPrompt = "Todoist quick add: "

-- commandToComplete _ c = c
-- nextCompletion _ = getNextCompletion

todoPrompt :: XPConfig -> X ()
todoPrompt c = do
  homeDir <- liftIO $ getHomeDirectory
  mkXPrompt TodoPrompt c (mkComplFunFromList c []) (todo homeDir)

todo homeDir = (spawn . (addTodolistItem homeDir))

projectByName :: ProjectName -> Project
projectByName n = head $ (filter $ (== n) . projectName) projects

togglebar :: X ()
togglebar = do
  spawn toggleBar
  onScreen' (sendMessage ToggleStruts) FocusCurrent 0

openBrowserTab :: String -> X ()
openBrowserTab url = do
  raise (className =? browserClass)
  sendKey controlMask xK_l
  sendKey controlMask xK_t
  P.pasteString url
  sendKey controlMask xK_Return

browserKey :: KeyMask -> KeySym -> X ()
browserKey mask key = do
  raise (className =? browserClass)
  sendKey mask key

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys (XConfig {XMonad.modMask = modm}) = do
  M.fromList $
    [ ((modm, xK_f), sendMessage $ Toggle FULL),
      ((0, xF86XK_MonBrightnessUp), spawn increaseBacklight),
      ((0, xF86XK_MonBrightnessDown), spawn decreaseBacklight),
      ((modm, xK_d), spawn customRunDialog),
      ((modm, xK_Return), spawn myTerminal),
      ((modm .|. controlMask, xK_x), shellPrompt myPrompt),
      ((modm, xK_bracketright), switchProjectPrompt myPrompt),
      ((modm, xK_u), muxPrompt mPrompt),
      ((modm .|. shiftMask, xK_bracketright), shiftToProjectPrompt myPrompt),
      ((modm, xK_b), switchProject $ projectByName "browser"),
      ((modm, xK_c), switchProject $ projectByName "console"),
      ((modm, xK_l), switchProject $ projectByName "workChat"),
      ((modm, xK_2), switchProject $ projectByName "dev-2"),
      ((modm, xK_Left),  sendMessage Shrink),
      ((modm, xK_Right), sendMessage Expand),
      ( (modm, xK_w),
        submap . M.fromList $
          [ ((0, xK_b), switchProject $ projectByName "virtualbox"),
            ((modm, xK_b), switchProject $ projectByName "virtualbox"),
            ((0, xK_s), switchProject $ projectByName "sms"),
            ((modm, xK_s), switchProject $ projectByName "sms"),
            ((0, xK_p), switchProject $ projectByName "soundControl"),
            ((modm, xK_p), switchProject $ projectByName "soundConrol"),
            ((0, xK_BackSpace), kill),
            ((0, xK_1), switchProject $ projectByName "console"),
            ((modm, xK_1), switchProject $ projectByName "console"),
            ((0, xK_2), switchProject $ projectByName "dev-2"),
            ((modm, xK_2), switchProject $ projectByName "dev-2"),
            ((0, xK_3), switchProject $ projectByName "dev-3"),
            ((modm, xK_3), switchProject $ projectByName "dev-3"),
            ((0, xK_r), switchProject $ projectByName "radio"),
            ((modm, xK_r), switchProject $ projectByName "radio"),
            ((0, xK_t), switchProject $ projectByName "todo"),
            ((modm, xK_t), switchProject $ projectByName "todo"),
            ((0, xK_h), switchProject $ projectByName "socialChat"),
            ((modm, xK_h), switchProject $ projectByName "socialChat"),
            ((0, xK_v), switchProject $ projectByName "videoChat"),
            ((modm, xK_v), switchProject $ projectByName "videoChat"),
            ((0, xK_k), switchProject $ projectByName "flashcards"),
            ((modm, xK_k), switchProject $ projectByName "flashcards"),
            ((modm, xK_w), spawn windowSelect),
            ((0, xK_m), switchProject $ projectByName "mail"),
            ((modm, xK_m), switchProject $ projectByName "mail")
          ]
      ),
      ( (modm, xK_bracketleft), spawn tmuxSessionSelect),
      ( (modm, xK_e),
        submap . M.fromList $
          [ ((0, xK_r), spawn toggleRedshift),
            ((modm, xK_r), spawn toggleRedshift),
            ((0, xK_x), spawn toggleBar),
            ((modm, xK_x), spawn toggleBar),
            ((0, xK_e), swapNextScreen),
            ((modm, xK_e), swapNextScreen)
          ]
      ),
      ( (modm, xK_m),
        submap . M.fromList $
          [ ( (0, xK_l),
              do
                spawn lockScreen
            ),
            ( (modm, xK_l),
              do
                spawn lockScreen
            ),
            ((0, xK_s), spawn customSleep),
            ((modm, xK_s), spawn customSleep),
            ((0, xK_a), do
              homeDir <- liftIO $ getHomeDirectory
              spawn (screenAttach homeDir)
            ),
            ((modm, xK_a), do
              homeDir <- liftIO $ getHomeDirectory
              spawn (screenAttach homeDir)
            ),
            ((0, xK_d), do 
              homeDir <- liftIO $ getHomeDirectory
              spawn (screenDetach homeDir)
            ),
            ((modm, xK_d), do 
              homeDir <- liftIO $ getHomeDirectory
              spawn (screenDetach homeDir)
            ),
            ((0, xK_m), do 
              homeDir <- liftIO $ getHomeDirectory
              spawn (toggleMedia homeDir)
            ),
            ((modm, xK_m), do 
              homeDir <- liftIO $ getHomeDirectory
              spawn (toggleMedia homeDir)
            ),
            ((0, xK_i), spawn toggleMic),
            ((modm, xK_i), spawn toggleMic)
          ]
      ),
      ( (modm, xK_t),
        submap . M.fromList $
          [ ((0, xK_x), togglebar),
            ((modm, xK_x), togglebar),
            ((0, xK_b), sendMessage $ Toggle NOBORDERS),
            ((modm, xK_b), sendMessage $ Toggle NOBORDERS),
            ((0, xK_f), withFocused $ windows . W.sink),
            ((modm, xK_f), withFocused $ windows . W.sink),
            ((0, xK_n), spawn customToggleNotifications),
            ((modm, xK_n), spawn customToggleNotifications),
            ((0, xK_b), spawn toggleBacklight),
            ((modm, xK_b), spawn toggleBacklight),
            ((0, xK_o), spawn toggleNoise),
            ((modm, xK_o), spawn toggleNoise)
          ]
      ),
      ( (modm, xK_a),
        submap . M.fromList $
          [ ( (0, xK_i),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (improvementsFile homeDir)
            ),
            ( (modm, xK_i),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (improvementsFile homeDir)
            ),
            ( (0, xK_t),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (thoughtsFile homeDir)
            ),
            ( (modm, xK_t),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (thoughtsFile homeDir)
            ),
            ( (0, xK_j),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (journalFile homeDir)
            ),
            ( (modm, xK_j),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (journalFile homeDir)
            ),
            ( (modm, xK_m),
              do
                homeDir <- liftIO $ getHomeDirectory
                date <- io $ liftM (formatTime defaultTimeLocale "[%Y-%m-%d %H:%M] ") getZonedTime
                appendFilePrompt' def (date ++) $ (musicFile homeDir)
            ),
            ((modm, xK_d), todoPrompt mPrompt)
          ]
      ),
      ( (modm, xK_t),
        submap . M.fromList $
          [ ( (0, xK_u), spawn openClipboardURL)]
      ),
      ((modm, xK_k), namedScratchpadAction scratchpads "PopupBrowser"),
      ( (modm, xK_g),
        submap . M.fromList $
          [ ((0, xK_c), namedScratchpadAction scratchpads "Calculator"),
            ((modm, xK_c), namedScratchpadAction scratchpads "Calculator"),
            ((modm, xK_p), namedScratchpadAction scratchpads "PythonInterpreter"),
            ((0, xK_p), namedScratchpadAction scratchpads "PythonInterpreter"),
            ((modm, xK_n), namedScratchpadAction scratchpads "NodeInterpreter"),
            ((0, xK_n), namedScratchpadAction scratchpads "NodeInterpreter"),
            ((modm, xK_h), namedScratchpadAction scratchpads "HaskellInterpreter"),
            ((0, xK_h), namedScratchpadAction scratchpads "HaskellInterpreter"),
            ((modm, xK_s), namedScratchpadAction scratchpads "ShellScratch"),
            ((0, xK_s), namedScratchpadAction scratchpads "ShellScratch"),
            ((modm, xK_b), namedScratchpadAction scratchpads "PopupBrowser"),
            ((0, xK_b), namedScratchpadAction scratchpads "PopupBrowser"),
            ((modm, xK_o), namedScratchpadAction scratchpads "PopupBrowser"),
            ((0, xK_o), namedScratchpadAction scratchpads "PopupBrowser")
          ]
      ),
      ((modm .|. shiftMask, xK_q), kill),
      ((modm .|. shiftMask, xK_e), io (exitWith ExitSuccess)), -- %! Quit xmonad
      ((modm, xK_i), onPrevNeighbour def W.view),
      ((modm, xK_x), swapNextScreen),
      ((modm, xK_z), spawn swapClipboards),
      ((modm, xK_o), browserKey controlMask xK_t),
      ((0, xK_F1), spawn toggleMic),
      ((0, xF86XK_AudioMicMute), spawn toggleMic),
      ((0, xK_Pause), spawn increaseVolume),
      ((0, xK_Scroll_Lock), spawn decreaseVolume),
      ((0, xF86XK_AudioRaiseVolume), spawn increaseVolume),
      ((0, xF86XK_AudioLowerVolume), spawn decreaseVolume),
      ((0, xF86XK_AudioMute), spawn muteVolume),
      ((0, xK_Print), spawn screenShot),
      ((controlMask, xK_F1), spawn muteVolume),
      ((controlMask, xK_space), spawn notificationClose),
      ((controlMask, xK_grave), spawn notificationHistory)
    ]

keysToRemove :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keysToRemove _ =
  M.fromList
    [ ((modm .|. shiftMask, xK_q), return ()),
      ((modm .|. shiftMask, xK_p), return ()),
      ((modm, xK_p), return ()),
      ((modm, xK_e), return ())
    ]

newKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
newKeys x = keys def x `M.difference` keysToRemove x

myLayout =
  id
    . avoidStruts
    . smartBorders
    . mkToggle (NOBORDERS ?? FULL ?? EOT)
    . mkToggle (single MIRROR)
    $ tiled
      -- \||| Mirror (TwoPane delta (1/2))
      ||| Full
      ||| tab
  where
    -- \||| simpleTabbed

    -- default tiling algorithm partitions the screen into two panes
    tiled = maximize (Tall nmaster delta ratio)

    tab = tabbedBottom shrinkText myTabConfig

    -- latex = windowNavigation (
    --                          combineTwo
    --                          (TwoPane delta 0.45)
    --                          (Full)
    --                          (combineTwo
    --                           (Mirror (TwoPane delta 0.85))
    --                           (Full)
    --                           (Full)
    --                          )
    --                         )

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

    myTabConfig =
      def
        { inactiveBorderColor = black,
          activeBorderColor = bg0,
          activeColor = bg1,
          fontName = "xft:DejaVu Sans Condensed-10",
          inactiveColor = black,
          inactiveTextColor = fg,
          activeTextColor = yellow
        }

emptyPlaceholder :: String -> String
emptyPlaceholder _ = ""

myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput = dbusOutput dbus,
      ppCurrent = wrap ("%{F" ++ fg ++ "}%{B" ++ bg1 ++ "} ") " %{B-}%{F-}",
      ppVisible = wrap ("%{F" ++ fg ++ "}%{B" ++ bg2 ++ "} ") " %{B-}%{F-}",
      ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}",
      ppHidden = wrap (" %{F" ++ fg ++ "}") "%{F-} ",
      ppWsSep = "",
      ppSep = " >>",
      ppTitle = emptyPlaceholder
    }

polybarString :: String -> String -> String
polybarString index s = "%{T" ++ index ++ "} " ++ s ++ " %{T-}"

barAliases =
  [ ("console", polybarString "3" "\xf303"),
    ("browser", polybarString "3" "\xf268"),
    ("radio", polybarString "3" "\xf1bc"),
    ("mail", polybarString "3" "\xeb1c"),
    ("gimp", polybarString "3" "\xf1fc"),
    ("soundControl", polybarString "3" "\xf9c2"),
    ("windows-authproxy", polybarString "3" "\xe70f"),
    ("socialChat", polybarString "3" "\xf1ff"),
    ("remmina", polybarString "3" "\xf25d"),
    ("videoChat", polybarString "3" "\xf03d"),
    ("virtualbox", polybarString "3" "\xf26c"),
    ("todo", polybarString "3" "\xf45e"),
    ("obs", polybarString "3" "\xf43c"),
    ("workChat", polybarString "3" "\xf198"),
    ("flashcards", polybarString "3" "\xf7c2"),
    ("sms", polybarString "3" "\xf086"),
    ("dev-2", polybarString "3" "\xf668"),
    ("dev-3", polybarString "3" "\xf121"),
    (" %{F" ++ fg ++ "}NSP%{F-} ", ""),
    ("Tabbed Bottom Simplest", "%{B" ++ dark_blue ++ "}%{F" ++ black ++ "}%{T3} \xf04e9 %{T-}%{B-}"),
    ("Full", "%{B" ++ dark_blue ++ "}%{F" ++ black ++ "}%{T3} \xf05af %{T-}%{B-}"),
    ("Maximize Tall", "%{B" ++ dark_blue ++ "}%{F" ++ black ++ "}%{T3} \xf0570 %{T-}%{B-}"),
    (" | ", "%{F#1c1816}%{T11}\x258f%{T-}%{F-}"),
    ("..", "")
  ]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString cut_str]
          }
  D.emit dbus signal
  where
    replaceNext :: [(String, String)] -> String -> String
    replaceNext [] str = str
    replaceNext (x:xs) str = replaceNext xs replaced
      where
        encoded = UTF8.encodeString (snd x)
        replaced = unpack $ replace (pack $ fst x) (pack $ encoded) (pack str)
    mapped_str = replaceNext barAliases str
    cut_str = intercalate "" (reverse (splitOn ">>" mapped_str))
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

activeLogHook :: PP
activeLogHook = def {ppOutput = workspaceLogger}

workspaceLogger :: String -> IO ()
workspaceLogger str = do
  localDir <- getXdgDirectory XdgState "activity.log"
  appendFile localDir (str ++ "\n")

configFileParser :: Parser FilePath
configFileParser = strOption (long "inputFile" <> short 'i' <> help "Input file " <> metavar "FILE")

xmonadRecompileParser :: Parser Bool
xmonadRecompileParser = switch (long "recompile" <> help "Recompile your ~/.xmonad/xmonad.hs")

xmonadRestartParser :: Parser Bool
xmonadRestartParser = switch (long "restart" <> help "Request a running xmonad process to restart")

xmonadReplaceParser :: Parser Bool
xmonadReplaceParser = switch (long "replace" <> help "Replace the running window manager with xmonad")

xmonadVersionParser :: Parser Bool
xmonadVersionParser = switch (long "version" <> help "Print the version number")

optionsParser :: Parser Options
optionsParser = Options <$> configFileParser <*> xmonadRecompileParser <*> xmonadVersionParser <*> xmonadReplaceParser <*> xmonadRestartParser

main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  -- opts <- execParser opts
  
  xmonad $
    docks $
      dynamicProjects projects $
        ewmh
          def
            { terminal = myTerminal,
              modMask = modm,
              borderWidth = myBorderWidth,
              focusedBorderColor = "#fb4934",
              manageHook =
                composeAll
                  [ manageDocks,
                    (isFullscreen --> doFullFloat),
                    manageHook def,
                    namedScratchpadManageHook scratchpads,
                    title =? "safeeyes" --> doFloat,
                    title =? "main_console" --> hasBorder False,
                    className =? socialChatClass --> doShift (projectName (projectByName "socialChat")),
                    className =? remoteAccessClass --> doShift (projectName (projectByName "remmina")),
                    className =? workChatClass --> doShift (projectName (projectByName "workChat")),
                    className =? customMailClass --> doShift (projectName (projectByName "mail")),
                    className =? "Mail" --> doShift (projectName (projectByName "mail")),
                    stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift (projectName (projectByName "browser")),
                    resource =? customSmsTitle --> doShift (projectName (projectByName "sms"))
                  ],
              layoutHook = myLayout,
              logHook =
                dynamicLogWithPP activeLogHook
                  >> dynamicLogWithPP (myLogHook dbus)
                  >> updatePointer (0.5, 0.5) (0, 0)
                  >> workspaceHistoryHook,
              keys =
                composeAll
                  [ myKeys,
                    newKeys
                  ],
              handleEventHook = docksEventHook <+> serverModeEventHook,
              workspaces = ["browser", "console", "socialChat", "workChat", "mail", "sms"],
              startupHook = do
                spawn launchBar
                onScreen' (switchProject $ projectByName "browser") FocusNew 0
                spawn socialChat
                spawn workChat
                spawn customMail
                spawn setupScripts
                spawn wifiApplet
                spawn screenShotDaemon
                -- for some reason this doesn't work. Using systemd service for now
                -- spawn autolock
                onScreen' (switchProject $ projectByName "sms") FocusNew 0
                onScreen' (switchProject $ projectByName "browser") FocusNew 0
                onScreen' (switchProject $ projectByName "console") FocusNew 0,
              handleExtraArgs = \ xs theConf -> case xs of
                [] -> return theConf
                _ -> return theConf
            }
  where
    prefs = defaultPrefs
    opts =
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Xmonad launcher"
            <> header "Xmonad"
        )

myBorderWidth = 0
