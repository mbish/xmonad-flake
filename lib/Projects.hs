module Projects where
browser = "google-chrome-stable"
setupScripts = "setupScripts"
customBrowser = browser ++ " --new-window"
browserClass = "google-chrome"

popupBrowser = "qutebrowser"
popupBrowserClass = "qutebrowser"

myTerminal = "launchTerminal"

startupConsoleInit = "consoleStartup"

customTerm = "st"
popupTerm title command = customTerm ++ " -g 100x40 -t " ++ title ++ " -e " ++ command

soundControl = "pavucontrol"
screenRecording = "obs"
workChat = "slack"
workChatClass = "Slack"

customRadio = browser ++ " --app=https://open.spotify.com/browse/featured"
imageEditing = "gimp"
todoList = browser ++ " --new-window --app=https://todoist.com/app"

socialChat = "discord"
socialChatClass = socialChat

remoteAccess = "remmina"
remoteAccessClass = remoteAccess

customSms = browser ++ " --new-window --app=https://messages.google.com/web"
customSmsTitle = "messages.google.com__web"

customFlashcards = "anki"
customHypervisor = "virtualbox"
customCalculator = "gnome-calculator"

customMail = "thunderbird"
customMailClass = "thunderbird"

wifiApplet = "nm-applet"

customTmux = "tmux"
customTmuxinator = "tmuxinator";

customNode = "node"

-- Right now this is just an alias to break a circular dependency during the haskell override
customGHCI = "ghci"

customPython = "python"

tmuxinatorCommand :: [Char] -> [Char]
tmuxinatorCommand pr = customTmux ++ " run-shell -t 9 \"" ++ customTmuxinator ++ " start " ++ pr ++ "\""

addTodolistItem :: FilePath -> [Char] -> [Char]
addTodolistItem homeDir pr = homeDir ++ "/bin/todoist-add.sh '" ++ pr ++ "'"

toggleBar = "polybar-msg cmd toggle"

increaseBacklight = "brightness-change +50"
decreaseBacklight = "brightness-change -50"

customRunDialog = "rofi -show combi -modes combi -matching fuzzy -combi-modes \"run,window\""
windowSelect = "rofi -show window"

tmuxSessionSelect = "tmux list-sessions -F \"#S\" | rofi -dmenu -mesg \"tmux session\" | xargs -I {} tmux switch -t {}"

toggleRedshift = "toggle-redshift"
screenLocker = "slock"
lockScreen = screenLocker
-- lockScreen = "xautolock -locknow"

customSleep = "systemctl suspend";
toggleMedia homeDir = homeDir ++ "/bin/toggle_media"
toggleMic = "toggle-mic"

screenDetach homeDir = homeDir ++ "/bin/wdetach"
screenAttach homeDir = homeDir ++ "/bin/wattach"

customToggleNotifications = "toggle-notifications"

toggleBacklight =  "toggle-backlight"

improvementsFile homeDir = homeDir ++ "/workspace/notes/improvements"
thoughtsFile homeDir = homeDir ++ "/workspace/notes/thoughts"
journalFile homeDir = homeDir ++ "/workspace/notes/journal"
musicFile homeDir = homeDir ++ "/workspace/notes/music"

swapClipboards = "swap-clipboards"

notificationClose = "dunstctl close"
notificationHistory = "dunstctl history-pop"

launchBar = "launch-polybar"
autolock = "xautolock -time 5 -locker " ++ screenLocker ++ " &"

increaseVolume = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%+"
decreaseVolume = "wpctl set-volume @DEFAULT_AUDIO_SINK@ 10%-"
muteVolume = "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

screenShotDaemon = "flameshot"
screenShot = "flameshot gui"

openClipboardURL = "xclip -o -selection primary | xargs -I '{}' google-chrome-stable {}"

toggleNoise =  "toggle-noise"
