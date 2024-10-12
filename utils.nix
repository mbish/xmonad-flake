{
  pkgs,
  inputs,
  system,
  vars,
  ...
}: rec {
  termBin = "${inputs.st.packages.${system}.default}/bin/st";
  tmuxBin = "${inputs.tmux.packages.${system}.default}/bin/tmux";
  toggle-redshift = let
    systemctl = "${pkgs.systemdMinimal}/bin/systemctl";
  in
    pkgs.writeShellScriptBin "toggle-redshift" ''
      (${systemctl} --user is-active redshift && ${systemctl} --user stop redshift) || (${systemctl} --user start redshift)
    '';
  kbdInit = pkgs.writeShellScriptBin "kbdInit" ''
    ${pkgs.xorg.xset}/bin/xset -b
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap -layout us -variant altgr-intl -option "lv3:bksl_switch"
    ${pkgs.xorg.xmodmap}/bin/xmodmap ${./xmodmap}
    ${pkgs.procps}/bin/pkill -f xcape
    ${pkgs.xcape}/bin/xcape -e "ISO_Level3_Shift=backslash"
    ${pkgs.xcape}/bin/xcape -e "Control_L=return"
    ${pkgs.xbindkeys}/bin/xbindkeys -p &
  '';
  launchTerminal = pkgs.writeShellScriptBin "launchTerminal" ''
    ${termBin} -e ${pkgs.systemdMinimal}/bin/systemd-run --scope --user -q ${tmuxBin} new-session -t default;
  '';
  consoleStartup = pkgs.writeShellScriptBin "consoleStartup" ''
    ${termBin} -T main_console -e $SHELL -ic '${pkgs.tmuxinator}/bin/tmuxinator start -p ${./tmuxinator-projects/default.yml}'
  '';
  setupScripts = pkgs.writeShellScriptBin "setupScripts" ''
    ${kbdInit}/bin/kbdInit
  '';
  toggle-mic = pkgs.writeShellScriptBin "toggle-mic" ''
    ${pkgs.pulseaudio}/bin/pactl set-source-mute "${vars.mic}" toggle
  '';
  toggle-backlight = let
    xbacklight = "${pkgs.xorg.xbacklight}/bin/xbacklight";
  in
    pkgs.writeShellScriptBin "toggle-backlight" ''
      PERCENT=`${xbacklight} -get`

      if [ "$PERCENT" -ne 0 ]; then
          ${xbacklight} -set 0 -steps 10
      else
          ${xbacklight} -set 50 -steps 10
      fi
    '';
  swap-clipboards = let
    xclip = "${pkgs.xclip}/bin/xclip";
  in
    pkgs.writeShellScriptBin "swap-clipboards" ''
      OLD_PRIMARY="`${xclip} -selection PRIMARY -o`"
      OLD_CLIPBOARD="`${xclip} -selection CLIPBOARD -o`"
      echo -n "$OLD_PRIMARY" | ${xclip} -selection CLIPBOARD -i
      echo -n "$OLD_CLIPBOARD" | ${xclip} -selection PRIMARY -i
      ${tmuxBin} set-buffer "$OLD_PRIMARY"
    '';
  toggle-notifications = let
    dunstctl = "${pkgs.dunst}/bin/dunstctl";
  in
    pkgs.writeShellScriptBin "toggle-notifications" ''
      if [ "$(${dunstctl} is-paused)" == "false" ]; then
          ${dunstctl} set-paused true
      else
          ${dunstctl} set-paused false
      fi
    '';

  qutebrowser = pkgs.writeShellScriptBin "qutebrowser" ''
    ${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel ${pkgs.qutebrowser}/bin/qutebrowser "$@"
  '';

  rofii = pkgs.rofi.overrideAttrs (final: prev: {
    nativeBuildInputs =
      prev.nativeBuildInputs
      ++ [
        pkgs.makeWrapper
      ];
    postInstall = ''
      wrapProgram $out/bin/rofi \
        --prefix PATH : $HOME/bin
    '';
  });
  rofi = pkgs.stdenv.mkDerivation rec {
    name = "rofi-custom";
    nativeBuildInputs = [pkgs.makeWrapper];
    phases = ["installPhase"];

    packagesInExe = [
      keyboard-layout
    ];

    installPhase = ''
      mkdir -p $out/bin
      cp ${pkgs.rofi}/bin/rofi $out/bin
      wrapProgram $out/bin/rofi \
        --prefix PATH : ${pkgs.lib.makeBinPath packagesInExe}
    '';
  };
  keyboard-layout = pkgs.writeShellScriptBin "keyboard-layout" ''
    if [ -z $1 ]; then
        LAYOUT="us"
    else
        LAYOUT="$1"
    fi
    ${pkgs.xorg.xmodmap}/bin/xmodmap ${./xmodmap}
    ${pkgs.xorg.setxkbmap}/bin/setxkbmap -layout us -variant altgr-intl -option "lv3:bksl_switch"
    ${pkgs.xorg.xmodmap}/bin/xmodmap ${./xmodmap}
    ${pkgs.systemdMinimal}/bin/systemctl --user restart xcape &
  '';
  mic = "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_TS_2018_10_13_53845-00.analog-stereo";
  mic-status = pkgs.writeShellScriptBin "mic-status" ''
    MUTED=$(${pkgs.pulseaudio}/bin/pactl get-source-mute "${mic}" 2>/dev/null| cut -f2 -d' ')
    if [ "$MUTED" == "yes" ]; then
        echo "%{F#fb4934}%{u-}"
    else
        echo ""
    fi
  '';
  brightnessChange = pkgs.writeShellScriptBin "brightness-change" ''
    dir_files=(/sys/class/backlight/*)
    if [[ "''${#dir_files[@]}" -gt 1 ]] ; then
       #More than 4 files
       exit 1
    elif [[ -e "''${dir_files[0]}" ]] ; then
       #non-empty
      VALUE=$(cat /sys/class/backlight/*/brightness | head -n 1)
      NEW_VALUE=$(bc <<< "$VALUE$1")
      echo $NEW_VALUE > /sys/class/backlight/*/brightness
    else
       exit 1
    fi
  '';
}
