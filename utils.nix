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
    "${termBin} -e ${pkgs.systemdMinimal}/bin/systemd-run --scope --user -q ${tmuxBin} new-session -t default";
  '';
  consoleStartup =
    pkgs.writeShellScriptBin "consoleStartup" ''
      ${termBin} -T main_console -e $SHELL -ic '${pkgs.tmuxinator}/bin/tmuxinator start -p ${./tmuxinator-projects/default.yml}'
    '';
  setupScripts = pkgs.writeShellScriptBin "setupScripts" ''
    ${kbdInit}/bin/kdbInit
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
}
