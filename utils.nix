{
  pkgs,
  inputs,
  system,
  ...
}:
rec {
  termBin = "${inputs.st.packages.${system}.default}/bin/st";
  tmuxBin = "${inputs.tmux.packages.${system}.default}/bin/tmux";
  toggle-redshift =
    let
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
    # ${pkgs.xcape}/bin/xcape -e "ISO_Level3_Shift=backslash"
    # ${pkgs.xcape}/bin/xcape -e "Control_L=return"
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
    ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
  '';
  toggle-backlight =
    let
      light = "${pkgs.light}/bin/light";
    in
    pkgs.writeShellScriptBin "toggle-backlight" ''
      PERCENT=$(${light} -G)
      if [ "$PERCENT" != "0.00" ]; then
          ${light} -S 0
      else
          ${light} -S 30
      fi
    '';
  swap-clipboards =
    let
      xclip = "${pkgs.xclip}/bin/xclip";
    in
    pkgs.writeShellScriptBin "swap-clipboards" ''
      OLD_PRIMARY="`${xclip} -selection PRIMARY -o`"
      OLD_CLIPBOARD="`${xclip} -selection CLIPBOARD -o`"
      echo -n "$OLD_PRIMARY" | ${xclip} -selection CLIPBOARD -i
      echo -n "$OLD_CLIPBOARD" | ${xclip} -selection PRIMARY -i
      ${tmuxBin} set-buffer "$OLD_PRIMARY"
    '';
  toggle-notifications =
    let
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

  rofii = pkgs.rofi.overrideAttrs (
    final: prev: {
      nativeBuildInputs = prev.nativeBuildInputs ++ [
        pkgs.makeWrapper
      ];
      postInstall = ''
        wrapProgram $out/bin/rofi \
          --prefix PATH : $HOME/bin
      '';
    }
  );
  rofi = pkgs.stdenv.mkDerivation rec {
    name = "rofi-custom";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    phases = [ "installPhase" ];

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
  mic-status = pkgs.writeShellScriptBin "mic-status" ''
    MUTED=$(${pkgs.wireplumber}/bin/wpctl get-volume @DEFAULT_AUDIO_SOURCE@ | grep "MUTED")
    if [ $? -eq 0 ]; then
        echo "%{F#fb4934}%{u-}"
    else
        echo ""
    fi
  '';
  toggle-noise = pkgs.writeShellScriptBin "toggle-noise" ''
    systemctl --user is-active --quiet noise && systemctl --user stop noise || systemctl --user start noise
  '';
}
