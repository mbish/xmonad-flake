{
  pkgs,
  lib,
  inputs,
  utils,
  ...
}: let
  templateFile = name: template: data:
    pkgs.stdenv.mkDerivation {
      name = "${name}";

      nativeBuildInpts = [pkgs.mustache-go];

      # Pass Json as file to avoid escaping
      passAsFile = ["jsonData"];
      jsonData = builtins.toJSON data;

      # Disable phases which are not needed. In particular the unpackPhase will
      # fail, if no src attribute is set
      phases = ["buildPhase" "installPhase"];

      buildPhase = ''
        ${pkgs.mustache-go}/bin/mustache $jsonDataPath ${template} > rendered_file
      '';

      installPhase = ''
        cp rendered_file $out
      '';
    };
  notification-status = pkgs.writeShellScriptBin "notification-status" ''
    if [ $(${pkgs.dunst}/bin/dunstctl is-paused) == "true" ]; then
        echo "%{F#fb4934}%{u-}"
    else
        echo ""
    fi
  '';
  systemctl = "${pkgs.systemdMinimal}/bin/systemctl";
  toggle-redshift = pkgs.writeShellScriptBin "toggle-redshift" ''
    #!/bin/env bash
    (${systemctl} --user is-active redshift && ${systemctl} --user stop redshift) || (${systemctl} --user start redshift)
  '';
  polybar-config = templateFile "config.ini" ./config.ini {
    inherit (utils) toggle-notifications;
    xmonad-log = "${pkgs.xmonad-log}/bin/xmonad-log";
    notification-status = "${notification-status}/bin/notification-status";
    mic-status = "${utils.mic-status}/bin/mic-status";
    toggle-mic = "${utils.toggle-mic}/bin/toggle-mic";
    toggle-redshift = "${toggle-redshift}/bin/toggle-redshift";
    sensor-path = "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp2_input";
  };
  fonts = [
    pkgs.unifont 
    "${pkgs.liberation_ttf}/share/fonts" 
    (builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts))
    "${pkgs.dejavu_fonts}/share/fonts" 
    "${pkgs.noto-fonts}/share/fonts"
  ];
  font_config = pkgs.makeFontsConf {
    fontDirectories = fonts;
  };
in
  pkgs.writeShellScriptBin "launch-polybar" ''
    # Terminate already running bar instances
    pkill -f /bin/polybar

    # wait until the processes have been shut down
    while ${pkgs.toybox}/bin/pgrep -x polybar >/dev/null; do sleep 1; done
    echo ${pkgs.dejavu_fonts}
    echo ${pkgs.noto-fonts}

    # launch polybar
    if [ ''${#@} -eq 0 ]; then
      MONITORS=$(${pkgs.xorg.xrandr}/bin/xrandr --listactivemonitors|head -n1|cut -f2 -d' ')
      if [ "$MONITORS" = "1" ]; then
          FONTCONFIG_FILE=${font_config} ${pkgs.polybarFull}/bin/polybar -c ${polybar-config} laptop &
      else
          FONTCONFIG_FILE=${font_config} ${pkgs.polybarFull}/bin/polybar -c ${polybar-config} desktop &
      fi
    else
      FONTCONFIG_FILE=${font_config} ${pkgs.polybarFull}/bin/polybar -c ${polybar-config} $@ &
    fi
    echo "Bars launched..."
  ''
