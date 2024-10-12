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
  mic = "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_TS_2018_10_13_53845-00.analog-stereo";
  speakers = "alsa_output.usb-Lenovo_ThinkPad_Thunderbolt_3_Dock_USB_Audio_000000000000-00.analog-stereo";
  notification-status = pkgs.writeShellScriptBin "notification-status" ''
    if [ $(${pkgs.dunst}/bin/dunstctl is-paused) == "true" ]; then
        echo "%{F#fb4934}%{u-}"
    else
        echo ""
    fi
  '';
  toggle-mic = pkgs.writeShellScriptBin "toggle-mic" ''
    ${pkgs.pulseaudio}/bin/pactl set-source-mute "${mic}" toggle
  '';
  systemctl = "${pkgs.systemdMinimal}/bin/systemctl";
  toggle-redshift = pkgs.writeShellScriptBin "toggle-redshift" ''
    #!/bin/env bash
    (${systemctl} --user is-active redshift && ${systemctl} --user stop redshift) || (${systemctl} --user start redshift)
  '';
  polybar-config = templateFile "config.ini" ./config.ini {
    inherit speakers;
    inherit (utils) toggle-notifications;
    xmonad-log = "${pkgs.xmonad-log}/bin/xmonad-log";
    notification-status = "${notification-status}/bin/notification-status";
    mic-status = "${utils.mic-status}/bin/mic-status";
    toggle-mic = "${toggle-mic}/bin/toggle-mic";
    toggle-redshift = "${toggle-redshift}/bin/toggle-redshift";
    sensor-path = "/sys/devices/platform/coretemp.0/hwmon/hwmon3/temp2_input";
  };
  fonts = [pkgs.unifont "${pkgs.liberation_ttf}/share/fonts" pkgs.nerdfonts "${pkgs.dejavu_fonts}/share/fonts" "${pkgs.noto-fonts}/share/fonts"];
  font_config = pkgs.makeFontsConf {
    fontDirectories = fonts;
  };
in
  pkgs.writeShellScriptBin "launch-polybar" ''
    # Terminate already running bar instances
    killall -q polybar

    # wait until the processes have been shut down
    while ${pkgs.toybox}/bin/pgrep -x polybar >/dev/null; do sleep 1; done
    echo ${pkgs.dejavu_fonts}
    echo ${pkgs.noto-fonts}

    # launch polybar
    MONITORS=$(${pkgs.xorg.xrandr}/bin/xrandr --listactivemonitors|head -n1|cut -f2 -d' ')

    if [ "$MONITORS" = "1" ]; then
        FONTCONFIG_FILE=${font_config} ${pkgs.polybarFull}/bin/polybar -c ${polybar-config} laptop &
    else
        FONTCONFIG_FILE=${font_config} ${pkgs.polybarFull}/bin/polybar -c ${polybar-config} desktop &
    fi
    echo "Bars launched..."
  ''
