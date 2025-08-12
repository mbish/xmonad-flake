{
  description = "xmonad";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    st = {
      url = "github:mbish/st-flake";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    tmux = {
      url = "github:mbish/tmux-flake";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    zsh = {
      url = "github:mbish/zsh-flake";
      flake = true;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs: let
    perSystem = system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          overlay
          inputs.nixgl.overlay
        ];
      };
      utils = pkgs.callPackage ./utils.nix {
        inherit inputs;
      };
      xmonadBuildScript = pkgs.writeShellScriptBin ''
        install -m 755 $out/bin/xmonad ~/.xmonad
      '';
      overlay = final: prev: {
        haskell =
          prev.haskell
          // {
            packageOverrides = hfinal: hprev:
              prev.haskell.packageOverrides hfinal hprev
              // {
                my-xmonad = (
                  hfinal.callCabal2nix ./my-xmonad.cabal ./. {}
                );
              };
          };
        xmonad = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.my-xmonad;
      };
      polybar = pkgs.callPackage ./polybar {inherit inputs utils;};
    in {
      packages = {
        test = utils.consoleStartup;
      };
      defaultPackage = let
        packagesInExe = [
          inputs.st.packages.${system}.default
          inputs.tmux.packages.${system}.default
          pkgs.findutils
          pkgs.anki
          pkgs.dunst
          pkgs.gimp
          pkgs.glibcLocales
          pkgs.nodejs_22
          pkgs.pavucontrol
          pkgs.polybarFull
          pkgs.python3
          pkgs.xautolock
          utils.qutebrowser
          pkgs.remmina
          pkgs.rofi
          pkgs.systemdMinimal
          pkgs.thunderbird
          pkgs.tmuxinator
          pkgs.procps
          pkgs.xorg.xbacklight
          utils.consoleStartup
          utils.launchTerminal
          utils.setupScripts
          utils.swap-clipboards
          utils.toggle-backlight
          utils.toggle-mic
          utils.toggle-notifications
          utils.toggle-redshift
          utils.rofi
          utils.keyboard-layout
          pkgs.light
          polybar
        ];
      in
        pkgs.xmonad.overrideAttrs (final: prev: {
          postInstall = ''
            cp ${utils.setupScripts}/bin/setupScripts $out/bin
            cp ${utils.launchTerminal}/bin/launchTerminal $out/bin
            cp ${utils.consoleStartup}/bin/consoleStartup $out/bin
            cp ${utils.toggle-redshift}/bin/toggle-redshift $out/bin
            cp ${utils.toggle-mic}/bin/toggle-mic $out/bin
            cp ${utils.toggle-notifications}/bin/toggle-notifications $out/bin
            cp ${utils.toggle-backlight}/bin/toggle-backlight $out/bin
            cp ${utils.swap-clipboards}/bin/swap-clipboards $out/bin
            cp ${polybar}/bin/launch-polybar $out/bin
            cp ${utils.rofi}/bin/rofi $out/bin
            cp ${utils.keyboard-layout}/bin/keyboard-layout $out/bin
            cp ${utils.mic-status}/bin/mic-status $out/bin;
            mv $out/bin/my-xmonad $out/bin/xmonad-${system}
            wrapProgram $out/bin/xmonad-${system} \
              --prefix PATH : ${pkgs.lib.makeBinPath packagesInExe} \
              --set LOCALE_ARCHIVE ${pkgs.glibcLocales}/lib/locale/locale-archive
            echo -e "#!/usr/bin/env bash\ninstall -m 755 $out/bin/xmonad-${system} ~/.xmonad" > $out/bin/build
            chmod u+x $out/bin/build
          '';
          nativeBuildInputs = [
            pkgs.makeWrapper
            pkgs.glibcLocales
            pkgs.qt5.wrapQtAppsHook
          ];
        });
    };
    systems = ["x86_64-linux" "aarch64-linux"];
  in
    inputs.flake-utils.lib.eachSystem systems perSystem;
}
