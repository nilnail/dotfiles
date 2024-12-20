#lpadmin -p laserjetV  -m laserjet.ppd -v parallel -D “HP LaserJet V” -L “Office 502/ Edit this configuration file to define what should be installed on
#''; your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-24.05.tar.gz";
  asahi-support = builtins.fetchTarball "https://github.com/tpwrules/nixos-apple-silicon/archive/main.tar.gz";
  unstable = import <unstable> { config = {allowUnfree = true;};};
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./apple-silicon-support
      #(import "${asahi-support}/apple-silicon-support")
      (import "${home-manager}/nixos")
    ];

  nix = {
    settings = {
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    };
  nixpkgs = {
    config = {
      packageOverrides = pkgs: { nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};};
      allowUnfree = true;
      allowBroken = true;
      permittedInsecurePackages = [
        "nix-2.15.3"
      ];
    };
  };

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = false;
  };

  fileSystems = {
    "/".label = "nixos";
    "/home".label = "home";
  };
  
  
  time.timeZone = "America/Toronto";

  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
    hostName = "neilsmacbook"; # Define your hostname.
  };

  hardware = {
    bluetooth = {
      enable = true;
    };
    asahi = {
      enable = false;
      withRust = false;
      useExperimentalGPUDriver = false;
      experimentalGPUInstallMode = "replace";
    };
    opengl = {
      enable = false;
      # extraPackages = [ pkgs.mesa-asahi-edge.drivers ];
    };
    opentabletdriver = {
        enable = false;
        daemon.enable = true;
        package = pkgs.opentabletdriver;
    };
  };

  services = {
    blueman.enable = true;
    dbus.enable = true;
    flatpak.enable = false;
    printing.enable = false;
    printing.drivers = [pkgs.hplip pkgs.epson-escpr];
    avahi = {
      enable = true;
      nssmdns4 = true;
    };
      
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    greetd = {
        enable = true;
        package = pkgs.greetd.greetd;
        settings = {
          default_session = {
            command = "${pkgs.greetd.greetd}/bin/agreety ";
          };
          initial_session = {
            command = "${pkgs.sway}/bin/sway ";
            # command = "sway";
            user = "neil";
          };
        };
      };

    gvfs.enable = true;
    tumbler.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  security.rtkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’1
  users = {
    mutableUsers = false;
    users = {
      root = {
        hashedPassword = "$6$dD.jU33HdH1vk3N4$qPepCTDgJln/E1nWrlsncWLNhewSj6J/6WUU21VZYjG4779JAFXcvaDXRHcKguG4daQHKxs5pLoD6F.grjxgv/";
        extraGroups = ["wheel" "input" "dialout" "i2p"];
      };
      neil = {
        isNormalUser = true;
        hashedPassword = "$6$dD.jU33HdH1vk3N4$qPepCTDgJln/E1nWrlsncWLNhewSj6J/6WUU21VZYjG4779JAFXcvaDXRHcKguG4daQHKxs5pLoD6F.grjxgv/";
        extraGroups = ["wheel" "dialout" "input" "i2p"];
      };
    };
  };

 
  fonts.packages = with pkgs; [
    #nerdfonts
    liberation_ttf
  ];

  programs = {
    dconf.enable = true;
    gamemode.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };
 
  ## PACKAGES
  ## ENVIRONMENT VARIABLES

  environment = {
    variables = {
      MOZ_ENABLE_WAYLAND="1";
      QT_QPA_PLATFORM="wayland";
     # SDL_VIDEODRIVER="wayland";
      NIXPKGS_ALLOW_UNFREE="1";
     SWAY_CURSOR_THEME="Adwaita";
 #    WLR_RENDERER="vulkan";
     LIBSEAT_BACKEND="logind";
  #    QT_STYLE_OVERRIDE = "kvantum";
  #    QT_QPA_PLATFORMTHEME = "plastique";

    };
    sessionVariables = {
      XDG_CURRENT_DESKTOP="sway";
      XDG_SESSION_TYPE="wayland";
      XDG_SESSION_DESKTOP="sway";
      DEFAULT_BROWSER="${pkgs.firefox}/bin/firefox";
      BROWSER="${pkgs.firefox}/bin/firefox";
    };
    systemPackages = with pkgs; [
      vim
      font-manager
      #rclone
      rofi-wayland
      #unstable.niri
      glib
      unzip
      lsof
      dbus-glib
      wget
      busybox
      #freecad
     # kicad-small
      dbus
      musl
      glibc
      lld
      binutils
      # qt6.qtwayland
      # libsForQt5.qt5.qtwayland
      # libsForQt5.qtstyleplugins
      # texlab
      # #obs-studio
      # pipewire
      # wireplumber
      # pciutils
      # polkit_gnome
      # #firefox-wayland
      # icu
      # icu72
      # baobab
      # unstable.legcord
      # dotool
      # htop
      # tetex
      # cmake
      # #librewolf
      # tor
      # planner
      # taisei
      # blueman
      # system-config-printer
      # wlroots
      # libreoffice
      #plover.dev
      #nur.repos.darkkirb.plover
      #nur.repos.federicoschonborn.gimp3
      #unstable.grapejuice
      #unstable.webcord
      #unstable.wineWowPackages.waylandFull
      #vkd3d
      #vkd3d-proton
      # seatd
      # swaylock
      # remmina
      # swayidle
      # wl-clipboard
      # mako
      # waybar
      # git
      # killall
      # sway-contrib.grimshot
      # grim
      # wev
      # slurp
      # xorg.xlsclients
      # #unstable.webcord
      # libhandy
      # keychain
      # miniaudio
      # libopus
      # libsodium
      # rnnoise
      # foliate
      # gnome-disk-utility
      # gnome-online-accounts-gtk
      # gnome-system-monitor
      # gnome-clocks
      # nodePackages.yarn
      # evince
      # gimp
      # mypaint
      # brightnessctl
      # pavucontrol
      #bitwarden-desktop
      #lmms
      #prismlauncher
      helvum
   #   thunderbird
      xorg.xkbcomp
      xorg.xmodmap
      pmount
      #chromium
   #   prusa-slicer
      findutils
      ripgrep
      openssl
      #lutris
      #blender
      #kdenlive
      gamemode
      #winetricks
      gst_all_1.gstreamer
      mono
      autoconf
      perl
      #opentabletdriver
   #   transmission_4-gtk
      #mpv
   #   svt-av1
      libaom
      dav1d
      xfce.thunar
      xfce.xfconf
      xfce.exo
      xfce.catfish
      fsearch
      gvfs
      gvfs
      qalculate-gtk
      nix-index
#      (haskellPackages.ghcWithPackages (pkgs: [ pkgs.random]))
#      ghcid
#      ghc_filesystem
   #   audacity
   #   lollypop
     # file-roller
      gnumake
      gcc
      #xine-lib
    #   (let
    # my-python-packages = python-packages: with python-packages; [
    #   pillow
    #   pip
    #   pipBuildHook
    #   pipInstallHook
    #   pandas
    #   matplotlib
      #wineWowPackages.fonts
      noto-fonts-cjk-sans
      corefonts
      font-manager
  #     python-lsp-server
  #     jupyter

  #      #other python packages you want
  #   ];
  #   python-with-my-packages = python3.withPackages my-python-packages;
  # in
  # python-with-my-packages)
     #  xf86_input_wacom
     #  libwacom
     #  libnotify
     #  ipafont
     #  ipaexfont
     #  hanazono
     #  libsForQt5.ark
     # # superTuxKart
     #  tree
     #  gst_all_1.gstreamer
     #  gst_all_1.gst-vaapi
     #  libgudev
     #  speex
     #  neofetch
     #  xdg-utils
     #  arduino
     #  arduino-core
     #  arduino-mk
     #  arduino-cli
    ];
  };

  xdg = {
    mime = {
      enable = true;
      defaultApplications = {
        "text/html" = "firefox.desktop";
        "x-scheme-handler/http" = "firefox.desktop";
        "x-scheme-handler/https" = "firefox.desktop";
        "x-scheme-handler/about" = "firefox.desktop";
        "x-scheme-handler/unknown" = "firefox.desktop";
      };
    };
    portal = {
      config = {
        common = {
          default = [
            "wlr"
          ];
        };
      };
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
      ];
      wlr = {
        enable = true;
        settings = {
           screencast = {
             output_name = "eDP-1";
             max_fps = 30;
             exec_before = "disable_notifications.sh";
             exec_after = "enable_notifications.sh";
             chooser_type = "simple";
             chooser_cmd = "${pkgs.slurp}/bin/slurp -f %o -or";
        };
      };
    };
  };
  };

  qt = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
    };

  system.stateVersion = "24.05"; 

### HOME MANAGER


  home-manager.users.neil =
    let
## COLOURS
      mainC = "232634";
      LmainC = "303446";
      MLmainC = "3a3e4f";
      LLmainC = "404455";
      ULmainC = "4f5262";
      LborderC = "484c5c";
      DborderC = "1b1e2b";
      accentC = "8caaee";
      textC = "ffffff";

    in {pkgs, ...}: {
    nixpkgs = {
        config = {
          packageOverrides = pkgs: { nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {inherit pkgs;};};
          allowUnfree = true;
          allowBroken = true;
          permittedInsecurePackages = [
            "nix-2.15.3"
          ];
        };
      };

    home.stateVersion = "24.05";


## XDG
    xdg = {
      enable = true;
      userDirs = {
        enable = true;
        createDirectories = true;
        extraConfig = {ORG_ROAM_DIR = "$HOME/org-roam";};
      };
    };  

  
## THEMES
    gtk.enable = true;
    gtk.iconTheme.package = pkgs.kora-icon-theme;
    gtk.iconTheme.name = "kora";
    gtk.theme.package = pkgs.catppuccin-gtk;
    gtk.theme.name = "catppuccin-frappe-blue-standard";
    home.pointerCursor = {
      package = pkgs.bibata-cursors-translucent;
      gtk.enable = true;
      name = "Bibata_Ghost";
      size = 22;
      x11.enable = true;
    };

## EMACS
    programs.emacs = {
      enable = false;
      package = pkgs.emacs29-pgtk;
      extraPackages = epkgs: [
epkgs.use-package
epkgs.ssh
epkgs.command-log-mode
epkgs.ivy
epkgs.doom-modeline
epkgs.doom-themes
epkgs.helpful
epkgs.rainbow-delimiters
epkgs.which-key
epkgs.ivy-rich
epkgs.counsel
epkgs.all-the-icons
epkgs.general
epkgs.evil
epkgs.evil-collection
epkgs.hydra
epkgs.auto-sudoedit
epkgs.arduino-mode
epkgs.arduino-cli-mode
epkgs.company
epkgs.company-racer
epkgs.flycheck
epkgs.flycheck-rust
epkgs.rust-mode
# epkgs.irony
# epkgs.company-irony
# epkgs.company-c-headers
epkgs.spinner
epkgs.haskell-mode
epkgs.python-mode
epkgs.typescript-mode
epkgs.clojure-mode
epkgs.nov
epkgs.nix-mode
epkgs.org
epkgs.org-roam
      ];
      extraConfig =
        ''
(setq inhibit-startup-message 1)
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure 0)
(use-package command-log-mode)

(column-number-mode 1)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package all-the-icons)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package company
  :init
  (global-company-mode 1))

(setq company-idle-delay 0.2)

(setq company-minimum-prefix-length 1)

(setq racer-cmd "/run/current-system/sw/bin/racer")
(setq racer-rust-src-path "/nix/store/j4iyf93342vhp8dhlpdrllwlargk1s93-rust-src/library")
(add-to-list 'auto-mode-alist '("\.rs\'" . rust-mode))
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common) ;
(setq company-tooltip-align-annotations t)

(use-package auto-sudoedit
  :init
  (auto-sudoedit-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-fucntion #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap descrip-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(general-define-key
  :keymaps 'global
"C-c r s" 'org-roam-node-find
"C-c r r" 'rclone-sync
"C-c r i" 'org-roam-node-insert)

(general-define-key
  :keymaps 'org-mode-map
"C-c r s" 'org-roam-node-find
"C-c r r" 'rclone-sync
"C-c r i" 'org-roam-node-insert)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-window-map (kbd "M-w") 'tear-off-window)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package nix-mode
  :mode "\\.nix\\'")

(use-package python-mode
  :mode "\\.py\\'")

(use-package haskell-mode
  :mode "\\.hs\\'"
  :config (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package org
  :mode ("\\.org\\'" . org-mode))

(setq org-roam-directory (file-truename "/home/neil/org-roam"))
(setq fint-file-visit-truename t)
(org-roam-db-autosync-mode)

(defun rclone-sync ()
"bisync the org-roam directory with the one on google drive"
(interactive)
  (shell-command "rclone bisync /home/neil/org-roam remote:org-roam/"))
        '';
    };

   services = {
      emacs = {
        enable = true;
        client = {
          enable = true;
          arguments = [ "-c" ];
        };
      };
   };


## FIREFOX
    programs.firefox = {
      enable = false;
      profiles.nixos = {
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          enhancer-for-youtube
          ublock-origin
          translate-web-pages
          search-by-image
        ];
        bookmarks = {};
        extraConfig = "";
        id = 0;
        isDefault = true;
        name = "nixos";
        path = "nixos.default";
        settings = {
          "browser.compactmode.show" = true;
          "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePined" = "google";
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.activity-stream.topSitesRows" = 2;
          "browser.shell.checkDefaultBrowser" = false;
          "browser.uidensity" = 1;
          "browser.urlbar.placeholderName" = "DuckDuckGo";
          "browser.urlbar.placeholderName.private" = "DuckDuckGo";
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.toolbars.bookmarks.visibility" = "never";
          "browser.uiCustomisation.state" = "{'placements':{'widget-overflow-fixed-list':[;downloads-button','fxa-toolbar-menu-button','library-button','_2e5ff8c8-32fe-46d0-9fc8-6b8986621f3c_-browser-action'],'nav-bar':['back-button','forward-button','urlbar-container','stop-reload-button'],'toolbar-menubar':['menubar-items'],'TabsToolbar':['tabbrowser-tabs','new-tab-button','alltabs-button'],'PersonalToolbar':[]},'seen':['save-to-pocket-button','developer-button','_2e5ff8c8-32fe-46d0-9fc8-6b8986621f3c_-browser-action'],'dirtyAreaCache':['nav-bar','PersonalToolbar','toolbar-menubar','TabsToolbar','widget-overflow-fixed-list'],'currentVersion':17,'newElementCount':5}";
        };
        userChrome = ''
:root {
  --navbarWidth     : 40vw; /* Set width of navbar. Use px for a fixed width or vw for a percentage of your window. */

  --animationSpeed  : 0.15s;
}
#back-button[disabled="true"] { display: none !important }
#forward-button[disabled="true"] { display: none !important }
#tracking-protection-icon-container { display: none !important }
#identity-box { display: none !important }
#TabsToolbar {
  margin-left : var(--navbarWidth) !important;
}
#nav-bar {
  margin-right: calc(100vw - var(--navbarWidth)) !important;
}
#urlbar-container {
  min-width   : 0px !important;
}
:root {
  --tab-block-margin: 0 !important;
  --toolbarbutton-border-radius: 0 !important;
  --tab-border-radius: 0 !important;
}
#context-navigation:not([hidden]) {
  padding: 0 0 0px !important;
}
toolbarbutton.bookmark-item:not(.subviewbutton) {
  margin: 0 !important;
}
#star-button-box, #urlbar-zoom-button {display: none !important}
:root {
  --tab-toolbar-navbar-overlap: 0px !important;
  --tab-min-height: 28px !important;
}
:root[uidensity="compact"] #nav-bar {
  margin-top  : -40px !important;
  height      : 28px !important;
}
:root:not([uidensity="compact"]):not([uidensity="touch"]) #nav-bar {
  margin-top  : -40px !important;
  height      : 28px !important;
}
:root[uidensity="touch"] #nav-bar {
  margin-top  : -49px !important;
  height      : 49px !important;
}
:root[sizemode="maximized"] #TabsToolbar {
  margin-top: 0px;
  background: transparent !important;
}
#TabsToolbar {
  margin-top: 0px;
}
#nav-bar {
  background  : none !important;
  box-shadow  : none !important;
}
#navigator-toolbox {
  border      : none !important;
}
.titlebar-spacer {
  display     : none !important;
}
.tabbrowser-tab {
  min-height: 20px !important;
}
#TabsToolbar { height: 20px !important; }

#urlbar-background {
  border      : none !important;
}
#urlbar:not(:hover):not([breakout][breakout-extend]) > #urlbar-background {
  box-shadow  : none !important;
  background  : none !important;
}
.tabbrowser-tab .tab-close-button {
  visibility: collapse !important;
}
.tab-text,
.tab-label,
.tab-throbber,
.tab-sharing-icon-overlay,
.tab-icon-overlay,
.tab-icon-sound,
.tab-icon-image {
  margin-top  : -10px !important;
}
.urlbar-icon, #userContext-indicator, #userContext-label {
  fill        : transparent !important;
  background  : transparent !important;
  color       : transparent !important;
}
#urlbar:hover .urlbar-icon,
#urlbar:active .urlbar-icon,
#urlbar[focused] .urlbar-icon {
  fill        : var(--toolbar-color) !important;
}
toolbarbutton,
.toolbarbutton-icon,
.subviewbutton,
#urlbar-background,
.urlbar-icon,
#userContext-indicator,

#userContext-label,
.urlbar-input-box,
#identity-box,
#tracking-protection-icon-container,
[anonid=urlbar-go-button],
.urlbar-icon-wrapper,
#tracking-protection-icon,
#identity-box image,
stack,
vbox,
tab:not(:active) .tab-background,
tab:not([beforeselected-visible])::after,
tab[visuallyselected] .tab-background::before,
tab[visuallyselected] .tab-background::before,
.tab-close-button {
  transition  : var(--animationSpeed) !important;
}
'';
      };
    };

## SWAY
    wayland.windowManager.sway = {
      checkConfig = false;
      enable = false;
      swaynag.enable = true;
      extraConfig = ''
bindgesture swipe:right workspace next 
bindgesture swipe:left workspace prev 
'';
      config = {
        fonts = {
          names = [ "pango:FuraCode Nerd Font"];
          size = 11.0;
        };
        output = {
            "eDP-1" = {
              mode = "2560x1600@60hz";
              background = "/home/neil/Pictures/wp7.png fill";
          };       
        };
        colors = {
          focused = { background = ULmainC; border = DborderC; childBorder = accentC; indicator = accentC; text = textC; };
          focusedInactive = { background = LLmainC; border = DborderC; childBorder = LborderC; indicator = accentC; text = textC; };
          unfocused = { background = LmainC; border = DborderC; childBorder = DborderC; indicator = accentC; text = textC; };
        };
        gaps.smartBorders = "on";
        window = {
          titlebar = false;
          hideEdgeBorders = "both";
        };
        input = {
          "*" = {
            xkb_layout = "custom";
            xkb_options = "ctrl:hyper_capscontrol,shift:both_capslock";
          };
          "*" = { accel_profile = "flat"; };
         "1386:890:Wacom_One_by_Wacom_S_Pen" = {
           events = "enabled";
           map_to_output = "eDP-1";
          };
         "0:0:OpenTabletDriver_Virtual_Artist_Tablet" = {
            events = "enabled";
         };
         "1452:641:Apple_Internal_Keyboard_/_Trackpad" = {
           events = "enabled";
           click_method = "clickfinger";
           scroll_factor = "0.75";
           scroll_method = "two_finger";
           tap = "enabled";
           tap_button_map = "lrm";
         };
           
        };
        seat = { seat0 = { xcursor_theme = "Bibata_Ghost 28"; }; };
        modifier = "Mod5";
        menu = "${pkgs.rofi-wayland}/bin/rofi";
        terminal = "/home/neil/.nix-profile/bin/footclient";
        startup = [
            { command = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";}
           # { command = "${pkgs.swaylock}/bin/swaylock -i ";}
            { command = "${pkgs.mako}/bin/mako";}
            { command = "${pkgs.waybar}/bin/waybar";}
           # { command = "${unstable.swaycons}/bin/swaycons";}
                 ];
        down = "j";
        up = "k";
        left = "h";
        right = "l";
        bars = [];
        modes = {
          lockme = {
            "Mod4+Alt+n" = "mode default";
          };
        };
        keybindings =
          let
            mod = "Mod5";
            launcher = "${pkgs.rofi-wayland}/bin/rofi";
          in {
            "Mod4+Alt+n" = "mode lockme";
            "${mod}+1" = "workspace 1";
            "${mod}+2" = "workspace 2";
            "${mod}+3" = "workspace 3";
            "${mod}+4" = "workspace 4";
            "${mod}+5" = "workspace 5";
            "${mod}+6" = "workspace 6";
            "${mod}+7" = "workspace 7";
            "${mod}+8" = "workspace 8";
            "${mod}+9" = "workspace 9";
            "${mod}+shift+1" = "move to workspace 1";
            "${mod}+shift+2" = "move to workspace 2";
            "${mod}+shift+3" = "move to workspace 3";
            "${mod}+shift+4" = "move to workspace 4";
            "${mod}+shift+5" = "move to workspace 5";
            "${mod}+shift+6" = "move to workspace 6";
            "${mod}+shift+7" = "move to workspace 7";
            "${mod}+shift+8" = "move to workspace 8";
            "${mod}+shift+9" = "move to workspace 9";
            "${mod}+Mod4+Up" = "workspace next";
            "${mod}+Mod4+Down" = "workspace prev";
            "Mod4+z" = "exec echo key control+z | dotool";
            "${mod}+Return" = "exec /home/neil/.nix-profile/bin/footclient";
            "${mod}+Shift+q" = "kill";
            "${mod}+d" = "exec ${launcher} -show drun";
            "${mod}+Shift+c" = "reload";
            "${mod}+Left" = "focus left";
            "${mod}+h" = "focus left";
            "${mod}+Up" = "focus up";
            "${mod}+k" = "focus up";
            "${mod}+Down" = "focus down";
            "${mod}+j" = "focus down";
            "${mod}+Right" = "focus right";
            "${mod}+l" = "focus right";
            "${mod}+Shift+Left" = "move left";
            "${mod}+Shift+h" = "move left";
            "${mod}+Shift+Up" = "move up";
            "${mod}+Shift+k" = "move up";
            "${mod}+Shift+Down" = "move down";
            "${mod}+Shift+j" = "move down";
            "${mod}+Shift+Right" = "move right";
            "${mod}+Shift+l" = "move right";
            "${mod}+b" = "splith";
            "${mod}+v" = "splitv";
            "${mod}+s" = "layout stacking";
            "${mod}+w" = "layout tabbed";
            "${mod}+e" = "layout toggle split";
            "${mod}+f" = "fullscreen";
            "${mod}+Shift+space" = "floating toggle";
            "${mod}+space" = "focus mode_toggle";
            "${mod}+a" = "focus parent";
            "${mod}+ctrl+c" = "exec ${pkgs.qalculate-gtk}/bin/qalculate-gtk";
            "${mod}+ctrl+e" = "exec emacsclient -c ";
            "${mod}+ctrl+f" = "exec ${pkgs.firefox}/bin/firefox";
            "${mod}+ctrl+s" = "exec ${pkgs.xfce.thunar}/bin/thunar";
            "Mod4+1" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
            "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
            "Mod4+2" = "exec ${pkgs.playerctl}/bin/playerctl previous";
            "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
            "Mod4+3" = "exec ${pkgs.playerctl}/bin/playerctl next";
            "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
            "XF86AudioRaiseVolume" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Master 3%+";
            "XF86AudioLowerVolume" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Master 3%-";
            "XF86AudioMute" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Master togglemute";
            "XF86AudioMicMute" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Capture togglemute";
            "Mod4+XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10%";
            "Mod4+XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";
            "--release XF86LaunchA" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save screen $HOME/Pictures/Screenshots/screenshot-$(${pkgs.coreutils}/bin/date +%B_%d_%Y_at_%H:%M:%S).png";
            "--release ${mod}+XF86LaunchA" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save active $HOME/Pictures/Screenshots/screenshotWindow-$(${pkgs.coreutils}/bin/date +%B_%d_%Y_at_%H:%M:%S).png";
            "--release shift+XF86LaunchA" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save area $HOME/Pictures/Screenshots/screenshotArea-$(${pkgs.coreutils}/bin/date +%B_%d_%Y_at_%H:%M:%S).png";
            "--release ctrl+shift+XF86LaunchA" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";
            "--release ctrl+${mod}+XF86LaunchA" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy active";
            "--release ctrl+XF86LaunchA" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy screen";
          };
      };
    };

## MAKO
    services.mako = {
      enable = true;
      actions = true;
      anchor = "top-right";
      backgroundColor = "#${LmainC}";
      borderColor = "#${accentC}";
      borderRadius = 0;
      borderSize= 3;
      #font =
      icons = true;
      output = "eP-1";
      extraConfig = "icon-location=right\ntext-alignment=center";
    };

## ALACRITTY
    # programs.alacritty = {
    #   enable = true;
    #   package = pkgs.alacritty;
    #   settings = {
    #     colors = {
    #     primary = {
    #         background = "#${mainC}";
    #         foreground = "#${textC}";
    #       };
    #       normal = {
    #         black = "#191E2A";
    #         red = "#FF3333";
    #         green = "#BAE67E";
    #         yellow = "#FFA759";
    #         blue = "#73D0FF";
    #         magenta = "#FFD580";
    #         cyan = "#95E6CB";
    #         white = "#C7C7C7";
    #       };
    #       bright = {
    #         black = "#686868";
    #         red = "#F27983";
    #         green = "#A6CC70";
    #         yellow = "#FFCC66";
    #         blue = "#5CCFE6";
    #         magenta = "#FFEE99";
    #         cyan = "#95E6CB";
    #         white = "#FFFFFF";
    #       };
    #     };

    #   };
    # };

## FOOT
    programs.foot = {
      enable = true;
      server.enable = true;
      settings = {
        main = {
          font = "monospace:size=11";
        };
        colors = {
          background = mainC;
          foreground = textC;
        };
      };
    };
    
    
    
## CONFIG FILES
    home.file = {
      ".config/xkb/symbols/custom".text =
        ''
xkb_symbols "basic" {
    include "us"
    name[Group1]= "English (US Custom)";
    key <CAPS> { [ Hyper_L ] };
    modifier_map Mod4 { Hyper_L };
    key <LWIN> { [ Meta_L ] };
    modifier_map Mod5 { Super_L };
    key <LALT> { [ Super_L ] };
};
'';

      ".config/OpenTabletDriver/settingsold.json".text =
        ''
{
  "Profiles": [
    {
      "Tablet": "Wacom CTL-472",
      "OutputMode": {
        "Path": "OpenTabletDriver.Desktop.Output.LinuxArtistMode",
        "Settings": [],
        "Enable": true
      },
      "Filters": [],
      "AbsoluteModeSettings": {
        "Display": {
          "Width": 1280.0,
          "Height": 800.0,
          "X": 640.0,
          "Y": 400.0,
          "Rotation": 0.0
        },
        "Tablet": {
          "Width": 152.0,
          "Height": 95.0,
          "X": 76.0,
          "Y": 47.5,
          "Rotation": 0.0
        },
        "EnableClipping": true,
        "EnableAreaLimiting": false,
        "LockAspectRatio": false
      },
      "RelativeModeSettings": {
        "XSensitivity": 10.0,
        "YSensitivity": 10.0,
        "RelativeRotation": 0.0,
        "RelativeResetDelay": "00:00:00.1000000"
      },
      "Bindings": {
        "TipActivationThreshold": 0.0,
        "TipButton": {
          "Path": "OpenTabletDriver.Desktop.Binding.MouseBinding",
          "Settings": [
            {
              "Property": "Button",
              "Value": "Left"
            }
          ],
          "Enable": true
        },
        "EraserActivationThreshold": 0.0,
        "EraserButton": null,
        "PenButtons": [
          {
            "Path": "OpenTabletDriver.Desktop.Binding.KeyBinding",
            "Settings": [
              {
                "Property": "Key",
                "Value": "Space"
              }
            ],
            "Enable": true
          },
          {
            "Path": "OpenTabletDriver.Desktop.Binding.KeyBinding",
            "Settings": [
              {
                "Property": "Key",
                "Value": "Control+Z"
              }
            ],
            "Enable": true
          }
        ],
        "AuxButtons": [],
        "MouseButtons": [],
        "MouseScrollUp": null,
        "MouseScrollDown": null
      }
    }
  ],
  "LockUsableAreaDisplay": true,
  "LockUsableAreaTablet": true,
  "Tools": []
}
'';
# ".local/share/applications/armcord.desktop".text =
# ''
# [Desktop Entry]
# Categories=Network;InstantMessaging
# Exec=${pkgs.armcord}/bin/armcord --enable-features=UseOzonePlatform --ozone-platform=wayland
# GenericName=All-in-one cross-platform voice and text chat for gamers
# Icon=discord
# MimeType=x-scheme-handler/discord
# Name=Armcord
# Type=Application
# '';

      ".config/legcord/themes/myNix/manifest.json".text =
''
{"theme":"src.css","name":"myNix","author":"Nyria#3863","description":"Declaratively Defined Discord Theming","supportsArmCordTitlebar":false}
'';

      ".config/legcord/themes/myNix/src.css".text =
''
/**
* @name myNix
*/
:root {
}
.theme-dark {
  --background-primary: #${MLmainC};
  --background-secondary: #${LmainC};
  --background-secondary-alt: #${MLmainC};
  --background-tertiary: #${mainC};
  --background-accent: #${accentC};

  --modal-background: #23283d;
  --background-mobile-primary: #23283d;
  --background-mobile-secondary: #${LmainC};
  --channeltextarea-background: #${LmainC};
  --background-message-hover: transparent;
  --background-modifier-hover: #00000010;
  --background-modifier-active: #0000001a;
  --background-modifier-selected: #0000001f;
  --deprecated-card-bg: #12141f63;
  --background-floating: #${mainC};
  --deprecated-quickswitcher-input-background: #${mainC};
  --elevation-low: none;
  --scrollbar-auto-thumb: #${mainC};
  --scrollbar-auto-track: #${LmainC};
  --scrollbar-thin-thumb: #141925;
  --activity-card-background: #${mainC};
  --input-background: #${LmainC};
}
'';
#        ".config/rclone/rclone.conf".text =
#  ''
# [remote]
# type = drive
# client_id = 1084254172415-3jkv4kfegb2bmrohnpt4kba448rgugra.apps.googleusercontent.com
# client_secret = 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
# scope = drive
# token = {"access_token":"ya29.a0AVA9y1v55UumlkqjLzoMiZfS79TAx53jnyjbc-7t1PPJwinInfYuFD8AeIPwfVWqn9LoesWNuUxZQZRHRflnVDK0EjSxDEMSOIpnTuVHYVq7-jeGSV5GaCFYzDpTlv_s9Y6zlGK1HgISAhgiJlhvRatH-kgmJwaCgYKATASAQASFQE65dr8VElsvwlRSPuYKBNWl3kjNA0165","token_type":"Bearer","refresh_token":"1//04U0pEKrAYzMzCgYIARAAGAQSNwF-L9IrcV-vjatokLGiJ7LkuNKLQQ5-fJ_BRQa1BYtRVWgw0HtvjpxPdhe6nWInUYWqW5PE40s","expiry":"2022-08-30T14:46:05.068038276-04:00"}
# team_drive =
#  '';

      ".config/waybar/config".text = 
''
{
    "layer": "top",
    "position": "top",
    "output": "eDP-1",
    "height": 20,
    "modules-left": ["sway/workspaces", "sway/mode", "sway/window"],
    "modules-center": ["clock"],
    "modules-right": ["battery", "custom/playerctl", "pulseaudio"],
    "sway/window": {
        "max-length": 50
    },
    "clock": {
        "interval": 1,
        "format": "{:%a, %d. %b  %H:%M:%S}"
    },
    "pulseaudio": {
    "format": "{volume}% {icon}",
    "format-bluetooth": "{volume}% {icon}",
    "format-muted": "",
    "format-icons": {
        "headphone": "",
        "hands-free": "",
        "headset": "",
        "phone": "",
        "portable": "",
        "car": "",
        "default": ["", ""]
    },
    "scroll-step": 1,
    "on-click": "pavucontrol"
    },
      "custom/weather": {
    "exec": "curl 'https://wttr.in/?format=1'",
    "interval": 3600
    },
}
'';
       ".config/waybar/style.css".text = 
''
* {
    border: none;
    border-radius: 0;
    /* `otf-font-awesome` is required to be installed for icons */
    font-family: Inconsolata;
    font-size: 14px;
    min-height: 0;
}

window#waybar {
    background-color: rgba(43, 48, 59, 0.5);
    border-bottom: 0px solid rgba(100, 114, 125, 0.5);
    color: #ffffff;
    transition-property: background-color;
    transition-duration: .5s;
}

window#waybar.hidden {
    opacity: 0.2;
}

/*
window#waybar.empty {
    background-color: transparent;
}
window#waybar.solo {
    background-color: #FFFFFF;
}
*/

window#waybar.termite {
    background-color: #3F3F3F;
}

window#waybar.chromium {
    background-color: #000000;
    border: none;
}

#workspaces button {
    padding: 0 5px;
    background-color: transparent;
    color: #ffffff;
    /* Use box-shadow instead of border so the text isn't offset */
    box-shadow: inset 0 -3px transparent;
}

/* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
#workspaces button:hover {
    background: rgba(0, 0, 0, 0.2);
    box-shadow: inset 0 -3px #ffffff;
}

#workspaces button.focused {
    background-color: #64727D;
    box-shadow: inset 0 -3px #ffffff;
}

#workspaces button.urgent {
    background-color: #eb4d4b;
}

#mode {
    background-color: #64727D;
    border-bottom: 3px solid #ffffff;
}

#clock,
#battery,
#temperature,
#backlight,
#network,
#pulseaudio,

#window,
#workspaces {
    margin: 0 4px;
}

/* If workspaces is the leftmost module, omit left margin */
.modules-left > widget:first-child > #workspaces {
    margin-left: 0;
}

/* If workspaces is the rightmost module, omit right margin */
.modules-right > widget:last-child > #workspaces {
    margin-right: 0;
}

#clock {
    background-color: #0d1117;
    opacity: 0.9;
}

#battery {
    background-color: #ffffff;
    color: #000000;
    opacity: 0.5;
}

#battery.charging, #battery.plugged {
    color: #ffffff;
    background-color: #26A65B;
}

@keyframes blink {
    to {
        background-color: #ffffff;
        color: #000000;
    }
}

#battery.critical:not(.charging) {
    background-color: #f53c3c;
    color: #ffffff;
    animation-name: blink;
    animation-duration: 0.5s;
    animation-timing-function: linear;
    animation-iteration-count: infinite;
    animation-direction: alternate;
}

label:focus {
    background-color: #000000;
}

#backlight {
    background-color: #90b1b1;
}

#network {
    background-color: #2980b9;
}

#network.disconnected {
    background-color: #f53c3c;
}

#pulseaudio {
    background-color: #f1c40f;
    color: #000000;
}

#pulseaudio.muted {
    background-color: #90b1b1;
    color: #2a5c45;
}


'';
  };

};
}
