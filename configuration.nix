# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  home-manager = builtins.fetchTarball "https://github.com/nix-community/home-manager/archive/release-22.05.tar.gz";
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      (import "${home-manager}/nixos")
    ];

    nix = {
      binaryCaches = [ "https://nix-gaming.cachix.org" ];
      binaryCachePublicKeys = [ "nix-gaming.cachix.org-1:nbjlureqMbRAxR1gJ/f3hxemL9svXaZF/Ees8vCUUs4=" ];
    };

    nixpkgs.config = {
      allowUnfree = true;
      };

  programs.dconf.enable = true;
  
  programs.steam.enable = true;

  nix.autoOptimiseStore = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  services.flatpak.enable = true;
  
  # Configure your filesystems
  fileSystems = {
    "/".label = "nixos";
    "/home/neil".label = "home";
    "/boot".label = "boot";
    "/home/neil/bulk".label = "bulky";
    "/home/neil/extra".label = "bulk";
  };
  
  # Use the systemd-boot EFI boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "neilspc"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Toronto";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp8s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };
  
  hardware.opengl.extraPackages = with pkgs; [
    rocm-opencl-icd
    rocm-opencl-runtime
    clinfo
  ];
  
hardware.opentabletdriver = {
    enable = true;
    daemon.enable = true;
    package = pkgs.opentabletdriver;
};

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  
  services.pipewire = {
    config.pipewire = {
      "context.properties" = {
        "link.max-buffers" = 64;
        "log.level" = 2;
        "default.clock.rate" = 384000;
      };
    };
  };

  users.users.neil = {
    isNormalUser = true;
    password = "balls";
    extraGroups = [ "wheel" "dialout" ];
  };

  boot.kernelPackages = with pkgs; linuxKernel.packages.linux_xanmod;

  boot.extraModulePackages = [
    config.boot.kernelPackages.v4l2loopback
  ];

  boot.kernelModules = [
    "v4l2loopback"
  ];


  boot.initrd.kernelModules = [
    "amdgpu"
  ];

  fonts.fonts = with pkgs; [
    nerdfonts
  ];

  ## PACKAGES

  environment.systemPackages = with pkgs; [
    vim
    rclone
    nwg-drawer
    rofi-wayland
    vulkan-validation-layers
    glib
    unzip
    lsof
    tor-browser-bundle-bin
    dbus-glib
    wget
    freecad
    kicad
    dbus
    ripcord
    bookworm
    koreader
    fbreader
    libsForQt5.qtstyleplugin-kvantum
    libsForQt5.qtstyleplugins
    qtstyleplugin-kvantum-qt4
    pipewire-media-session
    sway
    rnix-lsp
    clojure-lsp
    texlab
    nodePackages.npm
    nodePackages.typescript
    nodePackages.typescript-language-server
    nodePackages.bash-language-server
    obs-studio
    obs-studio-plugins.wlrobs
    obs-studio-plugins.obs-gstreamer
    obs-studio-plugins.obs-websocket
    beefi
    pipewire
    linuxKernel.kernels.linux_5_15
    linuxKernel.kernels.linux_xanmod
    pciutils
    polkit_gnome
    firefox-wayland
   # unstable.osu-lazer
    # unstable.armcord
    appimage-run
    icu
    (discord.override { nss = pkgs.nss; })
    gnome.nautilus
    gnome.gnome-disk-utility
    gnome.gnome-system-monitor
    gnome.gnome-tweaks
    yarn
    yarn2nix
    nodePackages.yarn
    evince
    lxappearance
    juno-theme
    gimp
    mypaint
    mypaint-brushes
    brightnessctl
    pavucontrol
    image-roll
    bitwarden
    polymc
    qt5ct
    jdk
    mesa
    helvum
    pmount
    chromium
    prusa-slicer
    mpdevil
    findutils
    ripgrep
    openssl
    lutris
    obs-studio
    blender
    gnome.eog
    kdenlive
    wineWowPackages.stagingFull
    #winePackages.staging
    gamemode
    gnome.zenity
    winetricks
    gst_all_1.gstreamer
    mono
    autoconf
    perl
    opentabletdriver
    mpd-mpris
    playerctl
    transmission
    transmission-gtk
    mpv
    xfce.thunar
    xfce.xfconf
    xfce.exo
    xfce.catfish
    fsearch
    gvfs
    gnome.gvfs
    betterdiscordctl
    glfw-wayland
    qalculate-gtk
    nix-index
    ghc
    ghcid
    #haskellPackages.ghc_9_2_1
    ghc_filesystem
    cabal2nix
    cabal-install
    haskellPackages.Cabal_3_6_3_0
    audacity-gtk3
    eww-wayland
    lollypop
    easytag
    gnome.gnome-control-center
    parted
    gparted
    tauon
    clementine
    deadbeef-with-plugins
    gnome.file-roller
    gnumake
    gcc
    xine-lib
    rhythmbox
    avizo
    celluloid
    (let
  my-python-packages = python-packages: with python-packages; [
    pillow
    pip
    pipBuildHook
    pipInstallHook
    pandas
    backports_csv
    wineWowPackages.fonts
    noto-fonts-cjk
    corefonts
    vistafonts
    ipafont
    font-manager
    python-lsp-server

     #other python packages you want
  ];
  python-with-my-packages = python3.withPackages my-python-packages;
in
python-with-my-packages)
    xf86_input_wacom
    libwacom
    zeroadPackages.zeroad-unwrapped
    zeroadPackages.zeroad-data
    libnotify
    bottles
    ipafont
    ipaexfont
    noto-fonts-cjk
    hanazono
    libsForQt5.ark
    rar
    superTuxKart
    tree
    gst_all_1.gstreamer
    gst_all_1.gst-vaapi
    libgudev
    speex
    xorg.xset
    neofetch
    gnome.gnome-weather
    xdg-utils
    arduino
    arduino-core
    arduino-mk
    arduino-cli
    evolution
    (let dbus-sway-environment = pkgs.writeTextFile {
    name = "dbus-sway-environment";
    destination = "/bin/dbus-sway-environment";
    executable = true;

    text = ''
  dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
  systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
  systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
      '';
         };
         in dbus-sway-environment)  
  ];




  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako
      dmenu
      wofi
      copyq
      waybar
      git
      killall
      sway-contrib.grimshot
      grim
      wev
      slurp
      xorg.xlsclients
      mplayer
      eww-wayland
    ];
  };

  # nixpkgs.overlays = [
  #   (self: super: {
  #    discord = super.discord.override {
  #       commandLineArgs =
  #         "--enable-features=UseOzonePlatform --ozone-platform=wayland";
  #     };
  #   })
  # ];


  services.dbus.enable = true;
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
      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];
      gtkUsePortal = true;
      wlr = {
        enable = true;
        settings = {
           screencast = {
             output_name = "DP-1";
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

  programs.gamemode.enable = true;

  services.greetd = {
    enable = true;
    package = pkgs.greetd.greetd;
    settings = {
      default_session = {
        command = "${pkgs.greetd.greetd}/bin/agreety --cmd sway";
      };
      initial_session = {
        command = "sway";
        user = "neil";
      };
    };
  };

  services.gvfs.enable = true;
  services.tumbler.enable = true;

  environment.variables = {
    MOZ_ENABLE_WAYLAND="1";
    QT_QPA_PLATFORM="wayland";
    SDL_VIDEODRIVER="wayland";
    NIXPKGS_ALLOW_UNFREE="1";
   SWAY_CURSOR_THEME="Adwaita";
#    QT_STYLE_OVERRIDE = "kvantum";
#    QT_QPA_PLATFORMTHEME = "plastique";

  };
  environment.sessionVariables = {
    XDG_CURRENT_DESKTOP="sway";
    XDG_SESSION_TYPE="wayland";
    XDG_SESSION_DESKTOP="sway";
    DEFAULT_BROWSER="${pkgs.firefox}/bin/firefox";
    BROWSER="${pkgs.firefox}/bin/firefox";
  };

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
    };



  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.jane = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  # };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  #   firefox
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
   networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?


### HOME MANAGER


  home-manager.users.neil = {pkgs, ...}: {
    home.packages = [
    ];
    nixpkgs.config.allowUnfree = true;

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

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
    gtk.iconTheme.package = pkgs.flat-remix-icon-theme;
    gtk.iconTheme.name = "Flat-Remix-Green-Dark";
    gtk.theme.package = pkgs.amarena-theme;
    gtk.theme.name = "amarena";
    home.pointerCursor = {
      package = pkgs.bibata-cursors-translucent;
      gtk.enable = true;
      name = "Bibata_Ghost";
      size = 36;
      x11.enable = true;
    };

## EMACS
    programs.emacs = {
      enable = true;
      package = pkgs.emacsPgtkNativeComp;
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
# epkgs.irony
# epkgs.company-irony
# epkgs.company-c-headers
epkgs.spinner
epkgs.lsp-mode
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
  :init (load-theme 'doom-city-lights t))

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
  (company-mode 1))

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
"C-c r i" 'org-roam-node-insert)

(general-define-key
  :keymaps 'org-mode-map
"C-c r s" 'org-roam-node-find
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

(use-package org
  :mode ("\\.org\\'" . org-mode))

(setq org-roam-directory (file-truename "/home/neil/org-roam"))
(setq fint-file-visit-truename t)
(org-roam-db-autosync-mode)

        '';
    };

services.emacs = {
  enable = true;
  client = {
    enable = true;
    arguments = [ "-c" ];
  };
};
## FIREFOX
    programs.firefox = {
      enable = true;
      profiles.nixos = {
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
  --navbarWidth     : 540px; /* Set width of navbar. Use px for a fixed width or vw for a percentage of your window. */
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
  padding: 0 0 2px !important;
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
  margin-top  : -28px !important;
  height      : 28px !important;
}
:root:not([uidensity="compact"]):not([uidensity="touch"]) #nav-bar {
  margin-top  : -28px !important;
  height      : 28px !important;
}
:root[uidensity="touch"] #nav-bar {
  margin-top  : -49px !important;
  height      : 49px !important;
}
:root[sizemode="maximized"] #TabsToolbar {
  margin-top: 1px;
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
  min-height: 28px !important;
}
#TabsToolbar { height: 28px !important; }

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



## EWW
    #programs.eww.enable = true;

## SWAY
    wayland.windowManager.sway = {
      enable = true;
      swaynag.enable = true;
      config = {
        colors = {
          focused = { background = "526170"; border = "152a1f"; childBorder = "94cf95"; indicator = "2e9ef4"; text = "eeeeec"; };
          focusedInactive = { background = "394450"; border = "151a1f"; childBorder = "4a515f"; indicator = "484e50"; text = "eeeeec"; };
          unfocused = { background = "242d35"; border = "151a1f"; childBorder = "13161d"; indicator = "292d2e"; text = "eeeeec"; };
        };
        gaps.smartBorders = "on";
        window = {
          hideEdgeBorders = "both";
        };
        input = {
          "9610:42:BY_Tech_Usb_Gaming_Keyboard" = {
            xkb_options = "ctrl:swapcaps";
          };
          "*" = { accel_profile = "flat"; };
         "1386:890:Wacom_One_by_Wacom_S_Pen" = {
           events = "enabled";
           map_to_output = "DP-1";
          };
         "0:0:OpenTabletDriver_Virtual_Tablet" = {
            events = "enabled";
            map_to_output = "DP-3";
          };
        };
        seat = { seat0 = { xcursor_theme = "Bibata_Ghost 36"; }; };
        output = {
          HDMI-A-1 = {
            res = "2560x1440@144hz";
            position = "4000,510";
            adaptive_sync = "on";
            bg = "~/Pictures/suikavertical1.jpg fill";
            transform = "90";
          };
          DP-1 = {
            res = "2560x1440@144hz";
            position = "1440,0";
            adaptive_sync = "on";
            bg = "~/Pictures/touhou.jpg fill";
            transform = "180";
          };
          DP-2 = {
            res = "2560x1440@144hz";
            position = "0,450";
            adaptive_sync = "on";
            bg = "~/Pictures/raymoo.jpg fill";
            transform = "270";
          };
          DP-3 = {
            res = "2560x1440@144hz";
            position = "1440,1440";
            adaptive_sync = "on";
            bg = "~/Pictures/suika.png fill";
            transform = "0";
          };
        };
        workspaceOutputAssign = [
          { output = "DP-1"; workspace = "U1";}
          { output = "DP-1"; workspace = "U2";}
          { output = "DP-1"; workspace = "U3";}
          { output = "DP-2"; workspace = "L1";}
          { output = "DP-2"; workspace = "L2";}
          { output = "DP-2"; workspace = "L3";}
          { output = "DP-3"; workspace = "D1";}
          { output = "DP-3"; workspace = "D2";}
          { output = "DP-3"; workspace = "D3";}
          { output = "HDMI-A-1"; workspace = "R1";}
          { output = "HDMI-A-1"; workspace = "R2";}
          { output = "HDMI-A-1"; workspace = "R3";}
        ];
        bars = [
        #   {command = "\${pkgs.waybar}/bin/waybar";}
        #   {mode = "dock";}
        #   {position = "top";}
        ];
        modifier = "Mod4";
        menu = "${pkgs.rofi-wayland}/bin/rofi";
        terminal = "${pkgs.alacritty}/bin/alacritty";
        startup = [
            { command = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";}
            { command = "${pkgs.eww-wayland}/bin/eww daemon";}
            { command = "${pkgs.eww-wayland}/bin/eww open statusbar";}
            { command = "${pkgs.swaylock}/bin/swaylock -i ~/Pictures/wp4.jpg";}
            { command = "${pkgs.sway}/bin/swaymsg 'workspace R1; exec ${pkgs.discord}/bin/discord --enable-features=UseOzonePlatform --ozone-platform=wayland; workspace L1'";}
            { command = "${pkgs.mako}/bin/mako";}
                 ];
        down = "j";
        up = "k";
        left = "h";
        right = "l";

        keybindings =
          let
            mod = "mod4";
            launcher = "${pkgs.rofi-wayland}/bin/rofi";
          in {
            "${mod}+x+Up" = "workspace next";
            "${mod}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
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
            "${mod}+Alt+Left" = "workspace L1";
            "${mod}+Alt+h" = "workspace L1";
            "${mod}+Alt+Down" = "workspace D1";
            "${mod}+Alt+j" = "workspace D1";
            "${mod}+Alt+Up" = "workspace U1";
            "${mod}+Alt+k" = "workspace U1";
            "${mod}+Alt+Right" = "workspace R1";
            "${mod}+Alt+l" = "workspace R1";
            "${mod}+Alt+Shift+Left" = "move container to workspace L1";
            "${mod}+Alt+Shift+h" = "move container to workspace L1";
            "${mod}+Alt+Shift+Down" = "move container to workspace D1";
            "${mod}+Alt+Shift+j" = "move container to workspace D1";
            "${mod}+Alt+Shift+Up" = "move container to workspace U1";
            "${mod}+Alt+Shift+k" = "move container to workspace U1";
            "${mod}+Alt+Shift+Right" = "move container to workspace R1";
            "${mod}+Alt+Shift+l" = "move container to workspace R1";
            "${mod}+Delete+Insert" = "workspace L1";
            "${mod}+Alt+F1" = "workspace L1";
            "${mod}+Alt+F5" = "workspace L2";
            "${mod}+Alt+F9" = "workspace L3";
            "${mod}+Alt+F3" = "workspace U1";
            "${mod}+Alt+F7" = "workspace U2";
            "${mod}+Alt+F11" = "workspace U3";
            "${mod}+Alt+F2" = "workspace D1";
            "${mod}+Alt+F6" = "workspace D2";
            "${mod}+Alt+F10" = "workspace D3";
            "${mod}+Alt+F4" = "workspace R1";
            "${mod}+Alt+F8" = "workspace R2";
            "${mod}+Alt+F12" = "workspace R3";
            "${mod}+Alt+Shift+F1" = "move container to workspace L1";
            "${mod}+Alt+Shift+F5" = "move container to workspace L2";
            "${mod}+Alt+Shift+F9" = "move container to workspace L3";
            "${mod}+Alt+Shift+F3" = "move container to workspace U1";
            "${mod}+Alt+Shift+F7" = "move container to workspace U2";
            "${mod}+Alt+Shift+F11" = "move container to workspace U3";
            "${mod}+Alt+Shift+F2" = "move container to workspace D1";
            "${mod}+Alt+Shift+F6" = "move container to workspace D2";
            "${mod}+Alt+Shift+F10" = "move container to workspace D3";
            "${mod}+Alt+Shift+F4" = "move container to workspace R1";
            "${mod}+Alt+Shift+F8" = "move container to workspace R2";
            "${mod}+Alt+Shift+F12" = "move container to workspace R3";
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
            "Alt+1" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
            "Alt+2" = "exec ${pkgs.playerctl}/bin/playerctl previous";
            "Alt+3" = "exec ${pkgs.playerctl}/bin/playerctl next";
            "XF86AudioRaiseVolume" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Master 3%+";
            "XF86AudioLowerVolume" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Master 3%-";
            "XF86AudioMute" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Master togglemute";
            "XF86AudioMicMute" = "exec ${pkgs.alsa-utils}/bin/amixer -q sset Capture togglemute";
            "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10";
            "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set -10";
            "--release print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save screen $HOME/Pictures/Screenshots/screenshot-$(${pkgs.coreutils}/bin/date +%B_%d_%Y_at_%H:%M:%S).png";
            "--release ${mod}+print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save active $HOME/Pictures/Screenshots/screenshotWindow-$(${pkgs.coreutils}/bin/date +%B_%d_%Y_at_%H:%M:%S).png";
            "--release shift+print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot save area $HOME/Pictures/Screenshots/screenshotArea-$(${pkgs.coreutils}/bin/date +%B_%d_%Y_at_%H:%M:%S).png";
            "--release ctrl+shift+print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy area";
            "--release ctrl+${mod}+print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy active";
            "--release ctrl+print" = "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot copy screen";
          };
      };
    };

## MAKO
    programs.mako = {
      enable = true;
      actions = true;
      anchor = "top-right";
      backgroundColor = "#242d35";
      borderColor = "#94cf95";
      borderRadius = 0;
      borderSize= 3;
      #font =
      icons = true;
      output = "DP-3";
      extraConfig = "icon-location=right\ntext-alignment=center";
    };

## ALACRITTY
    programs.alacritty = {
      enable = true;
      package = pkgs.alacritty;
      settings = {
        opacity = 0.7;
        primary = {
          background = "#000000";
          foreground = "CBCCC6";
        };
        normal = {
          black = "#191E2A";
          red = "#FF3333";
          green = "#BAE67E";
          yellow = "#FFA759";
          blue = "#73D0FF";
          magenta = "#FFD580";
          cyan = "#95E6CB";
          white = "#C7C7C7";
        };
        bright = {
          black = "#686868";
          red = "#F27983";
          green = "#A6CC70";
          yellow = "#FFCC66";
          blue = "#5CCFE6";
          magenta = "#FFEE99";
          cyan = "#95E6CB";
          white = "#FFFFFF";
        };

      };
    };
    
## MPD
  services.mpd = {
    enable = true;
    package = pkgs.mpd;
    musicDirectory = "$HOME/bulk";
    network = {
      listenAddress = "any";
      port = 6600;
      };
    };

  services.mpd-discord-rpc = {
    enable = true;
    package = pkgs.mpd-discord-rpc;
    settings = {
      hosts = [ "localhost:6600" ];
      format = {
        details = "$title";
        state = "On $album by $artist";
      };
    };
  };


## CONFIG FILES
    home.file = {
      ".config/test/what".text =
        ''
        okay so now i test it moar
        does this work right?
            time to pray
        '';

      ".config/eww/eww.yuck".text =
        ''
(defwindow statusbar
           :monitor 1
           :geometry (geometry :x "0%"
                               :y "0px"
                               :width "100%"
                               :height "15px"
                               :anchor "bottom center")
           :stacking "fg"
           :exclusive true
  (main-bar))

  (defwidget main-bar []
      (date)
      )
 (defwidget date []
      (box :class "date"
           :spacing 10
           :space-evenly "false"
           :halign "center"
        (label :text "''${date}"
               :class "time")))
(defwidget sound []
   (box))
  (defpoll date :interval "1s"
    "date +'%a,  %b  %d  %H:%M:%S'")
         '';
      ".config/eww/eww.scss".text = "";
      ".local/share/applications/discord.desktop".text =
''
[Desktop Entry]
Categories=Network;InstantMessaging
Exec=${pkgs.discord}/bin/discord #--enable-features=UseOzonePlatform --ozone-platform=wayland
GenericName=All-in-one cross-platform voice and text chat for gamers
Icon=discord
MimeType=x-scheme-handler/discord
Name=Discord
Type=Application
Version=1.4
'';
      ".config/rclone/rclone.conf".text =
''
[remote]
type = drive
client_id = 1084254172415-3jkv4kfegb2bmrohnpt4kba448rgugra.apps.googleusercontent.com
client_secret = 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
scope = drive
token = {"access_token":"ya29.a0AVA9y1tggPkmYn-YUScUMqbNJkHNjjiHjjOy7LxPB13Uta-FaPGZgi5iZQJMe7lU-EopeeOOyN5CWu6TIu8dwdz7YhVo31CzKPRFbpgGdeIYPU9IXJLtGuyFNgKqCsBPiFa2SXmvF_QssaXZbWGkqvXLsuZ8aCgYKATASAQASFQE65dr8uhbGEkAoj00D7N1XelwHFA0163","token_type":"Bearer","refresh_token":"1//04U0pEKrAYzMzCgYIARAAGAQSNwF-L9IrcV-vjatokLGiJ7LkuNKLQQ5-fJ_BRQa1BYtRVWgw0HtvjpxPdhe6nWInUYWqW5PE40s","expiry":"2022-08-28T15:33:08.253251787-04:00"}
team_drive = 

'';
    };
  };



}
