# My dotfiles

Dotfiles are the customization files that are used to personalize your Linux or other Unix-based system. You can tell that a file is a dotfile because the name of the file will begin with a period–a dot! The period at the beginning of a filename or directory name indicates that it is a hidden file or directory. This repository contains my personal dotfiles. They are stored here for convenience so that I may quickly access them on new machines or new installs. Also, others may find some of my configurations helpful in customizing their own dotfiles.

For managing dotfiles the bare git repository technique is used. Read more [The best way to store your dotfiles: A bare Git repository](https://www.atlassian.com/git/tutorials/dotfiles)

## Prerequisites

-   Install zsh

```bash
sudo dnf install zsh
```

Make zsh default shell:

```bash
chsh -s $(which zsh)
```

-   Install st from [Luke's build of st - the simple (suckless) terminal](https://github.com/LukeSmithxyz/st)

```bash
git clone https://github.com/LukeSmithxyz/st
cd st
sudo make install
```

-   Install xmonad and xmobar

```bash
sudo dnf install xmonad ghc-xmonad-contrib xmobar
```

-   Install slock to be able to lock screen

```bash
sudo dnf install slock
```

-   Install nitrogen to restore wallpapers
-   Install compton for transparency
-   Install dunst for system notifications

```bash
sudo dnf install dunst
```
