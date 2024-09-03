# Initial Setup:

## Install ripgrep: 

`sudo dnf install ripgrep`

## Fonts: 

```
cd ~/.local/share/fonts/
git clone --depth 1 https://git.sr.ht/~protesilaos/iosevka-comfy
sudo fc-cache -v
```

## Icons:

Download Symbols Nerd Font: https://www.nerdfonts.com/font-downloads


## Python

Inside a virtualenv:

```
pip install pyright ruff debugpy isort pylint
```
