# Path to Oh My Fish install.
set -gx OMF_PATH "/Users/taker/.local/share/omf"

# Customize Oh My Fish configuration path.
#set -gx OMF_CONFIG "/Users/taker/.config/omf"

# Load oh-my-fish configuration.
source $OMF_PATH/init.fish

set fish_path $HOME/.oh-my-fish

set -x PATH $HOME/goproject $PATH

set -x PATH $HOME/.pyenv/bin $PATH

source (pyenv init - | psub)
source ~/.config/fish/nvm-wrapper/nvm.fish

set fish_plugins theme peco osx tmux brew git

set fish_user_paths /opt/homebrew-cask/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin
set -x MANPATH /opt/homebrew-cask/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/help/man /usr/local/share/man /usr/share/man /opt/x11/share/man

