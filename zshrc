# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}

key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

# setup key accordingly
[[ -n "${key[Home]}"     ]]  && bindkey  "${key[Home]}"     beginning-of-line
[[ -n "${key[End]}"      ]]  && bindkey  "${key[End]}"      end-of-line
[[ -n "${key[Insert]}"   ]]  && bindkey  "${key[Insert]}"   overwrite-mode
[[ -n "${key[Delete]}"   ]]  && bindkey  "${key[Delete]}"   delete-char
[[ -n "${key[Up]}"       ]]  && bindkey  "${key[Up]}"       up-line-or-history
[[ -n "${key[Down]}"     ]]  && bindkey  "${key[Down]}"     down-line-or-history
[[ -n "${key[Left]}"     ]]  && bindkey  "${key[Left]}"     backward-char
[[ -n "${key[Right]}"    ]]  && bindkey  "${key[Right]}"    forward-char
[[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"   beginning-of-buffer-or-history
[[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}" end-of-buffer-or-history

# Finally, make sure the terminal is in application mode, when zle is
# active. Only then are the values from $terminfo valid.
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init () {
        printf '%s' "${terminfo[smkx]}"
    }
    function zle-line-finish () {
        printf '%s' "${terminfo[rmkx]}"
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

# Prompt.
setopt promptsubst
source /usr/share/git/git-prompt.sh
autoload -U promptinit
promptinit
autoload -U colors && colors
PROMPT="%(!.%{$fg[red]%}.%{$fg[green]%})%n@%m%{$reset_color%} %{$fg[blue]%}%~%{$fg[yellow]%}\$(__git_ps1)%{$reset_color%}> "
RPROMPT=""

# Colors.
alias ls='ls --color'
alias grep='grep --color'
alias vi='nvim'
eval `dircolors -b ~/.dircolors`

# Bindings.
typeset -g -A key
bindkey '[3~' delete-char
#bindkey 'Oc' forward-word
#bindkey 'Od' backward-word
bindkey '[7~' beginning-of-line
bindkey '[8~' end-of-line
bindkey '[5~' beginning-of-history
bindkey '[6~' end-of-history
bindkey '\033[1;5C' forward-word
bindkey '\033[1;5D' backward-word
xmodmap -e "keycode 166 = Prior" 
xmodmap -e "keycode 167 = Next"

# Completion.
autoload -U compinit; compinit
setopt nomenucomplete
setopt noautomenu

# Env variables.
