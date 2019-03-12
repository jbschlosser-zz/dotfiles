" First things first.
set nocompatible

" PLUGINS.
call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'michaeljsmith/vim-indent-object'
Plug 'tmhedberg/matchit'
Plug 'derekwyatt/vim-fswitch'
Plug 'itchyny/vim-haskell-indent'
Plug 'tpope/vim-commentary'
call plug#end()

" SETTINGS.
" --=== General ===--
set autoindent
set backspace=indent,eol,start
set expandtab
set history=100
set hlsearch
set laststatus=2
set nocompatible
set noruler
set number
set shiftwidth=4
set switchbuf=usetab
set tabpagemax=50
set tabstop=4
set showcmd
set encoding=utf-8
syntax enable
" Set the path for finding tags files.
set tags=./tags;/
" Turn off auto-comment insertion on newline.
set formatoptions-=c formatoptions-=r formatoptions-=o
" Make filename tab completion work like bash.
set wildmode=longest,list
" Save/load views automagically (not for non-existent files).
autocmd BufWinLeave * if expand("%") != "" | mkview | endif
autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
" Skip loading the vim defaults.
let g:skip_defaults_vim = 1
" --=== Status line ===--
set statusline=
set statusline+=%F " Relative filename.
set statusline+=%y " File type.
set statusline+=%m " Modified flag.
set statusline+=%r " Read-only flag.
set statusline+=%= " Left/right separator.
set statusline+=Column\ %c,\  " Cursor column.
set statusline+=Line\ %l/%L " Cursor line.
set statusline+=\ (%p%%) " Percent through the file.
" --=== Colors ===--
colorscheme default
" Change fold bar color.
highlight Folded ctermbg=darkgray ctermfg=green
" Diff colors.
highlight DiffAdd cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffText cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red

" KEY MAPPINGS.
" Use Ctrl-R for replacing selected text.
vnoremap <C-r> "hy:%s/<C-r>h//g<left><left>
" Use tab and shift-tab for cycling through tabs.
noremap <TAB> :tabn<CR>
noremap <S-TAB> :tabp<CR>
" Use ctrl-/ to clear search result buffer.
noremap <silent> <C-_> :noh<CR>
" Use alt-; for commenting.
vmap ; gc
" Remove the use of arrow keys!
"noremap <Left>  <NOP>
"noremap <Right> <NOP>
"noremap <Up>    <NOP>
"noremap <Down>  <NOP>
" Use Space for centering the screen on the current line.
noremap <Space> zz
" Keep cursor in the middle for PageUp/PageDown.
noremap <PageUp> HzzHzz
noremap <PageDown> LzzLzz
" Use Ctrl-PageUp/PageDown for moving up/down half a page.
noremap [5;5~ Hzz
noremap [6;5~ Lzz
" Fix Ctrl-Left and Ctrl-Right to move by individual words.
noremap <C-Left> b
noremap <C-Right> w
" Use Ctrl-Space for Omnicomplete.
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-x><C-o>
" Don't yank overwritten text when pasting in visual mode.
vnoremap p "_c<ESC>p
" Allow removal of single characters without yanking.
noremap x "_x
" Use F2 for toggling folds.
noremap <F2> za
" Use F5 for toggling all folds.
noremap <F5> zi
" Use Control-Backspace for deleting the last word.
imap <C-H> <C-W>

" PLUGIN SETTINGS.
" --=== NERDtree ===--
noremap <silent> <F1> :NERDTreeTabsToggle<CR>
" --=== OmniComplete ===--
set completeopt-=preview
" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode.
"autocmd CursorMovedI * if pumvisible() == 0|silent! pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|silent! pclose|endif
" --=== YouCompleteMe ===--
"set completeopt-=previewvimrc
"let g:ycm_add_preview_to_completeopt=0
let g:ycm_auto_trigger=0
"let g:ycm_autoclose_preview_window_after_completion=1

" FILE-TYPE SPECIFIC SETTINGS.
" --=== C ===--
" Use F3 for compiling Makefiles.
autocmd FileType c map <buffer> <F3> :make<CR>
" Automatic text wrapping after 80 columns.
autocmd FileType c set textwidth=80
autocmd FileType c set formatoptions+=t
" Use F11 for switching between header and source files.
autocmd FileType c map <buffer> <F11> :FSHere<CR>
" Use indentation-based folding for C source files.
autocmd FileType c set foldmethod=indent

" --=== C++ ===--
" Use F3 for compiling Makefiles.
autocmd FileType cpp map <buffer> <F3> :make<CR>
" Use F11 for switching between header and source files.
autocmd FileType cpp map <buffer> <F11> :FSHere<CR>
" Omnicomplete settings for C++.
autocmd FileType cpp setl ofu=ccomplete#CompleteCpp

" --=== JAVA ===--
" Use F3 for compiling Makefiles.
autocmd FileType java map <buffer> <F3> :!make<CR>
" Use F4 for running Android apps.
autocmd FileType java map <buffer> <F4> :!make run<CR>

" --=== HASKELL ===--
" Use F3 for compiling with stack.
autocmd FileType haskell map <buffer> <F3> :!stack build<CR>
" Use F4 for running unit tests with stack.
autocmd FileType haskell map <buffer> <F3> :!stack test<CR>

" --=== MAKE ===--
" Keep real tabs for Makefiles.
autocmd FileType make setlocal noexpandtab

autocmd BufNewFile,BufRead *.launch set filetype=xml

" --=== LaTeX ===--
autocmd FileType tex map <buffer> <F3> :!pdflatex <C-R>=expand("%:t")<CR><CR>
autocmd FileType tex map <buffer> <F4> :!zathura <C-R>=expand("%:t:r")<CR>.pdf<CR>

" --=== BINARY FILES ===--
" Switch to hex mode for certain files.
autocmd BufNewFile,BufRead *.hex silent %!xxd
autocmd BufNewFile,BufRead *.wav silent %!xxd
autocmd BufNewFile,BufRead *.vox silent %!xxd
