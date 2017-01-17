" First things first.
set nocompatible

" Plugins
call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'michaeljsmith/vim-indent-object'
Plug 'tmhedberg/matchit'
Plug 'derekwyatt/vim-fswitch'
call plug#end()

" Save/load views automagically (not for non-existent files).
autocmd BufWinLeave * if expand("%") != "" | mkview | endif
autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif

" BASIC EDITOR SETTINGS.
set autoindent
set backspace=indent,eol,start
set expandtab
"set foldlevel=1000
set history=100
set hlsearch
set laststatus=2
" Use mouse functionality (i.e. visual selections with the mouse,
" scrolling, etc.).
"set mouse=a
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

" EDITOR MAPPINGS.
" Use Ctrl-R for replacing selected text.
vnoremap <C-r> "hy:%s/<C-r>h//g<left><left>
" Use tab and shift-tab for cycling through tabs.
noremap <TAB> :tabn<CR>
noremap <S-TAB> :tabp<CR>
" Use ctrl-/ to clear search result buffer.
noremap <silent> <C-_> :noh<CR>
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
set completeopt-=preview
" If you prefer the Omni-Completion tip window to close when a selection is
" made, these lines close it on movement in insert mode or when leaving
" insert mode.
"autocmd CursorMovedI * if pumvisible() == 0|silent! pclose|endif
"autocmd InsertLeave * if pumvisible() == 0|silent! pclose|endif
" Change the behavior of paste in visual mode so that
" the overwritten text is not yanked.
vnoremap p "_c<ESC>p
" Allow removal of single characters without yanking.
noremap x "_x
" Use F2 for toggling folds.
noremap <F2> za
" Use F5 for toggling all folds.
noremap <F5> zi
" Use Control-Backspace for deleting the last word.
imap <C-H> <C-W>

" NERDtree.
noremap <silent> <F1> :NERDTreeTabsToggle<CR>

" FILE-TYPE SPECIFIC SETTINGS.
" Turn off auto-comment insertion on newline.
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Use F3 for compiling Makefiles.
autocmd FileType cpp map <buffer> <F3> :make<CR>
autocmd FileType haskell map <buffer> <F3> :make<CR>
autocmd FileType c map <buffer> <F3> :make<CR>
autocmd FileType java map <buffer> <F3> :!make<CR>
autocmd BufNewFile,BufRead *.cbp map <buffer> <F3> :call CompileCodeblocksProject()<CR>

" Use F4 for running Android apps.
autocmd FileType java map <buffer> <F4> :!make run<CR>

" Automatic text wrapping after 80 columns.
autocmd FileType c set textwidth=80
autocmd FileType c set formatoptions+=t

" Use F11 for switching between header and source files.
autocmd FileType cpp map <buffer> <F11> :FSHere<CR>
autocmd FileType c map <buffer> <F11> :FSHere<CR>

" Keep real tabs for Makefiles and python files.
autocmd FileType make setlocal noexpandtab

" Use indentation-based folding for C source files.
autocmd FileType c set foldmethod=indent

" Use syntax-based folding for CPP source files.
"autocmd FileType cpp set foldmethod=syntax
" Matches multi-line comment blocks for folding purposes.
"autocmd FileType cpp syn match comment "\v(^\s*//.*\n)+" fold

" Use F4 for running tests.
autocmd FileType c map <buffer> <F4> :!make check || cat tests/test-suite.log | grep 'Checks: ' -A500<CR>
" Use F6 for adding C unit tests.
autocmd BufNewFile,BufRead *tests.c map <buffer> <F6> :call AddTestsToSuite()<CR>
" Use special folding for C unit test files.
autocmd BufNewFile,BufRead *tests.c set foldmethod=expr
autocmd BufNewFile,BufRead *tests.c set foldexpr=GetCUnitTestFold(v:lnum)

" Omnicomplete settings for C++.
autocmd FileType cpp setl ofu=ccomplete#CompleteCpp

" YouCompleteMe stuff.
"set completeopt-=previewvimrc
"let g:ycm_add_preview_to_completeopt=0
let g:ycm_auto_trigger=0
"let g:ycm_autoclose_preview_window_after_completion=1

" Switch to hex mode for certain files.
autocmd BufNewFile,BufRead *.hex silent %!xxd
autocmd BufNewFile,BufRead *.wav silent %!xxd
autocmd BufNewFile,BufRead *.vox silent %!xxd

" STATUS LINE.
set statusline=
set statusline+=%F " Relative filename.
set statusline+=%y " File type.
set statusline+=%m " Modified flag.
set statusline+=%r " Read-only flag.
set statusline+=%= " Left/right separator.
set statusline+=Column\ %c,\  " Cursor column.
set statusline+=Line\ %l/%L " Cursor line.
set statusline+=\ (%p%%) " Percent through the file.

" MISCELLANEOUS.
" Make filename tab completion work like bash.
set wildmode=longest,list
" Change fold bar color.
highlight Folded ctermbg=darkgray ctermfg=green
" Diff colors.
highlight DiffAdd cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
highlight DiffText cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red
