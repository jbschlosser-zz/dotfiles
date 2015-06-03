" First things first.
set nocompatible

" Vundle
"filetype off
"set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()
"Plugin 'gmarik/Vundle.vim'
"Plugin 'phildawes/racer'
"call vundle#end()
filetype plugin indent on

" Save/load views automagically (not for non-existent files).
autocmd BufWinLeave * if expand("%") != "" | mkview | endif
autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif

" BASIC EDITOR SETTINGS.
set autoindent
set backspace=indent,eol,start
set expandtab
"set foldlevel=1000
set hidden
set history=100
set hlsearch
set laststatus=2
" Use mouse functionality (i.e. visual selections with the mouse,
" scrolling, etc.).
"set mouse=a
set noruler
set number
set shiftwidth=4
set switchbuf=usetab
set tabpagemax=50
set tabstop=4
set showcmd
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
" Expand % functionality with XML tag matching.
" Note: this remapping is here because calling the
" normal % from within the function causes
" infinite recursion.
noremap <C-S-Z> %
noremap <silent> % :call ExpandedMatchingBehavior()<CR>
onoremap <silent> % :call ExpandedMatchingBehavior()<CR>
vnoremap <silent> % :call ExpandedMatchingBehaviorVisual()<CR>
" Use F2 for toggling folds.
noremap <F2> za
"noremap <F2> 2.^~ylPr yllpl3pr.PrxFIfi2Prhhrty2l2W2h2pxhreW2Prohrb2Bh2Prkhrn3lmafoyl`a4PrshrthruWh2Prehrd$
" Use F5 for toggling all folds.
noremap <F5> zi
" Use Control-Backspace for deleting the last word.
imap <C-H> <C-W>

" NERDtree.
noremap <F1> :NERDTreeTabsToggle<CR>
let g:NERDTreeDirArrows=0

" FILE-TYPE SPECIFIC SETTINGS.
" Use F3 for compiling Makefiles.
autocmd FileType cpp map <buffer> <F3> :make<CR>
autocmd FileType haskell map <buffer> <F3> :make<CR>
autocmd FileType c map <buffer> <F3> :make<CR>
autocmd FileType rust map <buffer> <F3> :!cargo build<CR>
autocmd FileType java map <buffer> <F3> :!make<CR>
autocmd BufNewFile,BufRead *.cbp map <buffer> <F3> :call CompileCodeblocksProject()<CR>

" Use F4 for running tests.
autocmd FileType rust map <buffer> <F4> :!cargo test<CR>

" Use F4 for running Android apps.
autocmd FileType java map <buffer> <F4> :!make run<CR>

" Automatic text wrapping after 80 columns.
autocmd FileType c set textwidth=80
autocmd FileType c set formatoptions+=t

" Use F7 for validation of an XML file against its schema.
autocmd FileType xml map <buffer> <F7> :call ValidateAgainstXmlSchema()<CR>
autocmd FileType xsd map <buffer> <F7> :call ValidateAgainstXmlSchema()<CR>

" Use F11 for switching between header and source files.
autocmd FileType cpp map <buffer> <F11> :call ToggleBetweenHeaderAndSource()<CR>
autocmd FileType c map <buffer> <F11> :call ToggleBetweenHeaderAndSourceC()<CR>

" Keep real tabs for Makefiles and python files.
autocmd FileType make setlocal noexpandtab
autocmd FileType python setlocal noexpandtab

" Use indentation-based folding for C source files.
autocmd FileType c set foldmethod=indent

" Rust syntax.
autocmd BufNewFile,BufRead *.rs setlocal ft=rust

" Rust tag generation when opening.
autocmd BufNewFile,BufRead *.rs silent !rusty-tags vi > /dev/null 2>&1

" Vertical bar at column 81.
autocmd BufNewFile,BufRead *.rs set colorcolumn=81
autocmd BufNewFile,BufRead *.rs hi ColorColumn ctermbg=darkgrey

" Rust auto-complete.
let g:racer_cmd = "~/.vim/bundle/racer/target/release/racer"
let $RUST_SRC_PATH="/usr/local/src/rust/src/"
let $LD_LIBRARY_PATH="~/.multirust/toolchains/1.0.0/lib"

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

" FUNCTION DEFINITIONS.
" Folding for C unit test files.
function! GetCUnitTestFold(lnum)
    if getline(a:lnum) =~ '^START_TEST'
        return 'a1'
    endif
    if getline(a:lnum) =~ '^END_TEST'
        return 's1'
    endif
    return '='
endfunction

" Function to add tests.
function AddTestsToSuite()
    let l:winview = winsaveview()
    normal G$
    let l:flags = "w"
    let l:count = 0
    let l:add_tests = ""
    while search("START_TEST([a-zA-Z0-9_]*)", l:flags) > 0
        let text = getline(line("."))
        let test_name = substitute(text, "START_TEST(\\([a-zA-Z0-9_]*\\))", "\\1", "")
        let add_test_line = "ADD_TEST_TO_SUITE(".test_name.", s);"
        let l:add_tests = l:add_tests."    ".add_test_line."\n"
        let l:count = l:count + 1
        let l:flags = "W"
    endwhile
    silent g/ADD_TEST_TO_SUITE/d
    let l:success = 0
    if search("suite_create", "w") > 0
        let @b = l:add_tests
        silent put b
        let l:success = 1
    endif
    silent write
    silent call winrestview(l:winview)
    if l:success == 1
        echo l:count." tests added."
    else
        echo "No tests added."
    endif
endfunction

" Expands the % functionality to include XML tag matching.
:function ExpandedMatchingBehavior()
    :let current_tag = matchstr(expand("<cWORD>"), '^<\zs[^>^ ]*\ze')
    ":let current_tag = matchstr(expand("<cWORD>"), '^<\zs[^>]*\ze>$')
    :if current_tag == ""
        " Do the normal % behavior.
        :execute ":normal \<C-S-Z>"
    :else
        " Perform XML tag matching.
        :let ending_text = matchstr(current_tag, '^/\zs.*')
        :if ending_text == ""
            :let starting_text = current_tag 
            :let ending_tag = "</".starting_text.">"
            :call search(ending_tag)
        :else
            :let starting_tag = "<".ending_text
            ":let starting_tag = "<".ending_text.">"
            :call search(starting_tag, 'b')
        :endif
    :endif
:endfunction

" ^^ for visual mode.
:function ExpandedMatchingBehaviorVisual()
    :normal gv
    :call ExpandedMatchingBehavior()
:endfunction

function ToggleBetweenHeaderAndSource()
    let current_filename = expand("%:p")
    let base_filename = fnamemodify(current_filename, ":r")
    let header_match_index = matchend(current_filename, "\\.h$")
    let toggled_filename = base_filename.((header_match_index == -1) ? ".h" : ".cpp")
    if FileExists(toggled_filename)
        " Open the file in the current buffer.
        :execute ":edit ".EscapeSpaces(toggled_filename)
    else
        if header_match_index == -1
            " The current file's corresponding header could not be found.
            echo "File '".toggled_filename."' does not exist!"
        else
            let toggled_c_filename = base_filename.".c"
            if FileExists(toggled_c_filename)
                " Open the file in the current buffer.
                :execute ":edit ".EscapeSpaces(toggled_c_filename)
            else
                " The current header's c file could not be found.
                echo "File '".toggled_c_filename."' does not exist!"
            endif
        endif
    endif
endfunction

function ToggleBetweenHeaderAndSourceC()
    let current_filename = expand("%:p")
    let base_filename = fnamemodify(current_filename, ":r")
    let header_match_index = matchend(current_filename, "\\.h$")
    let toggled_filename = base_filename.((header_match_index == -1) ? ".h" : ".c")
    if FileExists(toggled_filename)
        " Open the file in the current buffer.
        :execute ":edit ".EscapeSpaces(toggled_filename)
    else
        echo "File '".toggled_filename."' does not exist!"
    endif
endfunction

" Function for validating an XML file against its schema.
:function ValidateAgainstXmlSchema()

    " Write the file before validating.
    :write! /tmp/file_to_validate.xml

    " Define a sign for highlighting a whole line.
    :sign define wholeline linehl=Error
    :sign unplace *

    " Loop through each line in the file and look for an XML schema specification.
    :for line in getline(0, "$")

        " Check the current line for an XML schema specification.
        :let schema_regex_matches = matchlist(line, 'xsi:noNamespaceSchemaLocation="\(.*\)"')
        :if len(schema_regex_matches) > 1

            " RUN THE SCHEMA VALIDATION COMMAND.
            " Extract the filename of the schema.
            :let schema_filename = schema_regex_matches[1]

            " Prepend the current file's working directory to the schema filename and surround
            " it in quotes.
            :let schema_filename = "\"" . expand("%:p:h") . "/" . schema_filename . "\""

            " Surround the filename of the current file in quotes.
            :let current_filename = "\"" . expand("%") . "\""

            " Build the command for schema validation.
            ":let command_to_execute = "xmllint --noout --schema " . schema_filename . " " . current_filename
            :let command_to_execute = "xmllint --noout --schema " . schema_filename . " /tmp/file_to_validate.xml"

            " Execute the schema validation command.
            :let command_output = system(command_to_execute)

            " OUTPUT THE VALIDATION RESULTS.
            :let output_lines = split(command_output, '\n')
            :for output_line in output_lines
                :let error_regex_matches = matchlist(output_line, ':\([0-9]\+\):')
                :if !empty(error_regex_matches)
                    :let line_number_to_highlight = error_regex_matches[1]
                    :execute ':sign place '.line_number_to_highlight.' name=wholeline line='.line_number_to_highlight.' file='.expand("%")
                :endif
                :let more_error_regex_matches = matchlist(output_line, 'line \([0-9]\+\)')
                :if !empty(more_error_regex_matches)
                    :let line_number_to_highlight = more_error_regex_matches[1]
                    :execute ':sign place '.line_number_to_highlight.' name=wholeline line='.line_number_to_highlight.' file='.expand("%")
                :endif
            :endfor

            " Output a nice header for the results.
            :echo "------------------------"
            :echo "Schema validation output"
            :echo "------------------------"
            :echo "\n"

            " Output the validation command results.
            :echo command_output

            :break
        :endif
    :endfor
:endfunction

function! WhichTab(filename)
    " Try to determine whether file is open in any tab.  
    " Return number of tab it's open in
    let buffername = bufname(a:filename)
    if buffername == ""
        return 0
    endif
    let buffernumber = bufnr(buffername)

    " tabdo will loop through pages and leave you on the last one;
    " this is to make sure we don't leave the current page
    let currenttab = tabpagenr()
    let tab_arr = []
    tabdo let tab_arr += tabpagebuflist()

    " return to current page
    exec "tabnext ".currenttab

    " Start checking tab numbers for matches
    let i = 0
    for tnum in tab_arr
        let i += 1
        if tnum == buffernumber
            return i
        endif
    endfor
endfunction

" Function to indicate whether or not a file exists.
function! FileExists(filepath)
    if findfile(a:filepath, ".") == a:filepath
        return 1
    endif
    return 0
endfunction

" Function to escape spaces in a string
function EscapeSpaces(string_input)
    let result = substitute(a:string_input, ' ', '\\ ', 'g')
    return result
endfunction

" Function to put output of a shell command into a scratch buffer.
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
    let expanded_cmdline = a:cmdline
    for part in split(a:cmdline, ' ')
        if part[0] =~ '\v[%#<]'
            let expanded_part = fnameescape(expand(part))
            let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
        endif
    endfor
    botright new
    setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
    resize 15
    "call setline(1, 'You entered:    ' . a:cmdline)
    "call setline(2, 'Expanded Form:  ' .expanded_cmdline)
    "call setline(3,substitute(getline(2),'.','=','g'))
    execute '$read !'. expanded_cmdline
    setlocal nomodifiable
    1
endfunction
