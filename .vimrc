set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'Syntastic'
Plugin 'https://github.com/kien/ctrlp.vim'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'garbas/vim-snipmate'
Plugin 'honza/vim-snippets'
Plugin 'The-NERD-Commenter'
Plugin 'https://github.com/scrooloose/nerdtree.git'
Plugin 'Auto-Pairs'
Plugin 'lyuts/vim-rtags'
" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line


autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o


"========================== Plugin Configurations ======================================

"== YouCompleteMe =======================
nnoremap <leader>jd :YcmCompleter GoToDefinitionElseDeclaration<CR>
nnoremap <F5> :YcmForceCompileAndDiagnostics<CR>
"let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
"Do not ask when starting vim
let g:ycm_confirm_extra_conf = 1
let g:ycm_collect_identifiers_from_tags_files = 1
"close preview window when user accepts the completion
"go to definition/declaration
"let g:ycm_extra_conf_globlist = ['~/work/dev/auvir/auvir_embed/stm32_cmake/stm32test-SPL']
" Settings from http://dougblack.io/words/a-good-vimrc.html

"== Syntastic ===========================
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" ===================================================================================

" ========================= Colors =========================

set t_Co=256
colorscheme xoria256
"colorscheme solarized
"colorscheme badwolf
"colorscheme molokai
syntax enable

" ========================= Copy and paste =========================


"copy to X CLIPBOARD
"nnoremap <leader>cc :w !xsel -i -b<CR>
"nnoremap <leader>cp :w !xsel -i -p<CR>
"nnoremap <leader>cs :w !xsel -i -s<CR>
" Paste from X CLIPBOARD
"nnoremap <leader>pp :r!xsel -p<CR>
"nnoremap <leader>ps :r!xsel -s<CR>
"nnoremap <leader>pb :r!xsel -b<CR>

"copy to X CLIPBOARD
nnoremap <C>c :w !xsel -i -p<CR>
" Paste from X CLIPBOARD
nnoremap <C>v :r! xsel -p<CR>


" ========================= Compile and build =========================

"set makeprg=make\

" ========================= Backups =========================

set nobackup
set noswapfile

" ========================= Indent =========================

set autoindent    " always set autoindenting on
set copyindent    " copy the previous indentation on autoindenting
"set filetype=xml  " abbrev -  :set ft=xml
set smartindent   " abbrev -  :set si

" ========================= Space and Tabs =========================

set tabstop=4       " number of visual spaces per TAB
set softtabstop=4   " number of spaces in tab when editing
set expandtab       " tabs are spaces
set pastetoggle=<F10>
set list
set listchars=tab:>-,trail:~,extends:>,precedes:<
set shiftwidth=4


" ========================= UI Config =========================

set number              " show line numbers
set showcmd             " show command in bottom bar
set cursorline          " highlight current line
set wildmenu            " visual autocomplete for command menu
set lazyredraw          " redraw only when we need to.
set showmatch           " highlight matching [{()}]

" ========================= Searching =========================

set incsearch           " search as characters are entered
set hlsearch            " highlight matches

" nnoremap <leader><space> :nohlsearch<CR> " turn off search highlight

let wordUnderCursor=expand("<cword>")
nnoremap <Leader>r :%s/\<<C-r><C-w>\>/

" ========================= Folding =========================

set foldenable          " enable folding
set foldlevelstart=10   " open most folds by default
set foldnestmax=10      " 10 nested fold max
set foldmethod=syntax   " fold based on syntax


let javaScript_fold=1         " JavaScript
let perl_fold=1               " Perl
let php_folding=1             " PHP
let r_syntax_folding=1        " R
let ruby_fold=1               " Ruby
let sh_fold_enabled=1         " sh
let vimsyn_folding='af'       " Vim script
let xml_syntax_folding=1      " XML


nnoremap <space> za     " space open/closes folds

" ========================= Commenting  =========================

au FileType c,cpp,h,hpp setlocal comments-=:// comments+=f://

" == Higlight TODO, FIXME, etc
if has("autocmd")
  " Highlight TODO, FIXME, NOTE, etc.
  if v:version > 701
    autocmd Syntax * call matchadd('Todo',  '\W\zs\(TODO\|FIXME\|CHANGED\|XXX\|BUG\|HACK\)')
    autocmd Syntax * call matchadd('Debug', '\W\zs\(NOTE\|INFO\|IDEA\)')
  endif
endif
" ========================= Search folders =======================

let &path.="/usr/include,/usr/local/include,~/work/dev/thirdparty"

" ========================= Compiler options, building  =========================

set makeprg=make\ -C\ ~/work/dev/auvir/auvir_model/build/debug

" ========================= Space and Tabs =========================

" ========================= Space and Tabs =========================


