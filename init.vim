call plug#begin('/root/.local/share/nvim/plugged')
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'jalvesaq/Nvim-R'
Plug 'gaalcaras/ncm-R'
Plug 'sirver/UltiSnips'
Plug 'SirVer/UltiSnips'
Plug 'ncm2/ncm2-ultisnips'
Plug 'lervag/vimtex'
Plug 'chrisbra/csv.vim'
Plug 'rakr/vim-one'
Plug 'junegunn/goyo.vim'
call plug#end()

"" CONFIGURATIONS FOR AUTOCOMPLETION
" enable ncm2 for all buffers
autocmd BufEnter * call ncm2#enable_for_buffer()
" IMPORTANT: :help Ncm2PopupOpen for more information
set completeopt=noinsert,menuone,noselect
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"
" UltiSnips + NCM
let g:UltiSnipsExpandTrigger = "<Plug>(ultisnips_expand_or_jump)"
let g:UltiSnipsJumpForwardTrigger = "<Plug>(ultisnips_expand_or_jump)"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
function! UltiSnipsExpandOrJumpOrTab()
  call UltiSnips#ExpandSnippetOrJump()
  if g:ulti_expand_or_jump_res > 0
    return ""
  else
    return "\<Tab>"
  endif
endfunction
inoremap <silent> <expr> <Tab>
      \ ncm2_ultisnips#expand_or("\<Plug>(ultisnips_try_expand)")
inoremap <silent> <Plug>(ultisnips_try_expand)
      \ <C-R>=UltiSnipsExpandOrJumpOrTab()<CR>
snoremap <silent> <Tab>
      \ <Esc>:call UltiSnips#ExpandSnippetOrJump()<cr>

""PERSONALIZATIONS FOR R
let R_assign = 2
set shiftwidth=2
set tabstop=2
set listchars=space:_,tab:>~ list
set et     "expand tabs to spaces

"" CONFIGURATIONS FOR THEME
"Credit joshdick
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
  if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  "For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
  "Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
  " < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
  if (has("termguicolors"))
    set termguicolors
  endif
endif
set background=dark " for the dark version
" set background=light " for the light version
colorscheme one 
let g:airline_theme = 'one'
