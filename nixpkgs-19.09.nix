#
# This is done to get a working combination of haskell-ide-engine and ghc
# As soon, as this works in 20.03, by my guest updating this.
#
let
  nixpkgs-20-03 = import (fetchTarball {
    url = "https://github.com/nixos/nixpkgs/tarball/ab3adfe1c769c22b6629e59ea0ef88ec8ee4563f";
    sha256 = "1m4wvrrcvif198ssqbdw897c8h84l0cy7q75lyfzdsz9khm1y2n1";
    # Recent version 2020-05-08:
    # url = "https://github.com/nixos/nixpkgs/tarball/210d8624ac4a69cbe23c8da692c5735ac4aa46d3";
    # sha256 = "04vbl6kvbzgrlsbk5vk0i5zcsribbkqw59d9m1dki8mswij09qlq";
  }) {};
  nodejsToUse = nixpkgs-20-03.nodejs;
  ourNeovim = nixpkgs-20-03.neovim.override {
          configure = {
            customRC = ''
                packloadall
                " TextEdit might fail if hidden is not set.
                set hidden
                
                " Some servers have issues with backup files, see #649.
                set nobackup
                set nowritebackup
                
                " Give more space for displaying messages.
                set cmdheight=2
                
                " Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
                " delays and poor user experience.
                set updatetime=300
                
                " Don't pass messages to |ins-completion-menu|.
                set shortmess+=c
                
                " Always show the signcolumn, otherwise it would shift the text each time
                " diagnostics appear/become resolved.
                set signcolumn=yes
                
                " Use tab for trigger completion with characters ahead and navigate.
                " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
                " other plugin before putting this into your config.
                inoremap <silent><expr> <TAB>
                      \ pumvisible() ? "\<C-n>" :
                      \ <SID>check_back_space() ? "\<TAB>" :
                      \ coc#refresh()
                inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
                
                function! s:check_back_space() abort
                  let col = col('.') - 1
                  return !col || getline('.')[col - 1]  =~# '\s'
                endfunction
                
                " Use <c-space> to trigger completion.
                inoremap <silent><expr> <c-space> coc#refresh()
                
                " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
                " position. Coc only does snippet and additional edit on confirm.
                if exists('*complete_info')
                  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
                else
                  imap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
                endif
                
                " Use `[g` and `]g` to navigate diagnostics
                nmap <silent> [g <Plug>(coc-diagnostic-prev)
                nmap <silent> ]g <Plug>(coc-diagnostic-next)
                
                " GoTo code navigation.
                nmap <silent> gd <Plug>(coc-definition)
                nmap <silent> gy <Plug>(coc-type-definition)
                nmap <silent> gi <Plug>(coc-implementation)
                nmap <silent> gr <Plug>(coc-references)
                
                " Use K to show documentation in preview window.
                nnoremap <silent> K :call <SID>show_documentation()<CR>
                
                function! s:show_documentation()
                  if (index(['vim','help'], &filetype) >= 0)
                    execute 'h '.expand('<cword>')
                  else
                    call CocAction('doHover')
                  endif
                endfunction
                
                " Highlight the symbol and its references when holding the cursor.
                autocmd CursorHold * silent call CocActionAsync('highlight')
                
                " Symbol renaming.
                nmap <leader>rn <Plug>(coc-rename)
                
                " Formatting selected code.
                xmap <leader>f  <Plug>(coc-format-selected)
                nmap <leader>f  <Plug>(coc-format-selected)
                
                augroup mygroup
                  autocmd!
                  " Setup formatexpr specified filetype(s).
                  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
                  " Update signature help on jump placeholder.
                  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
                augroup end
                
                " Applying codeAction to the selected region.
                " Example: `<leader>aap` for current paragraph
                xmap <leader>a  <Plug>(coc-codeaction-selected)
                nmap <leader>a  <Plug>(coc-codeaction-selected)
                
                " Remap keys for applying codeAction to the current line.
                nmap <leader>ac  <Plug>(coc-codeaction)
                " Apply AutoFix to problem on the current line.
                nmap <leader>qf  <Plug>(coc-fix-current)
                
                " Introduce function text object
                " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
                xmap if <Plug>(coc-funcobj-i)
                xmap af <Plug>(coc-funcobj-a)
                omap if <Plug>(coc-funcobj-i)
                omap af <Plug>(coc-funcobj-a)
                
                " Use <TAB> for selections ranges.
                " NOTE: Requires 'textDocument/selectionRange' support from the language server.
                " coc-tsserver, coc-python are the examples of servers that support it.
                nmap <silent> <TAB> <Plug>(coc-range-select)
                xmap <silent> <TAB> <Plug>(coc-range-select)
                
                " Add `:Format` command to format current buffer.
                command! -nargs=0 Format :call CocAction('format')
                
                " Add `:Fold` command to fold current buffer.
                command! -nargs=? Fold :call     CocAction('fold', <f-args>)
                
                " Add `:OR` command for organize imports of the current buffer.
                command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
                
                " Add (Neo)Vim's native statusline support.
                " NOTE: Please see `:h coc-status` for integrations with external plugins that
                " provide custom statusline: lightline.vim, vim-airline.
                set statusline^=%{coc#status()}%{get(b:,'coc_current_function',''')}
                set statusline+=%F
                
                " Mappings using CoCList:
                " Show all diagnostics.
                nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
                " Manage extensions.
                nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
                " Show commands.
                nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
                " Find symbol of current document.
                nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
                " Search workspace symbols.
                nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
                " Do default action for next item.
                nnoremap <silent> <space>j  :<C-u>CocNext<CR>
                " Do default action for previous item.
                nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
                " Resume latest coc list.
                nnoremap <silent> <space>p  :<C-u>CocListResume<CR>

                set termguicolors
                set diffopt=vertical,filler
                colorscheme base16-classic-light
                set list
                set listchars=tab:>-
                set expandtab
                set shiftwidth=2
                set softtabstop=2
                set tabstop=2


                " denite configuration
                call denite#custom#var('file/rec', 'command', ['${nixpkgs-20-03.ripgrep}/bin/rg', '--files', '--glob', '!.git'])

                call denite#custom#var('grep', 'command', ['rg'])
                call denite#custom#var('grep', 'default_opts',
                \ ['-i', '--vimgrep', '--no-heading'])
                call denite#custom#var('grep', 'recursive_opts', [])
                call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
                call denite#custom#var('grep', 'separator', ['--'])
                call denite#custom#var('grep', 'final_opts', [])
                call denite#custom#option('default', 'prompt', 'λ')

                
                nmap <leader>p :Denite -start-filter file/rec<CR>
                nmap <leader>b :Denite buffer<CR>
                nnoremap <leader>g :Denite grep<CR>


                augroup mygroup2
                  autocmd!
                  autocmd FileType denite nnoremap <silent><buffer><expr> <CR>  denite#do_map('do_action')
                  autocmd FileType denite nnoremap <silent><buffer><expr> d     denite#do_map('do_action', 'delete')
                  autocmd FileType denite nnoremap <silent><buffer><expr> p     denite#do_map('do_action', 'preview')
                  autocmd FileType denite nnoremap <silent><buffer><expr> <C-v> denite#do_map('do_action', 'vsplit')
                  autocmd FileType denite nnoremap <silent><buffer><expr> <C-x> denite#do_map('do_action', 'split')
                  autocmd FileType denite nnoremap <silent><buffer><expr> <Esc> denite#do_map('quit')
                  autocmd FileType denite nnoremap <silent><buffer><expr> i     denite#do_map('open_filter_buffer')
                  autocmd FileType denite nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
                  autocmd FileType denite-filter imap <silent><buffer> <Esc> <Plug>(denite_filter_quit)
                augroup END
            '';
            packages.myVimPackage =  {
            # see examples below how to use custom packages
            start = with nixpkgs-20-03.vimPlugins; [ fugitive base16-vim vim-nix coc-nvim denite ];
            # If a vim plugin has a dependency that is not explicitly listed in
            # opt that dependency will always be added to start to avoid confusion.
            opt = [ ];
          };
        };
      };
in
import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/b67bc34d4e3de1e89b8bb7cd6e375ba44f1ae8ca";
    sha256 = "1q9a64bl5afflgpa2iaw1q7z7s08c8xq9w6lndlnc5c3siajrp8v";
  })
  {
    config = {
      allowUnfree = true; # for vscode
      packageOverrides = pkgs:
      let 
        all-hies = import (fetchTarball {
       #    version 1.1, symbol lookup across files didn't work but better than nothing
          url    = "https://github.com/infinisil/all-hies/tarball/4b6aab017cdf96a90641dc287437685675d598da";
          sha256 = "0ap12mbzk97zmxk42fk8vqacyvpxk29r2wrnjqpx4m2w9g7gfdya";
        }) {};

        hies = all-hies.selection { selector = p:{ inherit (p) ghc865;  };};

        polysemy-plugin-source = fetchTarball {
          url = "http://hackage.haskell.org/package/polysemy-plugin-0.2.5.0/polysemy-plugin-0.2.5.0.tar.gz";
          sha256 = "0jnps8kwxd0hakis5ph77r45mv1qnkxdf5506shcjb1zmxqmxpjv";
        };

        polysemy-source = fetchTarball {
          url = "http://hackage.haskell.org/package/polysemy-1.3.0.0/polysemy-1.3.0.0.tar.gz";
          sha256 = "1p75i56qpl0v79vrlzw04117czzgwhn1l0vadvka8m7drmcvwsf6";
        };

      coc-config = pkgs.writeText "cocConfig.json" ''
              {
                      "languageserver": {
                              "haskell": {
                                      "command": "hie-wrapper",
                                      "args": ["--lsp"],
                                      "rootPatterns": [
                                              "*.cabal",
                                      ],
                                      "filetypes": [
                                              "hs",
                                              "lhs",
                                              "haskell"
                                      ],
                                      "initializationOptions": {
                                              "languageServerHaskell": {
                                              }
                                      }
                              }
                      }
              }
        '';

      in
      rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            backend = self.callCabal2nix "backend" (pkgs.lib.sourceFilesBySuffices ./backend [ ".hs" ".cabal"]) {};
            polysemy-plugin = self.callCabal2nix "polysemy-plugin" polysemy-plugin-source {};
            polysemy = self.callCabal2nix "polysemy" polysemy-source {};
          };
        };

        backend = haskellPackages.backend;
        nixpkgs2003 = nixpkgs-20-03;
      backendDev = haskellPackages.shellFor {
        shellHook = ''
          case "$-" in
          *i*) I=1 ;;
          *)   I=0 ;; 
          esac

          if [[ $I == 1 ]]
          then
          echo "*******************************************************"
          echo  To get a working configuration for coc-nvim do
          echo
          echo    cp ${coc-config} ~/.config/nvim/coc-settings.json
          echo "*******************************************************"
          fi
          '';
        packages = p: [p.backend];
        withHoogle = true;
        buildInputs = [ haskellPackages.cabal-install hies ourNeovim nodejsToUse haskellPackages.ghcid ]; # nixpkgs-20-03.haskellPackages.ormolu ];        
      };
    };
  };
}
