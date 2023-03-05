---------------------------------------------------------------------------
-- packer setup
---------------------------------------------------------------------------
vim.cmd [[packadd packer.nvim]]
require('packer').startup(function(use)
  use 'skywind3000/asyncrun.vim'
  use 'morhetz/gruvbox'
  use 'nvim-lualine/lualine.nvim'
  use 'jose-elias-alvarez/null-ls.nvim'
  use 'neovim/nvim-lspconfig'
  use 'nvim-tree/nvim-tree.lua'
  use 'nvim-treesitter/nvim-treesitter'
  use 'nvim-tree/nvim-web-devicons'
  use 'wbthomason/packer.nvim'
  use {
    'nvim-telescope/telescope.nvim',
    requires = { { 'nvim-lua/plenary.nvim' } },
  }
  use 'wincent/vim-clipper'
  use 'christoomey/vim-tmux-navigator'
  -- Provides :FSHere very useful for cpp files
  use 'derekwyatt/vim-fswitch'
  use 'preservim/vimux'
end)

---------------------------------------------------------------------------
-- Basic Settings
---------------------------------------------------------------------------

vim.opt.colorcolumn = '80,132'
vim.opt.number = true
vim.opt.list = true
vim.opt.listchars:append 'tab:> '
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.cursorline = true
vim.opt.cursorcolumn = true
vim.opt.swapfile = false
vim.opt.wrap = false
-- Map global leader from \ to Space
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
-- Persist undo tree across neovim sessions
vim.opt.undofile = true

local builtin = require 'telescope.builtin'
-- Find File(ff): Open another file in same dir as current file,
vim.keymap.set('n', '<leader>ff', function()
  local opts = {
    search_dirs = { vim.fn.expand '%:h' },
    find_command = { 'fd', '--max-depth', '1' },
  }
  builtin.find_files(opts)
end, {})

-- Keep your fingers from the home row OR use ctrl-[ instead
vim.keymap.set('i', 'jj', '<Esc>', {})

vim.keymap.set('n', '<leader>n', ':nohlsearch<CR>', {})
-- Project Find(pf): Open another file from project (git/hg repository)
vim.keymap.set('n', '<leader>pf', builtin.find_files, {})
-- Find Recent(fr): Open recently used files
vim.keymap.set('n', '<leader>fr', builtin.oldfiles, {})
-- Grep in files
vim.keymap.set('n', '<leader>/', builtin.live_grep, {})
-- derekwyatt/vim-fswitch
vim.keymap.set('n', '<leader>a', ':FSHere<CR>', {})
-- Easy split
vim.keymap.set('n', '<leader>s', ':sp<CR>', {})
vim.keymap.set('n', '<leader>v', ':vs<CR>', {})

--
-- Folding Rules
--

-- {{{
vim.opt.foldenable = true
vim.opt.foldmethod = 'marker'
-- }}}

---------------------------------------------------------------------------
-- Plugin Specific Settings
---------------------------------------------------------------------------

--
-- For skywind3000/asyncrun.vim
--

-- bb = buck build
-- nnoremap <leader>bb :AsyncRun -mode=term -pos=toggleterm buck query "owner('$(realpath %)')"<CR>
-- nnoremap <leader>bb :AsyncRun -mode=term -pos=tmux buck query "owner('$(realpath %)')"<CR>
-- nnoremap <Leader>bb :AsyncRun -mode=term -pos=tmux buck build $(buck query "owner('$(realpath %)')" \| head -1) 2> ~/vim_out.log<CR>
vim.keymap.set(
  'n',
  '<leader>bb',
  ':AsyncRun -mode=term -pos=tmux buck build $(buck query "owner(\'$(realpath %)\')" | head -1)<CR>',
  {}
)
-- le = load error,
-- Usefull while using AsyncRun with default -mode and -pos
vim.keymap.set('n', '<leader>le', ':cget ~/vim_out.log | :copen<CR>"owner(\'$(realpath %)\')" | head -1)<CR>', {})

--
-- For nvim-lualine/lualine.nvim
--

require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'ayu_dark',
  },
}

--
-- For neovim/nvim-lspconfig
--

local opts = { noremap = true, silent = true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', function()
    vim.lsp.buf.format { async = true }
  end, bufopts)
end
require('lspconfig').lua_ls.setup {
  settings = {
    Lua = {
      telemetry = { enable = false },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
    },
  },
  on_attach = on_attach,
}

--
-- For nvim-telescope/telescope.nvim
--

require('telescope').setup {
  pickers = {
    find_files = {
      hidden = true,
    },
    live_grep = {
      additional_args = function()
        return { '--hidden' }
      end,
    },
  },
}

--
-- For nvim-tree/nvim-tree.lua
--

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require('nvim-tree').setup()

--
-- For nvim-treesitter/nvim-treesitter
--

require('nvim-treesitter.configs').setup {
  ensure_installed = { 'c', 'cpp', 'lua', 'markdown', 'python', 'rust', 'vim' },
  highlight = {
    enable = true,
  },
}

--
-- For jose-elias-alvarez/null-ls.nvim
--

local null_ls = require 'null-ls'
null_ls.setup {
  on_attach = on_attach,
  sources = {
    null_ls.builtins.formatting.stylua,
  },
}

--
-- For wincent/vim-clipper
--

vim.g.ClipperPort = 5556

--
-- For christoomey/vim-tmux-navigator
--

vim.g.tmux_navigator_no_mappings = 0

---------------------------------------------------------------------------
-- Set Colorscheme
---------------------------------------------------------------------------

vim.cmd [[colorscheme gruvbox]]
