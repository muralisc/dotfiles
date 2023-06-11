-- vim: foldlevel=2:

-- Courtsey:
--   Vincent Driessen <vincent@datafox.nl>
--       http://nvie.com/posts/how-i-boosted-my-vim/
--   Tsung-Hsiang (Sean) Chang <vgod@vgod.tw>
--       https://github.com/vgod/vimrc/blob/master/vimrc
--   junegunn:
--       https://github.com/junegunn/dotfiles/blob/master/vimrc
--   https://github.com/yoshuawuyts/dotfiles
--   And Vim User Manual

-- Helpers
--   check a neovim lua var:
--   :lua print(vim.inspect(vim.opt.shiftwidth))

---------------------------------------------------------------------------
-- packer setup
---------------------------------------------------------------------------
vim.cmd([[packadd packer.nvim]])
require("packer").startup(function(use)
  -- The plugins are ordered by their names
  -- Helper:
  -- grep 'use(' ~/.config/nvim/init.lua | sort -t '/' -k 2

  -- asyncrun
  -- Usecase:
  --     Run a build command
  --     1. Async in vim
  --     2. Or in toggleterm
  --     3. Or in tmux
  use("skywind3000/asyncrun.vim")
  -- diffconflicts - easily address diffconfilicts in nvim :DiffConflicts
  use("whiteinge/diffconflicts")
  -- leap.nvim - mapped to s in 'normal' mode
  -- Usecase:
  --     Jump to a location in visible buffer area - use leap
  --     Jump to a location in any buffer area - use native vim search
  use("ggandor/leap.nvim")
  use("nvim-lualine/lualine.nvim")
  use("jose-elias-alvarez/null-ls.nvim")
  use("neovim/nvim-lspconfig")
  -- nvim-miniyank block paste fix for nvim
  -- Usecase:
  --     Fix for: Block paste not working when clipboard=unnamed
  --     https://github.com/neovim/neovim/issues/1822
  use("bfredl/nvim-miniyank")
  use("nvim-tree/nvim-tree.lua")
  use("nvim-treesitter/nvim-treesitter")
  use("nvim-tree/nvim-web-devicons")
  use("wbthomason/packer.nvim")
  -- tabular - Massively useful plugin for easily aligning text
  use("godlygeek/tabular")
  use({
    "nvim-telescope/telescope.nvim",
    requires = { { "nvim-lua/plenary.nvim" } },
  })
  use("freitass/todo.txt-vim")
  -- toggleterm.nvim (c-t, esc:c-j)
  --     Default Alternatives
  --         :sp term://zsh or
  --         :vs term://zsh
  --         ESC -> <c-\><c-n>
  use("akinsho/toggleterm.nvim")
  -- ultisnips
  use({ "SirVer/ultisnips", requires = { { "honza/vim-snippets" }, { "muralisc/snippets" } } })

  -- use("wincent/vim-clipper")
  -- vim-commentary
  --    map: gcc
  use("tpope/vim-commentary")
  -- vim-fswitch - Provides :FSHere very useful for cpp files
  use("derekwyatt/vim-fswitch")
  use("preservim/vim-markdown")
  -- vim-ledger
  --    Provides :LedgerAlign and :LedgerAlignBuffer
  --    Better aligned with Tabularize
  --    :Tabularize /=/l12c1r0
  use("ledger/vim-ledger")
  -- vim-plugin-viewdoc - For viewing help files
  use("powerman/vim-plugin-viewdoc")
  -- vim-tmux-navigator
  --    For compatability with tmux
  --    Using Meta-[hjkl] mappings in tmux to move panes
  use("christoomey/vim-tmux-navigator")
  -- vim-unimpaired: Awesome bracket maps
  --     [q ]q :cprevious :cnext
  --     [n ]n Go to git/hg confict marker
  --     yow - toggle wrap
  use("tpope/vim-unimpaired")
  -- Used by async run to run commands in tmux term
  use("preservim/vimux")

  -- Colorscheme Plugins

  -- Some colorscheme tested and conclusion
  -- solarized                - Good
  -- gruvbox                  - Good
  -- apprentice               - Good
  -- gotham                   - bad for diff highlight
  -- dracula                  - bad for types
  -- nord                     - bad for diff highlight
  -- onedark                  - GOOD
  -- base16-solarized-dark    - GOOD
  -- jellybeans               - bad for diff
  -- base16-summerfruit-dark  - GOOD
  -- catppuccin               - visual highlighting is not easily visible
  use("morhetz/gruvbox")
  use("EdenEast/nightfox.nvim") -- not good for diff view
  use("folke/tokyonight.nvim") -- not good for diff view
  use("rebelot/kanagawa.nvim") -- not good for diff view
  use("catppuccin/nvim") -- not good for diff view
  use("rose-pine/neovim")
end)

---------------------------------------------------------------------------
-- Basic Settings
---------------------------------------------------------------------------

--
-- Uncategorised settings
--

-- always show line numbers
vim.opt.number = true
-- Expand tabs to spaces by default (overloadable per file type later)
vim.opt.expandtab = true
-- number of spaces to use for autoindenting
vim.opt.shiftwidth = 4
vim.opt.swapfile = false
vim.opt.wrap = false
-- 132 is Github width :P
vim.opt.colorcolumn = "80,132"

vim.opt.clipboard= "unnamedplus"

--
-- Search settings
--

-- search/replace 'globally' (on a line) by default
vim.opt.gdefault = true
-- -- ignore case when searching
vim.opt.ignorecase = true
-- ignore case if search pattern is all lowercase, case-sensitive otherwise
vim.opt.smartcase = true

--
--- Vim commands
--

-- ignore case while filename complete
vim.opt.wildignorecase = true

--
--- Editing - Basic Settings influencing edition behavior
--

-- list: show invisible charecters
vim.opt.list = true
-- If 'set list' is enabled, the invisible characters are show using listchars
vim.opt.listchars:append({tab="> ",trail="·"})

--
--- UI - Settings influencing UI behaviors
--

-- set show matching parenthesis
vim.opt.showmatch = true
-- underline the current line, for quick orientation
vim.opt.cursorline = true
-- have a vertical line marking the cursor column
vim.opt.cursorcolumn = true

--
-- Undo/'Getting lost things' settings
--

-- Persist undo tree across neovim sessions
vim.opt.undofile = true

--
-- Folding Rules
--

-- {{{
vim.opt.foldenable = true
vim.opt.foldmethod = "marker"
-- 0-foldall 99-unfoldall
vim.opt.foldlevel = 99
-- which commands trigger auto-unfold
vim.opt.foldopen:append({
  "block",
  "hor",
  "insert",
  "jump",
  "mark",
  "percent",
  "quickfix",
  "search",
  "tag",
  "undo",
})
-- }}}

----------------------------------------------------------------------------
-- Shortcut Mappings
----------------------------------------------------------------------------

-- Resize
vim.keymap.set("n", "<Up>", "5<c-w>+", {})
vim.keymap.set("n", "<Down>", "5<c-w>-", {})
vim.keymap.set("n", "<Right>", "5<c-w>>", {})
vim.keymap.set("n", "<Left>", "5<c-w><", {})
-- Keep your fingers from the home row OR use ctrl-[ instead
vim.keymap.set("i", "jj", "<Esc>", {})
-- Thanks to Steve Losh for this liberating tip[perl/python compatible regex]
-- See http://stevelosh.com/blog/2010/09/coming-home-to-vim
vim.keymap.set("n", "/", "/\\v", {})
vim.keymap.set("v", "/", "/\\v", {})

-- Map global leader from \ to Space
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Clears the search register
vim.keymap.set("n", "<leader>n", ":nohlsearch<CR>", {})
-- Easy split
vim.keymap.set("n", "<leader>s", ":sp<CR>", {})
vim.keymap.set("n", "<leader>v", ":vs<CR>", {})


-- Clipboard madness {{{
vim.keymap.set("n", "p", "<Plug>(miniyank-autoput)", {})
vim.keymap.set("n", "P", "<Plug>(miniyank-autoput)", {})
vim.keymap.set("v", "<leader>P", "\"_dp", {})
-- }}}

vim.cmd [[
augroup FTOptions
  autocmd!
  autocmd Filetype markdown                     setlocal iskeyword+=# textwidth=80
augroup end
]]

---------------------------------------------------------------------------
-- Plugin Specific Settings
---------------------------------------------------------------------------

--
-- For skywind3000/asyncrun.vim
--

-- bb = buck build
-- nnoremap <leader>bb :AsyncRun -mode=term -pos=toggleterm buck query "owner('$(realpath %)')"<CR>
-- nnoremap <leader>bb :AsyncRun -mode=term -pos=tmux buck2 query "owner('$(realpath %)')"<CR>
-- nnoremap <Leader>bb :AsyncRun -mode=term -pos=tmux buck2 build $(buck query "owner('$(realpath %)')" \| head -1) 2> ~/vim_out.log<CR>
vim.keymap.set(
  "n",
  "<leader>bb",
  ":AsyncRun -mode=term -pos=tmux buck2 build $(buck query \"owner('$(realpath %)')\" | head -1)<CR>",
  {}
)

-- le = load error,
-- Usefull while using AsyncRun with default -mode and -pos
vim.keymap.set("n", "<leader>le", ":cget ~/vim_out.log | :copen<CR>\"owner('$(realpath %)')\" | head -1)<CR>", {})

--
-- For ggandor/leap.nvim
--

require("leap").set_default_keymaps()
-- color help from : https://vim.fandom.com/wiki/Xterm256_color_names_for_console_Vim
vim.api.nvim_set_hl(0, "LeapLabelPrimary", { ctermbg = 111, ctermfg = 016, bold = true })
vim.api.nvim_set_hl(0, "LeapLabelSecondary", { ctermbg = 046, ctermfg = 016 })

--
-- For nvim-lualine/lualine.nvim
--

require("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "ayu_dark",
  },
    sections = {
        lualine_c = {
    {
      'filename',
      file_status = true,      -- Displays file status (readonly status, modified status)
      newfile_status = false,  -- Display new file status (new file means no write after created)
      path = 3,                -- 3: Absolute path, with tilde as the home directory

      shorting_target = 40,    -- Shortens path to leave 40 spaces in the window
                               -- for other components. (terrible name, any suggestions?)
    }
  }
    }
})

--
-- For neovim/nvim-lspconfig
--

local opts = { noremap = true, silent = true }
vim.keymap.set("n", "<space>e", vim.diagnostic.open_float, opts)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist, opts)
local on_attach = function(_client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap = true, silent = true, buffer = bufnr }
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, bufopts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, bufopts)
  vim.keymap.set("n", "K", vim.lsp.buf.hover, bufopts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, bufopts)
  vim.keymap.set("n", "<space>D", vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set("n", "<space>rn", vim.lsp.buf.rename, bufopts)
  vim.keymap.set("n", "<space>ca", vim.lsp.buf.code_action, bufopts)
  vim.keymap.set("n", "gr", vim.lsp.buf.references, bufopts)
  vim.keymap.set("n", "<space>f", function()
    vim.lsp.buf.format({ async = true })
  end, bufopts)
end
require("lspconfig").lua_ls.setup({
  settings = {
    Lua = {
      telemetry = { enable = false },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
    },
  },
  on_attach = on_attach,
})

--
-- For nvim-telescope/telescope.nvim
--

require("telescope").setup({
  defaults = {
    path_display = { truncate = 3 },
  },
  pickers = {
    find_files = {
      hidden = true,
    },
    live_grep = {
      additional_args = function()
        return { "--hidden" }
      end,
    },
  },
})

local builtin = require("telescope.builtin")
-- Find File(ff): Open another file in same dir as current file,
-- Using keymaps from spacemacs
vim.keymap.set("n", "<leader>ff", function()
  local opts = {
    search_dirs = { vim.fn.expand("%:h") },
    find_command = { "fd", "--max-depth", "1" },
  }
  builtin.find_files(opts)
end, {})
-- cf(create file): create sibling file
vim.keymap.set("n", "<leader>cf", ":e %:h/<C-d>", {})
-- Project Find(pf): Open another file from project (git/hg repository)
vim.keymap.set("n", "<leader>pf", builtin.find_files, {})
-- Find Recent(fr): Open recently used files
vim.keymap.set("n", "<leader>fr", builtin.oldfiles, {})
-- Grep in files
vim.keymap.set("n", "<leader>/", builtin.live_grep, {})

--
-- For nvim-tree/nvim-tree.lua
--

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
require("nvim-tree").setup()
-- Using same mapping as spacemacs for opening treemacs
vim.keymap.set("n", "<leader>fn", ":NvimTreeFindFile<cr>", {})

--
-- For nvim-treesitter/nvim-treesitter
--

require("nvim-treesitter.configs").setup({
  ensure_installed = { "c", "cpp", "lua", "markdown", "python", "rust", "vim" },
  highlight = {
    enable = true,
  },
})

--
-- For jose-elias-alvarez/null-ls.nvim
--

local null_ls = require("null-ls")
null_ls.setup({
  -- on_attach = on_attach,
  sources = {
    null_ls.builtins.formatting.stylua,
  },
})

--
-- For akinsho/toggleterm.nvim
--

require("toggleterm").setup{
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.5
    end
  end,
  open_mapping = [[<c-t>]],
  shade_terminals = true,
  shading_factor = 9,
  persist_size = true,
  direction = 'vertical',
  start_in_insert = false,
}
function _G.set_terminal_keymaps()
  local opts = {noremap = true}
  vim.api.nvim_buf_set_keymap(0, 't', '<C-j>', [[<C-\><C-n>]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-h>', [[<C-\><C-n><C-W>h]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-j>', [[<C-\><C-n><C-W>j]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-k>', [[<C-\><C-n><C-W>k]], opts)
  vim.api.nvim_buf_set_keymap(0, 't', '<A-l>', [[<C-\><C-n><C-W>l]], opts)
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')

--
-- For SirVer/ultisnips
--
vim.g.UltiSnipsExpandTrigger = "<tab>"
vim.g.UltiSnipsListSnippets = "<c-tab>"

--
-- For wincent/vim-clipper
--

vim.g.ClipperPort = 5556

--
-- For christoomey/vim-tmux-navigator
--

-- For compatability with tmux
-- Using Meta-[hjkl] mappings in tmux to move panes
vim.g.tmux_navigator_no_mappings = 0

--
-- For derekwyatt/vim-fswitch
--

vim.keymap.set("n", "<leader>a", ":FSHere<CR>", {})

---------------------------------------------------------------------------
-- Set Colorscheme
---------------------------------------------------------------------------

if vim.opt.diff:get() then
 vim.cmd([[colorscheme gruvbox ]])
else
 vim.cmd([[colorscheme rose-pine ]])
end

