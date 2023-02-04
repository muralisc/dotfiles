vim.opt.number = true
vim.cmd([[packadd packer.nvim]])
require("packer").startup(function()
  use("wbthomason/packer.nvim")
  use("morhetz/gruvbox")
  vim.cmd([[colorscheme gruvbox]])
  use("nvim-treesitter/nvim-treesitter")
  require("nvim-treesitter.configs").setup({
    ensure_installed = { "c", "lua", "rust" },
    highlight = {
      enable = false,
    },
  })
  use("nvim-lualine/lualine.nvim")
  require("lualine").setup({
    options = {
      icons_enabled = true,
      theme = "ayu_dark",
    },
  })
  use({
    "nvim-telescope/telescope.nvim",
    requires = { { "nvim-lua/plenary.nvim" } },
  })
  require("telescope").setup({
    pickers = {
      find_files = {
        hidden = true,
      },
      live_grep = {
        additional_args = function(opts)
          return { "--hidden" }
        end,
      },
    },
  })
end)
vim.opt.list = true
vim.opt.listchars:append("tab:> ")
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.cursorline = true
vim.opt.cursorcolumn = true
vim.opt.swapfile = false
vim.opt.wrap = false
-- Map global leader from \ to Space
vim.g.mapleader = " "
-- Persist undo tree across neovim sessions
vim.opt.undofile = true

local builtin = require("telescope.builtin")
-- Open another file in same dir as current file, Using keymaps from spacemacs
vim.keymap.set("n", "<leader>ff", function()
  opts = {
    search_dirs = { vim.fn.expand("%:h") },
    find_command = { "fd", "--max-depth", "1" },
  }
  builtin.find_files(opts)
end, {})
-- FZF is faster than CtrlP for finding files in Directories
-- Telescope is not as fast as FZF but using it anyway for having a common tool
-- (pf - after projectile find/project find, using same as spacemacs)
vim.keymap.set("n", "<leader>pf", builtin.find_files, {})
-- Open recently used files
-- alternate for => :CtrlPMRUFiles <CR>
-- alternate for => :History from fzf
-- Using keymaps from spacemacs
-- See: https://develop.spacemacs.org/doc/DOCUMENTATION.html
vim.keymap.set("n", "<leader>fr", builtin.oldfiles, {})
-- Grep in files
vim.keymap.set("n", "<leader>/", builtin.live_grep, {})
