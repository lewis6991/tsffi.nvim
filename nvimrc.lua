vim.api.nvim_create_autocmd('FileType', {
  pattern = 'c',
  callback = function()
    vim.treesitter.start()
    -- vim.treesitter.get_parser()
  end,
})

vim.opt.runtimepath:append('.')

local tsffi = require('tsffi')

tsffi.init('.deps/tree-sitter/lib/include/tree_sitter/api.h')
tsffi.setup()
