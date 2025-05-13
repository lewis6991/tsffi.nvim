vim.api.nvim_create_autocmd('FileType', {
  pattern = 'c',
  callback = function()
    vim.treesitter.start()
    -- vim.treesitter.get_parser()
  end,
})

vim.opt.runtimepath:append('.')

require('tsffi').setup()
