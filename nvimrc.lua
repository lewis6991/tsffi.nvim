-- vim.api.nvim_create_autocmd('FileType', {
--   pattern = 'c',
--   callback = function()
--     vim.treesitter.start()
--   end,
-- })

vim.o.swapfile = false

vim.opt.runtimepath:append('.')

require('tsffi').setup()

vim.cmd.edit('dcn_3_2_0_sh_mask.h')

local runs = 5

local e = 0
for _ = 1, runs do
  local parser = assert(vim.treesitter.get_parser(0, 'c'))
  parser:invalidate(true)
  local s = vim.uv.hrtime()
  parser:parse(true)
  e = e + (vim.uv.hrtime() - s)
end

print(('Parse time %.2f ms'):format((e / 1e6) / runs))
