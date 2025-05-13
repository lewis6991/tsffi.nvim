vim.opt.runtimepath:append('.')

local tsffi = require('tsffi')
tsffi.setup()

vim.treesitter.language.add('c')

local parser = tsffi.tsffi._create_ts_parser('c')

local tree, ranges = parser:parse(nil, 'set a 1')
tree, ranges = parser:parse(tree, 'set a b')
print(type(tree))
