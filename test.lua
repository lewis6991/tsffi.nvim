assert(#arg == 3, 'Usage: nvim -l test.lua <ts_api> <ts_c_so> <ts_lang>')

local ts_api = arg[1]
local ts_c_so = arg[2]
local ts_lang = arg[3]

vim.opt.runtimepath:append('.')

local tsffi = require('tsffi')
tsffi.init(ts_api)

tsffi.tsffi._ts_add_language_from_object(ts_c_so, ts_lang)
local parser = tsffi.tsffi._create_ts_parser(ts_lang)

local tree, ranges = parser:parse(nil, 'set a 1')
tree, ranges = parser:parse(tree, 'set a b')
print(type(tree))
