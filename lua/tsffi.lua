local ffi = require('ffi')

local TS_LANGUAGE_VERSION = vim._ts_get_language_version()
local TS_MIN_COMPATIBLE_LANG_VERSION = vim._ts_get_minimum_language_version()

--- @class TSLanguage: userdata

--- @class TSPoint
--- @field row integer
--- @field column integer

--- @class TSRange: userdata
--- @field start_point TSPoint
--- @field end_point TSPoint
--- @field start_byte integer
--- @field end_byte integer

local function get_ts_cdef(path)
  local header_f = assert(io.open(path, 'r'))
  local header = header_f:read('*a')
  header = header:gsub('#[^\n]+\n', '\n')
  header = header:gsub('\nextern [^\n]+\n', '\n')
  header = header:gsub('\n}\n', '\n')
  header_f:close()
  return header
end

local Tree = {}

--- @class TSParser.ffi : TSParser
--- @field included_ranges fun(self: TSParser, include_bytes: boolean?): integer[]
--- @field set_included_ranges fun(self: TSParser, ranges: (Range6|TSNode)[])
--- @field _set_logger fun(self: TSParser, lex: boolean, parse: boolean, cb: TSLoggerCallback)
--- @field _logger fun(self: TSParser): TSLoggerCallback
local Parser = {}

--- @param ranges TSRange[]
--- @param length integer
--- @param include_bytes boolean
--- @return Range4[]|Range6[]
local function make_ranges(ranges, length, include_bytes)
  local r = {} --- @type Range4[]|Range6[]
  for i = 0, length - 1 do
    --- @type integer[]
    local s = {
      ranges[i].start_point.row,
      ranges[i].start_point.column,
      include_bytes and ranges[i].start_byte or nil,
    }
    vim.list_extend(s, {
      ranges[i].end_point.row,
      ranges[i].end_point.column,
      include_bytes and ranges[i].end_byte or nil,
    })
    r[#r + 1] = s
  end
  return r
end

--- @param tree? TSTree
--- @param source string|integer
--- @param include_bytes? boolean
--- @param timeout_ns? integer
--- @return TSTree?
--- @return Range4[]|Range6[]?
function Parser:parse(tree, source, include_bytes, timeout_ns)
  local new_tree --- @type TSTree?
  if type(source) == 'string' then
    new_tree = ffi.C.ts_parser_parse_string(self, tree, source, #source)
  elseif type(source) == 'number' then
    -- todo
  else
    error('expected either string or buffer handle')
  end

  if not new_tree then
    if not ffi.C.ts_parser_language(self) then
      error('Language was unset, or has an incompatible ABI.')
    end
    return
  end

  local n_ranges = ffi.new('int[1]')

  -- local changed = tree and ffi.C.ts_tree_get_changed_ranges(tree, new_tree, n_ranges)
  --   or ffi.C.ts_tree_included_ranges(new_tree, n_ranges)

  local changed = ffi.C.ts_tree_included_ranges(new_tree, n_ranges)

  return new_tree, make_ranges(changed, n_ranges[0], include_bytes)
end

local M = {}

--- @param ts_api_path string
function M.init(ts_api_path)
  ffi.cdef(get_ts_cdef(ts_api_path))
  ffi.metatype('TSParser', { __index = Parser })
  ffi.metatype('TSTree', { __index = Tree })
end

--- @param lang string
--- @return TSParser.ffi
function M._create_ts_parser(lang)
  local parser = ffi.gc(ffi.C.ts_parser_new(), ffi.C.ts_parser_delete)
  local language = M.langs[lang]
  if not language then
    error(('Language "%s" not found'):format(lang))
  end

  ffi.C.ts_parser_set_language(parser, language)

  return parser
end

--- @type table<string, TSLanguage>
M.langs = {}

--- @param path string
--- @param lang string
--- @param symbol_name? string
--- @return true?
function M._ts_add_language_from_object(path, lang, symbol_name)
  symbol_name = symbol_name or lang
  local symbol = 'tree_sitter_' .. symbol_name
  ffi.cdef(('TSLanguage * %s(void);'):format(symbol))
  local lang_lib = ffi.load(path)
  local lang_func = lang_lib[symbol]
  --- @type TSLanguage?
  local language = lang_func()

  if not language then
    error(("Language function '%s' returned NULL for '%s'"):format(symbol, lang))
  end

  --- @type integer
  local lang_version = ffi.C.ts_language_abi_version(language)

  if lang_version < TS_MIN_COMPATIBLE_LANG_VERSION then
    error(
      ('ABI version mismatch for %s: supported between %d and %d, found %d'):format(
        path,
        TS_MIN_COMPATIBLE_LANG_VERSION,
        TS_LANGUAGE_VERSION,
        lang_version
      )
    )
  end

  M.langs[lang] = language

  return true
end

--- @param lang string
--- @return boolean
function M._ts_has_language(lang)
  return M.langs[lang] ~= nil
end

--- @param lang string
--- @return boolean
function M._ts_remove_language(lang)
  local present = M._ts_has_language(lang)
  if M.langs[lang] then
    M.langs[lang] = nil
  end
  return present
end

M._ts_get_language_version = vim._ts_get_language_version
M._ts_get_minimum_language_version = vim._ts_get_minimum_language_version

return M -- Return the module table
