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

--- @param ranges TSRange[]
--- @param length integer
--- @param include_bytes? boolean
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

--- @class TSTree.ffi : TSTree
local TSTree = {}

do -- TSTree
  function TSTree:root()
    -- TODO(lewis6991)
  end

  ---@param start_byte integer
  ---@param end_byte_old integer
  ---@param end_byte_new integer
  ---@param start_row integer
  ---@param start_col integer
  ---@param end_row_old integer
  ---@param end_col_old integer
  ---@param end_row_new integer
  ---@param end_col_new integer
  ---@nodoc
  function TSTree:edit(
    start_byte,
    end_byte_old,
    end_byte_new,
    start_row,
    start_col,
    end_row_old,
    end_col_old,
    end_row_new,
    end_col_new
  )
    vim.validate('start_byte', start_byte, 'number')
    vim.validate('end_byte_old', end_byte_old, 'number')
    vim.validate('end_byte_new', end_byte_new, 'number')
    vim.validate('start_row', start_row, 'number')
    vim.validate('start_col', start_col, 'number')
    vim.validate('end_row_old', end_row_old, 'number')
    vim.validate('end_col_old', end_col_old, 'number')
    vim.validate('end_row_new', end_row_new, 'number')
    vim.validate('end_col_new', end_col_new, 'number')

    local start_point = ffi.new('TSPoint', { start_row, start_col })
    local old_end_point = ffi.new('TSPoint', { end_row_old, end_col_old })
    local new_end_point = ffi.new('TSPoint', { end_row_new, end_col_new })

    local edit = ffi.new('TSInputEdit', {
      start_byte,
      end_byte_old,
      end_byte_new,
      start_point,
      old_end_point,
      new_end_point,
    })

    ffi.C.ts_tree_edit(self, edit)
  end

  ---@param include_bytes boolean?
  ---@return Range4[]|Range6[]
  function TSTree:included_ranges(include_bytes)
    local len = ffi.new('int[1]')
    local ranges = ffi.gc(ffi.C.ts_tree_included_ranges(self, len), ffi.C.free)
    return make_ranges(ranges, len[0], include_bytes)
  end

  function TSTree:copy()
    return ffi.gc(ffi.C.ts_tree_copy(self), ffi.C.ts_tree_delete)
  end

  function TSTree:__tostring()
    -- TODO(lewis6991)
  end
end

--- @class TSQueryCursor.ffi : TSQueryCursor
local TSQueryCursor = {}

do -- TSQueryCursor
  function TSQueryCursor:remove_match()
    -- TODO(lewis6991)
  end

  function TSQueryCursor:next_capture()
    -- TODO(lewis6991)
  end

  function TSQueryCursor:next_match()
    -- TODO(lewis6991)
  end
end

--- @class TSParser.ffi : TSParser
local TSParser = {}

do -- TSParser
  --- @param tree? TSTree
  --- @param source string|integer
  --- @param include_bytes? boolean
  --- @param _timeout_ns? integer
  --- @return TSTree?
  --- @return Range4[]|Range6[]?
  function TSParser:parse(tree, source, include_bytes, _timeout_ns)
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

    ffi.gc(new_tree, ffi.C.ts_tree_delete)

    local n_ranges = ffi.new('int[1]')

    local changed = tree and ffi.C.ts_tree_get_changed_ranges(tree, new_tree, n_ranges)
      or ffi.C.ts_tree_included_ranges(new_tree, n_ranges)

    ffi.gc(changed, ffi.C.free)

    return new_tree, make_ranges(changed, n_ranges[0], include_bytes)
  end

  function TSParser:reset()
    -- TODO(lewis6991)
  end

  function TSParser:set_included_ranges()
    -- TODO(lewis6991)
  end

  function TSParser:included_ranges()
    -- TODO(lewis6991)
  end

  function TSParser:_set_logger()
    -- TODO(lewis6991)
  end

  function TSParser:_logger()
    -- TODO(lewis6991)
  end

  function TSParser:__tostring()
    -- TODO(lewis6991)
  end
end

local M = {}

--- @type table<string, TSLanguage>
M.langs = {}

--- @param lang string
--- @return TSLanguage
local function lang_check(lang)
  vim.validate('lang', lang, 'string')
  local l = M.langs[lang]
  if not l then
    error(('no such language: %s'):format(lang), 2)
  end
  return l
end

--- @param ts_api_path string
function M.init(ts_api_path)
  ffi.cdef(get_ts_cdef(ts_api_path))
  ffi.cdef([[void *free(void *);]])
  ffi.metatype('TSParser', { __index = TSParser })
  ffi.metatype('TSTree', { __index = TSTree })
  ffi.metatype('TSQueryCursor', { __index = TSQueryCursor })
end

--- @param lang string
--- @return TSParser
function M._create_ts_parser(lang)
  local parser = ffi.gc(ffi.C.ts_parser_new(), ffi.C.ts_parser_delete)
  ffi.C.ts_parser_set_language(parser, lang_check(lang))
  return parser
end

--- @param node TSNode
--- @param query TSQuery
--- @param start integer?
--- @param stop integer?
--- @param opts? { max_start_depth?: integer, match_limit?: integer}
--- @return TSQueryCursor
function M._create_ts_querycursor(node, query, start, stop, opts)
  local cursor = ffi.gc(ffi.C.ts_query_cursor_new(), ffi.C.ts_query_cursor_delete)

  ffi.C.ts_query_cursor_exec(cursor, query, node)

  if start and stop then
    local s = ffi.new('TSPoint', { start, 0 })
    local e = ffi.new('TSPoint', { stop, 0 })
    ffi.C.ts_qeury_cursor_set_point_range(cursor, s, e)
  end

  if opts then
    if opts.max_start_depth then
      ffi.C.ts_query_cursor_set_new_start_depth(cursor, opts.max_start_depth)
    elseif opts.match_limit then
      ffi.C.ts_query_cursor_set_match_limit(cursor, opts.match_limit)
    end
  end

  return cursor
end

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
