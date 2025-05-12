local ffi = require('ffi') --[[@as ffilib.tree-sitter]]

--- @type ffilib.tree-sitter.C
local C = setmetatable({}, {
  __index = function(_, k)
    return function(...)
      return ffi.C[k](...)
    end
  end,
})

--- @generic T
--- @param t `T`
--- @return {[0]:T}
local function newptr(t, ...)
  return ffi.new(t .. '[1]', ...)
end

--- @return {[0]:integer}
local function intptr()
  return ffi.new('int[1]')
end

local TS_LANGUAGE_VERSION = vim._ts_get_language_version()
local TS_MIN_COMPATIBLE_LANG_VERSION = vim._ts_get_minimum_language_version()

local function get_ts_cdef(path)
  local header_f = assert(io.open(path, 'r'))
  local header = header_f:read('*a') --- @type string
  header = header:gsub('#[^\n]+\n', '\n')
  header = header:gsub('\nextern [^\n]+\n', '\n')
  header = header:gsub('\n}\n', '\n')
  header_f:close()
  return header
end

--- @type fun(start_row: integer, start_col: integer): TSPoint]]
local TSPoint

local TSNode_ct --- @type ffi.ctype*

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
local TSTree = {
  root = C.ts_tree_root_node,
}

do -- TSTree
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

    local start_point = TSPoint(start_row, start_col)
    local old_end_point = TSPoint(end_row_old, end_col_old)
    local new_end_point = TSPoint(end_row_new, end_col_new)

    local edit = ffi.new('TSInputEdit', {
      start_byte,
      end_byte_old,
      end_byte_new,
      start_point,
      old_end_point,
      new_end_point,
    })

    C.ts_tree_edit(self, edit)
  end

  ---@param include_bytes boolean?
  ---@return Range4[]|Range6[]
  function TSTree:included_ranges(include_bytes)
    local len = intptr()
    local ranges = ffi.gc(C.ts_tree_included_ranges(self, len), C.free)
    return make_ranges(ranges, len[0], include_bytes)
  end

  --- @return TSTree
  function TSTree:copy()
    return ffi.gc(C.ts_tree_copy(self), C.ts_tree_delete)
  end

  function TSTree:__tostring()
    return '<tree>'
  end
end

--- @class TSParser.ffi : TSParser
local TSParser = {}

do -- TSParser
  --   { "__gc", parser_gc },
  -- static int parser_gc(lua_State *L)
  -- {
  --   logger_gc(ts_parser_logger(p));
  --   ts_parser_delete(p);
  --   return 0;
  -- }

  -- static bool on_parser_progress(TSParseState *state)
  -- {
  --   TSLuaParserCallbackPayload *payload = state->payload;
  --   uint64_t parse_time = os_hrtime() - payload->parse_start_time;
  --   return parse_time >= payload->timeout_threshold_ns;
  -- }

  -- static void logger_cb(void *payload, TSLogType logtype, const char *s)
  -- {
  --   TSLuaLoggerOpts *opts = (TSLuaLoggerOpts *)payload;
  --   if ((!opts->lex && logtype == TSLogTypeLex)
  --       || (!opts->parse && logtype == TSLogTypeParse)) {
  --     return;
  --   }
  --
  --   lua_State *lstate = opts->lstate;
  --
  --   lua_rawgeti(lstate, LUA_REGISTRYINDEX, opts->cb);
  --   lua_pushstring(lstate, logtype == TSLogTypeParse ? "parse" : "lex");
  --   lua_pushstring(lstate, s);
  --   if (lua_pcall(lstate, 2, 0, 0)) {
  --     luaL_error(lstate, "Error executing treesitter logger callback");
  --   }
  -- }
  --
  -- static int parser_set_logger(lua_State *L)
  -- {
  --   luaL_argcheck(L, lua_isboolean(L, 2), 2, "boolean expected");
  --   luaL_argcheck(L, lua_isboolean(L, 3), 3, "boolean expected");
  --   luaL_argcheck(L, lua_isfunction(L, 4), 4, "function expected");
  --
  --   TSLuaLoggerOpts *opts = xmalloc(sizeof(TSLuaLoggerOpts));
  --   lua_pushvalue(L, 4);
  --   LuaRef ref = luaL_ref(L, LUA_REGISTRYINDEX);
  --
  --   *opts = (TSLuaLoggerOpts){
  --     .lex = lua_toboolean(L, 2),
  --     .parse = lua_toboolean(L, 3),
  --     .cb = ref,
  --     .lstate = L
  --   };
  --
  --   TSLogger logger = {
  --     .payload = (void *)opts,
  --     .log = logger_cb
  --   };
  --
  --   ts_parser_set_logger(p, logger);
  --   return 0;
  -- }
  --
  -- static int parser_get_logger(lua_State *L)
  -- {
  --   TSLogger logger = ts_parser_logger(p);
  --   if (logger.log) {
  --     TSLuaLoggerOpts *opts = (TSLuaLoggerOpts *)logger.payload;
  --     lua_rawgeti(L, LUA_REGISTRYINDEX, opts->cb);
  --   } else {
  --     lua_pushnil(L);
  --   }
  --
  --   return 1;
  -- }

  --- @param tree? TSTree
  --- @param source string|integer
  --- @param include_bytes? boolean
  --- @param _timeout_ns? integer
  --- @return TSTree?
  --- @return Range4[]|Range6[]?
  function TSParser:parse(tree, source, include_bytes, _timeout_ns)
    local new_tree = nil
    if type(source) == 'string' then
      new_tree = C.ts_parser_parse_string(self, tree, source, #source)
    elseif type(source) == 'number' then
      local text = table.concat(vim.api.nvim_buf_get_text(source, 0, 0, -1, -1, {}), '\n')
      new_tree = C.ts_parser_parse_string(self, tree, text, #text)
      -- input = (TSInput){ (void *)buf, input_cb, TSInputEncodingUTF8, NULL };
      -- if (!lua_isnil(L, 5)) {
      --   uint64_t timeout_ns = (uint64_t)lua_tointeger(L, 5);
      --   TSLuaParserCallbackPayload payload =
      --     (TSLuaParserCallbackPayload){ .parse_start_time = os_hrtime(),
      --                                   .timeout_threshold_ns = timeout_ns };
      --   TSParseOptions parse_options = { .payload = &payload,
      --                                    .progress_callback = on_parser_progress };
      --   new_tree = ts_parser_parse_with_options(p, old_tree, input, parse_options);
      -- } else {
      --   // Tree-sitter retains parse options after use, so we must explicitly reset them here.
      --   new_tree = ts_parser_parse_with_options(p, old_tree, input, (TSParseOptions) { 0 });
      -- }
    else
      error('expected either string or buffer handle')
    end

    if new_tree then
      return ffi.gc(new_tree, C.ts_tree_delete)
    end

    if not new_tree then
      if not C.ts_parser_language(self) then
        error('Language was unset, or has an incompatible ABI.')
      end
      return
    end

    local n_ranges = intptr()

    local changed = tree and C.ts_tree_get_changed_ranges(tree, new_tree, n_ranges)
      or C.ts_tree_included_ranges(new_tree, n_ranges)

    ffi.gc(changed, C.free)

    return new_tree, make_ranges(changed, n_ranges[0], include_bytes)
  end

  function TSParser:reset()
    C.ts_parser_reset(self)
  end

  local function range_err()
    error('Ranges can only be made from 6 element long tables or nodes.', 2)
  end

  --- @param range Range6|TSNode
  --- @param tsrange TSRange
  local function range_from_lua(range, tsrange)
    if type(range) == 'table' then
      if #range ~= 4 then
        range_err()
      end
      tsrange.start_point.row = range[1]
      tsrange.start_point.column = range[2]
      tsrange.start_byte = range[3]
      tsrange.end_point.row = range[4]
      tsrange.end_point.column = range[5]
      tsrange.end_byte = range[6]
    elseif ffi.istype(range, TSNode_ct) then
      --- @cast range TSNode
      tsrange.start_point = C.ts_node_start_point(range)
      tsrange.end_point = C.ts_node_end_point(range)
      tsrange.start_byte = C.ts_node_start_byte(range)
      tsrange.end_byte = C.ts_node_end_byte(range)
    else
      range_err()
    end
  end

  --- @param ranges (Range6|TSNode)[])
  function TSParser:set_included_ranges(ranges)
    vim.validate('ranges', ranges, 'table')
    local tbl_len = #ranges

    --- @type TSRange[]
    local tsranges = ffi.new('TSRange[?]', tbl_len)

    for index = 0, tbl_len - 1 do
      range_from_lua(ranges[index], tsranges[index])
    end

    C.ts_parser_set_included_ranges(self, tsranges, tbl_len)
  end

  --- @param include_bytes? boolean
  function TSParser:included_ranges(include_bytes)
    local len = intptr()
    local ranges = C.ts_parser_included_ranges(self, len)
    return make_ranges(ranges, len[0], include_bytes)
  end

  --- @param _lex boolean
  --- @param _parse boolean
  --- @param _cb TSLoggerCallback
  function TSParser:_set_logger(_lex, _parse, _cb)
    -- TODO(lewis6991)
  end

  function TSParser:_logger()
    -- TODO(lewis6991)
  end

  function TSParser:__tostring()
    return '<parser>'
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

--- @param lang string
--- @return TSParser
function M._create_ts_parser(lang)
  local parser = ffi.gc(C.ts_parser_new(), C.ts_parser_delete)
  C.ts_parser_set_language(parser, lang_check(lang))
  return parser
end

-- typedef struct {
--   uint64_t parse_start_time;
--   uint64_t timeout_threshold_ns;
-- } TSLuaParserCallbackPayload;
--
-- static PMap(cstr_t) langs = MAP_INIT;
--
-- #ifdef HAVE_WASMTIME
-- static wasm_engine_t *wasmengine;
-- static TSWasmStore *ts_wasmstore;
-- #endif
--
-- // TSLanguage
--
-- #ifdef HAVE_WASMTIME
--
-- static const char *wasmerr_to_str(TSWasmErrorKind werr)
-- {
--   switch (werr) {
--   case TSWasmErrorKindParse:
--     return "PARSE";
--   case TSWasmErrorKindCompile:
--     return "COMPILE";
--   case TSWasmErrorKindInstantiate:
--     return "INSTANTIATE";
--   case TSWasmErrorKindAllocate:
--     return "ALLOCATE";
--   default:
--     return "UNKNOWN";
--   }
-- }
-- #endif
--
-- #ifdef HAVE_WASMTIME
-- static int tslua_add_language_from_wasm(lua_State *L)
-- {
--   return add_language(L, true);
-- }
-- #endif
--
-- static const TSLanguage *load_language_from_wasm(lua_State *L, const char *path,
--                                                  const char *lang_name)
-- {
-- #ifndef HAVE_WASMTIME
--   luaL_error(L, "Not supported");
--   return NULL;
-- #else
--   if (wasmengine == NULL) {
--     wasmengine = wasm_engine_new();
--   }
--   assert(wasmengine != NULL);
--
--   TSWasmError werr = { 0 };
--   if (ts_wasmstore == NULL) {
--     ts_wasmstore = ts_wasm_store_new(wasmengine, &werr);
--   }
--
--   if (werr.kind > 0) {
--     luaL_error(L, "Error creating wasm store: (%s) %s", wasmerr_to_str(werr.kind), werr.message);
--   }
--
--   size_t file_size = 0;
--   char *data = read_file(path, &file_size);
--
--   if (data == NULL) {
--     luaL_error(L, "Unable to read file", path);
--   }
--
--   const TSLanguage *lang = ts_wasm_store_load_language(ts_wasmstore, lang_name, data,
--                                                        (uint32_t)file_size, &werr);
--
--   xfree(data);
--
--   if (werr.kind > 0) {
--     luaL_error(L, "Failed to load WASM parser %s: (%s) %s", path, wasmerr_to_str(werr.kind),
--                werr.message);
--   }
--
--   if (lang == NULL) {
--     luaL_error(L, "Failed to load parser %s: internal error", path);
--   }
--
--   return lang;
-- #endif
-- }
--
-- static int add_language(lua_State *L, bool is_wasm)
-- {
--   const char *path = luaL_checkstring(L, 1);
--   const char *lang_name = luaL_checkstring(L, 2);
--   const char *symbol_name = lang_name;
--
--   if (!is_wasm && lua_gettop(L) >= 3 && !lua_isnil(L, 3)) {
--     symbol_name = luaL_checkstring(L, 3);
--   }
--
--   if (map_has(cstr_t, &langs, lang_name)) {
--     lua_pushboolean(L, true);
--     return 1;
--   }
--
--   const TSLanguage *lang = is_wasm
--                            ? load_language_from_wasm(L, path, lang_name)
--                            : load_language_from_object(L, path, lang_name, symbol_name);
--
--   uint32_t lang_version = ts_language_abi_version(lang);
--   if (lang_version < TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION
--       || lang_version > TREE_SITTER_LANGUAGE_VERSION) {
--     return luaL_error(L,
--                       "ABI version mismatch for %s: supported between %d and %d, found %d",
--                       path,
--                       TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION,
--                       TREE_SITTER_LANGUAGE_VERSION, lang_version);
--   }
--
--   pmap_put(cstr_t)(&langs, xstrdup(lang_name), (TSLanguage *)lang);
--
--   lua_pushboolean(L, true);
--   return 1;
-- }
--
-- static TSLanguage *lang_check(lua_State *L, int index)
-- {
--   const char *lang_name = luaL_checkstring(L, index);
--   TSLanguage *lang = pmap_get(cstr_t)(&langs, lang_name);
--   if (!lang) {
--     luaL_error(L, "no such language: %s", lang_name);
--   }
--   return lang;
-- }
--
-- static int tslua_inspect_lang(lua_State *L)
-- {
--   TSLanguage *lang = lang_check(L, 1);
--
--   lua_createtable(L, 0, 2);  // [retval]
--
--   {  // Symbols
--     uint32_t nsymbols = ts_language_symbol_count(lang);
--     assert(nsymbols < INT_MAX);
--
--     lua_createtable(L, (int)(nsymbols - 1), 1);  // [retval, symbols]
--     for (uint32_t i = 0; i < nsymbols; i++) {
--       TSSymbolType t = ts_language_symbol_type(lang, (TSSymbol)i);
--       if (t == TSSymbolTypeAuxiliary) {
--         // not used by the API
--         continue;
--       }
--       const char *name = ts_language_symbol_name(lang, (TSSymbol)i);
--       bool named = t != TSSymbolTypeAnonymous;
--       lua_pushboolean(L, named);  // [retval, symbols, is_named]
--       if (!named) {
--         char buf[256];
--         snprintf(buf, sizeof(buf), "\"%s\"", name);
--         lua_setfield(L, -2, buf);  // [retval, symbols]
--       } else {
--         lua_setfield(L, -2, name);  // [retval, symbols]
--       }
--     }
--
--     lua_setfield(L, -2, "symbols");  // [retval]
--   }
--
--   {  // Fields
--     uint32_t nfields = ts_language_field_count(lang);
--     lua_createtable(L, (int)nfields, 1);  // [retval, fields]
--     // Field IDs go from 1 to nfields inclusive (extra index 0 maps to NULL)
--     for (uint32_t i = 1; i <= nfields; i++) {
--       lua_pushstring(L, ts_language_field_name_for_id(lang, (TSFieldId)i));
--       lua_rawseti(L, -2, (int)i);  // [retval, fields]
--     }
--
--     lua_setfield(L, -2, "fields");  // [retval]
--   }
--
--   lua_pushboolean(L, ts_language_is_wasm(lang));
--   lua_setfield(L, -2, "_wasm");
--
--   lua_pushinteger(L, ts_language_abi_version(lang));  // [retval, version]
--   lua_setfield(L, -2, "abi_version");
--
--   {  // Metadata
--     const TSLanguageMetadata *meta = ts_language_metadata(lang);
--
--     if (meta != NULL) {
--       lua_createtable(L, 0, 3);
--
--       lua_pushinteger(L, meta->major_version);
--       lua_setfield(L, -2, "major_version");
--       lua_pushinteger(L, meta->minor_version);
--       lua_setfield(L, -2, "minor_version");
--       lua_pushinteger(L, meta->patch_version);
--       lua_setfield(L, -2, "patch_version");
--
--       lua_setfield(L, -2, "metadata");
--     }
--   }
--
--   lua_pushinteger(L, ts_language_state_count(lang));
--   lua_setfield(L, -2, "state_count");
--
--   {  // Supertypes
--     uint32_t nsupertypes;
--     const TSSymbol *supertypes = ts_language_supertypes(lang, &nsupertypes);
--
--     lua_createtable(L, 0, (int)nsupertypes);  // [retval, supertypes]
--     for (uint32_t i = 0; i < nsupertypes; i++) {
--       const TSSymbol supertype = *(supertypes + i);
--
--       uint32_t nsubtypes;
--       const TSSymbol *subtypes = ts_language_subtypes(lang, supertype, &nsubtypes);
--
--       lua_createtable(L, (int)nsubtypes, 0);
--       for (uint32_t j = 1; j <= nsubtypes; j++) {
--         lua_pushstring(L, ts_language_symbol_name(lang, *(subtypes + j)));
--         lua_rawseti(L, -2, (int)j);
--       }
--
--       lua_setfield(L, -2, ts_language_symbol_name(lang, supertype));
--     }
--
--     lua_setfield(L, -2, "supertypes");  // [retval]
--   }
--
--   return 1;
-- }
--
-- // TSParser
--
-- static void logger_gc(TSLogger logger)
-- {
--   if (!logger.log) {
--     return;
--   }
--
--   TSLuaLoggerOpts *opts = (TSLuaLoggerOpts *)logger.payload;
--   luaL_unref(opts->lstate, LUA_REGISTRYINDEX, opts->cb);
--   xfree(opts);
-- }
--
-- static const char *input_cb(void *payload, uint32_t byte_index, TSPoint position,
--                             uint32_t *bytes_read)
-- {
--   buf_T *bp = payload;
-- #define BUFSIZE 256
--   static char buf[BUFSIZE];
--
--   if ((linenr_T)position.row >= bp->b_ml.ml_line_count) {
--     *bytes_read = 0;
--     return "";
--   }
--   char *line = ml_get_buf(bp, (linenr_T)position.row + 1);
--   size_t len = (size_t)ml_get_buf_len(bp, (linenr_T)position.row + 1);
--   if (position.column > len) {
--     *bytes_read = 0;
--     return "";
--   }
--   size_t tocopy = MIN(len - position.column, BUFSIZE);
--
--   memcpy(buf, line + position.column, tocopy);
--   // Translate embedded \n to NUL
--   memchrsub(buf, '\n', NUL, tocopy);
--   *bytes_read = (uint32_t)tocopy;
--   if (tocopy < BUFSIZE) {
--     // now add the final \n. If it didn't fit, input_cb will be called again
--     // on the same line with advanced column.
--     buf[tocopy] = '\n';
--     (*bytes_read)++;
--   }
--   return buf;
-- #undef BUFSIZE
-- }
--

--- @class TSNode.ffi : TSNode
local TSNode = {
  child_with_descendant = C.ts_node_child_with_descendant,
  parent = C.ts_node_parent,
  next_sibling = C.ts_node_next_sibling,
  prev_sibling = C.ts_node_prev_sibling,
  next_named_sibling = C.ts_node_next_named_sibling,
  prev_named_sibling = C.ts_node_prev_named_sibling,
  equal = C.ts_node_eq,
  __eq = C.ts_node_eq,
  __len = C.ts_node_child_count,
  child_count = C.ts_node_child_count,
  named_child_count = C.ts_node_named_child_count,
  type = C.ts_node_type,
  symbol = C.ts_node_symbol,
  sexpr = C.ts_node_string,
  named = C.ts_node_is_named,
  missing = C.ts_node_is_missing,
  extra = C.ts_node_is_extra,
  has_changes = C.ts_node_has_changes,
  has_error = C.ts_node_has_error,
  child = C.ts_node_child,
  named_child = C.ts_node_named_child,
}

function TSNode:__tostring()
  return '<node ' .. self:type() .. '>'
end

--- Get a unique identifier for the node inside its own tree.
---
--- No guarantees are made about this identifier's internal representation,
--- except for being a primitive Lua type with value equality (so not a
--- table). Presently it is a (non-printable) string.
---
--- Note: The `id` is not guaranteed to be unique for nodes from different
--- trees.
--- @return string
function TSNode:id()
  -- TODO(lewis6991): test
  return tostring(rawget(self, 'id'))
end

--- Get the range of the node.
---
--- Return four or six values:
---
--- - start row
--- - start column
--- - start byte (if {include_bytes} is `true`)
--- - end row
--- - end column
--- - end byte (if {include_bytes} is `true`)
--- @param include_bytes? boolean
--- @return integer, integer, integer, integer
--- @overload fun(self: TSNode, include_bytes: true): integer, integer, integer, integer, integer, integer
function TSNode:range(include_bytes)
  local s = C.ts_node_start_point(self)
  local e = C.ts_node_end_point(self)
  if include_bytes then
    local sbyte = C.ts_node_start_byte(self)
    local ebyte = C.ts_node_end_byte(self)
    return s.row, s.column, sbyte, e.row, e.column, ebyte
  end
  return s.row, s.column, e.row, e.column
end

--- Get the node's start position. Return three values: the row, column and
--- total byte count (all zero-based).
--- @return integer, integer, integer
function TSNode:start()
  local s = C.ts_node_start_point(self)
  local sbyte = C.ts_node_start_byte(self)
  return s.row, s.column, sbyte
end

--- Get the node's end position. Return three values: the row, column and
--- total byte count (all zero-based).
--- @return integer, integer, integer
function TSNode:end_()
  local e = C.ts_node_end_point(self)
  local ebyte = C.ts_node_end_byte(self)
  return e.row, e.column, ebyte
end

--- Returns a list of all the node's children that have the given field name.
--- @param name string
--- @return TSNode[]
function TSNode:field(name)
  local r = {} --- @type TSNode[]
  for i = 0, self:child_count() - 1 do
    local child_field_name = C.ts_node_field_name_for_child(self, i)
    if name == child_field_name then
      r[#r + 1] = C.ts_node_child(self, i)
    end
  end
  return r
end

--- Get the smallest node within this node that spans the given range of (row,
--- column) positions
--- @param start_row integer
--- @param start_col integer
--- @param end_row integer
--- @param end_col integer
--- @return TSNode?
function TSNode:descendant_for_range(start_row, start_col, end_row, end_col)
  local startp = TSPoint(start_row, start_col)
  local endp = TSPoint(end_row, end_col)
  return C.ts_node_descendant_for_point_range(self, startp, endp)
end

--- Get the smallest named node within this node that spans the given range of
--- (row, column) positions
--- @param start_row integer
--- @param start_col integer
--- @param end_row integer
--- @param end_col integer
--- @return TSNode?
function TSNode:named_descendant_for_range(start_row, start_col, end_row, end_col)
  local startp = TSPoint(start_row, start_col)
  local endp = TSPoint(end_row, end_col)
  return C.ts_node_named_descendant_for_point_range(self, startp, endp)
end

--- Iterates over all the direct children of {TSNode}, regardless of whether
--- they are named or not.
--- Returns the child node plus the eventual field name corresponding to this
--- child node.
--- @return fun(): TSNode?, string?
function TSNode:iter_children()
  local child_index = 0

  return function()
    if child_index >= self:child_count() then
      return
    end

    local child = self:child(child_index)
    local field = C.ts_node_field_name_for_child(self, child_index)

    child_index = child_index + 1

    return child, field
  end
end

--- Check if the node has any of the given node types as its ancestor.
--- @param node_types string[]
--- @return boolean
function TSNode:__has_ancestor(node_types)
  local node = C.ts_tree_root_node(self:tree())
  while node:id() ~= self:id() and not C.ts_node_is_null(node) do
    local node_type = node:type()
    for _, type in ipairs(node_types) do
      if node_type == type then
        return true
      end
    end
    node = C.ts_node_child_with_descendant(node, self)
  end
  return false
end

--- Returns a list of the node's named children.
--- @return TSNode[]
function TSNode:named_children()
  local r = {} --- @type TSNode[]
  for i = 0, self:named_child_count() - 1 do
    r[#r + 1] = C.ts_node_named_child(self, i)
  end
  return r
end

function TSNode:root()
  return C.ts_tree_root_node(self:tree())
end

--- Get the |TSTree| of the node.
--- @return TSTree
function TSNode:tree()
  return rawget(self, 'tree')
end

--- Return the number of bytes spanned by this node.
--- @return integer
function TSNode:byte_length()
  local start_byte = C.ts_node_start_byte(self)
  local end_byte = C.ts_node_end_byte(self)
  return end_byte - start_byte
end

--- @class TSQueryCursor.ffi : TSQueryCursor
local TSQueryCursor = {}

do -- TSQueryCursor
  -- static int querycursor_gc(lua_State *L)
  -- {
  --   TSQueryCursor *cursor = querycursor_check(L, 1);
  --   ts_query_cursor_delete(cursor);
  --   return 0;
  -- }
  --
  function TSQueryCursor:remove_match(match_id)
    C.ts_query_cursor_remove_match(self, match_id)
  end

  function TSQueryCursor:next_capture()
    local match = newptr('TSQueryMatch')
    local capture_index = intptr()
    if not C.ts_query_cursor_next_capture(self, match, capture_index) then
      return
    end
    local capture = match[0].captures[capture_index[0]]
    return capture.index, capture.node, match[0].id
  end

  function TSQueryCursor:next_match()
    local match = newptr('TSQueryMatch')
    if not C.ts_query_cursor_next_match(self, match) then
      return
    end
    return match[0].id
  end
end

local TSQueryMatch = {}

--- @return integer match_id
--- @return integer pattern_index
function TSQueryMatch:info()
  return self.id, self.pattern_index + 1
end

--- @return table<integer,TSNode[]>
function TSQueryMatch:captures()
  local r = {} --- @type table<integer,TSNode[]>
  for i = 0, self.capture_count - 1 do
    local capture = self.captures[i]
    local index = capture.index + 1
    r[index] = r[index] or {}
    r[index][#r[index] + 1] = capture.node
  end
  return r
end

--- @class TSQuery.ffi : TSQuery
local TSQuery = {
  __tostring = function()
    return '<query>'
  end,
}

do --- TSQuery
  -- static int query_gc(lua_State *L)
  -- {
  --   TSQuery *query = query_check(L, 1);
  --   ts_query_delete(query);
  --   return 0;
  -- }

  --- Get information about the query's patterns and captures.
  ---@return TSQueryInfo
  function TSQuery:inspect()
    local r = {} --- @type TSQueryInfo
    --
    local n_pat = C.ts_query_pattern_count(self)
    local pats = {} --- @type table<integer, (integer|string)[][]>
    for i = 0, n_pat - 1 do
      local len = intptr()
      local step = C.ts_query_predicates_for_pattern(self, i, len)
      if len[0] > 0 then
        local pat = {} --- @type (integer|string)[][]
        local pred = {} --- @type (integer|string)[]
        local nextpred = 1
        for k = 0, len[0] - 1 do
          if step[k].type == 'TSQueryPredicateStepTypeDone' then
            pat[nextpred] = pred
            nextpred = nextpred + 1
            pred = {}
          elseif step[k].type == 'TSQueryPredicateStepTypeString' then
            local strlen = intptr()
            pred[#pred + 1] = C.ts_query_string_value_for_id(self, step[k].value_id, strlen)
          elseif step[k].type == 'TSQueryPredicateStepTypeCapture' then
            pred[#pred + 1] = step[k].value_id + 1
          else
            error('abort()')
          end
        end
        pats[i + 1] = pat
      end
    end
    r.patterns = pats

    local n_captures = C.ts_query_capture_count(self)
    local captures = {} --- @type table<integer, string>
    for i = 0, n_captures - 1 do
      local strlen = intptr()
      captures[i + 1] = C.ts_query_capture_name_for_id(self, i, strlen)
    end
    r.captures = captures

    return r
  end

  --- Disable a specific capture in this query; once disabled the capture cannot be re-enabled.
  --- {capture_name} should not include a leading "@".
  ---
  --- Example: To disable the `@variable.parameter` capture from the vimdoc highlights query:
  --- ```lua
  --- local query = vim.treesitter.query.get('vimdoc', 'highlights')
  --- query.query:disable_capture("variable.parameter")
  --- vim.treesitter.get_parser():parse()
  --- ```
  ---@param capture_name string
  function TSQuery:disable_capture(capture_name)
    C.ts_query_disable_capture(self, capture_name, #capture_name)
  end

  --- Disable a specific pattern in this query; once disabled the pattern cannot be re-enabled.
  --- The {pattern_index} for a particular match can be obtained with |:Inspect!|, or by reading
  --- the source of the query (i.e. from |vim.treesitter.query.get_files()|).
  ---
  --- Example: To disable `|` links in vimdoc but keep other `@markup.link`s highlighted:
  --- ```lua
  --- local link_pattern = 9 -- from :Inspect!
  --- local query = vim.treesitter.query.get('vimdoc', 'highlights')
  --- query.query:disable_pattern(link_pattern)
  --- local tree = vim.treesitter.get_parser():parse()[1]
  --- ```
  ---@param pattern_index integer
  function TSQuery:disable_pattern(pattern_index)
    C.ts_query_disable_pattern(self, pattern_index - 1)
  end
end
--
-- static const char *query_err_to_string(TSQueryError error_type)
-- {
--   switch (error_type) {
--   case TSQueryErrorSyntax:
--     return "Invalid syntax:\n";
--   case TSQueryErrorNodeType:
--     return "Invalid node type ";
--   case TSQueryErrorField:
--     return "Invalid field name ";
--   case TSQueryErrorCapture:
--     return "Invalid capture name ";
--   case TSQueryErrorStructure:
--     return "Impossible pattern:\n";
--   default:
--     return "error";
--   }
-- }
--
-- static void query_err_string(const char *src, int error_offset, TSQueryError error_type, char *err,
--                              size_t errlen)
-- {
--   int line_start = 0;
--   int row = 0;
--   const char *error_line = NULL;
--   int error_line_len = 0;
--
--   const char *end_str;
--   do {
--     const char *src_tmp = src + line_start;
--     end_str = strchr(src_tmp, '\n');
--     int line_length = end_str != NULL ? (int)(end_str - src_tmp) : (int)strlen(src_tmp);
--     int line_end = line_start + line_length;
--     if (line_end > error_offset) {
--       error_line = src_tmp;
--       error_line_len = line_length;
--       break;
--     }
--     line_start = line_end + 1;
--     row++;
--   } while (end_str != NULL);
--
--   int column = error_offset - line_start;
--
--   const char *type_msg = query_err_to_string(error_type);
--   snprintf(err, errlen, "Query error at %d:%d. %s", row + 1, column + 1, type_msg);
--   size_t offset = strlen(err);
--   errlen = errlen - offset;
--   err = err + offset;
--
--   // Error types that report names
--   if (error_type == TSQueryErrorNodeType
--       || error_type == TSQueryErrorField
--       || error_type == TSQueryErrorCapture) {
--     const char *suffix = src + error_offset;
--     bool is_anonymous = error_type == TSQueryErrorNodeType && suffix[-1] == '"';
--     int suffix_len = 0;
--     char c = suffix[suffix_len];
--     if (is_anonymous) {
--       int backslashes = 0;
--       // Stop when we hit an unescaped double quote
--       while (c != '"' || backslashes % 2 != 0) {
--         if (c == '\\') {
--           backslashes += 1;
--         } else {
--           backslashes = 0;
--         }
--         c = suffix[++suffix_len];
--       }
--     } else {
--       // Stop when we hit the end of the identifier
--       while (isalnum(c) || c == '_' || c == '-' || c == '.') {
--         c = suffix[++suffix_len];
--       }
--     }
--     snprintf(err, errlen, "\"%.*s\":\n", suffix_len, suffix);
--     offset = strlen(err);
--     errlen = errlen - offset;
--     err = err + offset;
--   }
--
--   if (!error_line) {
--     snprintf(err, errlen, "Unexpected EOF\n");
--     return;
--   }
--
--   snprintf(err, errlen, "%.*s\n%*s^\n", error_line_len, error_line, column, "");
-- }

-- void nlua_treesitter_free(void)
-- {
-- #ifdef HAVE_WASMTIME
--   if (wasmengine != NULL) {
--     wasm_engine_delete(wasmengine);
--   }
--   if (ts_wasmstore != NULL) {
--     ts_wasm_store_delete(ts_wasmstore);
--   }
-- #endif
-- }
--
-- void nlua_treesitter_init(lua_State *const lstate) FUNC_ATTR_NONNULL_ALL
-- {
-- #ifdef HAVE_WASMTIME
--   lua_pushcfunction(lstate, tslua_add_language_from_wasm);
--   lua_setfield(lstate, -2, "_ts_add_language_from_wasm");
-- #endif
--
--   lua_pushcfunction(lstate, tslua_inspect_lang);
--   lua_setfield(lstate, -2, "_ts_inspect_language");
-- }

---@param lang string Language to use for the query
---@param query string Query string in s-expr syntax
---@return TSQuery
function M._ts_parse_query(lang, query)
  vim.validate('lang', lang, 'string')
  vim.validate('query', query, 'string')

  local language = lang_check(lang)
  local error_offset = intptr()
  local error_type = newptr('TSQueryError')
  local tsquery = C.ts_query_new(language, query, #query, error_offset, error_type)

  if not tsquery then
    -- TODO(lewis6991): query_err_string(src, (int)error_offset, error_type, err_msg, sizeof(err_msg));
    error('not query')
  end

  return tsquery
end

--- @param node TSNode
--- @param query TSQuery
--- @param start integer?
--- @param stop integer?
--- @param opts? { max_start_depth?: integer, match_limit?: integer}
--- @return TSQueryCursor
function M._create_ts_querycursor(node, query, start, stop, opts)
  local cursor = ffi.gc(C.ts_query_cursor_new(), C.ts_query_cursor_delete)

  C.ts_query_cursor_exec(cursor, query, node)

  if start and stop then
    C.ts_query_cursor_set_point_range(cursor, TSPoint(start, 0), TSPoint(stop, 0))
  end

  if opts then
    if opts.max_start_depth then
      C.ts_query_cursor_set_max_start_depth(cursor, opts.max_start_depth)
    elseif opts.match_limit then
      C.ts_query_cursor_set_match_limit(cursor, opts.match_limit)
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

  local lang_version = C.ts_language_abi_version(language)

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

--- @param ts_api_path string
function M.init(ts_api_path)
  ffi.cdef(get_ts_cdef(ts_api_path))
  ffi.cdef([[void *free(void *);]])

  ffi.metatype('TSParser', { __index = TSParser })
  ffi.metatype('TSTree', { __index = TSTree })
  ffi.metatype('TSQueryCursor', { __index = TSQueryCursor })
  ffi.metatype('TSNode', { __index = TSNode })
  ffi.metatype('TSQuery', { __index = TSQuery })
  ffi.metatype('TSQueryMatch', { __index = TSQueryMatch })

  ---@diagnostic disable-next-line: cast-local-type
  TSPoint = ffi.typeof('TSPoint')

  TSNode_ct = ffi.typeof('TSNode')
end

return M
