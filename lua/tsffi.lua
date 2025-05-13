local ffi = require('ffi')
local C = ffi.C --- @type ffilib.tree-sitter.C

local MAXLNUM = 2 ^ 31 - 1

local has_nvim_ts_parser_parse_buf = false

local TS_LANGUAGE_VERSION = vim._ts_get_language_version()
local TS_MIN_COMPATIBLE_LANG_VERSION = vim._ts_get_minimum_language_version()

--- @param path string
--- @return string
local function get_ts_cdef(path)
  local header_f = assert(io.open(path, 'r'))
  local header = header_f:read('*a') --- @type string
  header = header:gsub('#[^\n]+\n', '\n')
  header = header:gsub('\nextern [^\n]+\n', '\n')
  header = header:gsub('\n}\n', '\n')
  header_f:close()
  return header
end

--- @param ranges TSRange.cdata[]
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

--- @class TSNode.ffi : TSNode
--- @field _node TSNode.cdata
local TSNode = {}

do --- TSNode
  TSNode.__index = TSNode

  --- @param node TSNode.cdata
  --- @return TSNode.ffi?
  function TSNode.new(node)
    if C.ts_node_is_null(node) then
      return
    end
    return setmetatable({ _node = node }, TSNode)
  end

  --- @return string
  function TSNode:__tostring()
    return '<node ' .. self:type() .. '>'
  end

  --- @param obj TSNode.ffi
  --- @return boolean
  function TSNode:__eq(obj)
    return self:equal(obj)
  end

  --- @param self TSNode.ffi
  --- @return integer
  function TSNode:__len()
    return self:child_count()
  end

  --- @param descendant TSNode.ffi
  --- @return TSNode.ffi?
  function TSNode:child_with_descendant(descendant)
    return TSNode.new(C.ts_node_child_with_descendant(self._node, descendant._node))
  end

  --- @return TSNode.ffi?
  function TSNode:next_sibling()
    return TSNode.new(C.ts_node_next_sibling(self._node))
  end

  --- @return TSNode.ffi?
  function TSNode:prev_sibling()
    return TSNode.new(C.ts_node_prev_sibling(self._node))
  end

  --- @return TSNode.ffi?
  function TSNode:next_named_sibling()
    return TSNode.new(C.ts_node_next_named_sibling(self._node))
  end

  --- @return TSNode.ffi?
  function TSNode:prev_named_sibling()
    local node = C.ts_node_prev_named_sibling(self._node)
    return TSNode.new(node)
  end

  --- @return TSNode.ffi?
  function TSNode:parent()
    return TSNode.new(C.ts_node_parent(self._node))
  end

  --- @param node TSNode.ffi
  --- @return boolean
  function TSNode:equal(node)
    return C.ts_node_eq(self._node, node._node)
  end

  --- @return integer
  function TSNode:child_count()
    return C.ts_node_child_count(self._node)
  end

  --- @return integer
  function TSNode:named_child_count()
    return C.ts_node_named_child_count(self._node)
  end

  --- @return boolean
  function TSNode:named()
    return C.ts_node_is_named(self._node)
  end

  --- @return boolean
  function TSNode:missing()
    return C.ts_node_is_missing(self._node)
  end

  --- @return boolean
  function TSNode:extra()
    return C.ts_node_is_extra(self._node)
  end

  --- @return boolean
  function TSNode:has_changes()
    return C.ts_node_has_changes(self._node)
  end

  --- @return boolean
  function TSNode:has_error()
    return C.ts_node_has_error(self._node)
  end

  --- @return string
  function TSNode:type()
    return ffi.string(C.ts_node_type(self._node))
  end

  function TSNode:symbol()
    return C.ts_node_symbol(self._node)
  end

  function TSNode:sexpr()
    return ffi.string(C.ts_node_string(self._node))
  end

  --- @param index integer
  --- @return TSNode.ffi?
  function TSNode:child(index)
    return TSNode.new(C.ts_node_child(self._node, index))
  end

  --- @param index integer
  --- @return TSNode.ffi?
  function TSNode:named_child(index)
    return TSNode.new(C.ts_node_named_child(self._node, index))
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
    return ffi.string(self._node.id)
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
  --- @return integer, integer, integer, integer, integer, integer
  --- @overload fun(self: TSNode.ffi, include_bytes: false): integer, integer, integer, integer
  --- @overload fun(self: TSNode.ffi, include_bytes: true): integer, integer, integer, integer, integer, integer
  function TSNode:range(include_bytes)
    local node = self._node
    local s = C.ts_node_start_point(node)
    local e = C.ts_node_end_point(node)
    if include_bytes then
      local sbyte = C.ts_node_start_byte(node)
      local ebyte = C.ts_node_end_byte(node)
      return s.row, s.column, sbyte, e.row, e.column, ebyte
    end
    return s.row, s.column, e.row, e.column
  end

  --- Get the node's start position. Return three values: the row, column and
  --- total byte count (all zero-based).
  --- @return integer, integer, integer
  function TSNode:start()
    local node = self._node
    local s = C.ts_node_start_point(node)
    local sbyte = C.ts_node_start_byte(node)
    return s.row, s.column, sbyte
  end

  --- Get the node's end position. Return three values: the row, column and
  --- total byte count (all zero-based).
  --- @return integer, integer, integer
  function TSNode:end_()
    local node = self._node
    local e = C.ts_node_end_point(node)
    local ebyte = C.ts_node_end_byte(node)
    return e.row, e.column, ebyte
  end

  --- Returns a list of all the node's children that have the given field name.
  --- @param name string
  --- @return TSNode[]
  function TSNode:field(name)
    local r = {} --- @type TSNode[]
    for i = 0, self:child_count() - 1 do
      local child_field_name = ffi.string(C.ts_node_field_name_for_child(self._node, i))
      if name == child_field_name then
        local child = C.ts_node_child(self._node, i)
        r[#r + 1] = child
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
  --- @return TSNode.ffi?
  function TSNode:descendant_for_range(start_row, start_col, end_row, end_col)
    local startp = ffi.new('TSPoint', start_row, start_col) --[[@as TSPoint.cdata]]
    local endp = ffi.new('TSPoint', end_row, end_col) --[[@as TSPoint.cdata]]
    return TSNode.new(C.ts_node_descendant_for_point_range(self._node, startp, endp))
  end

  --- Get the smallest named node within this node that spans the given range of
  --- (row, column) positions
  --- @param start_row integer
  --- @param start_col integer
  --- @param end_row integer
  --- @param end_col integer
  --- @return TSNode.ffi?
  function TSNode:named_descendant_for_range(start_row, start_col, end_row, end_col)
    local startp = ffi.new('TSPoint', start_row, start_col) --[[@as TSPoint.cdata]]
    local endp = ffi.new('TSPoint', end_row, end_col) --[[@as TSPoint.cdata]]
    return TSNode.new(C.ts_node_named_descendant_for_point_range(self._node, startp, endp))
  end

  -- --- Iterates over all the direct children of {TSNode}, regardless of whether
  -- --- they are named or not.
  -- --- Returns the child node plus the eventual field name corresponding to this
  -- --- child node.
  -- --- @return fun(): TSNode?, string?
  -- function TSNode:iter_children()
  --   local child_index = 0
  --
  --   return function()
  --     if child_index >= self:child_count() then
  --       return
  --     end
  --
  --     local child = self:child(child_index)
  --     local field = C.ts_node_field_name_for_child(self, child_index)
  --
  --     child_index = child_index + 1
  --
  --     return child, ffi.string(field)
  --   end
  -- end

  --- @return boolean
  function TSNode:is_null()
    return C.ts_node_is_null(self._node)
  end

  --- Check if the node has any of the given node types as its ancestor.
  --- @param node_types string[]
  --- @return boolean
  function TSNode:__has_ancestor(node_types)
    local node = self:root()
    while node and node:id() ~= self:id() and not node:is_null() do
      local node_type = node:type()
      for _, type in ipairs(node_types) do
        if node_type == type then
          return true
        end
      end
      node = TSNode.new(C.ts_node_child_with_descendant(node._node, self._node))
    end
    return false
  end

  -- --- Returns a list of the node's named children.
  -- --- @return TSNode[]
  -- function TSNode:named_children()
  --   local r = {} --- @type TSNode[]
  --   for i = 0, self:named_child_count() - 1 do
  --     local child = C.ts_node_named_child(self, i)
  --     r[#r + 1] = child
  --   end
  --   return r
  -- end

  --- @return TSNode.ffi?
  function TSNode:root()
    return TSNode.new(C.ts_tree_root_node(self._node.tree))
  end

  --- Get the |TSTree| of the node.
  --- @return TSTree.ffi
  function TSNode:tree()
    return self._node.tree
  end

  --- Return the number of bytes spanned by this node.
  --- @return integer
  function TSNode:byte_length()
    local start_byte = C.ts_node_start_byte(self._node)
    local end_byte = C.ts_node_end_byte(self._node)
    return end_byte - start_byte
  end
end

--- @class TSTree.ffi : TSTree
local TSTree = {}

do --- TSTree
  --- @return TSNode.ffi
  function TSTree:root()
    return assert(TSNode.new(C.ts_tree_root_node(self)))
  end

  --- @param start_byte integer
  --- @param end_byte_old integer
  --- @param end_byte_new integer
  --- @param start_row integer
  --- @param start_col integer
  --- @param end_row_old integer
  --- @param end_col_old integer
  --- @param end_row_new integer
  --- @param end_col_new integer
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

    local input = ffi.new(
      'TSInputEdit',
      start_byte,
      end_byte_old,
      end_byte_new,
      ffi.new('TSPoint', start_row, start_col),
      ffi.new('TSPoint', end_row_old, end_col_old),
      ffi.new('TSPoint', end_row_new, end_col_new)
    ) --[[@as TSInputEdit.cdata]]

    C.ts_tree_edit(self, input)
  end

  --- @param include_bytes boolean?
  --- @return Range4[]|Range6[]
  function TSTree:included_ranges(include_bytes)
    local len = ffi.new('uint32_t[1]')
    local ranges = C.ts_tree_included_ranges(self, len)
    ffi.gc(ranges, C.free)
    return make_ranges(ranges, len[0], include_bytes)
  end

  --- @return TSTree
  function TSTree:copy()
    local copy = C.ts_tree_copy(self)
    ffi.gc(copy, C.ts_tree_delete)
    return copy
  end
end

--- @class TSParser.ffi : TSParser
local TSParser = {}

do --- TSParser
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

  --- @param tree? TSTree.ffi
  --- @param source string|integer
  --- @param include_bytes? boolean
  --- @param timeout_ns? integer
  --- @return TSTree.ffi?
  --- @return Range4[]|Range6[]?
  function TSParser:parse(tree, source, include_bytes, timeout_ns)
    local new_tree = nil
    if type(source) == 'string' then
      new_tree = C.ts_parser_parse_string(self, tree, source, #source)
    elseif type(source) == 'number' then
      assert(vim.api.nvim_buf_is_valid(source), 'Invalid buffer handle')
      if has_nvim_ts_parser_parse_buf then
        new_tree = C.nvim_ts_parser_parse_buf(self, tree, source, timeout_ns or 0)
        if new_tree == nil then
          -- Make it actually nil
          new_tree = nil
        end
      else
        local textl = vim.api.nvim_buf_get_text(source, 0, 0, -1, -1, {})
        local text = table.concat(textl, '\n')
        new_tree = C.ts_parser_parse_string(self, tree, text, #text)
      end
    else
      error('expected either string or buffer handle')
    end

    if new_tree then
      ffi.gc(new_tree, C.ts_tree_delete)
    else
      if not C.ts_parser_language(self) then
        error('Language was unset, or has an incompatible ABI.')
      end
      return
    end

    local n_ranges = ffi.new('uint32_t[1]')

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

  --- @param range Range6|TSNode.ffi
  --- @param tsrange TSRange.cdata
  local function range_from_lua(range, tsrange)
    if type(range) == 'table' then
      if #range ~= 6 then
        range_err()
      end
      tsrange.start_point.row = range[1]
      tsrange.start_point.column = range[2]
      tsrange.start_byte = range[3]
      tsrange.end_point.row = range[4]
      tsrange.end_point.column = range[5]
      tsrange.end_byte = range[6]
      return
    end
    --- @cast range TSNode.ffi
    if range and ffi.istype('TSNode', range) then
      tsrange.start_point = C.ts_node_start_point(range._node)
      tsrange.end_point = C.ts_node_end_point(range._node)
      tsrange.start_byte = C.ts_node_start_byte(range._node)
      tsrange.end_byte = C.ts_node_end_byte(range._node)
    else
      range_err()
    end
  end

  --- @param ranges (Range6|TSNode.ffi)[])
  function TSParser:set_included_ranges(ranges)
    vim.validate('ranges', ranges, 'table')
    local tbl_len = #ranges
    if tbl_len == 0 then
      return
    end

    --- @type TSRange.cdata[]
    local tsranges = ffi.new('TSRange[?]', tbl_len)

    for index = 1, tbl_len do
      range_from_lua(ranges[index], tsranges[index - 1])
    end

    C.ts_parser_set_included_ranges(self, tsranges, tbl_len)
  end

  --- @param include_bytes? boolean
  --- @return Range4[]|Range6[]
  function TSParser:included_ranges(include_bytes)
    local len = ffi.new('uint32_t[1]')
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
end

--- @class TSQueryCapture.ffi
--- @field index integer
--- @field node TSNode.ffi

--- @class TSQueryMatch.ffi: TSQueryMatch
--- @field _match TSQueryMatch.cdata
--- @field _captures table<integer, TSQueryCapture.ffi>
local TSQueryMatch = {}

do --- TSQueryMatch
  TSQueryMatch.__index = TSQueryMatch

  --- @param match TSQueryMatch.cdata
  --- @return TSQueryMatch.ffi
  function TSQueryMatch.new(match)
    local captures = {} --- @type table<integer, TSQueryCapture.ffi>
    for i = 0, match.capture_count - 1 do
      local capture = match.captures[i]
      captures[i + 1] = {
        index = capture.index,
        node = assert(TSNode.new(capture.node)),
      }
    end
    return setmetatable({
      _captures = captures,
      _match = match,
    }, TSQueryMatch)
  end

  --- @return integer match_id
  --- @return integer pattern_index
  function TSQueryMatch:info()
    return self._match.id, self._match.pattern_index + 1
  end

  --- @return table<integer,TSNode[]>
  function TSQueryMatch:captures()
    local r = {} --- @type table<integer,TSNode.ffi[]>
    for _, capture in ipairs(self._captures) do
      local index = capture.index + 1
      r[index] = r[index] or {}
      r[index][#r[index] + 1] = capture.node
    end
    return r
  end
end

--- @class TSQueryCursor.ffi: TSQueryCursor.cdata
local TSQueryCursor = {}

do --- TSQueryCursor
  --- @param match_id integer
  function TSQueryCursor:remove_match(match_id)
    C.ts_query_cursor_remove_match(self, match_id)
  end

  --- @return integer? capture
  --- @return TSNode.ffi? captured_node
  --- @return TSQueryMatch.ffi? match
  function TSQueryCursor:next_capture()
    local match = ffi.new('TSQueryMatch') --[[@as TSQueryMatch.cdata]]

    local capture_index = ffi.new('uint32_t[1]')
    if C.ts_query_cursor_next_capture(self, match, capture_index) then
      local capture = match.captures[capture_index[0]]
      local node = TSNode.new(capture.node)
      return capture.index + 1, node, TSQueryMatch.new(match)
    end
  end

  --- @return TSQueryMatch.ffi? match
  function TSQueryCursor:next_match()
    local match = ffi.new('TSQueryMatch') --[[@as TSQueryMatch.ffi]]
    if C.ts_query_cursor_next_match(self, match) then
      return TSQueryMatch.new(match)
    end
  end
end

--- @class TSQuery.ffi : TSQuery
local TSQuery = {}

do --- TSQuery
  --- Get information about the query's patterns and captures.
  --- @return TSQueryInfo
  function TSQuery:inspect()
    local r = {}

    local n_pat = C.ts_query_pattern_count(self)
    local pats = {} --- @type table<integer, (integer|string)[][]>
    for i = 1, n_pat do
      local len = ffi.new('uint32_t[1]')
      local step = C.ts_query_predicates_for_pattern(self, i - 1, len)
      if len[0] > 0 then
        pats[i] = {}
        local pred = {} --- @type (integer|string)[]
        for k = 0, len[0] - 1 do
          if step[k].type == 'TSQueryPredicateStepTypeDone' then
            table.insert(pats[i], pred)
            pred = {}
          elseif step[k].type == 'TSQueryPredicateStepTypeString' then
            local strlen = ffi.new('uint32_t[1]')
            local str = C.ts_query_string_value_for_id(self, step[k].value_id, strlen)
            pred[#pred + 1] = ffi.string(str, strlen[0])
          elseif step[k].type == 'TSQueryPredicateStepTypeCapture' then
            pred[#pred + 1] = step[k].value_id + 1
          else
            error('abort()')
          end
        end
      end
    end
    r.patterns = pats

    local n_captures = C.ts_query_capture_count(self)
    local captures = {} --- @type table<integer, string>
    for i = 1, n_captures do
      local strlen = ffi.new('uint32_t[1]')
      local str = C.ts_query_capture_name_for_id(self, i - 1, strlen)
      captures[i] = ffi.string(str, strlen[0])
    end
    r.captures = captures

    return r
  end

  -- --- Disable a specific capture in this query; once disabled the capture cannot be re-enabled.
  -- --- {capture_name} should not include a leading "@".
  -- ---
  -- --- Example: To disable the `@variable.parameter` capture from the vimdoc highlights query:
  -- --- ```lua
  -- --- local query = vim.treesitter.query.get('vimdoc', 'highlights')
  -- --- query.query:disable_capture("variable.parameter")
  -- --- vim.treesitter.get_parser():parse()
  -- --- ```
  -- ---@param capture_name string
  -- function TSQuery:disable_capture(capture_name)
  --   C.ts_query_disable_capture(self, capture_name, #capture_name)
  -- end

  -- --- Disable a specific pattern in this query; once disabled the pattern cannot be re-enabled.
  -- --- The {pattern_index} for a particular match can be obtained with |:Inspect!|, or by reading
  -- --- the source of the query (i.e. from |vim.treesitter.query.get_files()|).
  -- ---
  -- --- Example: To disable `|` links in vimdoc but keep other `@markup.link`s highlighted:
  -- --- ```lua
  -- --- local link_pattern = 9 -- from :Inspect!
  -- --- local query = vim.treesitter.query.get('vimdoc', 'highlights')
  -- --- query.query:disable_pattern(link_pattern)
  -- --- local tree = vim.treesitter.get_parser():parse()[1]
  -- --- ```
  -- ---@param pattern_index integer
  -- function TSQuery:disable_pattern(pattern_index)
  --   C.ts_query_disable_pattern(self, pattern_index - 1)
  -- end
end

do --- lang and wasm
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
end

do --- query_err_string
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
end

local tsffi = {}

do --- tsffi
  --- @type table<string, [ffi.namespace*, TSLanguage.cdata]>
  tsffi._langs = {}

  --- @param lang string
  --- @return TSLanguage.cdata
  local function lang_check(lang)
    vim.validate('lang', lang, 'string')
    local l = tsffi._langs[lang][2]
    if not l then
      error(('no such language: %s'):format(lang), 2)
    end
    return l
  end

  --- @param node TSNode.ffi
  local function node_check(node)
    assert(ffi.istype('TSNode', node._node))
  end

  --- @param query TSQuery.ffi
  local function query_check(query)
    assert(ffi.istype('TSQuery', query))
  end

  --- @param lang string
  --- @return TSParser.ffi
  function tsffi._create_ts_parser(lang)
    local parser = C.ts_parser_new()
    ffi.gc(parser, C.ts_parser_delete)
    C.ts_parser_set_language(parser, lang_check(lang))
    return parser
  end

  --- @param lang string Language to use for the query
  --- @param query string Query string in s-expr syntax
  --- @return TSQuery.ffi
  function tsffi._ts_parse_query(lang, query)
    vim.validate('lang', lang, 'string')
    vim.validate('query', query, 'string')

    local language = lang_check(lang)
    local error_offset = ffi.new('uint32_t[1]')
    local error_type = ffi.new('TSQueryError[1]') --[[@as {[0]:TSQueryError}]]
    local tsquery = C.ts_query_new(language, query, #query, error_offset, error_type)

    if not tsquery then
      -- TODO(lewis6991): query_err_string(src, (int)error_offset, error_type, err_msg, sizeof(err_msg));
      error('not query')
    end

    ffi.gc(tsquery, C.ts_query_delete)

    return tsquery
  end

  --- @param node TSNode.ffi
  --- @param query TSQuery.ffi
  --- @param start integer?
  --- @param stop integer?
  --- @param opts? { max_start_depth?: integer, match_limit?: integer}
  --- @return TSQueryCursor.cdata
  function tsffi._create_ts_querycursor(node, query, start, stop, opts)
    node_check(node)
    query_check(query)

    local cursor = C.ts_query_cursor_new()
    ffi.gc(cursor, C.ts_query_cursor_delete)

    C.ts_query_cursor_exec(cursor, query, node._node)

    if start then
      local startp = ffi.new('TSPoint', start, 0) --[[@as TSPoint.cdata]]
      local endp = ffi.new('TSPoint', stop or MAXLNUM, 0) --[[@as TSPoint.cdata]]
      C.ts_query_cursor_set_point_range(cursor, startp, endp)
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
  function tsffi._ts_add_language_from_object(path, lang, symbol_name)
    symbol_name = symbol_name or lang
    local symbol = 'tree_sitter_' .. symbol_name
    ffi.cdef(('TSLanguage * %s(void);'):format(symbol))

    local mod = ffi.load(path)

    --- @type TSLanguage.cdata?
    local language = mod[symbol]()

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

    -- Need to store **both** the mod and the language to prevent them from
    -- being garbage collected.
    tsffi._langs[lang] = { mod, language }

    return true
  end

  --- @param lang string
  --- @return boolean
  function tsffi._ts_has_language(lang)
    return tsffi._langs[lang] ~= nil
  end

  --- @param lang string
  --- @return boolean
  function tsffi._ts_remove_language(lang)
    local present = tsffi._ts_has_language(lang)
    if tsffi._langs[lang] then
      tsffi._langs[lang] = nil
    end
    return present
  end
end

local M = {
  tsffi = tsffi,
}

--- @return string
local function get_current_module_path()
  -- debug.getinfo(1, "S") gets information about the current function (level 1)
  -- "S" specifies that we want the 'source' field.
  local info = debug.getinfo(1, 'S')

  -- The 'source' field contains the path of the file.
  -- It might be prefixed with '@' if it's a file path,
  -- or it could be different if the code was loaded from a string.
  local path = info.source

  -- Remove the '@' prefix if it exists
  if path:sub(1, 1) == '@' then
    --- @type string
    path = path:sub(2)
  end

  return vim.fs.abspath(path)
end

local otype = type

function M.setup()
  local modpath = get_current_module_path()
  local api_path = vim.fs.joinpath(vim.fs.dirname(vim.fs.dirname(modpath)), 'vendor', 'api.h')

  ffi.cdef(get_ts_cdef(api_path))

  ffi.cdef([[
    void *free(void *);

    TSTree *nvim_ts_parser_parse_buf(TSParser *p, TSTree *old_tree, int bufnr, uint64_t timeout_ns);
  ]])

  has_nvim_ts_parser_parse_buf = pcall(function()
    return C.nvim_ts_parser_parse_buf
  end)

  -- Can't metatype due to field-method conflicts:
  -- - TSNode.id
  -- - TSNode.tree
  -- - TSQueryMatch.captures
  --
  -- For now wrap all cdata objects in a table

  ffi.metatype('TSParser', {
    __index = TSParser,
    __tostring = function()
      return '<parser>'
    end,
  })

  ffi.metatype('TSQuery', {
    __index = TSQuery,
    __tostring = function()
      return '<query>'
    end,
  })

  ffi.metatype('TSTree', {
    __index = TSTree,
    -- __tostring = function()
    --   return '<tree>'
    -- end,
  })

  ffi.metatype('TSQueryCursor', {
    __index = TSQueryCursor,
    __tostring = function()
      return '<treecursor>'
    end,
  })

  -- ffi.metatype('TSNode', { __index = TSNode })
  -- ffi.metatype('TSQueryMatch', { __index = TSQueryMatch })

  function _G.type(obj)
    local r = otype(obj)
    if r == 'cdata' then
      return 'userdata'
    end
    return r
  end

  for k, v in
    pairs(tsffi --[[@as table<string,function> ]])
  do
    --- @diagnostic disable-next-line: no-unknown
    vim[k] = v
  end
end

return M
