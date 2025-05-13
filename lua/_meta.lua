--- @meta

--- @class ffilib.tree-sitter.C
local C = {}

--- @alias strptr ffi.cdata*

function C.free(ptr) end

function C.malloc(size) end

--- @param parser TSParser.cdata
--- @param old_tree TSTree.cdata?
--- @param bufnr integer
--- @param timeout_ns integer
--- @return TSTree.cdata?
function C.nvim_ts_parser_parse_buf(parser, old_tree, bufnr, timeout_ns) end

do --- Section - Types
  ---@class TSStateId: integer
  ---@class TSSymbol: integer
  ---@class TSFieldId: integer
  ---@class TSLanguage.cdata: ffi.cdata*
  ---@class TSParser.cdata: ffi.cdata*
  ---@class TSTree.cdata: ffi.cdata*
  ---@class TSQuery.cdata: ffi.cdata*
  ---@class TSQueryCursor.cdata: ffi.cdata*
  ---@class TSLookaheadIterator.cdata: ffi.cdata*

  --- This function signature reads one code point from the given string,
  --- returning the number of bytes consumed. It should write the code point
  --- to the `code_point` pointer, or write -1 if the input is invalid.
  --- @alias DecodeFunction
  --- | fun(string: string, length: integer, code_point: integer): integer

  ---@class TSInputEncoding
  ---@field TSInputEncodingUTF8 integer
  ---@field TSInputEncodingUTF16LE integer
  ---@field TSInputEncodingUTF16BE integer
  ---@field TSInputEncodingCustom integer

  ---@class TSSymbolType
  ---@field TSSymbolTypeRegular integer
  ---@field TSSymbolTypeAnonymous integer
  ---@field TSSymbolTypeSupertype integer
  ---@field TSSymbolTypeAuxiliary integer

  ---@class TSPoint.cdata: ffi.cdata*
  ---@field row integer
  ---@field column integer

  ---@class TSRange.cdata: ffi.cdata*
  ---@field start_point TSPoint.cdata
  ---@field end_point TSPoint.cdata
  ---@field start_byte integer
  ---@field end_byte integer

  --- @class TSInput
  --- @field payload any
  --- @field read fun(payload: any, byte_index: integer, position: TSPoint.cdata, bytes_read: integer): string
  --- @field encoding TSInputEncoding
  --- @field decode DecodeFunction

  --- @class TSParseState
  --- @field payload any
  --- @field current_byte_offset integer
  --- @field has_error boolean

  --- @class TSParseOptions
  --- @field payload any
  --- @field progress_callback fun(state: TSParseState): boolean

  --- @class TSLogType
  --- @field TSLogTypeParse integer
  --- @field TSLogTypeLex integer

  --- @class TSLogger
  --- @field payload any
  --- @field log fun(payload: any, log_type: TSLogType, buffer: string)

  --- @class TSInputEdit.cdata: ffi.cdata*
  --- @field start_byte integer
  --- @field old_end_byte integer
  --- @field new_end_byte integer
  --- @field start_point TSPoint.cdata
  --- @field old_end_point TSPoint.cdata
  --- @field new_end_point TSPoint.cdata

  --- @class TSNode.cdata: ffi.cdata*
  --- @field context [integer, integer, integer, integer]
  --- @field id any
  --- @field tree TSTree.cdata

  --- @class TSTreeCursor.cdata: ffi.cdata*
  --- @field tree TSTree.cdata
  --- @field id integer
  --- @field context [integer, integer, integer]

  --- @class TSQueryCapture.cdata: ffi.cdata*
  --- @field node TSNode.cdata
  --- @field index integer

  --- @class TSQuantifier
  --- @field TSQuantifierZero 0 must match the array initialization value
  --- @field TSQuantifierZeroOrOne 1
  --- @field TSQuantifierZeroOrMore 2
  --- @field TSQuantifierOne 3
  --- @field TSQuantifierOneOrMore 4

  --- @class TSQueryMatch.cdata: ffi.cdata*
  --- @field id integer
  --- @field pattern_index integer
  --- @field capture_count integer
  --- @field captures TSQueryCapture.cdata[]

  --- @class TSQueryPredicateStepType
  --- @field TSQueryPredicateStepTypeDone integer
  --- @field TSQueryPredicateStepTypeCapture integer
  --- @field TSQueryPredicateStepTypeString integer

  --- @class TSQueryPredicateStep
  --- @field type TSQueryPredicateStepType
  --- @field value_id integer

  --- @class TSQueryError
  --- @field TSQueryErrorNone 0
  --- @field TSQueryErrorSyntax 1
  --- @field TSQueryErrorNodeType 2
  --- @field TSQueryErrorField 3
  --- @field TSQueryErrorCapture 4
  --- @field TSQueryErrorStructure 5
  --- @field TSQueryErrorLanguage 6

  --- @class TSQueryCursorState
  --- @field payload any
  --- @field current_byte_offset integer

  --- @class TSQueryCursorOptions
  --- @field payload any
  --- @field progress_callback fun(state: TSQueryCursorState): boolean

  --- The metadata associated with a language.
  ---
  --- Currently, this metadata can be used to check the [Semantic Version](https://semver.org/)
  --- of the language. This version information should be used to signal if a given parser might
  --- be incompatible with existing queries when upgrading between major versions, or minor versions
  --- if it's in zerover.
  --- @class TSLanguageMetadata
  --- @field major_version integer
  --- @field minor_version integer
  --- @field patch_version integer
end

do --- Section - Parser
  --- Create a new parser.
  --- @return TSParser.cdata
  function C.ts_parser_new() end

  --- Delete the parser, freeing all of the memory that it used.
  --- @param self TSParser.cdata
  function C.ts_parser_delete(self) end

  --- Get the parser's current language.
  --- @param self TSParser.cdata
  --- @return TSLanguage.cdata
  function C.ts_parser_language(self) end

  --- Set the language that the parser should use for parsing.
  ---
  --- Returns a boolean indicating whether or not the language was successfully
  --- assigned. True means assignment succeeded. False means there was a version
  --- mismatch: the language was generated with an incompatible version of the
  --- Tree-sitter CLI. Check the language's ABI version using [`ts_language_abi_version`]
  --- and compare it to this library's [`TREE_SITTER_LANGUAGE_VERSION`] and
  --- [`TREE_SITTER_MIN_COMPATIBLE_LANGUAGE_VERSION`] constants.
  --- @param self TSParser.cdata
  --- @param language TSLanguage.cdata
  --- @return boolean
  function C.ts_parser_set_language(self, language) end

  --- Set the ranges of text that the parser should include when parsing.
  ---
  --- By default, the parser will always include entire documents. This function
  --- allows you to parse only a *portion* of a document but still return a syntax
  --- tree whose ranges match up with the document as a whole. You can also pass
  --- multiple disjoint ranges.
  ---
  --- The second and third parameters specify the location and length of an array
  --- of ranges. The parser does *not* take ownership of these ranges; it copies
  --- the data, so it doesn't matter how these ranges are allocated.
  ---
  --- If `count` is zero, then the entire document will be parsed. Otherwise,
  --- the given ranges must be ordered from earliest to latest in the document,
  --- and they must not overlap. That is, the following must hold for all:
  ---
  --- `i < count - 1`: `ranges[i].end_byte <= ranges[i + 1].start_byte`
  ---
  --- If this requirement is not satisfied, the operation will fail, the ranges
  --- will not be assigned, and this function will return `false`. On success,
  --- this function returns `true`
  --- @param self TSParser.cdata
  --- @param ranges TSRange.cdata[]
  --- @param count integer
  --- @return boolean
  function C.ts_parser_set_included_ranges(self, ranges, count) end

  --- Get the ranges of text that the parser will include when parsing.
  ---
  --- The returned pointer is owned by the parser. The caller should not free it
  --- or write to it. The length of the array will be written to the given
  --- `count` pointer.
  --- @param self TSParser.cdata
  --- @param count `int[1]`
  --- @return TSRange.cdata[]
  function C.ts_parser_included_ranges(self, count) end

  --- Use the parser to parse some source code and create a syntax tree.
  ---
  --- If you are parsing this document for the first time, pass `NULL` for the
  --- `old_tree` parameter. Otherwise, if you have already parsed an earlier
  --- version of this document and the document has since been edited, pass the
  --- previous syntax tree so that the unchanged parts of it can be reused.
  --- This will save time and memory. For this to work correctly, you must have
  --- already edited the old syntax tree using the [`ts_tree_edit`] function in a
  --- way that exactly matches the source code changes.
  ---
  --- The [`TSInput`] parameter lets you specify how to read the text. It has the
  --- following three fields:
  --- 1. [`read`]: A function to retrieve a chunk of text at a given byte offset
  ---    and (row, column) position. The function should return a pointer to the
  ---    text and write its length to the [`bytes_read`] pointer. The parser does
  ---    not take ownership of this buffer; it just borrows it until it has
  ---    finished reading it. The function should write a zero value to the
  ---    [`bytes_read`] pointer to indicate the end of the document.
  --- 2. [`payload`]: An arbitrary pointer that will be passed to each invocation
  ---    of the [`read`] function.
  --- 3. [`encoding`]: An indication of how the text is encoded. Either
  ---    `TSInputEncodingUTF8` or `TSInputEncodingUTF16`.
  ---
  --- This function returns a syntax tree on success, and `NULL` on failure. There
  --- are four possible reasons for failure:
  --- 1. The parser does not have a language assigned. Check for this using the
  ---    [`ts_parser_language`] function.
  --- 2. Parsing was cancelled due to a timeout that was set by an earlier call to
  ---    the [`ts_parser_set_timeout_micros`] function. You can resume parsing from
  ---    where the parser left out by calling [`ts_parser_parse`] again with the
  ---    same arguments. Or you can start parsing from scratch by first calling
  ---    [`ts_parser_reset`].
  --- 3. Parsing was cancelled using a cancellation flag that was set by an
  ---    earlier call to [`ts_parser_set_cancellation_flag`]. You can resume parsing
  ---    from where the parser left out by calling [`ts_parser_parse`] again with
  ---    the same arguments.
  --- 4. Parsing was cancelled due to the progress callback returning true. This callback
  ---    is passed in [`ts_parser_parse_with_options`] inside the [`TSParseOptions`] struct.
  ---
  --- [`read`]: TSInput::read
  --- [`payload`]: TSInput::payload
  --- [`encoding`]: TSInput::encoding
  --- [`bytes_read`]: TSInput::read
  --- @param self TSParser.cdata
  --- @param old_tree TSTree.cdata?
  --- @param input TSInput
  --- @return TSTree.cdata?
  function C.ts_parser_parse(self, old_tree, input) end

  --- Use the parser to parse some source code and create a syntax tree, with some options.
  ---
  --- See [`ts_parser_parse`] for more details.
  ---
  --- See [`TSParseOptions`] for more details on the options.
  --- @param self TSParser.cdata
  --- @param old_tree TSTree.cdata?
  --- @param input TSInput
  --- @param parse_options TSParseOptions
  --- @return TSTree.cdata?
  function C.ts_parser_parse_with_options(self, old_tree, input, parse_options) end

  --- Use the parser to parse some source code stored in one contiguous buffer.
  --- The first two parameters are the same as in the [`ts_parser_parse`] function
  --- above. The second two parameters indicate the location of the buffer and its
  --- length in bytes.
  --- @param self TSParser.cdata
  --- @param old_tree TSTree.cdata?
  --- @param string string
  --- @param length integer
  --- @return TSTree.cdata?
  function C.ts_parser_parse_string(self, old_tree, string, length) end

  --- Use the parser to parse some source code stored in one contiguous buffer with
  --- a given encoding. The first four parameters work the same as in the
  --- [`ts_parser_parse_string`] method above. The final parameter indicates whether
  --- the text is encoded as UTF8 or UTF16.
  --- @param self TSParser.cdata
  --- @param old_tree TSTree.cdata?
  --- @param string string
  --- @param length integer
  --- @param encoding TSInputEncoding
  --- @return TSTree.cdata?
  function C.ts_parser_parse_string_encoding(self, old_tree, string, length, encoding) end

  --- Instruct the parser to start the next parse from the beginning.
  ---
  --- If the parser previously failed because of a timeout or a cancellation, then
  --- by default, it will resume where it left off on the next call to
  --- [`ts_parser_parse`] or other parsing functions. If you don't want to resume,
  --- and instead intend to use this parser to parse some other document, you must
  --- call [`ts_parser_reset`] first.
  --- @param self TSParser.cdata
  function C.ts_parser_reset(self) end

  --- @deprecated use [`ts_parser_parse_with_options`] and pass in a callback instead, this will be removed in 0.26.
  ---
  --- Set the maximum duration in microseconds that parsing should be allowed to
  --- take before halting.
  ---
  --- If parsing takes longer than this, it will halt early, returning NULL.
  --- See [`ts_parser_parse`] for more information.
  --- @param self TSParser.cdata
  --- @param timeout_micros integer
  function C.ts_parser_set_timeout_micros(self, timeout_micros) end

  --- @deprecated use [`ts_parser_parse_with_options`] and pass in a callback instead, this will be removed in 0.26.
  ---
  --- Get the duration in microseconds that parsing is allowed to take.
  --- @param self TSParser.cdata
  --- @return integer
  function C.ts_parser_timeout_micros(self) end

  --- @deprecated use [`ts_parser_parse_with_options`] and pass in a callback instead, this will be removed in 0.26.
  ---
  --- Set the parser's current cancellation flag pointer.
  ---
  --- If a non-null pointer is assigned, then the parser will periodically read
  --- from this pointer during parsing. If it reads a non-zero value, it will
  --- halt early, returning NULL. See [`ts_parser_parse`] for more information.
  --- @param self TSParser.cdata
  --- @param flag integer
  function C.ts_parser_set_cancellation_flag(self, flag) end

  --- @deprecated use [`ts_parser_parse_with_options`] and pass in a callback instead, this will be removed in 0.26.
  ---
  --- Get the parser's current cancellation flag pointer.
  --- @deprecated
  --- @param self TSParser.cdata
  --- @return integer
  function C.ts_parser_cancellation_flag(self) end

  --- Set the logger that a parser should use during parsing.
  ---
  --- The parser does not take ownership over the logger payload. If a logger was
  --- previously assigned, the caller is responsible for releasing any memory
  --- owned by the previous logger.
  --- @param self TSParser.cdata
  --- @param logger TSLogger
  function C.ts_parser_set_logger(self, logger) end

  --- Get the parser's current logger.
  --- @param self TSParser.cdata
  --- @return TSLogger
  function C.ts_parser_logger(self) end

  --- Set the file descriptor to which the parser should write debugging graphs
  --- during parsing. The graphs are formatted in the DOT language. You may want
  --- to pipe these graphs directly to a `dot(1)` process in order to generate
  --- SVG output. You can turn off this logging by passing a negative number.
  --- @param self TSParser.cdata
  --- @param fd integer
  function C.ts_parser_print_dot_graphs(self, fd) end
end

do --- Section - Tree
  --- Create a shallow copy of the syntax tree. This is very fast.
  ---
  --- You need to copy a syntax tree in order to use it on more than one thread at
  --- a time, as syntax trees are not thread safe.
  --- @param self TSTree.cdata
  --- @return TSTree.cdata
  function C.ts_tree_copy(self) end

  --- Delete the syntax tree, freeing all of the memory that it used.
  --- @param self TSTree.cdata
  function C.ts_tree_delete(self) end

  --- Get the root node of the syntax tree.
  --- @param self TSTree.cdata
  --- @return TSNode.cdata
  function C.ts_tree_root_node(self) end

  --- Get the root node of the syntax tree, but with its position
  --- shifted forward by the given offset.
  --- @param self TSTree.cdata
  --- @param offset_bytes integer
  --- @param offset_extent TSPoint.cdata
  --- @return TSNode.cdata
  function C.ts_tree_root_node_with_offset(self, offset_bytes, offset_extent) end

  --- Get the language that was used to parse the syntax tree.
  --- @param self TSTree.cdata
  --- @return TSLanguage.cdata
  function C.ts_tree_language(self) end

  --- Get the array of included ranges that was used to parse the syntax tree.
  ---
  --- The returned pointer must be freed by the caller.
  --- @param self TSTree.cdata
  --- @param length `int[1]`
  --- @return TSRange.cdata[]
  function C.ts_tree_included_ranges(self, length) end

  --- Edit the syntax tree to keep it in sync with source code that has been
  --- edited.
  ---
  --- You must describe the edit both in terms of byte offsets and in terms of
  --- (row, column) coordinates.
  --- @param self TSTree.cdata
  --- @param edit TSInputEdit.cdata
  function C.ts_tree_edit(self, edit) end

  --- Compare an old edited syntax tree to a new syntax tree representing the same
  --- document, returning an array of ranges whose syntactic structure has changed.
  ---
  --- For this to work correctly, the old syntax tree must have been edited such
  --- that its ranges match up to the new tree. Generally, you'll want to call
  --- this function right after calling one of the [`ts_parser_parse`] functions.
  --- You need to pass the old tree that was passed to parse, as well as the new
  --- tree that was returned from that function.
  ---
  --- The returned ranges indicate areas where the hierarchical structure of syntax
  --- nodes (from root to leaf) has changed between the old and new trees. Characters
  --- outside these ranges have identical ancestor nodes in both trees.
  ---
  --- Note that the returned ranges may be slightly larger than the exact changed areas,
  --- but Tree-sitter attempts to make them as small as possible.
  ---
  --- The returned array is allocated using `malloc` and the caller is responsible
  --- for freeing it using `free`. The length of the array will be written to the
  --- given `length` pointer.
  --- @param old_tree TSTree.cdata
  --- @param new_tree TSTree.cdata
  --- @param length `int[1]`
  --- @return TSRange.cdata[]
  function C.ts_tree_get_changed_ranges(old_tree, new_tree, length) end

  --- Write a DOT graph describing the syntax tree to the given file.
  --- @param self TSTree.cdata
  --- @param file_descriptor integer
  function C.ts_tree_print_dot_graph(self, file_descriptor) end
end

do --- Section - Node
  --- Get the node's type as a null-terminated string.
  --- @param self TSNode.cdata
  --- @return strptr
  function C.ts_node_type(self) end

  --- Get the node's type as a numerical id.
  --- @param self TSNode.cdata
  --- @return TSSymbol
  function C.ts_node_symbol(self) end

  --- Get the node's language.
  --- @param self TSNode.cdata
  --- @return TSLanguage.cdata
  function C.ts_node_language(self) end

  --- Get the node's type as it appears in the grammar ignoring aliases as a
  --- null-terminated string.
  --- @param self TSNode.cdata
  --- @return strptr
  function C.ts_node_grammar_type(self) end

  --- Get the node's type as a numerical id as it appears in the grammar ignoring
  --- aliases. This should be used in [`ts_language_next_state`] instead of
  --- [`ts_node_symbol`].
  --- @param self TSNode.cdata
  --- @return TSSymbol
  function C.ts_node_grammar_symbol(self) end

  --- Get the node's start byte.
  --- @param self TSNode.cdata
  --- @return integer
  function C.ts_node_start_byte(self) end

  --- Get the node's start position in terms of rows and columns.
  --- @param self TSNode.cdata
  --- @return TSPoint.cdata
  function C.ts_node_start_point(self) end

  --- Get the node's end byte.
  --- @param self TSNode.cdata
  --- @return integer
  function C.ts_node_end_byte(self) end

  --- Get the node's end position in terms of rows and columns.
  --- @param self TSNode.cdata
  --- @return TSPoint.cdata
  function C.ts_node_end_point(self) end

  --- Get an S-expression representing the node as a string.
  ---
  --- This string is allocated with `malloc` and the caller is responsible for
  --- freeing it using `free`.
  --- @param self TSNode.cdata
  --- @return strptr
  function C.ts_node_string(self) end

  --- Check if the node is null. Functions like [`ts_node_child`] and
  --- [`ts_node_next_sibling`] will return a null node to indicate that no such node
  --- was found.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_is_null(self) end

  --- Check if the node is *named*. Named nodes correspond to named rules in the
  --- grammar, whereas *anonymous* nodes correspond to string literals in the
  --- grammar.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_is_named(self) end

  --- Check if the node is *missing*. Missing nodes are inserted by the parser in
  --- order to recover from certain kinds of syntax errors.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_is_missing(self) end

  --- Check if the node is *extra*. Extra nodes represent things like comments,
  --- which are not required the grammar, but can appear anywhere.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_is_extra(self) end

  --- Check if a syntax node has been edited.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_has_changes(self) end

  --- Check if the node is a syntax error or contains any syntax errors.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_has_error(self) end

  --- Check if the node is a syntax error.
  --- @param self TSNode.cdata
  --- @return boolean
  function C.ts_node_is_error(self) end

  --- Get this node's parse state.
  --- @param self TSNode.cdata
  --- @return TSStateId
  function C.ts_node_parse_state(self) end

  --- Get the parse state after this node.
  --- @param self TSNode.cdata
  --- @return TSStateId
  function C.ts_node_next_parse_state(self) end

  --- Get the node's immediate parent.
  --- Prefer [`ts_node_child_with_descendant`] for
  --- iterating over the node's ancestors.
  --- @param self TSNode.cdata
  --- @return TSNode.cdata
  function C.ts_node_parent(self) end

  --- Get the node that contains `descendant`.
  ---
  --- Note that this can return `descendant` itself.
  --- @param self TSNode.cdata
  --- @param descendant TSNode.cdata
  --- @return TSNode.cdata
  function C.ts_node_child_with_descendant(self, descendant) end

  --- Get the node's child at the given index, where zero represents the first
  --- child.
  --- @param self TSNode.cdata
  --- @param child_index integer
  --- @return TSNode.cdata
  function C.ts_node_child(self, child_index) end

  --- Get the field name for node's child at the given index, where zero represents
  --- the first child. Returns NULL, if no field is found.
  --- @param self TSNode.cdata
  --- @param child_index integer
  --- @return strptr?
  function C.ts_node_field_name_for_child(self, child_index) end

  --- Get the field name for node's named child at the given index, where zero
  --- represents the first named child. Returns NULL, if no field is found.
  --- @param self TSNode.cdata
  --- @param named_child_index integer
  --- @return strptr?
  function C.ts_node_field_name_for_named_child(self, named_child_index) end

  --- Get the node's number of children.
  --- @param self TSNode.cdata
  --- @return integer
  function C.ts_node_child_count(self) end

  --- Get the node's *named* child at the given index.
  ---
  --- See also [`ts_node_is_named`].
  --- @param self TSNode.cdata
  --- @param child_index integer
  --- @return TSNode.cdata
  function C.ts_node_named_child(self, child_index) end

  --- Get the node's number of *named* children.
  ---
  --- See also [`ts_node_is_named`].
  --- @param self TSNode.cdata
  --- @return integer
  function C.ts_node_named_child_count(self) end

  -- --- Get the node's child with the given field name.
  -- --- @param self TSNode.cdata
  -- --- @param name string
  -- --- @param name_length integer
  -- --- @return TSNode.cdata
  -- function C.ts_node_child_by_field_name(self, name, name_length) end

  -- --- Get the node's child with the given numerical field id.
  -- ---
  -- --- You can convert a field name to an id using the
  -- --- [`ts_language_field_id_for_name`] function.
  -- --- @param self TSNode.cdata
  -- --- @param field_id TSFieldId
  -- --- @return TSNode.cdata
  -- function C.ts_node_child_by_field_id(self, field_id) end

  --- Get the node's next sibling.
  --- @param self TSNode.cdata
  --- @return TSNode.cdata
  function C.ts_node_next_sibling(self) end

  --- Get the node's previous sibling.
  --- @param self TSNode.cdata
  --- @return TSNode.cdata
  function C.ts_node_prev_sibling(self) end

  --- Get the node's next *named* sibling.
  --- @param self TSNode.cdata
  --- @return TSNode.cdata
  function C.ts_node_next_named_sibling(self) end

  --- Get the node's previous *named* sibling.
  --- @param self TSNode.cdata
  --- @return TSNode.cdata
  function C.ts_node_prev_named_sibling(self) end

  --- Get the node's first child that contains or starts after the given byte offset.
  --- @param self TSNode.cdata
  --- @param byte integer
  --- @return TSNode.cdata
  function C.ts_node_first_child_for_byte(self, byte) end

  --- Get the node's first named child that contains or starts after the given byte offset.
  --- @param self TSNode.cdata
  --- @param byte integer
  --- @return TSNode.cdata
  function C.ts_node_first_named_child_for_byte(self, byte) end

  --- Get the node's number of descendants, including one for the node itself.
  --- @param self TSNode.cdata
  --- @return integer
  function C.ts_node_descendant_count(self) end

  --- Get the smallest node within this node that spans the given range of bytes.
  --- @param self TSNode.cdata
  --- @param start integer
  --- @param end_ integer
  --- @return TSNode.cdata
  function C.ts_node_descendant_for_byte_range(self, start, end_) end

  --- Get the smallest node within this node that spans the given range of
  --- (row, column) positions.
  --- @param self TSNode.cdata
  --- @param start TSPoint.cdata
  --- @param end_ TSPoint.cdata
  --- @return TSNode.cdata
  function C.ts_node_descendant_for_point_range(self, start, end_) end

  --- Gets the smallest named node within this node that spans the given range of bytes.
  --- Get the smallest named node within this node that spans the given range of
  --- bytes.
  --- @param self TSNode.cdata
  --- @param start integer
  --- @param end_ integer
  --- @return TSNode.cdata
  function C.ts_node_named_descendant_for_byte_range(self, start, end_) end

  --- Get the smallest named node within this node that spans the given range of
  --- (row, column) positions.
  --- @param self TSNode.cdata
  --- @param start TSPoint.cdata
  --- @param end_ TSPoint.cdata
  --- @return TSNode.cdata
  function C.ts_node_named_descendant_for_point_range(self, start, end_) end

  --- Edit the node to keep it in-sync with source code that has been edited.
  ---
  --- This function is only rarely needed. When you edit a syntax tree with the
  --- [`ts_tree_edit`] function, all of the nodes that you retrieve from the tree
  --- afterward will already reflect the edit. You only need to use [`ts_node_edit`]
  --- when you have a [`TSNode.cdata`] instance that you want to keep and continue to use
  --- after an edit.
  --- @param self TSNode.cdata
  --- @param edit TSInputEdit.cdata
  function C.ts_node_edit(self, edit) end

  --- Checks if two nodes are identical.
  --- @param self TSNode.cdata
  --- @param other TSNode.cdata
  --- @return boolean
  function C.ts_node_eq(self, other) end
end

do --- Section - TreeCursor
  --- Create a new tree cursor starting from the given node.
  ---
  --- A tree cursor allows you to walk a syntax tree more efficiently than is
  --- possible using the [`TSNode.cdata`] functions. It is a mutable object that is always
  --- on a certain syntax node, and can be moved imperatively to different nodes.
  ---
  --- Note that the given node is considered the root of the cursor,
  --- and the cursor cannot walk outside this node.
  --- @param node TSNode.cdata
  --- @return TSTreeCursor.cdata
  function C.ts_tree_cursor_new(node) end

  --- Delete a tree cursor, freeing all of the memory that it used.
  --- @param self TSTreeCursor.cdata
  function C.ts_tree_cursor_delete(self) end

  --- Re-initialize a tree cursor to start at the original node that the cursor was
  --- constructed with.
  --- @param self TSTreeCursor.cdata
  --- @param node TSNode.cdata
  function C.ts_tree_cursor_reset(self, node) end

  --- Re-initialize a tree cursor to the same position as another cursor.
  ---
  --- Unlike [`ts_tree_cursor_reset`], this will not lose parent information and
  --- allows reusing already created cursors.
  --- @param dst TSTreeCursor.cdata
  --- @param src TSTreeCursor.cdata
  function C.ts_tree_cursor_reset_to(dst, src) end

  --- Get the tree cursor's current node.
  --- @param self TSTreeCursor.cdata
  --- @return TSNode.cdata
  function C.ts_tree_cursor_current_node(self) end

  --- Get the field name of the tree cursor's current node.
  ---
  --- This returns `NULL` if the current node doesn't have a field.
  --- See also [`ts_node_child_by_field_name`].
  --- @param self TSTreeCursor.cdata
  --- @return strptr?
  function C.ts_tree_cursor_current_field_name(self) end

  --- Get the field id of the tree cursor's current node.
  ---
  --- This returns zero if the current node doesn't have a field.
  --- See also [`ts_node_child_by_field_id`], [`ts_language_field_id_for_name`].
  --- @param self TSTreeCursor.cdata
  --- @return TSFieldId
  function C.ts_tree_cursor_current_field_id(self) end

  --- Move the cursor to the parent of its current node.
  ---
  --- This returns `true` if the cursor successfully moved, and returns `false`
  --- if there was no parent node (the cursor was already on the root node).
  ---
  --- Note that the node the cursor was constructed with is considered the root
  --- of the cursor, and the cursor cannot walk outside this node.
  --- @param self TSTreeCursor.cdata
  --- @return boolean
  function C.ts_tree_cursor_goto_parent(self) end

  --- Move the cursor to the next sibling of its current node.
  ---
  --- This returns `true` if the cursor successfully moved, and returns `false`
  --- if there was no next sibling node.
  ---
  --- Note that the node the cursor was constructed with is considered the root
  --- of the cursor, and the cursor cannot walk outside this node.
  --- @param self TSTreeCursor.cdata
  --- @return boolean
  function C.ts_tree_cursor_goto_next_sibling(self) end

  --- Move the cursor to the previous sibling of its current node.
  ---
  --- This returns `true` if the cursor successfully moved, and returns `false` if
  --- there was no previous sibling node.
  ---
  --- Note, that this function may be slower than
  --- [`ts_tree_cursor_goto_next_sibling`] due to how node positions are stored. In
  --- the worst case, this will need to iterate through all the children up to the
  --- previous sibling node to recalculate its position. Also note that the node the cursor
  --- was constructed with is considered the root of the cursor, and the cursor cannot
  --- walk outside this node.
  --- @param self TSTreeCursor.cdata
  --- @return boolean
  function C.ts_tree_cursor_goto_previous_sibling(self) end

  --- Move the cursor to the first child of its current node.
  ---
  --- This returns `true` if the cursor successfully moved, and returns `false`
  --- if there were no children.
  --- @param self TSTreeCursor.cdata
  --- @return boolean
  function C.ts_tree_cursor_goto_first_child(self) end

  --- Move the cursor to the last child of its current node.
  ---
  --- This returns `true` if the cursor successfully moved, and returns `false` if
  --- there were no children.
  ---
  --- Note that this function may be slower than [`ts_tree_cursor_goto_first_child`]
  --- because it needs to iterate through all the children to compute the child's
  --- position.
  --- @param self TSTreeCursor.cdata
  --- @return boolean
  function C.ts_tree_cursor_goto_last_child(self) end

  --- Move the cursor to the node that is the nth descendant of
  --- the original node that the cursor was constructed with, where
  --- zero represents the original node itself.
  --- @param self TSTreeCursor.cdata
  --- @param goal_descendant_index integer
  function C.ts_tree_cursor_goto_descendant(self, goal_descendant_index) end

  --- Get the index of the cursor's current node out of all of the
  --- descendants of the original node that the cursor was constructed with.
  --- @param self TSTreeCursor.cdata
  --- @return integer
  function C.ts_tree_cursor_current_descendant_index(self) end

  --- Get the depth of the cursor's current node relative to the original
  --- node that the cursor was constructed with.
  --- @param self TSTreeCursor.cdata
  --- @return integer
  function C.ts_tree_cursor_current_depth(self) end

  --- Move the cursor to the first child of its current node that contains or starts after
  --- the given byte offset.
  ---
  --- This returns the index of the child node if one was found, and returns -1
  --- if no such child was found.
  --- @param self TSTreeCursor.cdata
  --- @param goal_byte integer
  --- @return integer
  function C.ts_tree_cursor_goto_first_child_for_byte(self, goal_byte) end

  --- Move the cursor to the first child of its current node that contains or starts after
  --- the given point.
  ---
  --- This returns the index of the child node if one was found, and returns -1
  --- if no such child was found.
  --- @param self TSTreeCursor.cdata
  --- @param goal_point TSPoint.cdata
  --- @return integer
  function C.ts_tree_cursor_goto_first_child_for_point(self, goal_point) end

  --- Create a copy of the tree cursor.
  --- @param cursor TSTreeCursor.cdata
  --- @return TSTreeCursor.cdata
  function C.ts_tree_cursor_copy(cursor) end
end

do --- Section - Query
  --- Create a new query from a string containing one or more S-expression
  --- patterns. The query is associated with a particular language, and can
  --- only be run on syntax nodes parsed with that language.
  ---
  --- If all of the given patterns are valid, this returns a [`TSQuery`].
  --- If a pattern is invalid, this returns `NULL`, and provides two pieces
  --- of information about the problem:
  --- 1. The byte offset of the error is written to the `error_offset` parameter.
  --- 2. The type of error is written to the `error_type` parameter.
  --- @param language TSLanguage.cdata
  --- @param source string
  --- @param source_len integer
  --- @param error_offset `int[1]`
  --- @param error_type {[0]:TSQueryError}
  --- @return TSQuery.cdata?
  function C.ts_query_new(language, source, source_len, error_offset, error_type) end

  --- Delete a query, freeing all of the memory that it used.
  --- @param self TSQuery.cdata
  function C.ts_query_delete(self) end

  --- Get the number of patterns, captures, or string literals in the query.
  --- @param self TSQuery.cdata
  --- @return integer
  function C.ts_query_pattern_count(self) end

  --- @param self TSQuery.cdata
  --- @return integer
  function C.ts_query_capture_count(self) end

  --- @param self TSQuery.cdata
  --- @return integer
  function C.ts_query_string_count(self) end

  --- Get the byte offset where the given pattern starts in the query's source.
  ---
  --- This can be useful when combining queries by concatenating their source
  --- code strings.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  --- @return integer
  function C.ts_query_start_byte_for_pattern(self, pattern_index) end

  --- Get the byte offset where the given pattern ends in the query's source.
  ---
  --- This can be useful when combining queries by concatenating their source
  --- code strings.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  --- @return integer
  function C.ts_query_end_byte_for_pattern(self, pattern_index) end

  --- Get all of the predicates for the given pattern in the query.
  ---
  --- The predicates are represented as a single array of steps. There are three
  --- types of steps in this array, which correspond to the three legal values for
  --- the `type` field:
  --- - `TSQueryPredicateStepTypeCapture` - Steps with this type represent names
  ---    of captures. Their `value_id` can be used with the
  ---   [`ts_query_capture_name_for_id`] function to obtain the name of the capture.
  --- - `TSQueryPredicateStepTypeString` - Steps with this type represent literal
  ---    strings. Their `value_id` can be used with the
  ---    [`ts_query_string_value_for_id`] function to obtain their string value.
  --- - `TSQueryPredicateStepTypeDone` - Steps with this type are *sentinels*
  ---    that represent the end of an individual predicate. If a pattern has two
  ---    predicates, then there will be two steps with this `type` in the array.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  --- @param step_count `int[1]`
  --- @return TSQueryPredicateStep[]
  function C.ts_query_predicates_for_pattern(self, pattern_index, step_count) end

  --- Check if the given pattern in the query has a single root node.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  --- @return boolean
  function C.ts_query_is_pattern_rooted(self, pattern_index) end

  --- Check if the given pattern in the query is 'non local'.
  ---
  --- A non-local pattern has multiple root nodes and can match within a
  --- repeating sequence of nodes, as specified by the grammar. Non-local
  --- patterns disable certain optimizations that would otherwise be possible
  --- when executing a query on a specific range of a syntax tree.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  --- @return boolean
  function C.ts_query_is_pattern_non_local(self, pattern_index) end

  --- Check if a given pattern is guaranteed to match once a given step is reached.
  --- The step is specified by its byte offset in the query's source code.
  --- @param self TSQuery.cdata
  --- @param byte_offset integer
  --- @return boolean
  function C.ts_query_is_pattern_guaranteed_at_step(self, byte_offset) end

  --- Get the name and length of one of the query's captures, or one of the
  --- query's string literals. Each capture and string is associated with a
  --- numeric id based on the order that it appeared in the query's source.
  --- @param self TSQuery.cdata
  --- @param index integer
  --- @param length `int[1]`
  --- @return strptr
  function C.ts_query_capture_name_for_id(self, index, length) end

  --- Get the quantifier of the query's captures. Each capture is * associated
  --- with a numeric id based on the order that it appeared in the query's source.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  --- @param capture_index integer
  --- @return TSQuantifier
  function C.ts_query_capture_quantifier_for_id(self, pattern_index, capture_index) end

  --- @param self TSQuery.cdata
  --- @param index integer
  --- @param length `int[1]`
  --- @return strptr
  function C.ts_query_string_value_for_id(self, index, length) end

  --- Disable a certain capture within a query.
  ---
  --- This prevents the capture from being returned in matches, and also avoids
  --- any resource usage associated with recording the capture. Currently, there
  --- is no way to undo this.
  --- @param self TSQuery.cdata
  --- @param name string
  --- @param length integer
  function C.ts_query_disable_capture(self, name, length) end

  --- Disable a certain pattern within a query.
  ---
  --- This prevents the pattern from matching and removes most of the overhead
  --- associated with the pattern. Currently, there is no way to undo this.
  --- @param self TSQuery.cdata
  --- @param pattern_index integer
  function C.ts_query_disable_pattern(self, pattern_index) end

  --- Create a new cursor for executing a given query.
  ---
  --- The cursor stores the state that is needed to iteratively search
  --- for matches. To use the query cursor, first call [`ts_query_cursor_exec`]
  --- to start running a given query on a given syntax node. Then, there are
  --- two options for consuming the results of the query:
  --- 1. Repeatedly call [`ts_query_cursor_next_match`] to iterate over all of the
  ---    *matches* in the order that they were found. Each match contains the
  ---    index of the pattern that matched, and an array of captures. Because
  ---    multiple patterns can match the same set of nodes, one match may contain
  ---    captures that appear *before* some of the captures from a previous match.
  --- 2. Repeatedly call [`ts_query_cursor_next_capture`] to iterate over all of the
  ---    individual *captures* in the order that they appear. This is useful if
  ---    don't care about which pattern matched, and just want a single ordered
  ---    sequence of captures.
  ---
  --- If you don't care about consuming all of the results, you can stop calling
  --- [`ts_query_cursor_next_match`] or [`ts_query_cursor_next_capture`] at any point.
  ---  You can then start executing another query on another node by calling
  ---  [`ts_query_cursor_exec`] again.
  --- @return TSQueryCursor.cdata
  function C.ts_query_cursor_new() end

  --- Delete a query cursor, freeing all of the memory that it used.
  --- @param self TSQueryCursor.cdata
  function C.ts_query_cursor_delete(self) end

  --- Start running a given query on a given node.
  --- @param self TSQueryCursor.cdata
  --- @param query TSQuery.cdata
  --- @param node TSNode.cdata
  function C.ts_query_cursor_exec(self, query, node) end

  --- Start running a given query on a given node, with some options.
  --- @param self TSQueryCursor.cdata
  --- @param query TSQuery.cdata
  --- @param node TSNode.cdata
  --- @param query_options TSQueryCursorOptions
  function C.ts_query_cursor_exec_with_options(self, query, node, query_options) end

  --- Manage the maximum number of in-progress matches allowed by this query
  --- cursor.
  ---
  --- Query cursors have an optional maximum capacity for storing lists of
  --- in-progress captures. If this capacity is exceeded, then the
  --- earliest-starting match will silently be dropped to make room for further
  --- matches. This maximum capacity is optional — by default, query cursors allow
  --- any number of pending matches, dynamically allocating new space for them as
  --- needed as the query is executed.
  --- @param self TSQueryCursor.cdata
  --- @return boolean
  function C.ts_query_cursor_did_exceed_match_limit(self) end

  --- @param self TSQueryCursor.cdata
  --- @return integer
  function C.ts_query_cursor_match_limit(self) end

  --- @param self TSQueryCursor.cdata
  --- @param limit integer
  function C.ts_query_cursor_set_match_limit(self, limit) end

  --- @deprecated use [`ts_query_cursor_exec_with_options`] and pass in a callback instead, this will be removed in 0.26.
  ---
  --- Set the maximum duration in microseconds that query execution should be allowed to
  --- take before halting.
  ---
  --- If query execution takes longer than this, it will halt early, returning nil.
  --- See [`ts_query_cursor_next_match`] or [`ts_query_cursor_next_capture`] for more information.
  --- @param self TSQueryCursor.cdata
  --- @param timeout_micros integer
  function C.ts_query_cursor_set_timeout_micros(self, timeout_micros) end

  --- @deprecated use [`ts_query_cursor_exec_with_options`] and pass in a callback instead, this will be removed in 0.26.
  ---
  --- Get the duration in microseconds that query execution is allowed to take.
  ---
  --- This is set via [`ts_query_cursor_set_timeout_micros`].
  --- @param self TSQueryCursor.cdata
  --- @return integer
  function C.ts_query_cursor_timeout_micros(self) end

  --- Set the range of bytes in which the query will be executed.
  ---
  --- The query cursor will return matches that intersect with the given point range.
  --- This means that a match may be returned even if some of its captures fall
  --- outside the specified range, as long as at least part of the match
  --- overlaps with the range.
  ---
  --- For example, if a query pattern matches a node that spans a larger area
  --- than the specified range, but part of that node intersects with the range,
  --- the entire match will be returned.
  ---
  --- This will return `false` if the start byte is greater than the end byte, otherwise
  --- it will return `true`.
  --- @param self TSQueryCursor.cdata
  --- @param start_byte integer
  --- @param end_byte integer
  --- @return boolean
  function C.ts_query_cursor_set_byte_range(self, start_byte, end_byte) end

  --- Set the range of (row, column) positions in which the query will be executed.
  ---
  --- The query cursor will return matches that intersect with the given point range.
  --- This means that a match may be returned even if some of its captures fall
  --- outside the specified range, as long as at least part of the match
  --- overlaps with the range.
  ---
  --- For example, if a query pattern matches a node that spans a larger area
  --- than the specified range, but part of that node intersects with the range,
  --- the entire match will be returned.
  ---
  --- This will return `false` if the start point is greater than the end point, otherwise
  --- it will return `true`.
  --- @param self TSQueryCursor.cdata
  --- @param start_point TSPoint.cdata
  --- @param end_point TSPoint.cdata
  --- @return boolean
  function C.ts_query_cursor_set_point_range(self, start_point, end_point) end

  --- Advance to the next match of the currently running query.
  ---
  --- If there is a match, write it to `*match` and return `true`.
  --- Otherwise, return `false`.
  --- @param self TSQueryCursor.cdata
  --- @param match TSQueryMatch.cdata
  --- @return boolean
  function C.ts_query_cursor_next_match(self, match) end

  --- @param self TSQueryCursor.cdata
  --- @param match_id integer
  function C.ts_query_cursor_remove_match(self, match_id) end

  --- Advance to the next capture of the currently running query.
  ---
  --- If there is a capture, write its match to `*match` and its index within
  --- the match's capture list to `*capture_index`. Otherwise, return `false`.
  --- @param self TSQueryCursor.cdata
  --- @param match TSQueryMatch.cdata
  --- @param capture_index `int[1]`
  --- @return boolean
  function C.ts_query_cursor_next_capture(self, match, capture_index) end

  --- Set the maximum start depth for a query cursor.
  ---
  --- This prevents cursors from exploring children nodes at a certain depth.
  --- Note if a pattern includes many children, then they will still be checked.
  ---
  --- The zero max start depth value can be used as a special behavior and
  --- it helps to destructure a subtree by staying on a node and using captures
  --- for interested parts. Note that the zero max start depth only limit a search
  --- depth for a pattern's root node but other nodes that are parts of the pattern
  --- may be searched at any depth what defined by the pattern structure.
  ---
  --- Set to `UINT32_MAX` to remove the maximum start depth.
  --- @param self TSQueryCursor.cdata
  --- @param max_start_depth integer
  function C.ts_query_cursor_set_max_start_depth(self, max_start_depth) end
end

do --- Section - Language
  --- Get another reference to the given language.
  --- @param self TSLanguage.cdata
  --- @return TSLanguage.cdata
  function C.ts_language_copy(self) end

  --- Free any dynamically-allocated resources for this language, if
  --- this is the last reference.
  --- @param self TSLanguage.cdata
  function C.ts_language_delete(self) end

  --- Get the number of distinct node types in the language.
  --- @param self TSLanguage.cdata
  --- @return integer
  function C.ts_language_symbol_count(self) end

  --- Get the number of valid states in this language.
  --- @param self TSLanguage.cdata
  --- @return integer
  function C.ts_language_state_count(self) end

  --- Get the numerical id for the given node type string.
  --- @param self TSLanguage.cdata
  --- @param string string
  --- @param length integer
  --- @param is_named boolean
  --- @return TSSymbol
  function C.ts_language_symbol_for_name(self, string, length, is_named) end

  --- Get the number of distinct field names in the language.
  --- @param self TSLanguage.cdata
  --- @return integer
  function C.ts_language_field_count(self) end

  --- Get the field name string for the given numerical id.
  --- @param self TSLanguage.cdata
  --- @param id TSFieldId
  --- @return strptr
  function C.ts_language_field_name_for_id(self, id) end

  --- Get the numerical id for the given field name string.
  --- @param self TSLanguage.cdata
  --- @param name string
  --- @param name_length integer
  --- @return TSFieldId
  function C.ts_language_field_id_for_name(self, name, name_length) end

  --- Get a list of all supertype symbols for the language.
  --- @param self TSLanguage.cdata
  --- @param length `int[1]`
  --- @return TSSymbol[]
  function C.ts_language_supertypes(self, length) end

  --- Get a list of all subtype symbol ids for a given supertype symbol.
  ---
  --- See [`ts_language_supertypes`] for fetching all supertype symbols.
  --- @param self TSLanguage.cdata
  --- @param supertype TSSymbol
  --- @param length `int[1]`
  --- @return TSSymbol[]
  function C.ts_language_subtypes(self, supertype, length) end

  --- Get a node type string for the given numerical id.
  --- @param self TSLanguage.cdata
  --- @param symbol TSSymbol
  --- @return strptr
  function C.ts_language_symbol_name(self, symbol) end

  --- Check whether the given node type id belongs to named nodes, anonymous nodes,
  --- or a hidden nodes.
  ---
  --- See also [`ts_node_is_named`]. Hidden nodes are never returned from the API.
  --- @param self TSLanguage.cdata
  --- @param symbol TSSymbol
  --- @return TSSymbolType
  function C.ts_language_symbol_type(self, symbol) end

  --- @deprecated use [`ts_language_abi_version`] instead, this will be removed in 0.26.
  ---
  --- Get the ABI version number for this language. This version number is used
  --- to ensure that languages were generated by a compatible version of
  --- Tree-sitter.
  ---
  --- See also [`ts_parser_set_language`].
  --- @param self TSLanguage.cdata
  --- @return integer
  function C.ts_language_version(self) end

  --- Get the ABI version number for this language. This version number is used
  --- to ensure that languages were generated by a compatible version of
  --- Tree-sitter.
  ---
  --- See also [`ts_parser_set_language`].
  --- @param self TSLanguage.cdata
  --- @return integer
  function C.ts_language_abi_version(self) end

  --- Get the metadata for this language. This information is generated by the
  --- CLI, and relies on the language author providing the correct metadata in
  --- the language's `tree-sitter.json` file.
  ---
  --- See also [`TSMetadata`].
  --- @param self TSLanguage.cdata
  --- @return TSLanguageMetadata
  function C.ts_language_metadata(self) end

  --- Get the next parse state. Combine this with lookahead iterators to generate
  --- completion suggestions or valid symbols in error nodes. Use
  --- [`ts_node_grammar_symbol`] for valid symbols.
  --- @param self TSLanguage.cdata
  --- @param state TSStateId
  --- @param symbol TSSymbol
  --- @return TSStateId
  function C.ts_language_next_state(self, state, symbol) end

  --- Get the name of this language. This returns `NULL` in older parsers.
  --- @param self TSLanguage.cdata
  --- @return strptr
  function C.ts_language_name(self) end
end

do --- Section - Lookahead Iterator
  --- Create a new lookahead iterator for the given language and parse state.
  ---
  --- This returns `NULL` if state is invalid for the language.
  ---
  --- Repeatedly using [`ts_lookahead_iterator_next`] and
  --- [`ts_lookahead_iterator_current_symbol`] will generate valid symbols in the
  --- given parse state. Newly created lookahead iterators will contain the `ERROR`
  --- symbol.
  ---
  --- Lookahead iterators can be useful to generate suggestions and improve syntax
  --- error diagnostics. To get symbols valid in an ERROR node, use the lookahead
  --- iterator on its first leaf node state. For `MISSING` nodes, a lookahead
  --- iterator created on the previous non-extra leaf node may be appropriate.
  --- @param self TSLanguage.cdata
  --- @param state TSStateId
  --- @return TSLookaheadIterator.cdata?
  function C.ts_lookahead_iterator_new(self, state) end

  --- Delete a lookahead iterator freeing all the memory used.
  --- @param self TSLookaheadIterator.cdata
  function C.ts_lookahead_iterator_delete(self) end

  --- Reset the lookahead iterator to another state.
  ---
  --- This returns `true` if the iterator was reset to the given state and `false`
  --- otherwise.
  --- @param self TSLookaheadIterator.cdata
  --- @param state TSStateId
  --- @return boolean
  function C.ts_lookahead_iterator_reset_state(self, state) end

  --- Reset the lookahead iterator.
  ---
  --- This returns `true` if the language was set successfully and `false`
  --- otherwise.
  --- @param self TSLookaheadIterator.cdata
  --- @param language TSLanguage.cdata
  --- @param state TSStateId
  --- @return boolean
  function C.ts_lookahead_iterator_reset(self, language, state) end

  --- Get the current language of the lookahead iterator.
  --- @param self TSLookaheadIterator.cdata
  --- @return TSLanguage.cdata
  function C.ts_lookahead_iterator_language(self) end

  --- Advance the lookahead iterator to the next symbol.
  ---
  --- This returns `true` if there is a new symbol and `false` otherwise.
  --- @param self TSLookaheadIterator.cdata
  --- @return boolean
  function C.ts_lookahead_iterator_next(self) end

  --- Get the current symbol of the lookahead iterator.
  --- @param self TSLookaheadIterator.cdata
  --- @return TSSymbol
  function C.ts_lookahead_iterator_current_symbol(self) end

  --- Get the current symbol type of the lookahead iterator as a null terminated
  --- string.
  --- @param self TSLookaheadIterator.cdata
  --- @return strptr
  function C.ts_lookahead_iterator_current_symbol_name(self) end
end

do --- Section - WebAssembly Integration
  --- @class TSWasmEngine: ffi.cdata*

  --- @class TSWasmStore: ffi.cdata*

  --- @class TSWasmErrorKind
  --- @field None 0
  --- @field Parse 1
  --- @field Compile 2
  --- @field Instantiate 3
  --- @field Allocate 4

  --- @class TSWasmError: ffi.cdata*
  --- @field kind TSWasmErrorKind
  --- @field message string

  --- Create a Wasm store.
  --- @param engine TSWasmEngine
  --- @param error TSWasmError
  --- @return TSWasmStore
  function C.ts_wasm_store_new(engine, error) end

  --- Free the memory associated with the given Wasm store.
  --- @param self TSWasmStore
  function C.ts_wasm_store_delete(self) end

  --- Create a language from a buffer of Wasm. The resulting language behaves
  --- like any other Tree-sitter language, except that in order to use it with
  --- a parser, that parser must have a Wasm store. Note that the language
  --- can be used with any Wasm store, it doesn't need to be the same store that
  --- was used to originally load it.
  --- @param self TSWasmStore
  --- @param name string
  --- @param wasm string
  --- @param wasm_len integer
  --- @param error TSWasmError
  --- @return TSLanguage.cdata?
  function C.ts_wasm_store_load_language(self, name, wasm, wasm_len, error) end

  --- Get the number of languages instantiated in the given wasm store.
  --- @param self TSWasmStore
  --- @return integer
  function C.ts_wasm_store_language_count(self) end

  --- Check if the language came from a Wasm module. If so, then in order to use
  --- this language with a Parser, that parser must have a Wasm store assigned.
  --- @param self TSLanguage.cdata
  --- @return boolean
  function C.ts_language_is_wasm(self) end

  --- Assign the given Wasm store to the parser. A parser must have a Wasm store
  --- in order to use Wasm languages.
  --- @param self TSParser.cdata
  --- @param wasm_store TSWasmStore
  function C.ts_parser_set_wasm_store(self, wasm_store) end

  --- Remove the parser's current Wasm store and return it. This returns NULL if
  --- the parser doesn't have a Wasm store.
  --- @param self TSParser.cdata
  --- @return TSWasmStore?
  function C.ts_parser_take_wasm_store(self) end
end

do --- Section - Global Configuration
  --- Set the allocation functions used by the library.
  ---
  --- By default, Tree-sitter uses the standard libc allocation functions,
  --- but aborts the process when an allocation fails. This function lets
  --- you supply alternative allocation functions at runtime.
  ---
  --- If you pass `NULL` for any parameter, Tree-sitter will switch back to
  --- its default implementation of that function.
  ---
  --- If you call this function after the library has already been used, then
  --- you must ensure that either:
  ---  1. All the existing objects have been freed.
  ---  2. The new allocator shares its state with the old one, so it is capable
  ---     of freeing memory that was allocated by the old allocator.
  --- @param new_malloc fun(size: integer): any
  --- @param new_calloc fun(count: integer, size: integer): any
  --- @param new_realloc fun(ptr: any, size: integer): any
  --- @param new_free fun(ptr: any)
  function C.ts_set_allocator(new_malloc, new_calloc, new_realloc, new_free) end
end
