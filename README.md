# tsffi.nvim

Very experimental plugin that aims to replace Neovim's treesitter interface with FFI bindings.

Why? The regular Lua-C API can be quite slow due to contexts switches. LuaJIT's FFI is fast.

## Usage

```lua
require('tsffi').setup()
```

## Test

```bash
nvim -u nvimrc.lua <C file>
# e.g. nvim -u nvimrc.lua vendor/api.h
```

## Todo
- [x] Stop crashes
- [x] Manage memory using `ffi.gc` and `ffi.free`
- [x] Use `ffi.metatype` where possible
  - Cannot use it for `TSNode` or `TSQueryMatch` without upstream changes.
- [ ] Make it more efficiently read nvim buffers.

  At the moment we just call `nvim_buf_get_text()` for the whole buffer before parsing.

  Ideally we should use the `TSInput` struct with the `read` callback, however callbacks
  are slow in LuaJIT-FFI.
