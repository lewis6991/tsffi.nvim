# tsffi.nvim

Very experimental plugin that aims to replace Neovim's treesitter interface with FFI bindings.

Why? The regular Lua-C API can be quite slow due to contexts switches. LuaJIT's FFI is fast.

## Requirements

Neovim 0.11

Optional: nvim built with https://github.com/neovim/neovim/pull/34008

## Usage

```lua
require('tsffi').setup()
```

## Test

```bash
nvim -u nvimrc.lua <C file>
# e.g. nvim -u nvimrc.lua vendor/api.h
```
