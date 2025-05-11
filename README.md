# tsffi.nvim

Very experimental plugin that aims to replace Neovim's treesitter interface with FFI bindings.

Why? The regular Lua-C API can be quite slow due to contexts switches. LuaJIT's FFI is fast.
