# tsffi.nvim

Very experimental plugin that aims to replace Neovim's treesitter interface with FFI bindings.

Why? The regular Lua-C API can be quite slow due to contexts switches. LuaJIT's FFI is fast.

## Usage

```bash
make .deps/tree-sitter
nvim -u nvimrc.lua <C file>
# e.g. nvim -u nvimrc.lua .deps/tree-sitter/lib/include/tree_sitter/api.h
```

### Status

Seems to work.
Can open buffer and it highlights, however it crashes when scrolling.

Example crash report:

```
Exception Type:        EXC_BAD_ACCESS (SIGSEGV)
Exception Codes:       KERN_INVALID_ADDRESS at 0x0000000102c8c004
Exception Codes:       0x0000000000000001, 0x0000000102c8c004

Termination Reason:    Namespace SIGNAL, Code 11 Segmentation fault: 11
Terminating Process:   exc handler [86440]

VM Region Info: 0x102c8c004 is not in any region.  Bytes after previous region: 638981  Bytes before following region: 49148
      REGION TYPE                    START - END         [ VSIZE] PRT/MAX SHRMOD  REGION DETAIL
      VM_ALLOCATE                 102bd0000-102bf0000    [  128K] rw-/rwx SM=PRV
--->  GAP OF 0xa8000 BYTES
      VM_ALLOCATE                 102c98000-102cb8000    [  128K] rw-/rwx SM=PRV

Thread 0 Crashed::  Dispatch queue: com.apple.main-thread
0   nvim                          	       0x1024b28e0 ts_language_symbol_count + 12 (language.c:20)
1   nvim                          	       0x1024b31d4 ts_language_symbol_name + 108 (language.c:176)
2   nvim                          	       0x1024b597c ts_node_type + 112 (node.c:466)
3   nvim                          	       0x1024eabb8 lj_vm_ffi_call + 100
4   nvim                          	       0x10253a88c lj_ccall_func + 1536 (lj_ccall.c:1195)
5   nvim                          	       0x1024ff5b0 lua_pcall + 232 (lj_api.c:1151)
6   nvim                          	       0x10223a0bc nlua_call_ref_ctx + 324 (executor.c:1596)
7   nvim                          	       0x102239f4c nlua_call_ref + 112 (executor.c:1573)
8   nvim                          	       0x1020dde6c decor_provider_invoke + 144 (decoration_provider.c:48)
9   nvim                          	       0x1020de898 decor_providers_invoke_line + 412 (decoration_provider.c:183)
10  nvim                          	       0x1020ed20c win_line + 1164 (drawline.c:1168)
11  nvim                          	       0x1020fc788 win_update + 11636 (drawscreen.c:2295)
12  nvim                          	       0x1020f8ea0 update_screen + 2944 (drawscreen.c:653)
13  nvim                          	       0x1022c3cc8 normal_redraw + 84 (normal.c:1370)
14  nvim                          	       0x1022c35b0 normal_check + 448 (normal.c:1468)
15  nvim                          	       0x1023bbe98 state_enter + 52 (state.c:40)
16  nvim                          	       0x1022b2abc normal_enter + 180 (normal.c:523)
17  nvim                          	       0x102248970 main + 4324 (main.c:654)
18  dyld                          	       0x18d12eb4c start + 6000
```
