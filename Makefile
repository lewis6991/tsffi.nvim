
TS = .deps/tree-sitter
TS_API = $(TS)/lib/include/tree_sitter/api.h

$(TS):
	git clone \
		--filter=blob:none \
		https://github.com/tree-sitter/tree-sitter.git \
		$@

TS_C = .deps/tree-sitter-c
TS_C_SO = $(TS_C)/libtree-sitter-c.dylib

$(TS_C):
	git clone \
		--filter=blob:none \
		https://github.com/tree-sitter/tree-sitter-c.git \
		$@

$(TS_C_SO): $(TS_C)
	make -C $(TS_C)

test: $(TS_C_SO) $(TS)
	nvim -l test.lua $(TS_API) $(TS_C_SO) c
