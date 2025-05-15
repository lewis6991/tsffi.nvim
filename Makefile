.PHONY: test
test:
	nvim -l test.lua

.PHONY: bigcfile
bigcfile:
	wget https://raw.githubusercontent.com/torvalds/linux/master/drivers/gpu/drm/amd/include/asic_reg/dcn/dcn_3_2_0_sh_mask.h
