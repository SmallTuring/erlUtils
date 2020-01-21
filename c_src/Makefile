AllNifDirs :=$(shell ls -l | grep ^d | awk '{print $$9}')

CurDir  := $(shell pwd)
MdPriv  := $(shell mkdir -p $(CurDir)/../priv/)

ECHO:
	@echo $(AllNifDirs)

all:$(AllNifDirs)

$(AllNifDirs):ECHO
	@make -C $@

clean:
	@for OneNifDir in $(AllNifDirs); do make -C $$OneNifDir clean; done
