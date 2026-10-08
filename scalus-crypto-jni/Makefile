# Makefile for scalus-crypto-jni: one JNI library statically linking libsodium, libsecp256k1
# and blst at the commits cardano-node links. The *_HOME variables come from `nix develop ..#ci-crypto`
# (host) or `..#ci-crypto-windows` (the `windows` target).

.PHONY: all clean info windows

UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Linux: hide the symbols of the static archives (only the JNI entry points stay exported) and bind
# our own calls inside the library, so they cannot resolve to another libsodium in the process.
# macOS: the oldest supported release (README "Platforms"); flake.nix builds the 3 archives for it too.
ifeq ($(UNAME_S),Linux)
    LIB_EXT := so
    SHARED_FLAG := -shared
    LDFLAGS := -Wl,--exclude-libs,ALL -Wl,-Bsymbolic
    ifeq ($(UNAME_M),x86_64)
        PLATFORM := linux_64
    else ifeq ($(UNAME_M),aarch64)
        PLATFORM := linux_arm64
    endif
else ifeq ($(UNAME_S),Darwin)
    LIB_EXT := dylib
    SHARED_FLAG := -dynamiclib
    ifeq ($(UNAME_M),x86_64)
        PLATFORM := osx_64
        MACOS_MIN := 10.15
    else ifeq ($(UNAME_M),arm64)
        PLATFORM := osx_arm64
        MACOS_MIN := 11.0
    endif
    LDFLAGS := -install_name @rpath/libscalus_crypto.dylib
endif

LIB_NAME := scalus_crypto
OUTPUT_LIB := lib$(LIB_NAME).$(LIB_EXT)
RESOURCES_DIR := src/main/resources/natives/$(PLATFORM)
SOURCES := $(wildcard native/*.c)
HEADERS := $(wildcard native/*.h)

CC := clang
CFLAGS := -O2 -Wall -fPIC
ifneq ($(MACOS_MIN),)
    CFLAGS += -mmacosx-version-min=$(MACOS_MIN)
endif

JAVA_HOME ?= $(shell java -XshowSettings:properties -version 2>&1 | grep 'java.home' | awk '{print $$3}')
JNI_INCLUDE := -I"$(JAVA_HOME)/include"
ifeq ($(UNAME_S),Linux)
    JNI_INCLUDE += -I"$(JAVA_HOME)/include/linux"
else ifeq ($(UNAME_S),Darwin)
    JNI_INCLUDE += -I"$(JAVA_HOME)/include/darwin"
endif

CRYPTO_CFLAGS := -I$(SODIUM_HOME)/include -I$(SECP256K1_HOME)/include -I$(BLST_HOME)/include
CRYPTO_LIBS := $(SODIUM_HOME)/lib/libsodium.a $(SECP256K1_HOME)/lib/libsecp256k1.a $(BLST_HOME)/lib/libblst.a

# Building needs the 3 libraries and (for the host target) a known platform. `clean` and `info` do not.
ifneq ($(filter-out clean info,$(or $(MAKECMDGOALS),all)),)
    ifeq ($(SODIUM_HOME),)
        $(error SODIUM_HOME is not set: run inside `nix develop ..#ci-crypto`)
    endif
    ifeq ($(SECP256K1_HOME),)
        $(error SECP256K1_HOME is not set: run inside `nix develop ..#ci-crypto`)
    endif
    ifeq ($(BLST_HOME),)
        $(error BLST_HOME is not set: run inside `nix develop ..#ci-crypto`)
    endif
endif
ifneq ($(filter all,$(or $(MAKECMDGOALS),all)),)
    ifeq ($(PLATFORM),)
        $(error unsupported host platform: $(UNAME_S) $(UNAME_M))
    endif
endif

all: $(RESOURCES_DIR)/$(OUTPUT_LIB)

$(RESOURCES_DIR)/$(OUTPUT_LIB): $(SOURCES) $(HEADERS) $(CRYPTO_LIBS)
	@mkdir -p $(RESOURCES_DIR)
	$(CC) $(CFLAGS) $(JNI_INCLUDE) $(CRYPTO_CFLAGS) $(SHARED_FLAG) $(LDFLAGS) -o $@ $(SOURCES) $(CRYPTO_LIBS)
	@echo "Built $(OUTPUT_LIB) for $(PLATFORM)"

# Windows x64, cross-compiled with MinGW. -ladvapi32: libsodium pulls in RtlGenRandom with
# #pragma comment(lib, "advapi32.lib"), which MinGW ignores.
WIN_DIR := src/main/resources/natives/windows_64

windows: $(WIN_DIR)/$(LIB_NAME).dll

$(WIN_DIR)/$(LIB_NAME).dll: $(SOURCES) $(HEADERS) $(CRYPTO_LIBS)
	@mkdir -p $(WIN_DIR) build/win32-include
	cp "$(JNI_MD_WIN32)" build/win32-include/jni_md.h
	x86_64-w64-mingw32-gcc -O2 -Wall -shared -static -static-libgcc -DSODIUM_STATIC \
		-I"$(JAVA_HOME)/include" -Ibuild/win32-include $(CRYPTO_CFLAGS) \
		-o $@ $(SOURCES) $(CRYPTO_LIBS) -ladvapi32
	@echo "Built $(LIB_NAME).dll for windows_64"

clean:
	rm -rf build src/main/resources/natives/*/lib$(LIB_NAME).* $(WIN_DIR)/$(LIB_NAME).dll

info:
	@echo "Platform: $(PLATFORM)"
	@echo "JAVA_HOME: $(JAVA_HOME)"
	@echo "SODIUM_HOME: $(SODIUM_HOME)"
	@echo "SECP256K1_HOME: $(SECP256K1_HOME)"
	@echo "BLST_HOME: $(BLST_HOME)"
