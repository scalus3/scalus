# Makefile for scalus-secp256k1-jni native library
# Builds JNI bindings for secp256k1 v0.6.0

# Default target
.PHONY: all clean

# Detect OS and architecture
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Determine platform directory
ifeq ($(UNAME_S),Linux)
    ifeq ($(UNAME_M),x86_64)
        PLATFORM := linux_64
        LIB_EXT := so
        LIB_PREFIX := lib
        SHARED_FLAG := -shared
        PLATFORM_FLAGS := -fPIC
    else ifeq ($(UNAME_M),aarch64)
        PLATFORM := linux_arm64
        LIB_EXT := so
        LIB_PREFIX := lib
        SHARED_FLAG := -shared
        PLATFORM_FLAGS := -fPIC
    endif
else ifeq ($(UNAME_S),Darwin)
    ifeq ($(UNAME_M),x86_64)
        PLATFORM := osx_64
        LIB_EXT := dylib
        LIB_PREFIX := lib
        SHARED_FLAG := -dynamiclib
        PLATFORM_FLAGS := -fPIC
    else ifeq ($(UNAME_M),arm64)
        PLATFORM := osx_arm64
        LIB_EXT := dylib
        LIB_PREFIX := lib
        SHARED_FLAG := -dynamiclib
        PLATFORM_FLAGS := -fPIC
    endif
endif

# Library name
LIB_NAME := scalus_secp256k1
OUTPUT_LIB := $(LIB_PREFIX)$(LIB_NAME).$(LIB_EXT)

# Directories
NATIVE_DIR := native
RESOURCES_DIR := src/main/resources/natives/$(PLATFORM)

# Compiler settings - use clang on all platforms
CC := clang
CFLAGS := -O2 -Wall $(PLATFORM_FLAGS)

# JNI includes
JAVA_HOME ?= $(shell java -XshowSettings:properties -version 2>&1 | grep 'java.home' | awk '{print $$3}')
JNI_INCLUDE := -I"$(JAVA_HOME)/include"
ifeq ($(UNAME_S),Linux)
    JNI_INCLUDE += -I"$(JAVA_HOME)/include/linux"
else ifeq ($(UNAME_S),Darwin)
    JNI_INCLUDE += -I"$(JAVA_HOME)/include/darwin"
endif

# secp256k1 library - use SECP256K1_HOME from nix environment
# Static linking: link directly to .a file for self-contained JAR (like bitcoin-s does)
SECP256K1_CFLAGS := -I$(SECP256K1_HOME)/include
SECP256K1_LIBS := $(SECP256K1_HOME)/lib/libsecp256k1.a

# Source files
SOURCES := $(NATIVE_DIR)/scalus_secp256k1.c

all: $(RESOURCES_DIR)/$(OUTPUT_LIB)

$(RESOURCES_DIR)/$(OUTPUT_LIB): $(SOURCES)
	@mkdir -p $(RESOURCES_DIR)
	$(CC) $(CFLAGS) $(JNI_INCLUDE) $(SECP256K1_CFLAGS) $(SHARED_FLAG) -o $@ $< $(SECP256K1_LIBS)
	@echo "Built $(OUTPUT_LIB) for $(PLATFORM)"

clean:
	rm -rf src/main/resources/natives/*/$(LIB_PREFIX)$(LIB_NAME).*

# Show detected configuration
info:
	@echo "Platform: $(PLATFORM)"
	@echo "Library: $(OUTPUT_LIB)"
	@echo "JAVA_HOME: $(JAVA_HOME)"
	@echo "SECP256K1_HOME: $(SECP256K1_HOME)"
	@echo "JNI_INCLUDE: $(JNI_INCLUDE)"
	@echo "SECP256K1_CFLAGS: $(SECP256K1_CFLAGS)"
	@echo "SECP256K1_LIBS: $(SECP256K1_LIBS)"
