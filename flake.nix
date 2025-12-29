{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:IntersectMBO/plutus/1.56.0.0";
    # cardano-node-flake.url = "github:input-output-hk/cardano-node/9.1.1";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , plutus
    # , cardano-node-flake
    , ...
    } @ inputs:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
                inherit system;
                config = {
                  # Explicitly set WebKitGTK ABI version to avoid evaluation warning
                  # WebKitGTK has multiple ABI versions (4.0, 4.1, 6.0) and Nix requires explicit selection
                  webkitgtk.abi = "4.1";
                };
        };
        uplc = plutus.packages.${system}.uplc;

        # secp256k1 with static library and required modules for JNI builds
        secp256k1Static = pkgs.secp256k1.overrideAttrs (old: {
          dontDisableStatic = true;
          configureFlags = (old.configureFlags or []) ++ [
            "--enable-experimental"
            "--enable-module-schnorrsig"
            "--enable-module-extrakeys"
            "--enable-module-ecdh"
          ];
        });

        # cardano-cli = cardano-node-flake.packages.${system}.cardano-cli;
      in
      {
        devShells = {
          default =
            let
              jdk = pkgs.openjdk25;
              graalvm = pkgs.graalvmPackages.graalvm-ce;
              metals = pkgs.metals.override { jre = graalvm; };
              bloop = pkgs.bloop.override { jre = graalvm; };
              sbt = pkgs.sbt.override { jre = jdk; };
              visualvm = pkgs.visualvm.override { jdk = jdk; };

              # Common JVM options for both app and sbt JVM
              commonJvmOpts = [
                # Memory settings - use percentage of physical RAM for portability
                "-XX:InitialRAMPercentage=25.0"     # Initial heap: 25% of physical RAM
                "-XX:MaxRAMPercentage=75.0"         # Max heap: 75% of physical RAM
                "-Xss64m"                           # Stack size for deep recursive calls in compiler

                # Enable native access for BLST JNI library (required for Java 22+)
                "--enable-native-access=ALL-UNNAMED"

                # Enable experimental features for Java 23
                "-XX:+UnlockExperimentalVMOptions"  # Allow use of experimental VM options

                # Garbage Collection - ZGC for ultra-low latency
#                "-XX:+UseZGC"                       # Use Z Garbage Collector (concurrent, low-latency)
                "-XX:+UseG1GC"                       # Use G1 Garbage Collector (stable, good for large heaps)

                # Memory optimizations
                "-XX:+UseStringDeduplication"       # Deduplicate identical strings to save memory
                "-XX:+OptimizeStringConcat"         # Optimize string concatenation operations

                # Code cache settings for better JIT performance
                "-XX:ReservedCodeCacheSize=512m"    # Reserve more space for compiled native code
                "-XX:InitialCodeCacheSize=64m"      # Start with larger initial code cache

                # Compilation settings
                "-XX:+TieredCompilation"            # Use tiered compilation (C1 + C2 compilers)

                # Memory efficiency
                "-XX:+UseCompressedOops"            # Use 32-bit pointers on 64-bit JVM (saves memory)

                # Java 23 preview features
                "--enable-preview"                  # Enable preview language features
              ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
                # Linux-specific optimizations (not available on macOS)
                "-XX:+UseTransparentHugePages"      # Use OS huge pages for better memory performance
              ];

              # App-specific JVM options (runtime performance focused)
              appJvmOpts = commonJvmOpts ++ [
                # JIT compiler optimizations for better runtime performance
                "-XX:MaxInlineLevel=15"             # Allow deeper method inlining (Scala benefits from this)
                "-XX:MaxInlineSize=270"             # Allow larger methods to be inlined
                "-XX:CompileThreshold=1000"         # Compile methods to native code after 1000 invocations
              ];

              # SBT-specific JVM options (optimized for long-running sbtn server)
              sbtJvmOpts = commonJvmOpts ++ [
                # NOTE: Do NOT use -XX:TieredStopAtLevel=1 here!
                # While it speeds up initial startup, it prevents C2 JIT optimization
                # which makes subsequent compilations 30-50% slower in long-running sbtn server

                # JIT settings optimized for compilation workloads
                "-XX:CompileThreshold=1000"         # Compile hot methods after 1000 invocations
                "-XX:+AlwaysPreTouch"               # Pre-touch heap pages to avoid GC pauses during compilation

                # SBT-specific optimizations
                "-Dsbt.boot.lock=false"             # Disable boot lock file (faster concurrent sbt instances)
                "-Dsbt.turbo=true"                  # Enable turbo mode for faster task execution
                "-Dsbt.supershell=false"            # Disable supershell for cleaner output and slight speedup
              ];
            in
            pkgs.mkShell {
              JAVA_HOME = "${jdk}";
              JAVA_OPTS = builtins.concatStringsSep " " appJvmOpts;
              SBT_OPTS = builtins.concatStringsSep " " sbtJvmOpts;
              # Fixes issues with Node.js 20+ and OpenSSL 3 during webpack build
              NODE_OPTIONS="--openssl-legacy-provider";
              # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
              buildInputs = [ pkgs.bashInteractive ];
              packages = with pkgs; [
                git
                jdk
                sbt
                mill
                metals
                scalafmt
                scalafix
                coursier
                bloop
                niv
                nixpkgs-fmt
                nodejs
                yarn
                uplc
                async-profiler
                visualvm
                llvm
                clang
                libsodium
                secp256k1
                blst
                pandoc
                texliveSmall
                # cardano-cli
              ];
              shellHook = ''
                unlink plutus-conformance 2>/dev/null || true
                ln -s ${plutus}/plutus-conformance plutus-conformance
                echo "${pkgs.secp256k1}"
                echo "${pkgs.libsodium}"
                echo "${pkgs.async-profiler}"
                # IMPORTANT: blst must NOT be in DYLD_LIBRARY_PATH/LD_LIBRARY_PATH
                # Root cause: blst-java (JVM) bundles its own JNI native library with SWIG-generated
                # symbols (e.g., new_P1__SWIG_2). The nix blst package is the raw C library without
                # these JNI symbols. On macOS, JNI uses dlsym(RTLD_DEFAULT) which searches ALL loaded
                # libraries for symbols. If nix blst is in the library path, its symbols conflict with
                # blst-java's, causing UnsatisfiedLinkError for SWIG JNI functions.
                # Solution: Keep blst in LIBRARY_PATH only (for Scala Native compile-time linking).
                export DYLD_LIBRARY_PATH="${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$DYLD_LIBRARY_PATH"
                export LIBRARY_PATH="${pkgs.blst}/lib:${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LIBRARY_PATH"
                export LD_LIBRARY_PATH="${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LD_LIBRARY_PATH"
                # For Scala Native tests, provide blst path separately (used by build.sbt)
                export BLST_NATIVE_LIB_PATH="${pkgs.blst}/lib"
              '';
            };
          ci =
            let
              jdk = pkgs.openjdk11;
              sbt = pkgs.sbt.override { jre = jdk; };

              # Common JVM options for CI environment (Java 11 - more conservative settings)
              ciCommonJvmOpts = [
                # Memory settings - use percentage of physical RAM for portability
                "-XX:InitialRAMPercentage=25.0"     # Initial heap: 25% of physical RAM
                "-XX:MaxRAMPercentage=75.0"         # Max heap: 75% of physical RAM
                "-Xss64m"                           # Stack size for deep recursive calls in compiler

                # Garbage Collection - G1GC for Java 11 stability
                "-XX:+UseG1GC"                      # Use G1 Garbage Collector (stable, good for large heaps)

                # Memory optimizations (Java 11 compatible)
                "-XX:+UseStringDeduplication"       # Deduplicate identical strings to save memory

                # Code cache settings - enabled for better JIT performance
                "-XX:ReservedCodeCacheSize=512m"    # Reserve space for compiled native code
                "-XX:InitialCodeCacheSize=64m"      # Start with larger initial code cache

                # Compilation settings
                "-XX:+TieredCompilation"            # Use tiered compilation (C1 + C2 compilers)

                # Memory efficiency
                "-XX:+UseCompressedOops"            # Use 32-bit pointers on 64-bit JVM (saves memory)
              ];

              # CI SBT-specific options (prioritize build speed for single-run builds)
              ciSbtJvmOpts = ciCommonJvmOpts ++ [
                # For CI single-run builds, TieredStopAtLevel=1 is acceptable since there's
                # no warm JVM benefit. For builds > 15 min, consider removing this flag.
#                "-XX:TieredStopAtLevel=1"           # Stop at C1 compiler (faster CI startup)
#                "-XX:CompileThreshold=1500"         # Higher threshold for native compilation

                # CI-specific optimizations
                "-Dsbt.boot.lock=false"             # Disable boot lock (faster in containerized CI)
                "-Dsbt.supershell=false"            # Disable supershell for cleaner CI logs
              ];
            in
            pkgs.mkShell {
              JAVA_HOME = "${jdk}";
              JAVA_OPTS = builtins.concatStringsSep " " ciCommonJvmOpts;
              SBT_OPTS = builtins.concatStringsSep " " ciSbtJvmOpts;
              # Fixes issues with Node.js 20+ and OpenSSL 3 during webpack build
              NODE_OPTIONS="--openssl-legacy-provider";
              # Fix locale warnings in CI
              LC_ALL = "C";
              LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
              packages = with pkgs; [
                jdk
                sbt
                nodejs
                uplc
                llvm
                libsodium
                secp256k1
                blst
              ];
              shellHook = ''
                unlink plutus-conformance 2>/dev/null || true
                ln -s ${plutus}/plutus-conformance plutus-conformance
                # IMPORTANT: blst must NOT be in LD_LIBRARY_PATH (same issue as macOS DYLD_LIBRARY_PATH)
                # See default shell comment for detailed explanation of blst-java JNI symbol conflict.
                export LIBRARY_PATH="${pkgs.blst}/lib:${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LIBRARY_PATH"
                export LD_LIBRARY_PATH="${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LD_LIBRARY_PATH"
                # For Scala Native tests, provide blst path separately (used by build.sbt)
                export BLST_NATIVE_LIB_PATH="${pkgs.blst}/lib"
              '';
            };
          ci-secp =
            let
              jdk = pkgs.openjdk11;
              sbt = pkgs.sbt.override { jre = jdk; };
            in
            pkgs.mkShell {
              JAVA_HOME = "${jdk}";
              SECP256K1_HOME = "${secp256k1Static}";
              packages = [
                jdk
                sbt
                pkgs.clang
                secp256k1Static
              ];
            };
        };
      })
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://iohk.cachix.org"
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    allow-import-from-derivation = true;
    experimental-features = [ "nix-command" "flakes" ];
    accept-flake-config = true;
  };
}
