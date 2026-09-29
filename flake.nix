{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    plutus.url = "github:IntersectMBO/plutus/1.63.0.0";
    # The C crypto libraries exactly as cardano-node 11.1.3 links them: its root iohkNix input
    # (iohk-nix 74bae4f8). Keep in step with CONTRIBUTING.md "Keeping crypto libraries in sync".
    sodium = { url = "github:input-output-hk/libsodium/dbb48cce5429cb6585c9034f002568964f1ce567"; flake = false; };
    secp256k1 = { url = "github:bitcoin-core/secp256k1/acf5c55ae6a94e5ca847e07def40427547876101"; flake = false; };
    blst = { url = "github:supranational/blst/6d960cd05d6fe2b5bc9ba161edf0c1a131b87c4c"; flake = false; };
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
        configureFlags = (old.configureFlags or [ ]) ++ [
          "--enable-experimental"
          "--enable-module-schnorrsig"
          "--enable-module-extrakeys"
          "--enable-module-ecdh"
        ];
      });

      # Mirrors iohk-nix overlays/crypto/*.nix at 74bae4f8. Only packaging differs: static
      # archives with position-independent code, linked into one JNI library.
      # On macOS the archives target the same oldest release as the JNI library (Makefile
      # MACOS_MIN), or the linker warns about objects built for a newer macOS. The stdenv preHook
      # exports MACOSX_DEPLOYMENT_TARGET=darwinMinVersion (14.0), and darwinMinVersionHook can only
      # raise it, so lower it in preConfigure, which runs after preHook. The cc-wrapper reads the
      # variable on every call.
      macosTarget = stdenv: pkgs.lib.optionalAttrs stdenv.hostPlatform.isDarwin {
        preConfigure = "export MACOSX_DEPLOYMENT_TARGET="
          + (if stdenv.hostPlatform.isAarch64 then "11.0" else "10.15");
      };
      nodeSodium = { stdenv, lib, autoreconfHook }: stdenv.mkDerivation ({
        pname = "libsodium-vrf";
        version = "1.0.18";
        src = inputs.sodium;
        nativeBuildInputs = [ autoreconfHook ];
        configureFlags = [ "--enable-static" "--disable-shared" "--with-pic" ]
          ++ lib.optional stdenv.hostPlatform.isMinGW "CFLAGS=-fno-stack-protector";
        enableParallelBuilding = true;
        doCheck = !stdenv.hostPlatform.isMinGW;
      } // macosTarget stdenv);
      nodeSecp256k1 = { stdenv, autoreconfHook }: stdenv.mkDerivation ({
        pname = "secp256k1";
        version = "0.3.2";
        src = inputs.secp256k1;
        nativeBuildInputs = [ autoreconfHook ];
        configureFlags = [
          "--enable-benchmark=no"
          "--enable-module-recovery"
          "--enable-static"
          "--disable-shared"
          "--with-pic"
        ];
        enableParallelBuilding = true;
        doCheck = !stdenv.hostPlatform.isMinGW;
      } // macosTarget stdenv);
      nodeBlst = { stdenv, lib }: stdenv.mkDerivation ({
        pname = "blst";
        version = "0.3.15";
        src = inputs.blst;
        buildPhase = ''
          actual=$(grep -m1 -E '^version\s*=' bindings/rust/Cargo.toml | cut -d'"' -f2)
          [ "$actual" = "0.3.15" ] || { echo "blst version mismatch: $actual"; exit 1; }
          ./build.sh -D__BLST_PORTABLE__ -fPIC ${lib.optionalString stdenv.hostPlatform.isWindows "flavour=mingw64"}
        '';
        installPhase = ''
          mkdir -p $out/lib $out/include
          cp libblst.a $out/lib/
          cp bindings/blst.h bindings/blst_aux.h $out/include/
        '';
      } // macosTarget stdenv);
      mingw = pkgs.pkgsCross.mingwW64;
      cryptoHost = {
        sodium = pkgs.callPackage nodeSodium { };
        secp256k1 = pkgs.callPackage nodeSecp256k1 { };
        blst = pkgs.callPackage nodeBlst { };
      };
      cryptoWindows = {
        sodium = mingw.callPackage nodeSodium { };
        secp256k1 = mingw.callPackage nodeSecp256k1 { };
        blst = mingw.callPackage nodeBlst { };
      };
      # jni_md.h for win32: Linux JDKs ship only the linux variant; jni.h is platform-neutral.
      jniMdWin32 = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/openjdk/jdk11u/jdk-11.0.24+8/src/java.base/windows/native/include/jni_md.h";
        hash = "sha256-TAaWEHXV5FRIkrSQYehnTbhmPRdXy7Vlha5ePMXSteU=";
      };

      # Common JVM options for both app and sbt JVM
      commonDevJvmOpts = [
        # NO heap settings here. .jvmopts owns them, for sbt and for CI alike.
        # The sbt launcher puts SBT_OPTS after .jvmopts on the java command line,
        # so a heap flag here would silently override the committed one, and a
        # percentage of physical RAM oversubscribes a machine that runs one sbtn
        # server per worktree. Leaving SBT_OPTS free also makes it the place for
        # a personal override, via a gitignored .envrc.local.
        "-Xss64m" # Stack size for deep recursive calls in compiler

        # Enable native access for BLST JNI library (required for Java 22+)
        "--enable-native-access=ALL-UNNAMED"

        # Enable experimental features for Java 23
        "-XX:+UnlockExperimentalVMOptions" # Allow use of experimental VM options

        # Garbage Collection - ZGC for ultra-low latency
        #                "-XX:+UseZGC"                       # Use Z Garbage Collector (concurrent, low-latency)
        "-XX:+UseG1GC" # Use G1 Garbage Collector (stable, good for large heaps)

        # Memory optimizations
        "-XX:+UseStringDeduplication" # Deduplicate identical strings to save memory
        "-XX:+OptimizeStringConcat" # Optimize string concatenation operations

        # Code cache settings for better JIT performance
        "-XX:ReservedCodeCacheSize=512m" # Reserve more space for compiled native code
        "-XX:InitialCodeCacheSize=64m" # Start with larger initial code cache

        # Compilation settings
        "-XX:+TieredCompilation" # Use tiered compilation (C1 + C2 compilers)

        # Memory efficiency
        "-XX:+UseCompressedOops" # Use 32-bit pointers on 64-bit JVM (saves memory)

        # Java 23 preview features
        "--enable-preview" # Enable preview language features
      ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
        # Linux-specific optimizations (not available on macOS)
        "-XX:+UseTransparentHugePages" # Use OS huge pages for better memory performance
      ];

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

            # App-specific JVM options (runtime performance focused)
            appJvmOpts = commonDevJvmOpts ++ [
              # JIT compiler optimizations for better runtime performance
              "-XX:MaxInlineLevel=15" # Allow deeper method inlining (Scala benefits from this)
              "-XX:MaxInlineSize=270" # Allow larger methods to be inlined
              "-XX:CompileThreshold=1000" # Compile methods to native code after 1000 invocations
            ];

            # SBT-specific JVM options (optimized for long-running sbtn server)
            sbtJvmOpts = commonDevJvmOpts ++ [
              # NOTE: Do NOT use -XX:TieredStopAtLevel=1 here!
              # While it speeds up initial startup, it prevents C2 JIT optimization
              # which makes subsequent compilations 30-50% slower in long-running sbtn server

              # JIT settings optimized for compilation workloads
              "-XX:CompileThreshold=1000" # Compile hot methods after 1000 invocations

              # SBT-specific optimizations
              "-Dsbt.boot.lock=false" # Disable boot lock file (faster concurrent sbt instances)
              "-Dsbt.turbo=true" # Enable turbo mode for faster task execution
              "-Dsbt.supershell=false" # Disable supershell for cleaner output and slight speedup
            ];
          in
          pkgs.mkShell {
            JAVA_HOME = "${jdk}";
            JAVA_OPTS = builtins.concatStringsSep " " appJvmOpts;
            SBT_OPTS = builtins.concatStringsSep " " sbtJvmOpts;
            # Fixes issues with Node.js 20+ and OpenSSL 3 during webpack build
            NODE_OPTIONS = "--openssl-legacy-provider";
            # This fixes bash prompt/autocomplete issues with subshells (i.e. in VSCode) under `nix develop`/direnv
            buildInputs = [ pkgs.bashInteractive ];
            packages = with pkgs; [
              git
              gh
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
              # Lean 4 + Z3, as in the `lean` shell, so UplcBlasterTest can run its proofs
              # (after `lake build` in scalus-verification/src/main/lean).
              elan
              z3
              # cardano-cli
            ];
            shellHook = ''
              unlink plutus-conformance 2>/dev/null || true
              ln -s ${plutus}/plutus-conformance plutus-conformance
              echo "${pkgs.secp256k1}"
              echo "${pkgs.libsodium}"
              echo "${pkgs.async-profiler}"
              # These libraries are for Scala Native only: blst in LIBRARY_PATH (link time) and
              # BLST_NATIVE_LIB_PATH (test run time, see build.sbt). The JVM does not use them:
              # scalus-crypto-jni links its own static copies at cardano-node's pins.
              export DYLD_LIBRARY_PATH="${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$DYLD_LIBRARY_PATH"
              export LIBRARY_PATH="${pkgs.blst}/lib:${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LIBRARY_PATH"
              export LD_LIBRARY_PATH="${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LD_LIBRARY_PATH"
              # For Scala Native tests, provide blst path separately (used by build.sbt)
              export BLST_NATIVE_LIB_PATH="${pkgs.blst}/lib"
            '';
          };
        # Lean 4 + Z3 for the scalus-verification module. `elan` fetches the exact
        # toolchain named in scalus-verification/src/main/lean/lean-toolchain (v4.24.0).
        # Blaster documents Z3 4.15.2; nixpkgs 25.11 ships 4.15.4, which works.
        lean = pkgs.mkShell {
          buildInputs = [ pkgs.bashInteractive ];
          packages = with pkgs; [
            elan
            z3
            git
          ];
        };
        bench =
          let
            jdk = pkgs.openjdk25;
            sbt = pkgs.sbt.override { jre = jdk; };
            # Common JVM options for both app and sbt JVM

            # App-specific JVM options (runtime performance focused)
            appJvmOpts = commonDevJvmOpts ++ [
              # JIT compiler optimizations for better runtime performance
              "-XX:MaxInlineLevel=15" # Allow deeper method inlining (Scala benefits from this)
              "-XX:MaxInlineSize=270" # Allow larger methods to be inlined
              "-XX:CompileThreshold=1000" # Compile methods to native code after 1000 invocations
            ];

          in
          pkgs.mkShell {
            JAVA_HOME = "${jdk}";
            JAVA_OPTS = builtins.concatStringsSep " " appJvmOpts;
            buildInputs = [ pkgs.bashInteractive ];
            packages = with pkgs; [
              git
              jdk
              sbt
            ];
          };
        ci =
          let
            # JDK 21: Scala 3.8+ requires JDK 17+ to run the compiler (3.3 LTS still
            # supports JDK 8+). ci-jvm cross-builds on 3.8.4/3.9.0, so the CI shell must be >= 17.
            jdk = pkgs.openjdk21;
            sbt = pkgs.sbt.override { jre = jdk; };

            # Common JVM options for CI environment (Java 21 - more conservative settings)
            ciCommonJvmOpts = [
              # NO heap settings here either: .jvmopts caps the heap at 8g, for CI
              # and for developers alike. That leaves ~8GB for Node.js (Scala.js
              # tests), Nix and the OS on a 16GB GitHub Actions runner. Do not
              # restore a percentage: MaxRAMPercentage=75% gave a ~12GB heap and
              # the OOM killer SIGTERMed the runner on every CI-JS run.
              "-Xss64m" # Stack size for deep recursive calls in compiler

              # Garbage Collection - G1GC for stability
              "-XX:+UseG1GC" # Use G1 Garbage Collector (stable, good for large heaps)

              # Memory optimizations
              "-XX:+UseStringDeduplication" # Deduplicate identical strings to save memory

              # Code cache settings - enabled for better JIT performance
              "-XX:ReservedCodeCacheSize=512m" # Reserve space for compiled native code
              "-XX:InitialCodeCacheSize=64m" # Start with larger initial code cache

              # Compilation settings
              "-XX:+TieredCompilation" # Use tiered compilation (C1 + C2 compilers)

              # Memory efficiency
              "-XX:+UseCompressedOops" # Use 32-bit pointers on 64-bit JVM (saves memory)
            ];

            # CI SBT-specific options (prioritize build speed for single-run builds)
            ciSbtJvmOpts = ciCommonJvmOpts ++ [
              # For CI single-run builds, TieredStopAtLevel=1 is acceptable since there's
              # no warm JVM benefit. For builds > 15 min, consider removing this flag.
              #                "-XX:TieredStopAtLevel=1"           # Stop at C1 compiler (faster CI startup)
              #                "-XX:CompileThreshold=1500"         # Higher threshold for native compilation

              # CI-specific optimizations
              "-Dsbt.boot.lock=false" # Disable boot lock (faster in containerized CI)
              "-Dsbt.supershell=false" # Disable supershell for cleaner CI logs
            ];
          in
          pkgs.mkShell ({
            JAVA_HOME = "${jdk}";
            JAVA_OPTS = builtins.concatStringsSep " " ciCommonJvmOpts;
            SBT_OPTS = builtins.concatStringsSep " " ciSbtJvmOpts;
            # Fixes issues with Node.js 20+ and OpenSSL 3 during webpack build
            # Limit Node.js heap to avoid OOM when running Scala.js tests alongside JVM
            NODE_OPTIONS = "--openssl-legacy-provider --max-old-space-size=2048";
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
            ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.chromium ];
            shellHook = ''
              unlink plutus-conformance 2>/dev/null || true
              ln -s ${plutus}/plutus-conformance plutus-conformance
              # For Scala Native only; the JVM uses scalus-crypto-jni (see the default shell).
              export LIBRARY_PATH="${pkgs.blst}/lib:${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LIBRARY_PATH"
              export LD_LIBRARY_PATH="${pkgs.secp256k1}/lib:${pkgs.libsodium}/lib:$LD_LIBRARY_PATH"
              # For Scala Native tests, provide blst path separately (used by build.sbt)
              export BLST_NATIVE_LIB_PATH="${pkgs.blst}/lib"
            '';
          } // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
            # Use the browser pinned by flake.lock, independently of the host PATH.
            CHROME_BIN = "${pkgs.chromium}/bin/chromium";
          });
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
        ci-crypto =
          let
            jdk = pkgs.openjdk11;
            sbt = pkgs.sbt.override { jre = jdk; };
          in
          pkgs.mkShell {
            JAVA_HOME = "${jdk}";
            SODIUM_HOME = "${cryptoHost.sodium}";
            SECP256K1_HOME = "${cryptoHost.secp256k1}";
            BLST_HOME = "${cryptoHost.blst}";
            # Clear JVM options inherited from the main devshell: JDK 11 rejects some of them.
            JAVA_OPTS = "";
            SBT_OPTS = "";
            packages = [ jdk sbt pkgs.clang ];
          };
        ci-crypto-windows =
          let
            jdk = pkgs.openjdk11;
          in
          pkgs.mkShell {
            JAVA_HOME = "${jdk}";
            SODIUM_HOME = "${cryptoWindows.sodium}";
            SECP256K1_HOME = "${cryptoWindows.secp256k1}";
            BLST_HOME = "${cryptoWindows.blst}";
            JNI_MD_WIN32 = "${jniMdWin32}";
            packages = [ jdk mingw.stdenv.cc mingw.buildPackages.binutils ];
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
