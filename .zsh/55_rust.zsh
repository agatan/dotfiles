if exists rustc; then
  export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/src
fi

if exists cargo; then
  source $HOME/.cargo/env
fi
