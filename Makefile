check:
	cargo check

lint:
	cargo fmt
	cargo clippy

release:
	cargo build --release
	cp ./target/release/amla .
