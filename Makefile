c:
	cargo check
it:
	cargo build --release
	cp ./target/release/amla .

f:
	cargo fmt
	cargo clippy

tidy:
	rm -rf a.out amla target/
