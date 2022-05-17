FROM rust:latest as builder
RUN rustup toolchain install nightly
# WORKDIR /zstd-rs
COPY . /redscript/
WORKDIR /redscript
RUN cargo +nightly build

FROM debian:bullseye-slim
COPY --from=builder /redscript/target/debug .