# Bilinear Accumulator (Allowlist)

Membership proof using BLS12-381 bilinear pairings. A G2 accumulator encodes the entire allowlist; membership is proven
with a G1 element verified via a pairing equation, without revealing or iterating the full set.

## How it works

The validator is parameterized by the compressed accumulator (a `ByteString`). To spend a locked UTxO, the spender
provides a G2 membership proof as the redeemer. The validator uncompresses both the accumulator and the proof, takes
the first transaction signatory's public key hash as the claimed member, and verifies the pairing equation using a
hardcoded Common Reference String (powers of tau).

`AllowlistValidator.scala` contains the on-chain validator. The CRS in this example uses a known tau value and is
suitable only for testing; a real deployment would require a secure setup ceremony.
