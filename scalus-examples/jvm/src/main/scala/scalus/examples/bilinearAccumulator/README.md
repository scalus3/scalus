# Bilinear Accumulator (Allowlist)

Membership proof using BLS12-381 bilinear pairings. A G2 accumulator encodes the entire allowlist; membership is proven with a G1 element verified via a pairing equation, without revealing or iterating the full set.

The validator checks the proof and requires the proven public key hash to be a transaction signatory.

## Files

| File                       | Purpose                              |
|----------------------------|--------------------------------------|
| `AllowlistValidator.scala` | On-chain validator with BLS12-381    |
| `AllowlistCRS.scala`       | Hardcoded powers of tau for test CRS |
