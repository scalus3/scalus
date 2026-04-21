# HTLC (Hash Time-Locked Contract)

The committer deposits cryptocurrency and commits to the SHA3-256 hash of a secret. The receiver can claim by revealing
the preimage before the deadline. After the deadline, the committer reclaims the deposit.

Used as a building block for cross-chain atomic swaps.

## On-chain state

```
Config (validator parameter)
├── committer : PubKeyHash
├── receiver  : PubKeyHash
├── image     : ByteString    -- SHA3-256 hash
└── timeout   : PosixTime
```

## Actions

| Action    | When           | Effect                           |
|-----------|----------------|----------------------------------|
| `Reveal`  | Before timeout | Receiver proves preimage, claims |
| `Timeout` | After timeout  | Committer reclaims               |

## Files

| File                     | Purpose                        |
|--------------------------|--------------------------------|
| `HtlcValidator.scala`    | On-chain spending validator    |
| `HtlcContract.scala`     | PlutusV3 compilation           |
| `HtlcTransactions.scala` | Off-chain transaction builders |
