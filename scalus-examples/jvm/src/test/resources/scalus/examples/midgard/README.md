# Midgard `da_bond_pool` Aiken baseline

`da_bond_pool.plutus.json` holds the `da_bond_pool.da_bond_pool.spend` entry of
the Midgard blueprint, with only the definitions it references. Mint, spend and
else share the same compiled code.

- Source: https://github.com/Anastasia-Labs/midgard, branch
  `colll78/canonical-v1-watcher-l1-source-checkpoint`, commit `17fdffd9b`
- Files: `onchain/aiken/validators/da-bond-pool.ak`,
  `onchain/aiken/lib/midgard/da-bond-pool.ak`
- Compiler: `aiken v1.1.23+8949565` (not the pinned fork `v1.1.23+5adf783`)
- Command: `aiken build -t silent`, default env
- Size: 4168 bytes, hash `fba9e869e1bdf4aa08d26abe599a5836958cd21a380cafacdf3d183d`
  (no parameters applied)
- Parameters, in application order: `init_ref`, `hub_oracle_policy_id`,
  `da_params_policy_id`, `parameters`
