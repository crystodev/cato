# Cato

A set of tools written in Haskell to interact with native tokens on Cardano blockchain

These tools are intended to play with the new native tokens of Cardano chain and help to understand how the system works by playing with it.

If you are interested in developing on native tokens, have a look at [these web pages](https://developers.cardano.org/en/development-environments/native-tokens/native-tokens/).

## Requirements

- a running local Cardano Node from Mary Era
- configure a ".env" file in your main folder (based on env.example)
- an address with some Ada
  - for Testnet, you can get some Test Ada (tAda) from this Faucet

## Utilities provided

- *address-infos* to get balance and utxo from address
- *burn-token* to burn previously minted token asset
- *create-address* to build a set of keys and addresses
- *create-policy* to create a new currency policy
- *mint-token* to mint new tokens asset according to a policy
- *policy-info* to get policy details and associated tokens (if created with *mint-token*)
- *send-ada* to transfer Ada between addresses
- *send-token* to transfer token assets and Ada between addresses

## Running utilities

Build with

```bash
  stack build
```

and run your program with

```bash
  stack exec <utility> -- <params>
```

Alternatively you can install your application with

```bash
  stack install
```

and run the utility from the command line.
