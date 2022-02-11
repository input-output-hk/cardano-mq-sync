# Cardano MQ Sync

* This is a proof-of-concept project to attempt to create a process that dumps all blocks and ledger events to rabbitmq
* This requires an already running rabbitmq on localhost at the moment

## Build and Run

```
nix build .#
result/bin/cardano-mq-sync -c /path/to/cardano-node-config.json -s /path/to/node.socket
```

## Development Environment

Development environment includes cabal, ghcid, haskell-language-server and hlint

```
nix develop
```
