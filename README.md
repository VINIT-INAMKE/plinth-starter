# Plinth Template

A minimal Plutus V3 smart contract template for Cardano development using [Plinth](https://github.com/IntersectMBO/plinth-template).

## Quick Start

```bash
# Enter development shell
nix develop

# Build contracts
make build

# Generate blueprint
make blueprint
```

Your blueprint will be at `blueprints/contract.json`.

---

## Project Structure

```
├── src/
│   └── Validator.hs      # Your validator (modify this!)
├── app/
│   └── GenBlueprint.hs   # Blueprint generator
├── blueprints/           # Generated output
├── nix/                  # Nix configuration
├── Makefile              # Build commands
└── smartcontracts.cabal  # Project config
```

---

## Workflow

### 1. Write Your Contract

Edit `src/Validator.hs`:

```haskell
-- Define your datum (the UTxO state)
data MyDatum = MyDatum
  { owner :: PubKeyHash
  }

-- Define your redeemer (actions)
data MyRedeemer = Unlock | Cancel

-- Write your validation logic
typedValidator :: ScriptContext -> Bool
typedValidator ctx = ...
```

### 2. Build & Generate Blueprint

```bash
make build
make blueprint
```

### 3. Use in Frontend (MeshJS)

```typescript
import blueprint from './contract.json';
import { resolvePlutusScriptAddress } from '@meshsdk/core';

// Get script from blueprint
const script = {
  code: blueprint.validators[0].compiledCode,
  version: 'V3',
};

// Derive script address (0 = testnet, 1 = mainnet)
const scriptAddress = resolvePlutusScriptAddress(script, 0);
```

---

## Adding Multiple Validators

1. Create new file: `src/MyMintingPolicy.hs`
2. Add to `smartcontracts.cabal`:
   ```
   exposed-modules:
     Validator
     MyMintingPolicy
   ```
3. Update `app/GenBlueprint.hs` to include it

---

## Commands

| Command | Description |
|---------|-------------|
| `make build` | Build all contracts |
| `make blueprint` | Generate blueprint JSON |
| `make clean` | Clean build artifacts |
| `make format` | Format Haskell code |
| `make lint` | Run HLint |
| `make shell` | Enter nix shell |

---

## Requirements

- [Nix](https://nixos.org/download.html) with flakes enabled
- See [IOG Nix Setup Guide](https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md)

---

## Based On

This template is based on [IntersectMBO/plinth-template](https://github.com/IntersectMBO/plinth-template).

## License

Apache-2.0
