# Plinth Template

A minimal Plutus V3 smart contract template for Cardano development using [Plinth](https://github.com/IntersectMBO/plinth-template).

## Use This Template

### Option 1: GitHub Template (Recommended)

Click **"Use this template"** button on GitHub, or:

```bash
gh repo create my-contract --template VINIT-INAMKE/plinth-starter --public --clone
cd my-contract
```

### Option 2: Clone Directly

```bash
git clone https://github.com/VINIT-INAMKE/plinth-starter.git my-contract
cd my-contract
rm -rf .git
git init
git add .
git commit -m "Initial commit from plinth-starter template"
```

---

## Customize for Your Project

After cloning, rename the project:

1. **Rename cabal file:**
   ```bash
   mv smartcontracts.cabal my-project.cabal
   ```

2. **Update package name** in `my-project.cabal`:
   ```
   name:            my-project
   description:     My Cardano smart contract
   ```

3. **Update flake description** in `flake.nix`:
   ```nix
   description = "My Cardano Smart Contract";
   ```

4. **Update blueprint metadata** in `app/GenBlueprint.hs`:
   ```haskell
   contractId = Just "my-project"
   preambleTitle = "My Smart Contract"
   ```

---
## Nix Setup

### 1. Install Nix (Multi-user)

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

### 2. Enable Flakes

Add to `/etc/nix/nix.conf` (create if doesn't exist):

```
experimental-features = nix-command flakes
```

### 3. Add Yourself as Trusted User

Append to `/etc/nix/nix.conf`:

```bash
# Replace 'yourusername' with output of `whoami`
trusted-users = yourusername root
```

### 4. Configure IOG Binary Cache

Append to `/etc/nix/nix.conf`:

```
extra-substituters = https://cache.iog.io
extra-trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

### 5. Restart Nix Daemon

```bash
sudo systemctl restart nix-daemon.service
```

### 6. Verify Setup

```bash
nix develop
# Accept flake config prompts with 'y'
```

> **Note**: First run will download dependencies. If you see `"warning: ignoring untrusted substituter"`, your trusted-users config isn't working. Restart the daemon and try again.

---

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

## Based On

This template is based on [IntersectMBO/plinth-template](https://github.com/IntersectMBO/plinth-template).

## License

Apache-2.0
