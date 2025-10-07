# RMLGym

**RMLGym** is a tool for modifying reinforcement learning environments to incorporate RML specifications into the reward function. It works by augmenting OpenAI's Gym environments and using RML specifications to shape rewards.

---

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Configuration](#configuration)
- [Example: Pendulum-v0](#example-pendulum-v0)
- [RML Specification](#rml-specification)
- [Compiling and Running the RML Monitor](#compiling-and-running-the-rml-monitor)
- [Citation](#citation)
- [Resources and Links](#resources-and-links)

---

## Features

- Integrate RML (Runtime Monitoring Language) specifications in RL reward functions.
- Easy configuration through YAML files.
- Compatible with OpenAI Gym environments.
- Support for custom monitors and reward shaping.

---

## Installation

### 1. Install Prerequisites

- Follow the [Runtime Monitoring Language implementation guide](https://rmlatdibris.github.io/implementation.html) for RML requirements.

### 2. Install RMLGym

```bash
git clone https://github.com/hishamu7776/rml-gym.git
cd rml-gym/
pip install -e .
```

---

## Quick Start

```python
import gym
from functools import partial
import rmlgym

# Load environment with RML configuration
env = rmlgym.make('config_path/config.yaml', env=None)

# For parallel simulations
env_fn = partial(rmlgym.make, 'config_path/config.yaml')
```

---

## Configuration

Create a configuration YAML file specifying:

- `env_name:` Gym environment name (if not provided as an object)
- `host:` Hostname or IP of the RML monitor
- `port:` Port of the RML monitor
- `variables:` Variables to monitor (from obs, info, or state)
- `reward:` Reward mapping for RML verdicts

### Example Sections

```yaml
env_name: Pendulum-v0
host: 127.0.0.1
port: 8081
variables:
  - name: omega
    type: float
    location: obs
    identifier: 2
  - name: theta
    type: float
    location: obs
    identifier: 0
reward:
    name: task
    true: 1
    currently_true: 2
    currently_false: -1
    false: -1
```

---

## Example: Pendulum-v0

See above for a sample configuration for the Pendulum-v0 environment.

---

## RML Specification

Define your specifications in RML language. Example for `Pendulum-v1`:

```js
good_theta1 matches {theta: x} with x <= 0.5;
good_theta2 matches {theta: x} with x >= -0.5;
any matches _;
Main = Good \/ Bad;
Good = (good_theta1 /\ good_theta2) (Main \/ empty);
Bad = any Main;
```

---

## Compiling and Running the RML Monitor

1. **Compile RML Specification**
   - Download the compiler from [RML Compiler GitHub](https://github.com/RMLatDIBRIS/compiler)
   - Or use the included `compiler-rml` folder.

   ```bash
   java -jar rml-compiler.jar --input file.rml --output file.pl
   ```

2. **Run Prolog Monitor**
   - Download from [Monitor GitHub](https://github.com/RMLatDIBRIS/monitor)
   - Or use the included `monitor_rml` folder.

   ```bash
   sh ./online_monitor.sh ./file.pl 8080
   ```

---

## Citation

If you use RMLGym in your research, please cite:

**Paper:**  
["RMLGym: a Formal Reward Machine Framework for Reinforcement Learning"](https://ceur-ws.org/Vol-3579/paper1.pdf)

**BibTeX:**
```bibtex
@inproceedings{unniyankal2023rmlgym,
  title={RMLGym: a Formal Reward Machine Framework for Reinforcement Learning.},
  author={Unniyankal, Hisham and Belardinelli, Francesco and Ferrando, Angelo and Malvone, Vadim},
  year={2023}
}
```

---

## Resources and Links

- [Paper PDF](https://ceur-ws.org/Vol-3579/paper1.pdf)
- [RML Compiler](https://github.com/RMLatDIBRIS/compiler)
- [RML Monitor](https://github.com/RMLatDIBRIS/monitor)
- [RML Implementation Guide](https://rmlatdibris.github.io/implementation.html)
