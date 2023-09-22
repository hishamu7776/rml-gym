# rml-gym
RMLGym is a tool for modifying reinforcement learning environments to incorporate RML specifications in reward function. RMLGym works by augmenting OpenAI's gym environment and uses RML specification to define reward functions. RMLGym aims to bridge the gap between RL and runtime verification, a technique that monitors and verifies the behavior of a system at runtime. The framework uses Runtime Monitoring Language (RML), a runtime verification tool that allows more complex properties to be defined and verified, as reward constructors. The reward constructors generate rewards based on the satisfaction or violation of the specifications, rather than a predefined reward function.

## CONFIG setup
To create an RMLgym environment, you need to provide a configuration file that specifies the details RML properties you want to enforce. You can pass the path to this file as an argument when you initialize the environment like this:
 
```Python
import gym
from functools import partial
import rmlgym

# For a single environment object
env = rmlgym.make('config_path/config.yaml', env=None)

# For the function version required by some algorithms in order to run simulations in parallel
env_fn = partial(rmlgym.make, 'config_path/config.yaml')
```

You can either use an existing environment or specify a new one in the configuration file that defines the RML properties for the RMLgym environment. The configuration file has several sections that explain the RML specifications and other settings. You can see an example of a configuration file below. The sections can be in any order.

### ```env_name:```
If an environment object is not provided upon initialization, the registered name of the environment must be specified in the config file. For more information on registering a custom environment, we recommend reading the [OpenAI Gym](https://github.com/openai/gym) README.md file for more information.

### ```host:```

### ```port:```

### ```variables:```
In this section of the configuration file, users define the environment variables that need to be recorded in order to evaluate the specifications. The recorded variable values will make up the multi-variate signal used in the robustness degree calculation. To define a variable, users must specify a `name`, the data `type` (`bool`, `int`, or `float`), `location`, and `identifier`. The name must match the name used in the specification. The location specifies how to access the variable in the environment. The location can be one of the following 3 options: 
1. __obs__: This option pulls the data from the output `next_observation` from the environment' `.step()` function. Provided the observation is a vector (we do not support dictionary observations yet), the `identifier` is the integer index that identifies the variable within the vector.
2. __info__: This option pulls the data from the `info` dictionary output by the `.step()` function. This is useful for accessing state data that isn't included in the observation, but could be crucial for defining _safe_ behavior. The `identifier` for this option is the dictionary key associated with the desired data. We do not support nested dictionaries at this time, but hope to include support in the future.
3. __state__: This option pulls from the environment's internal variables. The `identifier` is the variable name (e.g. `var_name`), which would record the value of `env.var_name` at each timestep. When possible, we recommend modifying the Gym environment so this information is included in the `info` output instead of accessing directly. 

### ```reward:```


## Example: Pendulum-v0
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

## Installation