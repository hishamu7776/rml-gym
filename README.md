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
Host is the hostname or IP address of RML monitor.

### ```port:```
Port is the port of RML monitor.

### ```variables:```
In this section of the configuration file, users define the environment variables that need to be recorded in order to evaluate the specifications. The recorded variable values will make up the multi-variate signal used in the robustness degree calculation. To define a variable, users must specify a `name`, the data `type` (`bool`, `int`, or `float`), `location`, and `identifier`. The name must match the name used in the specification. The location specifies how to access the variable in the environment. The location can be one of the following 3 options: 
1. __obs__: This option pulls the data from the output `next_observation` from the environment' `.step()` function. Provided the observation is a vector (we do not support dictionary observations yet), the `identifier` is the integer index that identifies the variable within the vector.
2. __info__: This option pulls the data from the `info` dictionary output by the `.step()` function. This is useful for accessing state data that isn't included in the observation, but could be crucial for defining _safe_ behavior. The `identifier` for this option is the dictionary key associated with the desired data. We do not support nested dictionaries at this time, but hope to include support in the future.
3. __state__: This option pulls from the environment's internal variables. The `identifier` is the variable name (e.g. `var_name`), which would record the value of `env.var_name` at each timestep. When possible, we recommend modifying the Gym environment so this information is included in the `info` output instead of accessing directly. 

### ```reward:```
This section provides reward associated with the verdicts. It has `true`, `false`, `currently_true`, and `currently_false` as verdicts. A `name` has to specify for the specification.


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
### Install prerequisites for RML
We recommend following [Runtime Monitoring Language](https://rmlatdibris.github.io/implementation.html) website for installing the requirements for RML.
### Install RMLGym
You could follow the following commands.
```
git clone https://github.com/hishamu7776/rml-gym.git
cd rml-gym/
pip install -e .

```
## How to run
This section explains how to use rmlgym library to train your reinforcement learning agents. The environment can be trained using any library which supports OpenAI's gym environment. 
### RML Specification 
Firstly, you need to specify the specification in RML language. The following example is used to define the properties for Pendulum-v1 environment of gym library.
```js
good_theta1 matches {theta: x} with x <= 0.5;
good_theta2 matches {theta: x} with x >= -0.5;
any matches _;
Main = Good \/ Bad;
Good = (good_theta1 /\ good_theta2) (Main \/ empty);
Bad = any Main;
```
### Compile RML Specification to Prolog Specification
The RML Compiler will be available to download at github of [RML](https://github.com/RMLatDIBRIS/compiler).
Compiler is also availabe in compiler-rml folder.
```shell
java -jar rml-compiler.jar --input file.rml --output file.pl
```

### Run Prolog Monitor
Once you compiled the RML property, you need to run the interpret prolog specification and on monitor. The interpreter can be found in github page [Monitor](https://github.com/RMLatDIBRIS/monitor). 
Monitor is also availabe in monitor_rml folder.
Use the following command to run the monitor after downloading the monitor.
```shell
sh ./online_monitor.sh ./file.pl 8080
```
### Create Config File
Once you setup monitor, you could create the configuration file. Include the port number and host of monitor in the configuration file like shown above.

### Load the environment

You could load the environment as follows.
```Python
import gym
from functools import partial
import rmlgym

# Normal way to load environment
config_path = './examples/environment.yaml'
rml_env = rmlgym.make(config_path)

# Alternative method
rml_env = RMLGym(config_path)

# For the function version required by some algorithms in order to run simulations in parallel
env_fn = partial(rmlgym.make, config_path)

```
