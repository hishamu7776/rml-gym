import gym
import rmlgym
from functools import partial

if __name__ == "__main__":
    import numpy as np
    
    config_path = './examples/pendulum_keep_up.yaml'
    env = partial(rmlgym.make, config_path)
    num_evals = 100
    max_ep_len = 200
    render = False  # True

    ep_returns = []
    ep_lengths = []

    for ep in range(num_evals):
        env.reset()
        ep_return = 0
        ep_len = 0
        for i in range(max_ep_len):
            if render:
                env.render()
                # time.sleep(1e-3)
            th, thdot = env.state
            u = np.array([((-32.0 / np.pi) * th) + ((-1.0 / np.pi) * thdot)])
            _, r, done, info = env.step(u)
            ep_return += r
            ep_len += 1
            if done and i < (max_ep_len - 1):
                print(f"Failed: {env.state[0]} > {np.pi / 3.}; step: {i}")
                break
        ep_returns.append(ep_return)
        ep_lengths.append(ep_len)
    
    # Compute the averages and print them
    ep_rets = np.array(ep_returns)
    ep_lens = np.array(ep_lengths)
    print(f'Avg Return: {np.mean(ep_rets)} +- {np.std(ep_rets)}, Avg Length: {np.mean(ep_lens)}')