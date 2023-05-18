import gym
from rmlgym.rmlgym import RMLGym
from stable_baselines3 import PPO

config_path = './examples/pendulum_keep_up.yaml'

# Use try-except block to catch any error or exception
# Create gym environment
env = RMLGym(config_path)


#env = gym.make('Pendulum-v1',  render_mode="rgb_array")
# Create PPO instance with verbose=1
model = PPO("MlpPolicy", env, verbose=1)
print("Model Created...")
# Train the model
model.learn(total_timesteps=10000)
print("Model learned...")
# Save the model
model.save("ppo_pendulum")
print("Model saved...")
# Load the model
model = PPO.load("ppo_pendulum")
print("Model loaded...")
# Use the model to predict actions
obs = env.reset()[0]
print("reset env...")
env.close()
'''
while True:
  action, _states = model.predict(obs)
  obs, reward, done,truncated, info = env.step(action)  
  env.render()
  if done:
    obs = env.reset()[0]
'''
