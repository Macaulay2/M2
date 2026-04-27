import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(projection='3d')
t = np.linspace(-10, 10, 100)
ax.plot(t, t**2, t**3)
plt.show()
