import pine_py
import matplotlib
import numpy as np
import matplotlib.pyplot as plt
import math

xdata = np.arange(0, 100)
closes = [math.sin(x) for x in xdata]

class MyCallback:

    def print(self, _str):
        print("The calculation Result is: " + _str)
    
    def plot(self, floats):
        print(type(floats[0]))

        x = np.arange(0, len(closes))

        plt.subplot(2,  1,  1)
        plt.plot(x, closes)
        plt.title('Origin data')

        plt.subplot(2,  1,  2)
        plt.plot(x, floats)
        plt.title('Pine data')
        plt.show()

# with open("ma.pine", "r") as f:
#     contents = f.read()

# with open("ema.pine", "r") as f:
#     contents = f.read()

with open("script.pine", "r") as f:
    contents = f.read()


instance = MyCallback()
pine_py.load_script(contents, closes, instance)

