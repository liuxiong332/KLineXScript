import pine_py

# print(pine_py.__doc__)

class MyCallback:

    def print(self, _str):
        print(_str)

with open("script.pine", "r") as f:
    contents = f.read()

instance = MyCallback()
print(pine_py.load_script(contents, [100.0, 200.0], instance))
