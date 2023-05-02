# move_object

`void move_object(string | object destination)`

The low-level move operation that moves the current object from one environment to
another. If `destination` is a string, it is interpreted as an in-game path,
loading the object at that path.

You probably won't want to use this function directly, as it does not handle
things like weight, volume, or light levels. Instead, your mudlib should
provide an appropriate higher-level function that wraps this one, and takes
care of any additional game logic.
