# write_socket

`int write_socket(int | float | string)`

Write a string to the interactive that's inhabiting the current object.
Returns 1 upon successful write, else 0.

If the object is not interactive, or the socket is not connected, the output
will instead be written to the debug log, and the function will return 0.

