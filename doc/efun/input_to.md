# input_to

`int input_to(function a, int no_echo = 0)`

This function is used to set a function to be called when the command giver 
enters their next line of input. The function will be passed the input string 
as the only argument. The function will be called in the same object as the 
call to `input_to`.

if `no_echo` is set to 1, the input will not be echoed to the command giver.

If the command giver is not interactive, the function will not be called.

This function returns 1 if the command giver is interactive, and the handler
was successfully set up, otherwise 0.

### Examples

```c
void init()
{
    input_to(get_name);
}

void get_name(string str)
{
    write("Hello, " + str + "!\n");
}
```
