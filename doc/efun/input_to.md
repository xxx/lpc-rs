# input_to

`int input_to(string | function f, int no_echo = 0)`

This function is used to set a function to be called when the command giver 
enters their next line of input. The function will be passed the input string 
as the only argument. The function will be called in the same object as the 
call to `input_to`, and `previous_object()` there is that object.

`f` is a function pointer, or the name of a function in the calling object
(`static` included). A name the object does not define is an error.

if `no_echo` is set to 1, the input will not be echoed to the command giver.

If the command giver is not interactive, the function will not be called.

When the function's receiver cannot be loaded, the error goes to the master's
`error_handler` and the command giver sees `Canceled.`

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
