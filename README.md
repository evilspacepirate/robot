# robot
A utility that will run a command when a file timestamp has changed. Timestamps are checked for all files matching the pattern in the current directory and subdirectories of the current directory every 250 milliseconds.

## Usage

```
robot <command> <file_pattern> [ .. <file_pattern> ]
```

# Example

This example will recompile the hello_world binary every time the hello_world.c is changed.
```
robot "gcc hello_world.c -o hello_world" "*.c"
```
# License

ISC flavored...

Permission to use, copy, modify, and/or distribute
this software for any purpose with or without fee
is hereby granted, provided that the above copyright
notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR
DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE
FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
