# crypt

`string crypt(string str, string|int salt = 0)`

Hash the password `str` the way Unix `crypt(3)` does and return the hash.
The salt names the algorithm:

- Absent, an int, or `""`: a fresh SHA-512 hash (`$6$`, a random
  16-character salt, 5000 rounds). This is the default for new passwords.
- A stored hash: the same algorithm and salt, so `crypt(p, stored) == stored`
  verifies `p`. Every family glibc's `crypt(3)` has produced is recognized:
  classic DES (a 2-character salt or a 13-character hash), `$1$` MD5, `$5$`
  SHA-256, `$6$` SHA-512, and `$2a$` / `$2b$` / `$2y$` bcrypt.
- A bare family prefix: a fresh salt for that family. `$6$`, `$6$rounds=N$`,
  `$5$`, `$5$rounds=N$`, `$1$`, `$2b$` and `$2b$NN$` (bcrypt cost `NN`, 4 to
  31, 10 when omitted; each step doubles the time).

Any other salt is an error. DES reads only the first 8 characters of the
password and bcrypt the first 72 bytes; MD5 and the SHA families read all
of it.

The hash is computed in the driver, not by the host's C library, so a
password and salt give the same hash on every platform.

### Porting a password file

An old lib's `crypt(p, stored) == stored` check works unchanged on its DES
hashes. To move players to SHA-512 as they log in:

```c
if (crypt(p, password) != password)
    return 0;
if (strlen(password) == 13)      /* classic DES: re-hash on the way in */
{
    password = crypt(p, 0);
    save_me();
}
```

Hashes from FluffOS's `oldcrypt` (MudOS's own algorithm, not DES) are not
recognized.

### Examples

```c
string hashed = crypt(new_password, 0);        /* "$6$..." */
string strong = crypt(new_password, "$2b$12$"); /* bcrypt, cost 12 */
if (crypt(typed, hashed) == hashed)
    write("Welcome back.\n");
```
