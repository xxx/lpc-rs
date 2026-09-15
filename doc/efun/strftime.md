# strftime

```c
string strftime(string fmt)
string strftime(string fmt, int t)
```

Return a string containing `t` formatted according to `fmt` in the driver's
local time zone. Ordinary characters are copied unchanged; conversions
beginning with `%` are replaced with date or time fields. Use `%%` for a
literal percent sign. An empty format returns an empty string.

### Arguments and time zone

| Argument | Meaning |
|---|---|
| `fmt` | A string containing literal text and the conversions listed below. |
| `t` | Seconds since the Unix epoch, 1970-01-01 00:00:00 UTC; defaults to the current time when omitted. |

An explicit `0` means the epoch, and negative timestamps represent earlier
times. Supply seconds, not milliseconds. When `t` is omitted, the clock is
read on each call, including calls made again during a transaction retry.

The driver's local time zone and its daylight-saving offset at `t` determine
the calendar fields. There is no argument to select UTC or a player's time
zone. Formatting the same timestamp on drivers in different zones can
produce different dates as well as different times. `%s` always gives the
Unix timestamp, independent of the zone.

Month and weekday names are English. `%c`, `%x`, `%X`, and `%r` use the fixed
formats below; they do not change with the host's locale. The syntax is
based on C's `strftime`, but only the conversions and modifiers documented
here are supported.

### Date conversions

| Conversion | Result |
|---|---|
| `%Y` | Full year, padded to at least four digits, such as `2001`; years outside `0000`–`9999` include a sign. |
| `%y` | Two-digit year, `00`–`99`. |
| `%C` | Century (year divided by 100), such as `20` for 2001. |
| `%m` | Month, `01`–`12`. |
| `%b`, `%h` | Abbreviated month name, such as `Sep`. |
| `%B` | Full month name, such as `September`. |
| `%d` | Day of the month, `01`–`31`. |
| `%e` | Day of the month padded with a space, such as ` 9`. |
| `%a` | Abbreviated weekday name, such as `Sun`. |
| `%A` | Full weekday name, such as `Sunday`. |
| `%w` | Weekday number, `0` (Sunday) through `6` (Saturday). |
| `%u` | ISO weekday number, `1` (Monday) through `7` (Sunday). |
| `%j` | Day of the year, `001`–`366`. |
| `%q` | Quarter of the year, `1`–`4`. |
| `%U` | Week number, `00`–`53`; week 1 begins on the year's first Sunday. |
| `%W` | Week number, `00`–`53`; week 1 begins on the year's first Monday. |
| `%V` | ISO week number, `01`–`53`; weeks begin on Monday. |
| `%G`, `%g` | ISO week's year, as a full year or two-digit year. |

For `%U` and `%W`, days before week 1 belong to week 0. ISO week 1 is the
week containing January 4. Near New Year, the ISO week's year may differ
from the calendar year: use `%G-W%V-%u` for an ISO week date. For example,
January 1, 2021 belongs to `2020-W53-5`.

### Time and zone conversions

| Conversion | Result |
|---|---|
| `%H` | Hour on a 24-hour clock, `00`–`23`. |
| `%k` | Hour on a 24-hour clock padded with a space, such as ` 1`. |
| `%I` | Hour on a 12-hour clock, `01`–`12`. |
| `%l` | Hour on a 12-hour clock padded with a space, such as ` 1`. |
| `%M` | Minute, `00`–`59`. |
| `%S` | Second, `00`–`59` for an explicit Unix timestamp. |
| `%p`, `%P` | `AM`/`PM` or `am`/`pm`, respectively. |
| `%z` | UTC offset as `+HHMM` or `-HHMM`, such as `-0700`. |
| `%:z` | UTC offset with a colon, such as `-07:00`. |
| `%::z` | UTC offset including seconds, such as `-07:00:00`. |
| `%:::z` | UTC offset showing only whole hours, such as `-07`; minutes and seconds are omitted. |
| `%Z` | Numeric UTC offset, such as `-07:00`; the driver does not emit zone names or abbreviations. |
| `%s` | Seconds since the Unix epoch as a decimal integer. |

For an unambiguous local timestamp, include `%:z` or use `%+`.

### Combined formats and literal characters

| Conversion | Equivalent format or result |
|---|---|
| `%F` | `%Y-%m-%d` |
| `%D`, `%x` | `%m/%d/%y` |
| `%v` | `%e-%b-%Y` |
| `%R` | `%H:%M` |
| `%T`, `%X` | `%H:%M:%S` |
| `%r` | `%I:%M:%S %p` |
| `%c` | `%a %b %e %H:%M:%S %Y` |
| `%+` | `%Y-%m-%dT%H:%M:%S%.f%:z`, an ISO 8601 / RFC 3339 timestamp. |
| `%%` | One percent sign. |
| `%n` | A newline. |
| `%t` | A tab. |

### Fractional seconds

An explicit `t` has whole-second precision, so its fractional fields are
zero. Omitting `t` retains the current clock's subsecond value;
`strftime(fmt)` can therefore differ from `strftime(fmt, time())` for
formats that include fractional seconds.

| Conversion | Result |
|---|---|
| `%f` | Nanoseconds within the second as nine digits, such as `000000000`. |
| `%.f` | A decimal point followed by 3, 6, or 9 fractional digits as needed; empty when the fraction is zero. |
| `%.3f`, `%.6f`, `%.9f` | A decimal point followed by exactly 3, 6, or 9 fractional digits. |
| `%3f`, `%6f`, `%9f` | Exactly 3, 6, or 9 fractional digits, without a decimal point. |

Shorter fractional formats truncate rather than round. `%+` includes the
fraction only when it is nonzero.

### Padding

Most numeric fields use leading zeroes by default; `%e`, `%k`, and `%l`
use spaces. Insert one modifier after `%` to change a numeric field's
padding:

| Modifier | Effect | Example for day 9 |
|---|---|---|
| `-` | Remove padding. | `%-d` produces `9`. |
| `_` | Pad with spaces. | `%_d` produces ` 9`. |
| `0` | Pad with zeroes. | `%0e` produces `09`. |

These modifiers apply to individual numeric fields such as `%d`, `%H`,
and `%j`. They cannot be applied to names, zone offsets, combined formats,
or the fixed fractional formats such as `%.3f`. For example, `%-B` and
`%_F` are errors. Arbitrary field widths such as `%4Y` and C's `E`/`O`
modifiers such as `%Ec` or `%Od` are unsupported.

### Errors

Invalid argument types, an out-of-range timestamp, an unknown conversion
such as `%Q`, or a trailing unescaped `%` raise an error. Invalid formats
are not returned as literal text. `%#z` is a parsing-only conversion and
also raises an error when used here.

### Examples

```c
write("It is now " + strftime("%F %R %:z") + ".\n");
```

The following results assume the driver's local time zone is UTC:

```c
strftime("%F %T", 1000000000)         // "2001-09-09 01:46:40"
strftime("%A, %B %-d", 1000000000)    // "Sunday, September 9"
strftime("%I:%M %p", 1000000000)      // "01:46 AM"
strftime("%+", 1000000000)            // "2001-09-09T01:46:40+00:00"
strftime("%G-W%V-%u", 1609459200)     // "2020-W53-5"
strftime("[%d][%_d][%-d]", 0)         // "[01][ 1][1]"
strftime("%T%.3f", 0)                // "00:00:00.000"
strftime("%s", -1)                   // "-1"
strftime("100%% complete", 0)        // "100% complete"
```

### See also

[ctime](ctime.md), [localtime](localtime.md), [time](time.md),
[sprintf](sprintf.md)
