# Intl::Format::Number
A module for formatting numbers in a localized manner

To use, simply say

```
use Intl::Format::Number;

# Assuming user-language is 'en' (English)
say format-number 4834853;  # 4,834,853
say format-number 12.394;   # 12.394
say format-number 1/3;      # 0.333333333333333
```

It's that easy :-)

Full support for negative numbers and other formatting styles and options specified in TR 35 / CLDR (e.g. percent, scientific, currency, etc), are NYI but will be available soon.

Performance is about an order of magnitude slower than `.Str`, but an order of magnitude faster than `.base(10)`, so it should be acceptable for most uses.

### Options

The Raku-defined variable `$*TOLERANCE` is used in the case of extremely long decimals (e.g. any repeating decimal like `1/3`).
Raku defaults it to `1e-15` (thus providing 15 digits of decimals) but you can set it to any number.
You are highly advised *not* to set it to 0 unless you *also* specify a parameter to limit the display of decimal digits.

Formatting options (passed as arguments):
 * **`:$maximum-integer-digits`**  
This limits the number of whole numbers shown (formatting `1234` with this set to 2 will show `34`)
 * **`:$minimum-integer-digits`**  
Pads the the whole numbers to at least this many (formatting `12` with this set to 4 will show `0012`)
 * **`:$maximum-fractional-digits`**  
Limits the number of decimal digits shown (formatting `1.2345` with this set to 2 will show `1.23`)
 * **`:$minimum-fractional-digits`**  
Pads the the decimal digits to at least this many (formatting `1.2` with this set to 3 will show `1.200`)
 * **`:$show-decimal`** *(experimental)*  
If true, forces the decimal to be shown (warning: name may change to `force-decimal` in the future).

### Todo

  * Support for exponential numbers
  * Support for percentage/permillage formatting
  * Full support for negative numbers (mainly affects accounting numbers for now)
  * Better documentation
  * Proper rounding of values (currently just truncated)
  * More tests
  * Cleanup code

## Version history
  * **v0.2.0**
    * First version to use RakuAST (requires Rakudo 2022.12 or higher)
    * Initial support for negative numbers (should work for most all languages)
    * Initial support for formatting options
    * New and improved number format parser (faster, more accurate, and more maintainable!)
    * Added test file for grammar parsing
  * **v0.1.1**
    * Fix bug for ***n* = 0**, ***n*** **=** **10*ˣ***, and ***n* < 0**
  * **v0.1.0**
    * Initial version

## License and Copyright
© 2020-2023 Matthew Stephen Stuckwisch.
Licensed under the Artistic License 2.0.