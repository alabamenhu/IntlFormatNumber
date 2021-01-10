# IntlFormatNumber
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

Support for negative numbers and other formatting styles and options specified in TR 35 / CLDR (e.g. percent, scientific, currency, etc), are NYI but will be available soon.

Performance is about an order of magnitude slower than `.Str`, but an order of magnitude  faster than `.base(10)`, so it should be acceptable for most uses.

## Version history

  * **v0.1.0**
    * Initial version

## License and Copyright
© 2020 Matthew Stephen Stuckwisch.
Licensed under the Artistic License 2.0.