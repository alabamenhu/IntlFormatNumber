# Intl::Format::Number
A module for formatting numbers in a localized manner

To use, simply say

```
use Intl::Format::Number;

# Assuming user-language is 'en' (English)
say format-number 4834853;  # 4,834,853
say format-number 12.394;   # 12.394
say format-number 1/3;      # 0.333333333333333

# A separate call can get a number formatter which allows you to specify more options
my $formatter = local-number-formatter('en', :minimum-significant-digits<2>); 
```

It's that easy :-)

Currently supports percent, permille and scientific formatting in addition to the basic number format for languages with decimal number systems (sorry ancient Romans or Mayans!).

Other formatting styles (like compact) styles and options specified in TR 35 / CLDR, are NYI but will be available soon (current will be available via a separate module).

Performance is about an order of magnitude slower than `.Str` for most formatting, and I continue to try to optimize it farther.
Balancing performance with the ability to, e.g., format a `FatRat` of several thousand digits, isn't easy :-) 

### Options

The Raku-defined variable `$*TOLERANCE` is used in the case of extremely long decimals (e.g. any repeating decimal like `1/3`).
Raku defaults it to `1e-15` (thus providing 15 digits of decimals) but you can set it to any number.
You are highly advised *not* to set it to 0 as this may cause an infinite loop unless.
(In the future, setting the maximum fractional digits to `Inf` and `$*TOLERANCE` to `0` will generate an error).

Basic options
 * **`:language<…>`**  
 Sets the language to use for formatting.  If the Unicode U Extension tag `nu` (for numbering system) is present, it will be respected.  
 * **`:type<…>`** (*standard, percent, permille, scientific, compact*)  
 Sets the type of number desired.  If percent or permille is selected, the number is multiplied by 100 or 1000 prior to display (such that .123 becomes 12.3% or 123‰). Compact numbers are not yet supported.
 * **`:length<…>`**  
 Sets the length to use for formatting.  Defaults to `standard`, which is the only option available outside of compact numbers.
 
  
Number formatter options (for use with `get-number-formatter`)

|            option            |                  description                  |               example                |
|:----------------------------:|:---------------------------------------------:|:------------------------------------:|
|   `maximum-integer-digits`   |        No more whole digits than this         |  when **2** <br>`1234` yields `34`   |
|   `minimum-integer-digits`   |   Pad with zeros to have at least this many   |   when **4**<br>`12` yields `0012`   |
| `maximum-fractional-digits`  |       No more decimal digits than this        | when **2**<br>`1.2345` yields `1.23` |
| `minimum-fractional-digits`  | Add trailing zeros to have at least this many |  when **3**<br>`1.2` yields `1.200`  |
| `maximum-significant-digits` | Limits the amount of significant digits used  |  when **2**<br>`1234` yields `1200`  |
| `minimum-significant-digits` |        Add trailing zeros if necessary        |   when **3**<br>`1` yields `1.00`    |
|        `show-decimal`        |        Forces the decimal to be shown         |   when **True**<br>`1` yields `1.`   |
|         `show-sign`          |      Shows the sign regardless polarity       |   when **True**<br>`1` yields `+1`   |
|          `symbols`           |    Adjusts the symbols used in formatting.    |                                      |
|       `number-system`        |   Set the digits used. Must be a CLDR code.   | when **arab**, `1234` yields  `١٢٣٤` |
|            `rast`            |      Returns a `RakuAST` node instead         |                                      |

If significant digits are specified, then the integral/fractional digits are currently ignored.
Presently, the `RakuAST` node generated is a `RakuAST::Sub`.  Additional documentation on it will
be included in future updates to improve its integration with other modules.

### Todo
  * Better documentation
  * More tests
  * Cleanup code
  
## Version history
  * **v0.4.0**
    * Formally add support for `local-number-formatter` and `number-formatter`
  * **v0.3.0**
    * Added support for exponential formats (`:type<exponential>`)
    * Adjust some code for the newest version of RakuAST
  * **v0.2.1**
    * Added support for percent and permille formats (`:type<percent>` and `:type<permille>`)
    * Respects language tag's numering system (`:language<en-u-nu-limb>` will use Limbu numerals)
    * Fixed (mostly) a caching bug
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