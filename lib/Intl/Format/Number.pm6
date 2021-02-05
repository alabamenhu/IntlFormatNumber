unit module Number;
use Intl::UserLanguage;
use Intl::CLDR;
use Intl::Format::Util::Digits;

my %dec-format-cache;
sub get-number-formatter(
        $language,
        :$length = 'standard',
        :$type = 'decimal',
        Int  :primary-grouping-size($grouping-size),
        Int  :$secondary-grouping-size,
        Int  :$maximum-integer-digits,
        Int  :$minimum-integer-digits,
        Int  :$maximum-fraction-digits,
        Int  :$minimum-fraction-digits,
        Bool :$show-decimal = False,
        # overrides will go here
) is export {
    .return with %dec-format-cache{$language ~ $length ~ $type};

    my \numbers = cldr{$language}.numbers;

    # Obtain the digits and symbols for the number system
    my     $default-system := numbers.numbering-systems.default;
    my str @digits         := %digits{$default-system};
    my     $symbols        := cldr{$language}.numbers.symbols{$default-system};

    die "NYI: compact formatters" unless $length eq 'standard';

    # Obtain the pattern
    my $patterns = parse-pattern numbers.{$type ~ '-formats'}{$default-system}.standard.pattern;
    my $pattern = $patterns.positive;

    # Compile our values
    enum ShowSign <never always default>;
    my $show-sign       = default;
    my $group-small     = $grouping-size // $pattern.grouping-size;
    my $group-large     = $secondary-grouping-size // $pattern.secondary-grouping-size // $group-small;
    my $max-int-digits  = $maximum-integer-digits  // $pattern.maximum-integer-digits;
    my $min-int-digits  = $minimum-integer-digits  // $pattern.minimum-integer-digits;
    my $max-frac-digits = $maximum-fraction-digits // $pattern.maximum-fraction-digits;
    my $min-frac-digits = $minimum-fraction-digits // $pattern.minimum-fraction-digits;

    # HEADER
    #   - sub declaration
    #   - special value returns (NaN, ±Inf)
    #   - set up variables
    #     - native str is a bit faster
    #     - formatting code assumes positive numbers
    #     - digits are set up.
    my str $code = qq:to/HEADER/;
        sub decimal-formatter (\$number) \{
            return {$symbols.nan.raku} if \$number === NaN;
            return {(($symbols.plus if $show-sign == always) ~ $symbols.infinity).raku} if \$number ===  Inf;
            return {($symbols.minus ~ $symbols.infinity).raku} if \$number === -Inf;
            my str \$result;
            my \$n = abs \$number;
            my constant @digits = {@digits.raku};
        HEADER

    # TODO: this might bump for extremely large numbers
    # Lob off digits via modular arithmetic
    if $max-int-digits < Inf {
        $code ~= qq:to/CHOP-UPPER/;
            \$n %= {10 ** ($max-int-digits - 1)});
        CHOP-UPPER
    }

    # FIND-POWER
    #   - determines the initial number of digits we'll have
    #     (note that log10(0) is -Inf, which can't be made native
    $code ~= qq:to/FIND-POWER/;
            my int \$power = \$n == 0 ?? 1 !! 1 + floor log10 \$n;
            \$power max= {$min-int-digits};
        FIND-POWER

    # SET-GROUPING
    #   - If we're using commas, set a 'timer'
    if $group-small > 0 {
        $code ~= qq:to/SET-GROUPING/;
            my int \$next-grouper = {$group-small};
        SET-GROUPING
    }

    # INTEGER-HEADER
    #   - sets up the loop.
    #   - place = what we're dividing by
    $code ~= qq:to/INTEGER-HEADER/;
            my \$int-val = \$n.Int;
            my \$int-old;
            my int \$digit;
            for ^\$power \{
                \$int-old = \$int-val;
                \$int-val = \$int-val div 10;
                \$digit   = \$int-old - \$int-val * 10;
        INTEGER-HEADER

    # INTEGER-GROUPING
    #   - if grouping digits
    #     - insert a comma when timer = 0
    #     - reset timer to secondary value
    if $group-small > 0 {
        $code ~= qq:to/INTEGER-GROUPING/;
                if \$next-grouper-- == 0 \{
                    \$next-grouper = {$group-large - 1};
                    \$result = {$symbols.group.raku} ~ \$result;
                }
        INTEGER-GROUPING
    }

    # INTEGER-FOOTER
    #   - adds the digit
    $code ~= qq:to/INTEGER-FOOTER/;
                \$result = \@digits[\$digit] ~ \$result;
            }
        INTEGER-FOOTER

    # INTEGER-FOOTER
    #   - adds the digit
    unless $show-sign == never {
        $code ~= qq:to/SIGN/;
                \$result = (\$number < 0 ?? {$symbols.minus.raku} !! {($show-sign == always ?? $symbols.plus !! '').raku }) ~ \$result;
            SIGN
    }

    # SET-UP-FRACTION
    #   - isolate fractional component
    #   - because $max-frac-digits == Inf could be infinite look (1/3), use $*TOLERANCE to cut off
    $code ~= qq:to/SET-UP-FRACTION/;
            \$n -= \$n.Int;
            my \$limit = \$*TOLERANCE;
        SET-UP-FRACTION

    # FORCE/OPT DECIMAL
    #   - If optional, only shows up if we'll be using fractional digits
    if $show-decimal {
        $code ~= qq:to/FORCE-DECIMAL/;
            \$result ~= {$symbols.decimal.raku};
        FORCE-DECIMAL
    }elsif $max-frac-digits > 0 {
        $code ~= qq:to/OPT-DECIMAL/;
            \$result ~= {$symbols.decimal.raku} if \$n > \$limit;
        OPT-DECIMAL
    }

    if $min-frac-digits > 0 || $max-frac-digits > 0 {
        $code ~= qq:to/FRACTION-HEADER/;
            if \$n > \$limit \{
        FRACTION-HEADER
    }

    # FORCE-FRACTION
    #   - generate digits but without checking.
    #   - TODO: adjust limit once at the end (if $max is different)
    if $min-frac-digits > 0 {
        $code ~= qq:to/FORCE-FRACTION/;
                for ^{$min-frac-digits} \{
                        \$n *= 10;
                        \$digit = floor \$n;
                        \$n -= \$digit;
                        \$result ~= \@digits[\$digit];
                };
                \$limit *= {10 ** $min-frac-digits};
        FORCE-FRACTION
    }

    # FLEX FRACTION
    #   - loop is the *maximum* number of digits we may have
    #   - check limit after each one (we currently don't round like we should!)
    if $max-frac-digits > $min-frac-digits {
        $code ~= qq:to/FLEX-FRACTION/;
                for ^{$max-frac-digits - $min-frac-digits} \{
                        \$n *= 10;
                        \$limit *= 10;
                        \$digit = floor \$n;
                        \$n -= \$digit;
                        \$result ~= \@digits[\$digit];
                        last if \$n < \$limit;
                }
        FLEX-FRACTION
    }

    if $min-frac-digits > 0 || $max-frac-digits > 0 {
        $code ~= "}\n"
    }


    # FOOTER
    #   - the most basic, return the result
    $code ~= qq:to/FOOTER/;
            return \$result;
        }
        FOOTER

    use MONKEY;
    # Store cache and return
    %dec-format-cache{$language ~ $length ~ $type} := EVAL $code;
}

multi sub format-number (
    $number,                           #= The number that will be formatted
    :$language = INIT {user-language}, #= The language to use
    :$length   = 'standard',           #= Formatting length, options include standard, short, and long
    :$type     = 'decimal',            #= The number style to use (decimal, percent, scientific currency)
) is export {
    return get-number-formatter($language,:$length,:$type).($number);
}

class NumberFormat {
    has $.positive;
    has $.negative;
}

class NumberPattern {
    has $.grouping-size;
    has $.secondary-grouping-size;
    has $.fraction-grouping-size;
    has $.maximum-integer-digits;
    has $.minimum-integer-digits;
    has $.minimum-fraction-digits;
    has $.maximum-fraction-digits;
    has $.maximum-significant-digits;
    has $.minimum-significant-digits;
    has $.pad-character;
    has $.pad-position;
    has $.prefix;
    has $.suffix;
    has $.user-plus-sign;
    has $.minimum-exponent-digits;
}

enum NumberPadding <no-padding  pre-prefix-padding  pre-number-padding  post-suffix-padding  post-number-padding>;
enum CurrencyType  <no-currency  standard-currency  iso-currency  name-currency  narrow-currency>;


grammar Pattern {
    token TOP { <pattern> [ ';' <negative> ]? }
    token negative       {
        | <pattern>
        | <prefix> '#' <suffix>?  # avoids a code block to guarantee
        |          '#' <suffix>   # either prefix and/or suffix
    }
    token pattern {
        # TODO: padding may only be indicated *once*, but realistically, it's never in CLDR
        <padding>?
        <prefix>?
        <padding>?
        <number>
        <exponent>?
        <padding>?
        <suffix>?
        <padding>?
        <?{ $<padding> ≤ 1}> # Enforce a single match
    }
    token number     {
        | <integer> ['.' <fraction>]? # the decimal is needed for exponent, ignored otherwise
        | <sig-digits>
    }
    token prefix     { <text>+ }
    token suffix     { <text>+ }
    token text       { <quoted> | <raw> | <replace> }
    token quoted     { \' (<-[']>+)? \' }
    token raw        { <-[.,;*'0..9#@%‰]>+ } # %‰¤+- have meaning, TODO TR 35 says E doesn't need to be quoted
    token replace    { <[%‰]> } # %‰¤+- have meaning, TODO TR 35 says E doesn't need to be quoted
    token integer    {
        <[#,]>*    # placeholder digits / grouping separators
        <[0,]>*    # mandatory / rounding digits / grouping separators
                   #   well, mandatory unless there's an E.  So much for
                   #   clear standards.
        <?{ $/.contains('0') || $/.contains('#') }> # must have a single digit
        <!{ $/.ends-with: ',' }>                    # must end with a digit
    }
    token fraction   {
        <[0,]>*    # mandatory / rounding digits / grouping separators (rare)
        <[#,]>*    # placeholder (generally 3) / grouping separators
        <?{ $/.Str.chars > 0 }> # must have a single digit of some type
    }
    token sig-digits {
        <[#,]>*
        <[@,]>+ # At least one @, flanked by #, all may have grouping separators
        <[#,]>*
    }
    token exponent   { 'E'  ('+')? ('0'+) }
    token padding    { '*' (.) }
    # the docs say the pad character can be anything, and that
    # that the the * escapes.  So frankly, I'm not sure why
    # it *can't* be a single quote per the BNF,
    # see http://unicode.org/reports/tr35/tr35-numbers.html#Padding
    # Plus the formal definition excludes higher plane stuff which seems silly
}

class PatternActions {
    has %.symbols;
    method TOP ($/) {
        my %positive = $<pattern>.made;
        my %negative;
        if $<negative> {
            if $<negative>.made<mirror> {
                %negative = %positive;
                %negative<prefix suffix> = $<negative>.made<prefix suffix>;
            } else {
                %negative = $<negative>.made
            }
        } else {
            %negative = %positive;
            %negative<prefix> = |%negative<prefix>, (:replace('-') ); # by default, add on a negative symbol
        }
        make NumberFormat.new:
                positive => NumberPattern.new(|%positive),
                negative => NumberPattern.new(|%negative);
    }
    method number ($/) {
        if $<integer> {
            my %number = $<integer>.made;
            if $<fraction> {
                %number{.key} = .value for $<fraction>.made
            }
            make %number;
        } else {
            make $<sig-digits>.made
        }
    }
    method pattern ($/) {
        my %result := $<number>.made.append(
                prefix => '',
                suffix => '',
                currency => no-currency
                );
        %result<prefix> = $<prefix> ?? $<prefix>.made !! ();
        %result<suffix> = $<suffix> ?? $<suffix>.made !! ();

        if $<exponent> {
            %result.append($<exponent>.made);
            # According to TR35, when using exponents, significant digits are used.
            # They are determined by a different method which is not entirely intuitive.
            # If the mantissa (in $<number>) includes a period, then...
            if $<number>.Str.contains('.') {
                # If the mantissa contains at least one 0:
                if $<number>.Str.contains('0') {
                    # Use the number of 0s BEFORE the period plus number of # and 0 after
                    # TODO this can be done in two statements by moving the if as a posfix
                    my @split = $<number>.Str.split('.');
                    %result<maximum-significant-digits> =
                            @split[0].comb('0').elems
                            + @split[1].comb.grep({$^a eq '#' || $^a eq '0'}).elems;
                    # otherwise (no 0s)
                } else {
                    # Use 1 + number of # after period
                    %result<maximum-significant-digits> = 1 + $<number>.Str.split('.')[1].comb( '#' ).elems;
                }
                # otherswise (no period)
            } else {
                # if the mantissa has at least one 0, then number of 0s, otherwise infinity
                %result<maximum-significant-digits> = $<number>.Str.comb('0').elems;
                %result<maximum-significant-digits> = ∞ unless %result<maximum-significant-digits>;
            }
            # Additionally, when using exponents, the maximum number of integer digits
            # not only can be known but DEFINES exponential grouping.  This is defined
            # by the number of digits (# or 0) before the period in the mantissa.
            # Normally, the maximum digits will be 1 (a single 0 or #) but if there
            # are three, then during formatting, the powers are limited to multiples
            # of the maximum integer value.
            %result<maximum-integer-digits> =
                    $<number>.Str.split('.')[0].grep({$^a eq '#' || $^a eq '0'}).elems;
        } else {
            %result<use-plus-sign> = False;
            %result<minimum-exponent-digits> = 0; # no exponential formatting
        }

        if $<padding> {
            my $padding = $<padding>[0];
            %result<pad-character> = $padding.made;
            # Pardon the nasty logic.
            %result<pad-position> =
                    $padding.from < $<number>.from
                    ?? $<prefix> && $padding.from < $<prefix>.from  # before number
                        ?? pre-prefix-padding                         # before prefix
                        !! pre-number-padding                         # after  prefix or no prefix
                    !! $<suffix> && $padding.from > $<suffix>.from  # after number
                        ?? pre-prefix-padding                         # after  suffix
                        !! pre-number-padding                         # before suffix or no suffix
            } else {
            %result<pad-character> = '';
            %result<pad-position>  = no-padding;
        }
        make %result;

    }
    method negative ($/) {
        if $<pattern> {
            make $<pattern>.made
        } else {
            make %(
                :mirror,
                :prefix($<prefix> ?? $<prefix>.made !! ''),
                :suffix($<suffix> ?? $<suffix>.made !! ''),
            )
        }
    }
    method integer ($/) {
        my $grouping-size;
        my $secondary-grouping-size;

        my @groups = $/.Str.split(',');
        given @groups {
            when 1 { $secondary-grouping-size = $grouping-size = ∞                  }
            when 2 { $secondary-grouping-size = $grouping-size = @groups.tail.chars }
            default { # when more than 2 commas, only the two closest to the decimal
                $secondary-grouping-size = @groups[*-2].chars;        # are considered
                $grouping-size = @groups.tail.chars;
            }
        }

        # minimum integer digits defines padded 0s, so we just count the zeros in
        # the pattern string.  The maximum is not used UNLESS the pattern is all
        # zeros, and then it is set to the same as the minimum.
        my $minimum-integer-digits = @groups.join.comb('0').elems;
        my $maximum-integer-digits = @groups.head.substr(0,1) eq '0'
                ?? $minimum-integer-digits
                !! ∞;

        make %(
            :$maximum-integer-digits,
            :$minimum-integer-digits,
            :$grouping-size,
            :$secondary-grouping-size,
            :minimum-significant-digits(0), # Not used with int/frac formatting
            :maximum-significant-digits(0), # Not used with int/frac formatting
            :maximum-fraction-digits(0), # Will be overwritten if fraction exists
            :minimum-fraction-digits(0), # Will be overwritten if fraction exists
            :fraction-grouping-size(0),  # Will be overwritten if fraction exists
        );
    }
    method fraction ($/) {
        my @groups = $/.Str.split(',');

        # Based on the first element IFF there was a comma, otherwise ∞ (no separator)
        my $fraction-grouping-size = @groups > 1 ?? @groups.head.chars !! ∞;

        # Minimum fraction digits defines padded 0s, so we just count the zeros in
        # the pattern string.  The maximum is only used if the entire string is 0s,
        # otherwise it's set to 0 (ironically) to be ignored.
        my $minimum-fraction-digits = @groups.join.comb('0').elems;
        my $maximum-fraction-digits = @groups.tail.substr(*-1) eq '0'
                ?? $minimum-fraction-digits
                !! ∞;

        make %(
            :$maximum-fraction-digits,
            :$minimum-fraction-digits,
            :$fraction-grouping-size,
        );
    }
    method sig-digits ($/) {
        my $grouping-size;
        my $secondary-grouping-size;

        my @groups = $/.Str.split(',');
        given @groups {
            when 1 { $secondary-grouping-size = $grouping-size = 0                  }
            when 2 { $secondary-grouping-size = $grouping-size = @groups.tail.chars }
            default { # when more than 2 commas, only the two closest to the decimal
                $secondary-grouping-size = @groups[*-2].chars;        # are considered
                $grouping-size = @groups.tail.chars;
            }
        }

        # The minimum is the number of @ in the string.
        # The maximum is the number of trailing #, plus the @
        my $minimum-significant-digits = @groups.join.comb('@').elems;
        my $maximum-significant-digits = $minimum-significant-digits + @groups.join.split('@').tail.chars;

        make %(
            :$minimum-significant-digits,
            :$maximum-significant-digits,
            :$grouping-size,
            :$secondary-grouping-size,
            :maximum-integer-digits(0),  # Not used with sig-digit formatting
            :minimum-integer-digits(0),  # Not used with sig-digit formatting
            :maximum-fraction-digits(0), # Not used with sig-digit formatting
            :minimum-fraction-digits(0), # Not used with sig-digit formatting
            :fraction-grouping-size(0),  # Not used with sig-digit formatting, but should?
        )
    }
    method exponent ($/) {
        make %(
            :use-plus-sign(?$0),
            :minimum-exponent-digits($1.chars)
        )
    }
    method padding ($/) { make $0.Str }
    method quoted ($/)  {
        make $0 ?? :exact($0.Str) !! :exact("'") # an empty escaped string counts as a single quote
    }
    method text ($/) {
        make $<quoted>.made  with $<quoted>;
        make $<raw>.made     with $<raw>;
        make $<replace>.made with $<replace>;
    }
    method prefix ($/) {
        my @text = $<text>.map(*.made);

        # I'm fairly certain currency code will be handled elsewhere
        #my $currency;
        #if my $offset = $text.index('¤') {
        #  $currency = do given $offset {
        #    when $text.substr($offset,5) eq '¤¤¤¤¤' { die "Quintiple currency symbol currently invalid"  }
        #    when $text.substr($offset,4) eq '¤¤¤¤'  { narrow-currency   }
        #    when $text.substr($offset,3) eq '¤¤¤'   { name-currency     }
        #    when $text.substr($offset,2) eq '¤¤'    { iso-currency      }
        #    when $text.substr($offset,1) eq '¤'     { standard-currency }
        #  }
        #} else {
        #  $currency = no-currency;
        #}
        make @text;
    }
    method suffix ($/) {
        make $<text>.map(*.made);
    }
}

my %pattern-cache;
sub parse-pattern($pattern) {
    .return with %pattern-cache{$pattern};
    %pattern-cache{$pattern} = Pattern.parse($pattern, :actions(PatternActions)).made;
}