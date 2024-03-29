use v6.d;
unit grammar Grammar;
# This follows TR35 as closely as possible.  However, while TR35 says that
# a literal E need not be quoted, the documentation on CLDR's website states
# that E is repositionable, which is only really feasible if it can go anywhere.
# For now, we follow TR35, and assume that if E exists, it will immediately
# follow the base number, and will be immediately followed by an optional sign
# and some number of digits.

# TOP is a dummy rule -- called only to set up dynamic variables
# If someone else wishes to use the grammar, they may want to
# implement the full set of actions methods.  All data here is
# created within the grammar, and stored into %*data
token TOP {
    :my %*data =
        negative-type => 'simple',
        type                       => 'decimal',
        minimum-integer-digits     => 0,
        maximum-integer-digits     => ∞, # (not definable in patterns, but makes things easier for formatters)
        minimum-fractional-digits  => 0,
        maximum-fractional-digits  => ∞, # (not definable in patterns, but makes things easier for formatters)
        minimum-significant-digits => 0,
        maximum-significant-digits => 0,
        minimum-exponential-digits => 0,
        primary-grouping-size      => 0,
        secondary-grouping-size    => 0,
        fractional-grouping-size   => 0,
        exponential-power-multiple => 0,
        exponential-forces-sign    => False,
        rounding-value             => 0,
        # Negative format values
        negative-prefix            => [],
        negative-suffix            => [],
        negative-padding-type      => '',
        negative-padding-char      => '',
        # Positive format values
        positive-prefix            => [],
        positive-suffix            => [],
        positive-padding-type      => '',
        positive-padding-char      => '',
    ;
    <patterns>
    { make %*data }
}

# If only one token is given, the negative is identical to the positive,
# but with a minus directly in front of the number
token patterns {
    $<positive>=<pattern>
    [
        # TODO: Per TR 35, no need to fully parse the negative pattern, just its prefix/suffix
        # <http://unicode.org/reports/tr35/tr35-numbers.html#32-special-pattern-characters>
        # If there is an explicit negative subpattern, it serves only to specify
        # the negative prefix and suffix; the number of digits, minimal digits, and
        # other characteristics are ignored in the negative subpattern. That means
        # that "#,##0.0#;(#)" has precisely the same result as "#,##0.0#;(#,##0.0#)".
        # However in the CLDR data, the format is normalized so that the other
        # characteristics are preserved, just for readability.
        ';'
        :my $*negative = True;
        $<negative>=<pattern>
    ]?
}

# The pattern will be taken in as a sequence of elements
# Each element has unambiguous start values, which allows this
token pattern {
    :my @*fix := $*negative ?? %*data<negative-prefix> !! %*data<positive-prefix>;
    <padding('start')>?
    <pattern-element>+
    <padding('end')>?
}

proto
token pattern-element { * }
token pattern-element:raw-text     { <-[.,;*'0..9#@%‰-]>+ { @*fix.push: %( type => 'text',    'text' => $/.Str    ) } }
token pattern-element:quoted-text  { \' <( .+? )> \'       { @*fix.push: %( type => 'text',    'text' => $/.Str    ) } }
token pattern-element:quote        { \' \'                 { @*fix.push: %( type => 'text',    'text' => "'"       ) } }
token pattern-element:minus        { '-'                   { @*fix.push: %( type => 'replace', 'symb' => 'minus'   ) } }
token pattern-element:percent      { '%'                   { @*fix.push: %( type => 'replace', 'symb' => 'percent' ) } }
token pattern-element:permille     { '‰'                  { @*fix.push: %( type => 'replace', 'symb' => 'permille') } }
token pattern-element:currency     { '¤'+                  { @*fix.push: %( type => 'replace', 'symb' => 'currency', length => $/.Str.chars) } }

# If there's a negative element, then it has the possibility to use only a single dummy digit.
# In this case, it's interpreted as repeating the numerical data from the positive, just with
# a different prefix and suffix.  This generates the negative type of 'circumfix'
#token pattern-element:negative     {
#    <?{$*negative}>                           # check we're in negative pattern
#    '#'
#    <before <-[0..9@#,]>>                     # ensure we're not in a regular pattern before...
#    { %*data<negative-type> = 'circumfix' }   # setting the negative type
#}


token pattern-element:number {
    # Padding is allowed either at the beginning or end of the number pattern
    <padding('before')>?

    # With negative numbers, we don't care about the actual values
    # and can just skip this part entirely
    [ <?{$*negative}> <[@#0..9]>+ ['.' <[@#0..9]>*]? [E '+'?<[0..9]>+]?
    | # or else, we do the positive pattern, which is the real workhorse

        # Set up variables (final grouping values calculated later)
        :my @*grouping;
        :my $*grouping = 0;
        :my $*fractional-multiplier = 0.1;

        # Integer parts.
        [
        | '#' <.dummy-digit>        [',' <.integer-grouping> ]?
        | '@' <.significant-digit>  [',' <.integer-grouping> ]?
        | <integral-digit>          [',' <.integer-grouping> ]?
        ]+
        <.handle-grouping>

        [
            '.' # Decimal is optional
            [ # Optional fractional parts, cannot be composed entirely of commas.
            | '#' <.dummy-digit>       [',' <.fractional-grouping> ]?
            | '@' <.significant-digit> [',' <.fractional-grouping> ]?
            | <fractional-digit>       [',' <.fractional-grouping> ]?
            ]*
        ]?

        [
            'E' # if exponential
            <exponential-plus>?
            <exponential-digit>+
            {
                %*data<type> = 'scientific';
                # Here we need to readjust based on what we've seen.
                # The MAXIMUM significant digits is defined as (per TR 35 § 3.4)
                #   If the mantissa pattern contains a period:
                #       ?? If the mantissa pattern contains at least one 0:
                #           ?? Return the number of 0s before the period added to the number of #s or 0s after the period
                #           !! Return 1 plus the number of #s after the period
                #       !! If the mantissa pattern contains at least one 0:
                #           ?? Return the number of 0s.
                #           !! Return positive infinity.
                # The approach used below feels hacky, but I'm not sure a cleaner approach since exponentials are
                # treated as if significant digits, and I don't want to add another variable with necessary flags,
                # etc, (please submit a PR if you know!). Also, this is not complete, as per TR 35 § 3.5, TODO:
                #   Significant digits may be used together with exponential notation.
                #   Such patterns are equivalent to a normal exponential pattern with
                #   a minimum and maximum integer digit count of one, a minimum fraction
                #   digit count of Minimum Significant Digits - 1, and a maximum fraction
                #   digit count of Maximum Significant Digits - 1. For example, the
                #   pattern "@@###E0" is equivalent to "0.0###E0".
                %*data<maximum-significant-digits> =
                    $*grouping # This will be < 0 if any fractional digits
                        ?? (%*data<minimum-integer-digits> max 1) + $*grouping
                        !! (%*data<minimum-integer-digits> max Inf)
            }

        ]?

        # Tidy up:
        # Padding is allowed either at the beginning or end of the number pattern
        # Pattern elements will also now affix to the suffix portion rather than prefix
        <padding('after')>?
        { @*fix := $*negative ?? %*data<negative-suffix> !! %*data<positive-suffix> }
    ] # close off the positive pattern
}
method dummy-digit {
    # The dummy digit normally has no meaning except to provide
    # spacing for grouping size.  However, for scientific numbers,
    # it can provide the multiplier that dictates available powers
    # (e.g. engineering notation only allowing 10^3, 10^6, etc),
    # unless significant digits are used, in which case dummy digits
    # before the significant digit are ignored, and after it (hence
    # the conditional check) are included for the maximum significant digits.
    %*data<exponential-power-multiple>++;
    %*data<maximum-significant-digits>++ if %*data<minimum-significant-digits>;
    $*grouping++;
    self
}
method significant-digit {
    # Significant digits may be used together with exponential notation.
    # Such patterns are equivalent to a normal exponential pattern with
    # a minimum and maximum integer digit count of one, a minimum fraction
    # digit count of Minimum Significant Digits - 1, and a maximum fraction
    # digit count of Maximum Significant Digits - 1. For example, the pattern
    # "@@###E0" is equivalent to "0.0###E0".
    %*data<minimum-significant-digits>++;
    %*data<maximum-significant-digits>++;
    self
}
method integer-grouping {
    @*grouping.push: $*grouping;
    $*grouping = 0;
    self
}

method handle-grouping {
    # Per the standard, only the final two group sizes are used.
    # For simplicity of implementation, we just capture all of them,
    #   and then choose the last two, assuming there at least two
    # Having a single element (added here because no commas at all)
    #   will be treated as "no groupings" and result in the group
    #   code not being inserted.
    @*grouping.push: $*grouping;
    $*grouping = 0;

    if @*grouping > 2 {
        (%*data<secondary-grouping-size>, %*data<primary-grouping-size>) = @*grouping.tail(2);
    } elsif @*grouping == 2 {
        %*data<primary-grouping-size>   = @*grouping.tail;
        %*data<secondary-grouping-size> = @*grouping.tail;
    }
    self;
}
method handle-integral-digit($digit) {
    %*data<rounding-value> = %*data<rounding-value> * 10 + $digit;
    %*data<minimum-integer-digits>++;
    $*grouping++;
    self
}
method handle-fractional-digit($digit) {
    %*data<rounding-value> = %*data<rounding-value> + $*fractional-multiplier * $digit;
    $*fractional-multiplier /= 10;
    %*data<minimum-fractional-digits>++;
    $*grouping++;
    self
}
method fractional-grouping {
    # In the (odd) event that someone uses two fractional groupers, the standard doesn't say
    # how they should be treated.  I'm going to assume that they should be treated in the
    # same way as integral: only the trailing one will be counted.
    %*data<fractional-grouping-size> = $*grouping;
    $*grouping = 0;
    self;
}

token padding($position){
    '*' <(.
    {
        if $*negative {
            %*data<negative-padding-char> = $/.Str;
            %*data<negative-padding-type> = $position;
        } else {
            %*data<positive-padding-char> = $/.Str;
            %*data<positive-padding-type> = $position;
        }
    }
}
# the docs say the pad character can be anything, and that
# that the the * escapes.  So frankly, I'm not sure why
# it *can't* be a single quote per the BNF,
# see http://unicode.org/reports/tr35/tr35-numbers.html#Padding
# Plus the formal definition excludes higher plane stuff which seems silly

token integral-digit    { <[0..9]> { } <.handle-integral-digit(  $/.Str.Int)>    }
token fractional-digit  { <[0..9]> { } <.handle-fractional-digit($/.Str.Int)>    }
token exponential-digit { <[0..9]> {     %*data<minimum-exponential-digits>++  } }
token exponential-plus  {   '+'    {     %*data<exponential-forces-sign> = True} }
