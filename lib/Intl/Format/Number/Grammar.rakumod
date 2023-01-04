use v6.d;
unit grammar Grammar;
# This follows TR35 as closely as possible.  However, while TR35 says that
# a literal E need not be quoted, the documentation on CLDR's website states
# that E is repositionable, which is only really feasible if it can go anywhere.
# For now, we follow TR35, and assume that if E exists, it will immediately
# follow the base number, and will be immediately followed by an optional sign
# and some number of digits.

# TOP is a dummy rule -- called only to set up dynamic variables
# that both grammar and actions need to have access to
token TOP {
    :my %*data =
        negative-type => 'simple',
        positive => %(
            type                       => '',
            minimum-integer-digits     => 0,
            minimum-fractional-digits  => 0,
            minimum-exponential-digits => 0,
            minimum-significant-digits => 0,
           #maximum-integer-digits     => ∞, (not definable in patterns)
           #maximum-fractional-digits  => ∞, (not definable in patterns)
            maximum-significant-digits => 0,
            primary-grouping-size      => 0,
            secondary-grouping-size    => 0,
            fractional-grouping-size   => 0,
            exponential-power-multiple => 0,
            exponential-forces-sign    => False,
            rounding-value             => 0,
            prefix                     => [],
            suffix                     => [],
            padding-type               => '',
            padding-char               => '',
        ),
        negative => %(
            type                       => '',
            minimum-integer-digits     => 0,
            minimum-fractional-digits  => 0,
            minimum-exponential-digits => 0,
            minimum-significant-digits => 0,
           #maximum-integer-digits     => ∞, (not definable in patterns)
           #maximum-fractional-digits  => ∞, (not definable in patterns)
            maximum-significant-digits => 0,
            primary-grouping-size      => 0,
            secondary-grouping-size    => 0,
            fractional-grouping-size   => 0,
            exponential-power-multiple => 0,
            exponential-forces-sign    => False,
            rounding-value             => 0,
            prefix                     => [],
            suffix                     => [],
            padding-type               => '',
            padding-char               => '',
        ),
    ;
    <patterns>
}

# If only one token is given, the negative is identical to the positive,
# but with a minus directly in front of the number
token patterns {
    :my %*d := %*data<positive>;
    $<positive>=<pattern>
    [
        ';'
        :my $*negative = True;
        {%*d := %*data<negative>}
        $<negative>=<pattern>
    ]?
}

# The pattern will be taken in as a sequence of elements
# Each element has unambiguous start values, which allows this
token pattern {
    :my @*fix := %*d<prefix>;
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
token pattern-element:negative     {
    <?{$*negative}>                           # check we're in negative pattern
    '#'
    <before <-[0..9@#,]>>                     # ensure we're not in a regular pattern before...
    { %*data<negative-type> = 'circumfix' }   # setting the negative type
}


token pattern-element:number {
    # Padding is allowed either at the beginning or end of the number pattern
    <padding('before')>?

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

    '.'                # obligatory decimal
    <.handle-grouping>

    [ # Optional fractional parts, cannot be composed entirely of commas.
    | '#' <.dummy-digit>       [',' <.fractional-grouping> ]?
    | '@' <.significant-digit> [',' <.fractional-grouping> ]?
    | <fractional-digit>       [',' <.fractional-grouping> ]?
    ]*
    { %*d<fractional-grouping-size> = $*grouping}

    [
        'E' # if exponential
        <exponential-plus>?
        <exponential-digit>+
    ]?

    # Tidy up:
    # Padding is allowed either at the beginning or end of the number pattern
    # Pattern elements will also now affix to the suffix portion rather than prefix
    # If we reached here in a negative pattern, then we have a fully unique pattern
    <padding('after')>?
    { @*fix := %*d<suffix> }
    { %*data<negative-type> = 'unique' if $*negative }

}
method dummy-digit {
    # The dummy digit normally has no meaning except to provide
    # spacing for grouping size.  However, for scientific numbers,
    # it can provide the multiplier that dictates available powers
    # (e.g. engineering notation only allowing 10^3, 10^6, etc),
    # unless significant digits are used, in which case dummy digits
    # before the significant digit are ignored, and after it (hence
    # the conditional check) are included for the maximum significant digits.
    %*d<exponential-power-multiple>++;
    %*d<maximum-significant-digits>++ if %*d<minimum-significant-digits>;
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
    %*d<minimum-significant-digits>++;
    %*d<maximum-significant-digits>++;
    self
}
method integer-grouping {
    @*grouping.push: $*grouping;
    $*grouping = 0;
    self
}

method handle-grouping {
    # Per the standard, only the final two group sizes are used. For simplicity of implementation,
    # we just capture all of them, and then choose the last two.
    # 0 will be treated as "no groupings" and result in the group code not being inserted
    if @*grouping > 1 {
        (%*d<secondary-grouping-size>, %*d<primary-grouping-size>) = @*grouping.tail(2);
    }elsif @*grouping == 1 {
        %*d<primary-grouping-size>   = @*grouping.tail;
        %*d<secondary-grouping-size> = @*grouping.tail;
    }
    $*grouping = 0;
    self;
}
method handle-integral-digit($digit) {
    %*d<rounding-value> = %*d<rounding-value> * 10 + $digit;
    self
}
method handle-fractional-digit($digit) {
    %*d<rounding-value> = %*d<rounding-value> + $*fractional-multiplier * $digit;
    $*fractional-multiplier /= 10;
    self
}
method fractional-grouping {
    # In the (odd) event that someone uses two fractional groupers, the standard doesn't say
    # how they should be treated.  I'm going to assume that they should be treated in the
    # same way as integral: only the trailing one will be counted.
    %*d<fractional-grouping-size> = $*grouping;
    $*grouping = 0;
    self;
}

token padding($position){
    '*' <(.)>
    {
        %*d<padding-char> = $/.Str;
        %*d<padding-type> = $position
    }
}
# the docs say the pad character can be anything, and that
# that the the * escapes.  So frankly, I'm not sure why
# it *can't* be a single quote per the BNF,
# see http://unicode.org/reports/tr35/tr35-numbers.html#Padding
# Plus the formal definition excludes higher plane stuff which seems silly

token integral-digit    { <[0..9]> { } <.handle-integral-digit(  $/.Str.Int)> }
token fractional-digit  { <[0..9]> { } <.handle-fractional-digit($/.Str.Int)> }
token exponential-digit { <[0..9]> {     %*d<minimum-exponential-digits>++  } }
token exponential-plus  {   '+'    {     %*d<exponential-forces-sign> = True} }
