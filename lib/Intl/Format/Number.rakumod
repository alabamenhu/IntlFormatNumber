use v6.d;
use User::Language;
use experimental :rakuast;

multi sub format-number (
    $number,                           #= The number that will be formatted
    :$language = INIT {user-language}, #= The language to use
    :$length   = 'standard',           #= Formatting length, options include standard, short, and long
    :$type     = 'decimal',            #= The number style to use (decimal, percent, scientific, currency)
) is export {
    my $code = $language ~ $type ~ $length;
    state %cache;
    my $formatter = %cache{$code};
    unless $formatter {
        $formatter = get-number-formatter($language, $type, $length);
        %cache{$code} = $formatter;
    }

    return $formatter($number);
}

constant Op_NumEq   = RakuAST::Infix.new('==');
constant Op_Asgn    = RakuAST::Infix.new('=');
constant Var_N      = RakuAST::Var::Lexical.new('$n');
constant Var_Result = RakuAST::Var::Lexical.new('$result');
constant Var_Digs   = RakuAST::Var::Lexical.new('@digits');

use Intl::LanguageTag:auth<zef:guifa>:ver<0.12.1+>;
sub format-number-rakuast(LanguageTag() $language, $type, $length) is export(:ast) {
    say "creating formatter for $language $type $length";
    use Intl::CLDR:auth<zef:guifa>:ver<0.7.4+>;
    use Intl::Format::Util::Digits;
    use Intl::Format::Number::Grammar;
    use Intl::Format::Number::Actions;
    use experimental :rakuast;


    # Grab initial data.
    my \numbers  = cldr{$language}.numbers;
    say $language.extensions<u><nu> || "!!***!!!";
    my $num-sys := $language.extensions<u><nu>         #= The number system being used
                || numbers.numbering-systems.default;
    my $symbols := numbers.symbols{$num-sys};          #= Symbols data, in hash-y format
    my $pattern := $type ne 'permille'                 #= Pattern data pre-parsing
                ?? numbers{$type ~ '-formats'}{$num-sys}{$length}.pattern
                !! numbers<percent-formats>{$num-sys}{$length}.pattern.subst('%','‰');
    my %format  := Grammar.parse($pattern).made;       #= Pattern data post-parsing


    #——————————————————
    my $digits-declare := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            name => '@digits',
            scope => 'my',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyPostfix.new(
                    postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('list')),
                    operand => RakuAST::ApplyPostfix.new(
                        postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('comb')),
                        operand => RakuAST::StrLiteral.new(%digits{$num-sys}),
                    )
                )
            )
        )
    );

    # $result = PREFIX ~ $result
    #——————————————————
    my $prefix = RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_Result,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => rast-fix(%format, $symbols, :type<prefix>),
                infix => RakuAST::Infix.new('~'),
                right => Var_Result,
            )
        )
    );
    # $result = $result ~ SUFFIX
    #——————————————————
    my $suffix = RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_Result,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_Result,
                infix => RakuAST::Infix.new('~'),
                right => rast-fix(%format, $symbols, :type<suffix>)
            )
        )
    );

    # my Str $result = '';
    my $result-declare := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$result',
            type  => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Str')
            ),
            initializer => RakuAST::Initializer::Assign.new(RakuAST::StrLiteral.new(''))
        )
    );


    # Prefix
    #    $neg-unique ?? ($neg ?? int-neg !! int-pos) !! int
    #   decimal?
    #    $neg-unique ?? ($neg ?? fra-neg !! fra-pos) !! fra
    #   exp? ($neg-unique ?? ($neg ?? fra-exp !! fra-exp) !! exp)
    # Suffix



    my @statements is List =
        $result-declare,   # $result = '';
        $digits-declare,   # @digits = 0,1,2...
        rast-abs-n,
        rast-percent($type),
        rast-number(%format, $symbols),
        $prefix,
        $suffix,
    ;

    RakuAST::Sub.new(
        name => RakuAST::Name.from-identifier('format-number'),
        signature => RakuAST::Signature.new(
            parameters => (
                # The number, the required positional
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$number'),
                ),
                # show-sign, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$show-sign'),
                    names => ['show-sign'],
                    default => RakuAST::StrLiteral.new('default')
                ),
                # show-decimal, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$show-decimal'),
                    names => ['show-decimal'],
                    default => RakuAST::Term::Name.new(RakuAST::Name.from-identifier('False'))
                ),
                # max-int-digs, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$max-int-digs'),
                    names => ['max-int-digs', 'maximum-integer-digits'],
                    default => RakuAST::Var::Lexical::Constant.new('Inf')
                ),
                # min-int-digs, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$min-int-digs'),
                    names => ['min-int-digs', 'minimum-integer-digits'],
                    default => (
                        %format<positive><minimum-integer-digits> == Inf
                            ?? RakuAST::Var::Lexical::Constant.new('Inf')
                            !! RakuAST::IntLiteral.new(%format<positive><minimum-integer-digits>)
                    )
                ),
                # max-frac-digs, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$max-frac-digs'),
                    names => ['max-frac-digs', 'maximum-fractional-digits'],
                    default => RakuAST::Var::Lexical::Constant.new('Inf')
                ),
                # min-int-digs, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$min-frac-digs'),
                    names => ['min-frac-digs', 'minimum-fractional-digits'],
                    default => RakuAST::IntLiteral.new(%format<positive><minimum-integer-digits>)
                ),
                # show-sign, an optional named argument
                RakuAST::Parameter.new(
                    target => RakuAST::ParameterTarget::Var.new('$show-plus'),
                    names => ['show-plus'],
                    default => RakuAST::Term::Name.new(
                        RakuAST::Name.from-identifier('False')
                    )
                ),
            ),
        ),
        body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(|@statements)
        )
    )
}

sub get-number-formatter(|c) {
    use MONKEY-SEE-NO-EVAL;
    EVAL format-number-rakuast |c;
}



sub rast-fix(%data, \symbols, :$type!) {
    # There are two factors in generating the prefix.
    # (1) What is the negative style?
    #     (a) simple (negative and positive use the same)
    #     (b) circumfix or unique (use different, so run twice and wrap in a conditional)
    # (2) Can merge all text that's not a - (assume a simple terminal - if there isn't one and it's simple)

    if %data<negative-type> eq 'simple' {
        my $result = rast-text(%data<positive>{$type}, symbols, :auto-negative($type eq 'prefix'));
        return $result;
    } else {
        # if $number < 0 || $show-plus { NEG } else { POS }
        return RakuAST::Ternary.new(
            condition => RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$number'),
                        infix => RakuAST::Infix.new('<'),
                        right => RakuAST::IntLiteral(0)
                    ),
                    infix => RakuAST::Infix.new('||'),
                    right => RakuAST::Var::Lexical.new('$show-plus'),
                )
            ),
            then => rast-text(%data<negative>{$type}, symbols),
            else => rast-text(%data<positive>{$type}, symbols),
        );
    }
}

sub rast-sign(\symbols)  {
    RakuAST::Ternary.new(
        condition => RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$number'),
                infix => RakuAST::Infix.new('<'),
                right => RakuAST::IntLiteral.new(0)
            )
        ),
        then => RakuAST::StrLiteral.new(symbols.minus),
        else => RakuAST::Ternary.new(
            condition => RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new('$show-plus'),
            ),
            then => RakuAST::StrLiteral.new(symbols.plus),
            else => RakuAST::StrLiteral.new('')
        ),
    )
}

sub rast-text(@fix, \symbols, :$auto-negative = False) {
    my $standard-sign = True;
    my @strs;

    for @fix {
        FIRST my $str = '';
        LAST @strs.push(RakuAST::StrLiteral.new($str)) if $str;

        if .<type> eq 'text' {
            $str ~= .<text>
        } elsif .<symb> eq 'percent' {
            @strs.push: RakuAST::StrLiteral.new(symbols.percent)
        } elsif .<symb> eq 'permille' {
            @strs.push: RakuAST::StrLiteral.new(symbols.permille)
        } else #`[ .<symb> eq 'minus'] {
            # Push current string
            @strs.push: RakuAST::StrLiteral.new($str);
            @strs.push: rast-sign(symbols);

            # (re)set variables
            $str = '';
            $standard-sign = False;
        }
    }

    @strs.push: rast-sign(symbols) if $auto-negative && $standard-sign;

    # wrap with string joins
    return RakuAST::ApplyListInfix.new(
        infix => RakuAST::Infix.new('~'),
        operands => @strs.List
    );
}


sub rast-number (%data, \symbols) {
    # The number has potentially five components:
    #   (0) if exponential/percent, preprocess
    #   (1) integer digits
    #   (2) decimal
    #   (3) fractional digits
    #   (4) exponential symbol
    #   (5) exponent
    # It's also possible for special values (NaN, neg/positive infinity) which are replaced by the symbol

    RakuAST::Statement::If.new(
        condition => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => RakuAST::Infix.new('==='),
            right => RakuAST::Var::Lexical::Constant.new('NaN')
        ),
        then => block-blockoid-statement-list(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => Var_Result,
                    infix => Op_Asgn,
                    right => RakuAST::ApplyInfix.new(
                        left => Var_Result,
                        infix => RakuAST::Infix.new('~'),
                        right => RakuAST::StrLiteral.new(symbols.nan)
                    )
                )
            )
        ),
        elsifs => [
            RakuAST::Statement::Elsif.new(
                condition => RakuAST::ApplyInfix.new(
                    left => Var_N,
                    infix => RakuAST::Infix.new('==='),
                    right => RakuAST::Var::Lexical::Constant.new('Inf'),
                ),
                then => block-blockoid-statement-list(
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(
                            left => Var_Result,
                            infix => Op_Asgn,
                            right => RakuAST::ApplyInfix.new(
                                left => Var_Result,
                                infix => RakuAST::Infix.new('~'),
                                right => RakuAST::StrLiteral.new(symbols.infinity)
                            ),
                        ),
                    ),
                ),
            ),
        ],
        else => block-blockoid-statement-list(
            rast-max-int(),
           |rast-integer(%data<positive>, symbols),
           |rast-setup-fract(),
           #|rast-decimal(symbols),
           |rast-fract(symbols),
        )
    );
}

sub block-blockoid-statement-list(+@items) is pure {
    RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new: |@items
        )
    )
}


#| my $n = abs $number;
sub rast-abs-n is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$n',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('abs'),
                    args => RakuAST::ArgList.new(
                        RakuAST::Var::Lexical.new('$number')
                    )
                )
            )
        )
    )
}

#| $n = $n % (10 ** ($max - 1)) if $max != Inf
sub rast-max-int is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => RakuAST::Infix.new('%'),
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::IntLiteral.new(10),
                    infix => RakuAST::Infix.new('**'),
                    right => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$max-int-digs'),
                        infix => RakuAST::Infix.new('-'),
                        right => RakuAST::IntLiteral.new(1)
                    )
                )
            )
        ),
        condition-modifier => RakuAST::StatementModifier::Unless.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$max-int-digs'),
                    infix => Op_NumEq,
                    right => RakuAST::Var::Lexical::Constant.new('Inf'),
                )
            )
        )
    )
}

#| MAGNITUDE HEADER: is pure
#|
#| my $power = $n == 0 ?? 1 !! 1 + floor log10 $n;
#| $power = $max-int-digs if $min-int-digs > $power;
sub rast-magnitude is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$power',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::Ternary.new(
                    condition => RakuAST::ApplyInfix.new(
                        left => Var_N,
                        infix => Op_NumEq,
                        right => RakuAST::IntLiteral.new(0)
                        ),
                    then => RakuAST::IntLiteral.new(1),
                    else => RakuAST::ApplyInfix.new(
                        left => RakuAST::IntLiteral.new(1),
                        infix => RakuAST::Infix.new('+'),
                        right => RakuAST::Call::Name.new(
                            name => RakuAST::Name.from-identifier('floor'),
                            args => RakuAST::ArgList.new(
                                RakuAST::Call::Name.new(
                                    name => RakuAST::Name.from-identifier('log10'),
                                    args => RakuAST::ArgList.new(
                                        Var_N
                                    )
                                )
                            )
                        )
                    ),
                )
            )
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$power'),
            infix => Op_Asgn,
            right => RakuAST::Var::Lexical.new('$min-int-digs')
        ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$min-int-digs'),
                    infix => RakuAST::Infix.new('>'),
                    right => RakuAST::Var::Lexical.new('$power'),
                )
            )
        )
    )
}

#| INTEGER CODE
#| $n %= 10**(MAX-INT-DIGITS - 1) if $max-int-digits ne Inf; # ] Defined in rast-clamp
#|
#| my int $power = $n == 0 ?? 1 !! 1 + floor log10 $n;       # ⎤ Defined in rast-magnitude
#| $power = $min-int-digs if $max-int-digs > $power;         # ⎦
#|
#| my int $next-grouper = GROUP-SMALL;                       # ] Defined in rast-int-grouper-declare (only if group > 0)
#|
#| # INTEGER PORTION
#| my $int-val = $n.Int;                                     # ⎤ ⎤ Defined in this sub as @int-setup
#| my $int-old;                                              # ⎥ ⎥
#| my int $digit;                                            # ⎥ ⎦
#| for ^$power {                                             # ⎥ ⎤ Defined in this sub as $int-loop
#|     $int-old = $int-val;                                  # ⎥ ⎥
#|     $int-val = $int-val div 10;                           # ⎥ ⎥
#|     $digit   = $int-old - $int-val * 10;                  # ⎥ ⎥
#|     if $next-grouper-- == 0 {                             # ⎥ ⎥ ⎤ Defined in rast-int-grouper-insert
#|         $next-grouper = GROUP-LARGE - 1;                  # ⎥ ⎥ ⎥ (only used if group > 0)
#|         $result = $symbols.group.raku ~ $result;          # ⎥ ⎥ ⎥
#|     }                                                     # ⎥ ⎥ ⎦
#|     $result = @digits[$digit] ~ $result;                  # ⎥ ⎥
#| }                                                         # ⎦ ⎦
#|
sub rast-integer($*pattern, $*symbols) {
    my $needs-grouper = so $*pattern<primary-grouping-size>;

    my $clamper          = rast-clamp($*pattern);
    my @magnitude       := rast-magnitude;
    my $grouper-declare  = $needs-grouper ?? rast-int-grouper-declare() !! Empty;
    my $grouper-insert   = $needs-grouper ?? rast-int-grouper-insert()  !! Empty;

    my @int-setup = RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$int-val',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyPostfix.new(
                    operand => Var_N,
                    postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('Int')
                    )
                )
            )
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$int-old',
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$digit',
        )
    );

    my @int-loop-statements =
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$int-old'),
                infix => Op_Asgn,
                right => RakuAST::Var::Lexical.new('$int-val'),
            )
        ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$int-val'),
                infix => Op_Asgn,
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$int-val'),
                    infix => RakuAST::Infix.new('div'),
                    right => RakuAST::IntLiteral.new(10)
                )
            )
        ),
        # $digit = $int-old - $int-val * 10;
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$digit'),
                infix => Op_Asgn,
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$int-old'),
                    infix => RakuAST::Infix.new('-'),
                    right => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$int-val'),
                        infix => RakuAST::Infix.new('*'),
                        right => RakuAST::IntLiteral.new(10)
                    )
                )
            )
        ),
        $grouper-insert,
        # $result = @digits[$digit] ~ $result;
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => Var_Result,
                infix => Op_Asgn,
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::ApplyPostfix.new(
                        operand => RakuAST::Var::Lexical.new('@digits'),
                        postfix => RakuAST::Postcircumfix::ArrayIndex.new(
                            RakuAST::SemiList.new(
                                RakuAST::Statement::Expression.new(
                                    expression => RakuAST::Var::Lexical.new('$digit')
                                )
                            )
                        )
                    ),
                    right => Var_Result,
                    infix => RakuAST::Infix.new('~'),
                )
            )
        ),
    ;

    # for ^power { … }
    my $int-loop = RakuAST::Statement::For.new(
        source => RakuAST::ApplyPrefix.new(
            prefix => RakuAST::Prefix.new('^'),
            operand => RakuAST::Var::Lexical.new('$power')
        ),
        body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(|@int-loop-statements)
            )
        )
    );

    return $clamper, |@magnitude, $grouper-declare, |@int-setup, $int-loop;
}

#| my $next-grouper = PRIMARY-GROUPING-SIZE   # $*pattern defined in caller
sub rast-int-grouper-declare {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$next-grouper',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::IntLiteral.new($*pattern<primary-grouping-size> + 1)
            )
        )
    )
}

#| if $next-grouper-- == 0 {
#|     $next-grouper = SECONDARY-GROUPING-SIZE;
#|     $result ~ GROUP;
sub rast-int-grouper-insert {
    RakuAST::Statement::If.new(
        condition => RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::ApplyPrefix.new(
                    operand => RakuAST::Var::Lexical.new('$next-grouper'),
                    prefix => RakuAST::Prefix.new('--')
                ),
                infix => Op_NumEq,
                right => RakuAST::IntLiteral.new(0)
            ),
        ),
        then => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    # $next-grouper = SECONDARY-GROUPING-SIZE
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$next-grouper'),
                            infix => Op_Asgn,
                            right => RakuAST::IntLiteral.new($*pattern<secondary-grouping-size>)
                        )
                    ),
                    # $result = $result ~ SYMBOLS.GROUP
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(
                            left => Var_Result,
                            infix => Op_Asgn,
                            right => RakuAST::ApplyInfix.new(
                                right => Var_Result,
                                infix => RakuAST::Infix.new('~'),
                                left => RakuAST::StrLiteral.new($*symbols.group)
                            )
                        )
                    ),
                )
            )
        )
    )
}

#| $n %= 10**(MAX-INT-DIGITS - 1) if $max-int-digits < Inf;
sub rast-clamp($*pattern) {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => RakuAST::Infix.new('%'),
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::IntLiteral.new(10),
                    infix => RakuAST::Infix.new('**'),
                    right => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$max-int-digs'),
                        infix => RakuAST::Infix.new('-'),
                        right => RakuAST::IntLiteral.new(1)
                    )
                )
            )
        ),
        condition-modifier => RakuAST::StatementModifier::Unless.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$max-int-digs'),
                    infix => RakuAST::Infix.new('==='),
                    right => RakuAST::Var::Lexical::Constant.new('Inf'),
                )
            )
        )
    )
}

sub rastlog($start, $var, $end = '') {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::ApplyInfix.new(
                left => RakuAST::StrLiteral.new($start),
                infix => RakuAST::Infix.new('~'),
                right => RakuAST::ApplyInfix.new(
                    right => RakuAST::StrLiteral.new($end),
                    infix => RakuAST::Infix.new('~'),
                    left => RakuAST::Var::Lexical.new($var)
                )
            ),
            postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('say'))
        )
    )
}

#| $n -= $n.Int;
#| my $limit = $*TOLERANCE;
sub rast-setup-fract is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => RakuAST::Infix.new('-'),
                right => RakuAST::ApplyPostfix.new(
                    operand => Var_N,
                    postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('Int'))
                )
            ),
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name => '$limit',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::Var::Dynamic.new('$*TOLERANCE')
            )
        )
    )
}

sub rast-decimal(\symbols) {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_Result,
            infix => Op_Asgn,
            right =>  RakuAST::ApplyInfix.new(
                left => Var_Result,
                infix => RakuAST::Infix.new('~'),
                right => RakuAST::StrLiteral.new(symbols.decimal)
            )
        ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    right => RakuAST::Var::Lexical.new('$show-decimal'),
                    infix => RakuAST::Infix.new('||'),
                    left => RakuAST::ApplyInfix.new(
                        # TODO: handle rounding here potentially
                        right => Var_N,
                        infix => RakuAST::Infix.new('>'),
                        left => RakuAST::Var::Lexical.new('$limit'),
                    )
                )
            )
        )
    )
}

sub rast-fract(\symbols) {
    #=  if $n > $limit && $max-frac-digits > 0 {
    #=      for ^{$min-frac-digits} {
    #=          $n = $n * 10;
    #=          $digit = floor $n;
    #=          $n = $n - $digit;
    #=          $result = $result ~ @digits[$digit];
    #=      };
    #=      $limit = $limit * {10 ** $min-frac-digits};
    #=      for ^{$max-frac-digits - $min-frac-digits} {
    #=          last if $n < $limit;
    #=          $n = $n * 10;
    #=          $digit = floor $n;
    #=          $n = $n - $digit;
    #=          $result = $result ~ @digits[$digit];
    #=          $limit = $limit * 10;
    #=      }
    #= } else {
    #=     $result = $result ~ {$symbols.decimal} if $show-decimal
    #= }
    RakuAST::Statement::If.new(
        condition => RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                infix => RakuAST::Infix.new('&&'),
                left =>  RakuAST::ApplyInfix.new(
                    left => Var_N,
                    infix => RakuAST::Infix.new('>'),
                    right => RakuAST::Var::Lexical.new('$limit')
                ),
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$max-frac-digs'),
                    infix => RakuAST::Infix.new('>'),
                    right => RakuAST::IntLiteral.new(0)
                )
            )
        ),
        then => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(
                            left => Var_Result,
                            infix => Op_Asgn,
                            right =>  RakuAST::ApplyInfix.new(
                                left => Var_Result,
                                infix => RakuAST::Infix.new('~'),
                                right => RakuAST::StrLiteral.new(symbols.decimal)
                            )
                        ),
                    ),
                    RakuAST::Statement::For.new(
                        source => RakuAST::ApplyPrefix.new(
                            prefix => RakuAST::Prefix.new('^'),
                            operand => RakuAST::Var::Lexical.new('$min-frac-digs')
                        ),
                        body => RakuAST::Block.new( body => RakuAST::Blockoid.new(
                            RakuAST::StatementList.new(
                                |rast-fract-inner-loop()
                            )
                        ))
                    ),
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$limit'),
                            infix => Op_Asgn,
                            right => RakuAST::ApplyInfix.new(
                                left => RakuAST::Var::Lexical.new('$limit'),
                                infix => RakuAST::Infix.new('*'),
                                right => RakuAST::ApplyInfix.new(
                                    right => RakuAST::Var::Lexical.new('$min-frac-digs'),
                                    infix => RakuAST::Infix.new('**'),
                                    left => RakuAST::IntLiteral.new(10)
                                )
                            )
                        )
                    ),
                    RakuAST::Statement::For.new(
                        source => RakuAST::ApplyPrefix.new(
                            prefix => RakuAST::Prefix.new('^'),
                            operand => RakuAST::Var::Lexical.new('$max-frac-digs')
                        ),
                        body => RakuAST::Block.new( body => RakuAST::Blockoid.new(
                            RakuAST::StatementList.new(
                                RakuAST::Statement::Expression.new(
                                    expression => RakuAST::Call::Name.new(
                                        name => RakuAST::Name.from-identifier('last'),
                                    ),
                                    condition-modifier => RakuAST::StatementModifier::If.new(
                                        RakuAST::Statement::Expression.new(
                                            expression => RakuAST::ApplyInfix.new(
                                                left => Var_N,
                                                infix => RakuAST::Infix.new('<'),
                                                right => RakuAST::Var::Lexical.new('$limit'),
                                            )
                                        )
                                    )
                                ),
                                |rast-fract-inner-loop(),
                                RakuAST::Statement::Expression.new(
                                    expression => RakuAST::ApplyInfix.new(
                                        left => RakuAST::Var::Lexical.new('$limit'),
                                        infix => Op_Asgn,
                                        right => RakuAST::ApplyInfix.new(
                                            left => RakuAST::Var::Lexical.new('$limit'),
                                            infix => RakuAST::Infix.new('*'),
                                            right => RakuAST::IntLiteral.new(10)
                                        )
                                    )
                                )
                            )
                        ))
                    )
                ),
            )
        ),
        else => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(
                            left => Var_Result,
                            infix => Op_Asgn,
                            right =>  RakuAST::ApplyInfix.new(
                                left => Var_Result,
                                infix => RakuAST::Infix.new('~'),
                                right => RakuAST::StrLiteral.new(symbols.decimal)
                            )
                        ),
                        condition-modifier => RakuAST::StatementModifier::If.new(
                            RakuAST::Statement::Expression.new(
                                expression => RakuAST::Var::Lexical.new('$show-decimal')
                            )
                        )
                    ),
                )
            )
        )
    )
}

sub rast-fract-inner-loop is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => RakuAST::Infix.new('*'),
                right => RakuAST::IntLiteral.new(10)
            )
        ),
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$digit'),
            infix => Op_Asgn,
            right => RakuAST::ApplyPostfix.new(
                operand => Var_N,
                postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('floor'))
            )
        ),
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => RakuAST::Infix.new('-'),
                right => RakuAST::Var::Lexical.new('$digit')
            )
        ),
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_Result,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_Result,
                infix => RakuAST::Infix.new('~'),
                right => RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Var::Lexical.new('@digits'),
                    postfix => RakuAST::Postcircumfix::ArrayIndex.new(
                        RakuAST::SemiList.new(
                            RakuAST::Statement::Expression.new(
                                expression => RakuAST::Var::Lexical.new('$digit')
                            )
                        )
                    )
                ),
            )
        ),
    ),
}

sub rast-percent($type) {
    if $type eq 'percent' || $type eq 'permille' {
        return RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => Op_Asgn,
                right => RakuAST::ApplyInfix.new(
                    left => Var_N,
                    infix => RakuAST::Infix.new('*'),
                    right => RakuAST::IntLiteral.new($type eq 'percent' ?? 100 !! 1000)
                )
            )
        )
    }

    return Empty
}


sub rast-exponential(\symbols) {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_Result,
            infix => Op_Asgn,
            right => RakuAST::ApplyInfix.new(
                left => Var_Result,
                infix => RakuAST::Infix.new('~'),
                right => RakuAST::StrLiteral.new(symbols.exponential)
            )
        )
    ),

}