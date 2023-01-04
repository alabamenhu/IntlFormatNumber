use v6.d;
use User::Language;
multi sub format-number (
    $number,                           #= The number that will be formatted
    :$language = INIT {user-language}, #= The language to use
    :$length   = 'standard',           #= Formatting length, options include standard, short, and long
    :$type     = 'decimal',            #= The number style to use (decimal, percent, scientific, currency)
                         ) is export {
    my $code = $language ~ $type ~ $length;
    state %cache;
    my &formatter  = %cache{$code}
        // %cache{$code} = get-number-formatter($language, $type, $length);

    return formatter $number;
}


sub format-number-rakuast($language, $type, $length) is export(:ast) {
    use Intl::Format::Util::DigitsRAST;
    use Intl::CLDR;
    use Intl::Format::Number::Grammar;
    use Intl::Format::Number::Actions;
    use experimental :rakuast;

    my \numbers  = cldr{$language}.numbers;
    my $num-sys := numbers.numbering-systems.default;
    my $symbols := numbers.symbols{$num-sys};
    my $pattern := numbers{$type ~ "-formats"}{$num-sys}{$length}.pattern;
    my %format  := Grammar.parse($pattern, :actions(Actions)).made;

    my $max-int-digits = 9;
    my $min-int-digits = 1;
    my $group-small = 3;

    my $digits-declare; #= Sets up the digits to use based on numerical system
    my $prefix; #= Determines the prefix for the number

    # my @digits := 0,1,2,3,4,5,6,7,8,9; (or local equivalent)
    $digits-declare := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '@digits',
            initializer => RakuAST::Initializer::Bind.new(%digits{$num-sys})
            )
        );

    my $prefix-sequence;
    for %format<positive-prefix>[] -> $elem {
        $prefix-sequence ~= do given $elem<type> {
            when 'text'     { $elem<text>      }
            when 'percent'  { $symbols.percent }
            when 'minus'    { $symbols.minus   }
            when 'permille' { $symbols.minus   }
        }
    }
    my $suffix-sequence;
    for %format<positive-suffix>[] -> $elem {
        $suffix-sequence ~= do given $elem<type> {
            when 'text'     { $elem<text>      }
            when 'percent'  { $symbols.percent }
            when 'minus'    { $symbols.minus   }
            when 'permille' { $symbols.minus   }
        }
    }

    my $prefix = RakuAST::ApplyInfix.new(
        left => RakuAST::Var::Lexical.new('$result'),
        infix => RakuAST::Infix.new('='),
        right => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$result'),
            infix => RakuAST::Infix.new('~'),
            right => RakuAST::StrLiteral.new($prefix-sequence)
            )
        );

    my $suffix = RakuAST::ApplyInfix.new(
        left => RakuAST::Var::Lexical.new('$result'),
        infix => RakuAST::Infix.new('='),
        right => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$result'),
            infix => RakuAST::Infix.new('~'),
            right => RakuAST::StrLiteral.new($prefix-sequence)
            )
        );

    # check negatives

    # return 'NaN' if \$number === NaN
    my $nan-check := RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('return'),
            args => RakuAST::ArgList.new(
                RakuAST::StrLiteral.new($symbols.nan)
                )
            ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    infix => RakuAST::Infix.new('==='),
                    left => RakuAST::Var::Lexical.new('$number'),
                    right => RakuAST::Var::Lexical::Constant.new('NaN')
                    )
                )
            )
        );

    # return ($show-sign eq 'always' ?? '+Inf' !! 'Inf') if $number === Inf;
    my $pos-inf-check := RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('return'),
            args => RakuAST::ArgList.new(
                RakuAST::Ternary.new(
                    condition => RakuAST::ApplyInfix.new(
                        infix => RakuAST::Infix.new('eq'),
                        left => RakuAST::Var::Lexical.new('$show-sign'),
                        right => RakuAST::StrLiteral.new('always')
                        ),
                    then => RakuAST::StrLiteral.new($symbols.plus ~ $symbols.infinity),
                    else => RakuAST::StrLiteral.new($symbols.infinity),
                    )
                )
            ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    infix => RakuAST::Infix.new('==='),
                    left => RakuAST::Var::Lexical.new('$number'),
                    right => RakuAST::Var::Lexical::Constant.new('Inf')
                    )
                )
            )
        );

    # return ($show-sign eq 'always' ?? '+Inf' !! 'Inf') if $number === Inf;
    my $neg-inf-check := RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('return'),
            args => RakuAST::ArgList.new(
                RakuAST::Ternary.new(
                    condition => RakuAST::ApplyInfix.new(
                        infix => RakuAST::Infix.new('eq'),
                        left => RakuAST::Var::Lexical.new('$show-sign'),
                        right => RakuAST::StrLiteral.new('never')
                        ),
                    then => RakuAST::StrLiteral.new($symbols.infinity),
                    else => RakuAST::StrLiteral.new($symbols.minus ~ $symbols.infinity),
                    )
                )
            ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    infix => RakuAST::Infix.new('==='),
                    left => RakuAST::Var::Lexical.new('$number'),
                    right => RakuAST::ApplyPrefix.new(
                        operand => RakuAST::Var::Lexical::Constant.new('Inf'),
                        prefix => RakuAST::Prefix.new('-')
                        )
                    )
                )
            )
        );

    # my str $result = '';
    my $result-declare := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$result',
            type  => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('Str')
                )
            )
        );

    # my $n = abs n;
    my $no-negative := RakuAST::Statement::Expression.new(
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
        );

    # $n = $n % (10 ** ($max - 1)) if $max != Inf
    my $handle-max-int := RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$n'),
            infix => RakuAST::Infix.new('='),
            right => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$n'),
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
                    infix => RakuAST::Infix.new('=='),
                    right => RakuAST::Var::Lexical::Constant.new('Inf'),
                    )
                )
            )
        );

    # my $power = $n == 0 ?? 1 !! 1 + floor log10 $n
    my $magntiude-declare := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$power',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::Ternary.new(
                    condition => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$n'),
                        infix => RakuAST::Infix.new('=='),
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
                                        RakuAST::Var::Lexical.new('$n')
                                        )
                                    )
                                )
                            )
                        ),
                    )
                )
            )
        );

    my $magntiude-expand := RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$power'),
            infix => RakuAST::Infix.new('='),
            right => RakuAST::Var::Lexical.new('$min-int-digs')
            ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$max-int-digs'),
                    infix => RakuAST::Infix.new('>'),
                    right => RakuAST::Var::Lexical.new('$power'),
                    )
                )
            )
        );


    my $grouper-declare;
    my $grouper-insert;
    if $group-small == 0 {
        $grouper-declare := Empty;
        $grouper-insert  := Empty;
    } else {
        $grouper-declare := RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(
                scope => 'my',
                name  => '$next-grouper',
                initializer => RakuAST::Initializer::Assign.new(
                    RakuAST::IntLiteral.new($group-small)
                    )
                )
            );

        $grouper-insert := RakuAST::Statement::If.new(
            condition => RakuAST::Statement::Expression(
            RakuAST::ApplyInfix.new(
                left => RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Var::Lexical.new('$next-grouper'),
                    postfix => RakuAST::Postfix.new('--')
                    ),
                infix => RakuAST::Infix.new('=='),
                right => RakuAST::IntLiteral(0)
                ),
                then => RakuAST::Block.new(
                    body => RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                expression => RakuAST::ApplyInfix.new(
                                    left => RakuAST::Var::Lexical.new('$next-grouper'),
                                    infix => '=',
                                    right => RakuAST::IntLiteral($group-large)
                                    )
                                ),
                            RakuAST::Statement::Expression.new(
                                expression => RakuAST::ApplyInfix.new(
                                    left => RakuAST::Var::Lexical.new('$result'),
                                    infix => '~',
                                    right => RakuAST::StrLiteral($symbols.group)
                                    )
                                )
                            )
                        )
                    )
            )
        }


    my $int-setup1 := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$int-val',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyDottyInfix.new(
                    infix => class RakuAST::DottyInfix::Call,
                    left => RakuAST::Var::Lexical('$n'),
                    right => RakuAST::Name.from-identifier('Int')
                    )
                )
            )
        );

    my $int-setup2 := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$int-old',
            )
        );

    my $int-setup3 := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            scope => 'my',
            name  => '$digit',
            )
        );


    my @int-loop-statements is List =
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$int-old'),
                infix => '=',
                right => RakuAST::Var::Lexical.new('$int-val'),
                )
            ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$int-val'),
                infix => '=',
                right => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$int-val'),
                    infix => RakuAST::Infix.new('div'),
                    right => RakuAST::IntLiteral.new(10)
                    )
                )
            ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$digit'),
                infix => '=',
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
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$result'),
                infix => '=',
                right => RakuAST::ApplyInfix.new(
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
                    left => RakuAST::Var::Lexical.new('$result'),
                    infix => RakuAST::Infix.new('~'),
                    )
                )
            ),
        ;

    my $int-loop := RakuAST::Statement::For.new(
        source => RakuAST::ApplyPrefix.new(
            prefix => RakuAST::Prefix.new('^'),
            operand => RakuAST::Var::Lexical.new('$power')
            ),
        body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new( |@int-loop-statements )
            )
        );

    my @statements is List =
        $result-declare,   # $result = '';
        $prefix,
        $nan-check,        # return 'NaN' when NaN
        $pos-inf-check,    # return '∞'   when Inf
        $neg-inf-check,    # return '-∞'  when -Inf
        $digits-declare,   # @digits = 0,1,2...
        $no-negative,      # $n = abs $number
        $handle-max-int,   # $n = $n % 10 ** ($max-digs - 1) unless $max-digs == Inf
        $magntiude-declare,# $power = $n == 0 ?? 1 !! 1 + floor log10 $n
        $magntiude-expand, # $power = $min-digs if $min-digs > $power
        $grouper-declare,  # $next-grouper = X (only if > 0)
        $int-setup1,       # my $int-val = $n.Int
        $int-setup2,       # my $int-old;
        $int-setup3,       # my $digit;
        $int-loop,         # for ^power { add-int-digits }




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
            # max-int-digs, an optional named argument
            RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$max-int-digs'),
                names => ['max-int-digs', 'maximum-integer-digits'],
                default => (
                $max-int-digits == Inf
                    ?? RakuAST::Var::Lexical::Constant.new('Inf')
                    !! RakuAST::IntLiteral.new($max-int-digits)
                )
                ),
            # min-int-digs, an optional named argument
            RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$min-int-digs'),
                names => ['min-int-digs', 'minimum-integer-digits'],
                default => (
                $max-int-digits == Inf
                    ?? RakuAST::Var::Lexical::Constant.new('Inf')
                    !! RakuAST::IntLiteral.new($min-int-digits)
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