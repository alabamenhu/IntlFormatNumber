use v6.d;
use Intl::LanguageTag:auth<zef:guifa>:ver<0.12.3+>;
use User::Language:auth<zef:guifa>:ver<0.5.1+>;
use experimental :rakuast;

subset NumberFormatLength of Str where 'standard' | 'short' | 'long';
subset NumberFormatType   of Str where 'decimal'  | 'percent' | 'permille' | 'scientific' | 'engineering';

#| Formats a number appropriately for the language
multi sub format-number (
    Numeric()           $number,                          #= The number that will be formatted
    LanguageTag()      :$language = INIT {user-language}, #= The language to use
    NumberFormatLength :$length   = 'standard',           #= Formatting length, options include standard, short, and long
    NumberFormatType   :$type     = 'decimal',            #= The number style to use (decimal, percent, scientific, currency)
) is export {
    state %cache;
    my $code := $language ~ $type ~ $length;
    my $formatter := %cache{$code};
    return $formatter($number) if $formatter;

    # The code wasn't cached so we have to prepare data and generate it
    use Intl::CLDR:auth<zef:guifa>:ver<0.7.4+>;
    use Intl::Format::Number::Grammar;
    my \numbers := cldr{$language}.numbers;
    my \num-sys := $language.extensions<u><nu>                                          #= The number system being used
                || numbers.numbering-systems.default;
    my \symbols := numbers.symbols{num-sys};                                            #= Symbols data, in hash-y format
    my \pattern := $type ne 'permille'                                                  #= Pattern data pre-parsing
                ?? numbers{$type ~ '-formats'}{num-sys}{$length}.pattern
                !! numbers<percent-formats>{num-sys}{$length}.pattern.subst('%','‰');
    my \format  := Grammar.parse(pattern).made;                                         #= Pattern data post-parsing

    use MONKEY-SEE-NO-EVAL; # RakuAST is by definition safe
    $formatter := EVAL format-number-rakuast(format, $type, num-sys, symbols);
    %cache{$code} := $formatter;
    return $formatter($number);
}

sub local-number-formatter(
    LanguageTag()      :$language = INIT {user-language}, #= The language to use
    NumberFormatLength :$length   = 'standard',           #= Formatting length, options include standard, short, and long
    NumberFormatType   :$type     = 'decimal',            #= The number style to use (decimal, percent, scientific, currency)
    *%options,
) is export {
    use Intl::CLDR:auth<zef:guifa>:ver<0.7.4+>;
    use Intl::Format::Number::Grammar;
    my \numbers := cldr{$language}.numbers;
    my \num-sys := $language.extensions<u><nu>                                          #= The number system being used
                || numbers.numbering-systems.default;
    my \symbols := numbers.symbols{num-sys};                                            #= Symbols data, in hash-y format
    my \pattern := $type ne 'permille'                                                  #= Pattern data pre-parsing
                ?? numbers{$type ~ '-formats'}{num-sys}{$length}.pattern
                !! numbers<percent-formats>{num-sys}{$length}.pattern.subst('%','‰');
    my \format  := Grammar.parse(pattern).made;                                         #= Pattern data post-parsing
    handle-options(format,%options);
    use MONKEY-SEE-NO-EVAL; # RakuAST is safe
    return EVAL format-number-rakuast(format, $type, num-sys, symbols)
}

#| Obtains a number formatter, optionally as a RakuAST sub
sub number-formatter($pattern, :$rast = False, *%options ) is export {
    use Intl::Format::Number::Grammar;
    my %format = Grammar.parse($pattern).made;
    handle-options(%format, %options); #= Pattern data post-parsing
    my $node := format-number-rakuast(%format, %format<type>, %format<number-system>, %format<symbols>);
    use MONKEY-SEE-NO-EVAL; # we are safe to use this here
    return $rast ?? $node !! (EVAL $node);
}


# Generate the following using the following (from cldr supplement)
# "numberingSystems.xml".IO.lines.grep(/numeric/)
#     .map(*.match: /'id="'(<.alpha>+).*?'digits="'['&#x'(<[0..9A..F]>+)|(.)]/)
#     .map({':'~.[0]~'('~(.[1].substr(0,1) eq <123456789ABCDEF>.comb.any ?? .[1].Str.parse-base(16) !! .[1].ord)~'), '})
#| The offset for the 0 digit for a number system
constant %digit-offset =  :adlm(125264),    :ahom(71472),     :arab(1632),     :arabext(1776),    :bali(6992),
        :beng(2534),      :bhks(72784),     :brah(69734),     :cakm(69942),    :cham(43600),      :deva(2406),
        :diak(72016),     :fullwide(65296), :gong(73120),     :gonm(73040),    :gujr(2790),       :guru(2662),
        :hanidec(12295),  :hmng(93008),     :hmnp(123200),    :java(43472),    :kali(43264),      :kawi(73552),
        :khmr(6112),      :knda(3302),      :lana(6784),      :lanatham(6800), :laoo(3792),       :latn(48),
        :lepc(7232),      :limb(6470),      :mathbold(120782),:mathdbl(120792),:mathmono(120822), :mathsanb(120812),
        :mathsans(120802),:mlym(3430),      :modi(71248),     :mong(6160),     :mroo(92768),      :mtei(44016),
        :mymr(4160),      :mymrshan(4240),  :mymrtlng(43504), :nagm(124144),   :newa(70736),      :nkoo(1984),
        :olck(7248),      :orya(2918),      :osma(66720),     :rohg(68912),    :saur(43216),      :segment(130032),
        :shrd(70096),     :sind(70384),     :sinh(3558),      :sora(69872),    :sund(7088),       :takr(71360),
        :talu(6608),      :tamldec(3046),   :telu(3174),      :thai(3664),     :tibt(3872),       :tirh(70864),
        :tnsa(92864),     :vaii(42528),     :wara(71904),     :wcho(123632);

#| Sanity checks the formatting options,
sub handle-options(%format is raw, %options) {
    for %options.kv -> $key, $value {
        given $key {
            when 'maximum-integer-digits'     | 'max-int-digs'  { %format<maximum-integer-digits>     = $value      }
            when 'minimum-integer-digits'     | 'min-int-digs'  { %format<minimum-integer-digits>     = $value      }
            when 'maximum-fractional-digits'  | 'max-frac-digs' { %format<maximum-fractional-digits>  = $value      }
            when 'minimum-fractional-digits'  | 'min-frac-digs' { %format<minimum-fractional-digits>  = $value      }
            when 'maximum-significant-digits' | 'max-sig-digs'  { %format<maximum-significant-digits> = $value      }
            when 'minimum-significant-digits' | 'min-sig-digs'  { %format<minimum-significant-digits> = $value      }
            when 'minimum-exponential-digits' | 'min-exp-digs'  { %format<minimum-exponential-digits> = $value      }
            when 'exponential-power-multiple'                   { %format<exponential-power-multiple> = $value      }
            when 'type'                                         { %format<type>                       = $value      }
            when 'digits'                                       { %format<digits>                     = $value.list }
        }
    }

    # Sanity checks
    with %format {
        # Bad values
        if .<primary-grouping-size> < 0 {
            warn "Primary grouping size ({.<primary-grouping-size>}) cannot be less than zero.\n"
               ~ "Setting it to 0.";
           .<primary-grouping-size> = 0
        }
        if .<secondary-grouping-size> < 0 {
            warn "Secondary grouping size ({.<secondary-grouping-size>}) cannot be less than zero.\n"
               ~ "Setting it to 0.";
           .<secondary-grouping-size> = 0
        }
        if .<fractional-grouping-size> < 0 {
            warn "Fractional grouping size ({.<fractional-grouping-size>}) cannot be less than zero.\n"
               ~ "Setting it to 0.";
           .<fractional-grouping-size> = 0
        }
        if .<minimum-exponential-digits> < 0 {
            warn "Minimum exponential digits ({.<minimum-exponential-digits>}) cannot be less than zero.\n"
               ~ "Setting it to 0.";
           .<minimum-exponential-digits> = 0
        }
        if .<exponential-power-multiple> < 0 {
            warn "Exponential power multiple ({.<exponential-power-multiple>}) cannot be less than zero.\n"
               ~ "Setting it to 0.";
           .<exponential-power-multiple> = 0
        }


        # Incompatible values
        if .<maximum-integer-digits> < .<minimum-integer-digits> {
            warn "Maximum integer digits ({.<maximum-integer-digits>}) cannot be less than minimum integer digits ({.<minimum-integer-digits>}).\n"
               ~ "Setting them both to {.<minimum-integer-digits> ≥ 0 ?? .<minimum-integer-digits> !! '0 because they cannot be negative.'}";
            .<maximum-integer-digits> = .<minimum-integer-digits> max 0;
        }
        if .<maximum-fractional-digits> < .<minimum-fractional-digits> max 0 {
            warn "Maximum fractional digits ({.<maximum-fractional-digits>}) cannot be less than minimum fractional digits ({.<minimum-fractional-digits>}).\n"
               ~ "Setting them both to {.<minimum-fractional-digits>}";
           .<maximum-fractional-digits> = .<minimum-fractional-digits>
        }
        if .<maximum-significant-digits> < .<minimum-significant-digits> max 0 {
            warn "Maximum significant digits ({.<maximum-significant-digits>}) cannot be less than minimum significant digits ({.<minimum-significant-digits>}).\n"
               ~ "Setting them both to {.<minimum-significant-digits>}";
           .<maximum-significant-digits> = .<minimum-significant-digits>
        }
        if .<type> ne any <decimal percent permille scientific engineering> {
            warn "Number format type ({.<type>}) must be any of 'decimal', 'percent' 'permille', or 'scientific'.\n"
               ~ "Setting it to 'decimal'";
           .<type> = 'decimal'
        }
        unless .<number-system>:exists {
            # no warn, this is expected
            .<number-system> = 'latn';
        }
        if .<number-system> !(elem) %digit-offset.keys {
            warn "Number system should be a valid CLDR identifier for digits.\n"
               ~ "Setting it to 'latn' (Latin / Western Arabic)";
            .<digits> = 'latn';
        }
        unless .<symbols>:exists {
            .<symbols> = %();
        }
        constant default-symbols = %(
            :decimal<.>, :group<,>,
            :exponential<E>, :superscripting-exponent<^>,
            :minus<->, :plus<+>,
            :nan<NaN>, :infinity<∞>,
            :percent<%>, :permille<‰>,
            :approximately<~>, :currency-decimal<,>,
            :currency-group<,>
        );
        for default-symbols.kv -> $key, $value {
            .<symbols>{$key} //= $value;
        }
    }
}



##########################
# HELPER SUBS AND VALUES #
################################################################
# The following subs and constants make life much easier when  #
# working with RakuAST. The constants make things look cleaner #
# and more performant, and the subs reduce indentation levels  #
# although they don't actually improve performance.            #
################################################################

#| Wraps a list in a block with required intermediate elements
sub block-blockoid-statement-list(*@items) is pure {
    RakuAST::Block.new(
        body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new: |@items
        )
    )
}

#| Wraps a list in a blockoid with required intermediate elements
sub blockoid-statement-list(*@items) is pure {
    RakuAST::Blockoid.new(RakuAST::StatementList.new: |@items)
}

# These constants make life so much easier
constant NumEqual      = RakuAST::Infix.new('==');
constant ExactlyEqual  = RakuAST::Infix.new('===');
constant Or            = RakuAST::Infix.new('||');
constant And           = RakuAST::Infix.new('&&');
constant Assign        = RakuAST::Infix.new(':=');
constant Divide        = RakuAST::Infix.new('/');
constant IntDivide     = RakuAST::Infix.new('div');
constant Minus         = RakuAST::Infix.new('-');
constant Plus          = RakuAST::Infix.new('+');
constant Times         = RakuAST::Infix.new('*');
constant Greater       = RakuAST::Infix.new('>');
constant GreaterEqual  = RakuAST::Infix.new('>=');
constant LessThan      = RakuAST::Infix.new('<');
constant Divisible     = RakuAST::Infix.new('%%');
constant Mod           = RakuAST::Infix.new('%');
constant Power         = RakuAST::Infix.new('**');
constant Concat        = RakuAST::Infix.new('~');
constant Predecrement  = RakuAST::Prefix.new('--');
constant ZeroToN       = RakuAST::Prefix.new('^');
constant One           = RakuAST::IntLiteral.new(1);
constant Ten           = RakuAST::IntLiteral.new(10);
constant Zero          = RakuAST::IntLiteral.new(0);
constant Var_N         = RakuAST::Var::Lexical.new('$n');
constant Result        = RakuAST::Var::Lexical.new('$result');
#constant MinSigDigs    = RakuAST::Var::Lexical.new('$MIN-SIG-DIGS');
#constant LocalDigits   = RakuAST::Var::Lexical::Constant.new('digits');
#constant LatinDigits   = RakuAST::Var::Lexical::Constant.new('latn-digits');
constant Infinity      = RakuAST::NumLiteral.new(Inf);
constant NotNumber     = RakuAST::NumLiteral.new(NaN);
constant Tolerance     = RakuAST::Var::Dynamic.new('$*TOLERANCE');
constant CallLast      = RakuAST::Call::Name.new:   name => RakuAST::Name.from-identifier('last');
constant FloorMethod   = RakuAST::Call::Method.new: name => RakuAST::Name.from-identifier('floor');
constant CeilingMethod = RakuAST::Call::Method.new: name => RakuAST::Name.from-identifier('ceiling');
constant AbsMethod     = RakuAST::Call::Method.new: name => RakuAST::Name.from-identifier('abs');
constant Log10Method   = RakuAST::Call::Method.new: name => RakuAST::Name.from-identifier('log10');
constant MakeInt       = RakuAST::Call::Method.new: name => RakuAST::Name.from-identifier('Int');
constant MakeStr       = RakuAST::Call::Method.new: name => RakuAST::Name.from-identifier('Str');
constant RAST-ImplicitConstant = RakuAST::VarDeclaration::Implicit::Constant;
constant Var_X = RakuAST::Var::Lexical.new('$x');
constant Var_C = RakuAST::Var::Lexical.new('$c');
constant Var_R = RakuAST::Var::Lexical.new('$r');


use MONKEY-SEE-NO-EVAL;



sub format-number-rakuast(
    Associative \format,
    Str          $type,
    Str          $number-system,
    Associative \symbols        # must respond to specific methods
) is export(:ast) {
    use experimental :rakuast;

    # my Str $result = '';
    my $result-declare := RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new( :scope<my>,:name<$result>,
            type  => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str')),
            initializer => RakuAST::Initializer::Assign.new(RakuAST::StrLiteral.new(''))
        )
    );

    my @statements =
        # Define the two subs that we need first,
        # if we need them (transliterate is a noop if 'latn')
        rast-magnitude-declare(),
        rast-transliterate($number-system, |(BEGIN %(($*RAKU.compiler.name eq 'rakudo' ?? 'nqp' !! 'ordhyper') => True ))),
        # Now declare our results object and the $n variable used for initial formatting
        $result-declare,
        rast-abs-n(),
        # Handle any adjustments to the value of $n and declare additional
        # variables potentially needed later (e.g. significant digits).
        # These are noops if not necessary
        rast-percent-adjust(format<type>),
        # Next format the number.  We shift this off to a different sub because
        # logic is pretty different based on various formatting options given.
        rast-number(format,symbols,$number-system),
        # Lastly, affix whatever prefix/suffix may apply.
        # In most cases, this will convert into noops.
        rast-fix(format, symbols, :type<prefix>),
        rast-fix(format, symbols, :type<suffix>);

    RakuAST::Sub.new(
        name => RakuAST::Name.from-identifier('format-number'),
        body => blockoid-statement-list(@statements),
        signature => RakuAST::Signature.new(
            parameters => (
                RakuAST::Parameter.new( target => RakuAST::ParameterTarget::Var.new('$number') ),
    )   )   )
}

sub get-number-formatter(|c) is export(:test) {
    use MONKEY-SEE-NO-EVAL;
    EVAL format-number-rakuast |c;
}

#| ([~] PREFIX/SUFFIX)
sub rast-fix(%data, \symbols, :$type!) {
    # There are two factors in generating the prefix/suffix.
    # (1) What is the negative style?
    #     (a) simple (negative and positive use the same)
    #     (b) unique (use different)
    # (2) Can merge all text that's not a - (assume a simple terminal - if there isn't one and it's simple)

    my $result;
    if %data<negative-type> eq 'simple' {
        # It's simple, so we use positive patterns always.
        # The only question is whether to automatically include a
        # negation symbol which is based on whether it's a prefix
        $result = rast-text(%data{"positive-$type"}, symbols, :auto-negative($type eq 'prefix'), :show-sign(%data<show-sign>));
    } else {
        # It's complex, so we use
        # ($number < 0 || $show-plus) ?? NEG !! POS
        # but optimize roughly
        my $negative := rast-text(%data{"negative-$type"}, symbols, :show-sign(%data<show-sign>));
        my $positive := rast-text(%data{"positive-$type"}, symbols, :show-sign(%data<show-sign>));

        # Both negative and positive patterns have text for this fix
        # TODO: handle $*show-sign logic
        if $negative || $positive {
            $result = RakuAST::Ternary.new(
                condition => RakuAST::Statement::Expression.new(
                        left => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$number'),
                            infix => LessThan,
                            right => RakuAST::IntLiteral(0)
                        ),
                ),
                then => ($negative // RakuAST::StrLiteral.new('')),
                else => ($positive // RakuAST::StrLiteral.new('')),
            )
        }
        # else { $result = Nil } (it should already be it)
    }



    # TODO: return empty if nothing here
    with $result {
        return RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => Result,
                infix => Assign,
                right => ($type eq 'prefix'
                    ?? RakuAST::ApplyInfix.new(:left($result),:infix(Concat),:right(Result ))
                    !! RakuAST::ApplyInfix.new(:left(Result ),:infix(Concat),:right($result))
                )
            )
        )
    } else {
        return Empty
    }
}

# (used by rast-fix to process pattern symbols, returns Nil if no patterns)
sub rast-text(@fix, \symbols, :$auto-negative = False, :$show-sign = False) {
    return Nil unless @fix || $auto-negative;

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
            @strs.push: rast-sign(symbols, $show-sign);

            # (re)set variables
            $str = '';
            $standard-sign = False;
        }
    }

    @strs.push: rast-sign(symbols, $show-sign) if $auto-negative && $standard-sign;

    # wrap with string joins, unless a single item
    if @strs > 1 {
        return RakuAST::ApplyListInfix.new(
            infix => Concat,
            operands => @strs.list
        );
    } else {
        return @strs.head
    }
}

#| Expression providing a minus sign for * < 0, and a plus for * ≥ 0 if $*show-sign
sub rast-sign(\symbols, $show-sign)  {
    # Equivalent to
    #     $number < 0 ?? '-' !! '';
    # Or, if $show-sign is True,
    #     $number < 0 ?? '-' !! '+'
    RakuAST::Ternary.new(
        condition => RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => RakuAST::Var::Lexical.new('$number'),
                infix => LessThan,
                right => Zero
            )
        ),
        then => RakuAST::StrLiteral.new(symbols<minus>),
        else => RakuAST::StrLiteral.new($show-sign ?? symbols<plus> !! ''),
    )
}

sub rast-number (\format, \symbols, \number-system) {
    my @number-statements;

    # The two different formatting systems require keeping up with a few different variables,
    # so it's actually a bit easier to break into two different methods.
    if format<minimum-significant-digits> == 0
    && format<type> ne 'scientific' | 'engineering' {
        # We are using max/min digits
        @number-statements = rast-min-max-digits(format,symbols, number-system)
    } else {
        # We are using significant digits (exponential or other specialized ones)
        @number-statements = rast-significant-digits(format,symbols,number-system);
    }

    # Package it up.  No matter what, the two special Num values are given
    # their own formatting style.  Otherwise, use the decimal formats.
    # TODO: add in RBNF for non-decimal formats.
    #
    # if    $n === NaN {   $result ~ 'NaN'   }
    # elsif $n === Inf {   $result ~  '∞'    }
    # else             { [number-statements] }
    RakuAST::Statement::If.new(
        condition => RakuAST::ApplyInfix.new( :left(Var_N), :infix(ExactlyEqual), :right(NotNumber)),
        then => block-blockoid-statement-list(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                    right => RakuAST::ApplyInfix.new(:left(Result),:infix(Concat),:right(RakuAST::StrLiteral.new: symbols<nan>))
        )   ),  ),
        elsifs => [
            RakuAST::Statement::Elsif.new(
                condition => RakuAST::ApplyInfix.new(:left(Var_N), :infix(ExactlyEqual), :right(Infinity)),
                then => block-blockoid-statement-list(
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new(:left(Result), :infix(Assign),
                            right => RakuAST::ApplyInfix.new(:left(Result),:infix(Concat),:right(RakuAST::StrLiteral.new: symbols<infinity>)),
        ),  ),  ),  ),  ],
        else => block-blockoid-statement-list(@number-statements)
    );
}


#| my $n = abs $number;
sub rast-abs-n is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(:scope<my>,:name<$n>,
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('abs'),
                    args => RakuAST::ArgList.new(RakuAST::Var::Lexical.new('$number'))
   )   )   )   )
}

#| $n = $n % (10 ** ($max - 1)) [empty if $max-int-digs is Inf]
sub rast-max-int(\format) {
    return Empty if format<maximum-integer-digits> === Inf | Any;
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(:left(Var_N), :infix(Assign),
            right => RakuAST::ApplyInfix.new(:left(Var_N) , :infix(Mod),
                right => RakuAST::ApplyInfix.new(:left(Ten), :infix(Power),
                    right => RakuAST::IntLiteral.new(format<maximum-integer-digits> - 1)
    )   )   )   )
}


sub rastlog($start, $var, $end = '') {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::ApplyInfix.new(
                left => RakuAST::StrLiteral.new($start),
                infix => Concat,
                right => RakuAST::ApplyInfix.new(
                    right => RakuAST::StrLiteral.new($end),
                    infix => Concat,
                    left => RakuAST::Var::Lexical.new($var)
                )
            ),
            postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('say'))
        )
    )
}


sub rast-decimal(\symbols) {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Result,
            infix => Assign,
            right =>  RakuAST::ApplyInfix.new(
                left => Result,
                infix => Concat,
                right => RakuAST::StrLiteral.new(symbols.decimal)
            )
        ),
        condition-modifier => RakuAST::StatementModifier::If.new(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    right => RakuAST::Var::Lexical.new('$show-decimal'),
                    infix => Or,
                    left => RakuAST::ApplyInfix.new(
                        # TODO: handle rounding here potentially
                        left => Var_N,
                        infix => Greater,
                        right => RakuAST::Var::Lexical.new('$limit'),
                    )
                )
            )
        )
    )
}


sub rast-percent-adjust($type) {
    if $type eq 'percent' || $type eq 'permille' {
        return RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => Assign,
                right => RakuAST::ApplyInfix.new(
                    left => Var_N,
                    infix => Times,
                    right => RakuAST::IntLiteral.new($type eq 'percent' ?? 100 !! 1000)
                )
            )
        )
    }
    return Empty
}


#| Handles the exponential part of significant number formatting.
#| A series of expresions using $exponential-power and
sub rast-exponential(\format, \symbols, \number-system) {
    # For the exponential section, first format the exponential symbol.
    # Then add a sign if needed (power of 0 is no sign, negative always,
    # plus only if <exponential-forces-sign>, and format the abs value:
    # $result = $result ~ '-' if $exp-pow < 0;                                  # don't force sign
    # $result = $result ~ ($exp-pow == 0 ?? '' !! ($exp-pow < 0 ?? '-' !! '+)); # if forced
    # $exp-pow = abs $exp-pow;
    # $result = translit($result.Str)
    constant ExpPower = RakuAST::Var::Lexical.new('$exponential-power');
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
            right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat),
                right => RakuAST::StrLiteral.new(symbols<exponential>)
    )   )   ),
    (format<exponential-forces-sign>
        ?? RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat),
                    right => RakuAST::Ternary.new(
                        condition => RakuAST::ApplyInfix.new( :left(ExpPower), :infix(NumEqual), :right(Zero) ),
                        then => RakuAST::StrLiteral.new(''),
                        else => RakuAST::Ternary.new(
                            condition => RakuAST::ApplyInfix.new( :left(ExpPower), :infix(LessThan), :right(Zero) ),
                            then => RakuAST::StrLiteral.new(symbols<minus>),
                            else => RakuAST::StrLiteral.new(symbols<plus>)
        )   )   )   )   )
        !! RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                right => RakuAST::ApplyInfix.new( left => Result, infix => Concat,
                    right => RakuAST::StrLiteral.new(symbols<minus>)
            )   ),
            condition-modifier => RakuAST::StatementModifier::If.new(
                RakuAST::ApplyInfix.new( :left(ExpPower), :infix(LessThan), :right(Zero),
    ))  )   ),
    RakuAST::Statement::Expression.new(expression =>
        RakuAST::ApplyInfix.new(
            left => ExpPower,
            infix => Assign,
            right => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('abs'),
                args => RakuAST::ArgList.new(ExpPower),
    )   )   ),
    RakuAST::Statement::Expression.new(expression =>
        RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
            right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat),
                right => rast-transliterate-wrap(
                    RakuAST::ApplyPostfix.new( postfix => MakeStr, operand => ExpPower),
                    number-system
    )   )   )   ),
}

sub rast-transliterate-wrap(\node, \number-system) {
    number-system eq 'latn'
        ?? node
        !! RakuAST::Call::Name.new(
            name => RakuAST::Name.from-identifier('transliterate'),
            args => RakuAST::ArgList.new(node)
        )
}

sub rast-significant-digits(\format, \symbols, \number-system) {
    # Based on magnitude, calculate the minimum number of digits and format
    # as far as we need to go.  This will look VERY similar to min/max formatting
    # except that we assume infinite digits on each side

    # SETUP
    # $power = $n == 0 ?? 1 !! $n.log10;
    # $n = $n.round(10 ** ($power - $max-significant-digits));
    #
    # INTEGER DIGITS
    # $result = $result ~ ($power > 0 ?? $n.floor.Str !! '0');
    #
    # FRACTION
    # $n = $n - $n.floor;
    # my $completed-digits = $power max 0;
    # my $grouping-location = $completed-digits - $primary-grouping;

    my $needs-grouper = so format<primary-grouping-size>;
    my \MaxSigDigs = format<maximum-significant-digits> === Inf
        ?? RakuAST::Var::Lexical.new('$local-max-sig-digs')
        !! RakuAST::IntLiteral.new(format<maximum-significant-digits>);

    #| $n = $n.round(10 ** ($power - $max-significant-digits));
    #| # (cuts off extra digits)
    my @initial-n-adjust := rast-round(
        RakuAST::ApplyInfix.new( :left(Ten), :infix(Power),
            right => RakuAST::ApplyInfix.new(
                :left(RakuAST::Var::Lexical.new('$power')),
                :infix(Minus),
                :right(MaxSigDigs)
            )
        ),
    :calculated);

    my $integer-digits = RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
            right => rast-transliterate-wrap(
                RakuAST::ApplyInfix.new( :left(Result), :infix(Concat),
                    right => RakuAST::Ternary.new(
                        condition => RakuAST::ApplyInfix.new(:left(RakuAST::Var::Lexical.new('$power')), :infix(Greater), :right(Zero)),
                        then => RakuAST::ApplyPostfix.new(
                            postfix => MakeStr,
                            operand => RakuAST::ApplyPostfix.new( postfix => FloorMethod, operand => Var_N)
                        ),
                        else => RakuAST::StrLiteral.new('0'),
                    )
                ),
                \number-system
            )
        )
    );
    my $remove-integer-digits = RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new( :left(Var_N), :infix(Assign),
            right => RakuAST::ApplyInfix.new(:left(Var_N), :infix(Minus),
                right => RakuAST::ApplyPostfix.new( :operand(Var_N), :postfix(FloorMethod))
    )   )   );
    my $completed-digits-declare = RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$completed-digits>,
            initializer => RakuAST::Initializer::Bind.new(
                RakuAST::ApplyInfix.new( :left(RakuAST::Var::Lexical.new('$power')), :infix(RakuAST::Infix.new('max')), :right(Zero))
    )   )   );

    #| $result = $result.substr(0…) ~ GROUPER ~ $result.substr(…*) and $grouping-location -= $secondary-grouping
    #| while $grouping-location > 0;
    my @int-grouping := $needs-grouper
        ?? (RakuAST::Statement::Expression.new(
                expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$grouping-location>,
                    initializer => RakuAST::Initializer::Bind.new(
                        RakuAST::ApplyInfix.new(
                            left => RakuAST::ApplyPostfix.new( :operand(Result),
                                postfix => RakuAST::Call::Method.new( name => RakuAST::Name.from-identifier('chars') )
                            ),
                            infix => Minus,
                            right => RakuAST::IntLiteral.new(format<primary-grouping-digits>)
                        )
                    )
                )
            ),
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::ApplyInfix.new(
                        left => Result,
                        infix => Assign,
                        right => RakuAST::ApplyListInfix.new(
                            infix => Concat,
                            operands => (
                                RakuAST::ApplyPostfix(operand => Result,
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('substr'),
                                        args => RakuAST::ArgList.new((Zero,RakuAST::Var::Lexical.new('$grouping-location')))
                                    )
                                ),
                                RakuAST::StrLiteral.new(symbols.group),
                                RakuAST::ApplyPostfix(operand => Result,
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('substr'),
                                        args => RakuAST::ArgList.new(Zero)
                                    )
                                ),
                            ),
                        )
                    ),
                    infix => RakuAST::Infix.new('and'),
                    right => RakuAST::ApplyInfix.new(
                        left => RakuAST::ApplyInfix.new(
                            left => RakuAST::VarLexical.new('$grouping-location'),
                            infix => Assign,
                            right => RakuAST::ApplyInfix.new(
                                left => RakuAST::VarLexical.new('$grouping-location'),
                                infix => Minus,
                                right => RakuAST::IntLiteral.new(format<secondary-grouping-size>)
                            )
                        ),
                        infix => RakuAST::Infix.new('and'),
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::VarLexical.new('$completed-digits'),
                            infix => Assign,
                            right => RakuAST::ApplyInfix.new( left =>RakuAST::VarLexical.new('$completed-digits'), :infix(Plus), :right(One))
                        )
                    )
                ),
                loop-modifier => RakuAST::StatementModifier::While.new(
                    RakuAST::ApplyInfix.new( :left(RakuAST::Var::Lexical.new: '$grouping-location'), :infix(LessThan), :right(Zero))
                )
            )
        )
        !! Empty;

        #| $n = $n * 10 ** -($power - $max-significant-digits);
        #| my $digit-difference = $max-significant-digits - $min-significant-digits;
        #| my $fractional-mod-difference = $n % 10 ** $digit-difference;
        #| my $fractional-digits-to-strip = 0;
        #| my $fractional-modder = 10;
        #| for ^$digit-difference {
        #|     last if $fractional-mod-difference % $fractional-modder > 0;
        #|     $fractional-digits-to-strip++;
        #|     $fractional-modder *= 10;
        #| }
        my @fractional-digits = RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new( :left(Var_N), :infix(Assign),
                right => RakuAST::ApplyInfix.new( :left(Var_N), :infix(Times),
                    right => RakuAST::ApplyInfix.new( :left(Ten), :infix(Power),
                        right => RakuAST::ApplyPrefix.new( prefix => RakuAST::Prefix.new('-'),
                            operand => RakuAST::ApplyInfix.new( :infix(Minus),
                                left => RakuAST::Var::Lexical.new('$power'),
                                right => MaxSigDigs
        )   )   )   )   )   ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$digit-difference>,
                initializer => RakuAST::Initializer::Bind.new(
                    RakuAST::ApplyInfix.new(
                        :left(MaxSigDigs),
                        :infix(Minus),
                        :right(RakuAST::IntLiteral.new(format<minimum-significant-digits>))
        )   )   )   ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$fractional-mod-difference>,
                initializer => RakuAST::Initializer::Bind.new(
                    RakuAST::ApplyInfix.new( :left(Var_N), :infix(Mod),
                        right => RakuAST::ApplyInfix.new(
                            :left(Ten),
                            :infix(Power),
                            :right(RakuAST::Var::Lexical.new('$digit-difference'))
        )   )   )   )   ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$fractional-digits-to-strip>,
                initializer => RakuAST::Initializer::Bind.new(Zero)
        )   ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$fractional-modder>,
                initializer => RakuAST::Initializer::Bind.new(Ten)
        )   ),
        RakuAST::Statement::For.new(
            source => RakuAST::ApplyPrefix.new( :prefix(ZeroToN), :operand(RakuAST::Var::Lexical.new('$digit-difference'))),
            body => block-blockoid-statement-list(
                RakuAST::Statement::Expression.new(
                    expression => CallLast,
                    condition-modifier => RakuAST::StatementModifier::If.new(
                        RakuAST::ApplyInfix.new( :right(Zero), :infix(Greater),
                            left => RakuAST::Var::Lexical.new('$fractional-mod-difference'),
                            infix => Mod,
                            right => RakuAST::Var::Lexical.new('$fractional-modder')
                )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$fractional-digits-to-strip'),
                        infix => Assign,
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$fractional-digits-to-strip'),
                            infix => Plus,
                            right => One
                )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$fractional-modder'),
                        infix => Assign,
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$fractional-modder'),
                            infix => Times,
                            right => Ten
                )   )   ),
        )   ),
        RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                right => RakuAST::ApplyListInfix.new( :infix(Concat),
                    operands => (
                        Result,
                        RakuAST::StrLiteral.new(symbols.decimal),
                        rast-transliterate-wrap(
                            RakuAST::ApplyPostfix.new(
                                postfix => RakuAST::Call::Method.new(
                                    name => RakuAST::Name.from-identifier('substr'),
                                    args => RakuAST::ArgList.new(
                                        Zero,
                                        RakuAST::ApplyInfix.new(
                                            left => RakuAST::ApplyInfix.new(
                                                left => MaxSigDigs,
                                                infix => Minus,
                                                right => RakuAST::ApplyInfix.new(
                                                    left => RakuAST::Var::Lexical.new('$power'),
                                                    infix => RakuAST::Infix.new('max'),
                                                    right => RakuAST::IntLiteral.new(format<minimum-significant-digits>),
                                            )   ),
                                            infix => Minus,
                                            right => RakuAST::Var::Lexical.new('$fractional-digits-to-strip')
                                )   )   ),
                                operand => RakuAST::ApplyPostfix.new( operand => Var_N, postfix => MakeStr)
                            ),
                            number-system
        )   )   )   )   );

    # if $n { …} elsif … { pad-zero } else { decimal }
    my $fraction-if = RakuAST::Statement::If.new(
        condition => Var_N,
        then => block-blockoid-statement-list(@fractional-digits),
        elsifs => [
            RakuAST::Statement::Elsif.new(
                condition => RakuAST::ApplyInfix.new(
                    left => RakuAST::Var::Lexical.new('$completed-digits'),
                    infix => RakuAST::Infix.new('<='), # 10. is more significant than 10
                    right => RakuAST::IntLiteral.new(format<minimum-significant-digits>)
                ),
                then => block-blockoid-statement-list(
                    rastlog('C2: ', '$result'),
                    RakuAST::Statement::Expression.new(
                        expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                            right => RakuAST::ApplyListInfix.new( :infix(Concat),
                                operands => (
                                    Result,
                                    RakuAST::StrLiteral.new(symbols.decimal),
                                    RakuAST::ApplyInfix.new(
                                        left => RakuAST::StrLiteral.new(%digit-offset{number-system}.chr),
                                        infix => RakuAST::Infix.new('x'),
                                        right => RakuAST::ApplyInfix.new(
                                            left => RakuAST::IntLiteral.new(format<minimum-significant-digits>),
                                            infix => Minus,
                                            right => RakuAST::Var::Lexical.new('$completed-digits')
            )   )   )   )   )   )   )   )
        ],
        |(else => block-blockoid-statement-list(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                    right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat), :right(RakuAST::StrLiteral.new(symbols.decimal)))
                ),
        )   ) if format<show-decimal>)
    );

    my $set-max-sig-digits := (format<maximum-significant-digits> === Inf
        ?? RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$local-max-sig-digs>,
            initializer => RakuAST::Initializer::Bind.new(
                RakuAST::ApplyInfix.new( :left(RakuAST::IntLiteral.new(format<minimum-significant-digits>)), :infix(RakuAST::Infix.new('max')),
                        right => RakuAST::ApplyPostfix.new( postfix => CeilingMethod,
                            operand => RakuAST::ApplyPostfix.new( postfix => AbsMethod,
                                operand => RakuAST::ApplyPostfix.new( postfix => Log10Method,
                                    operand => RakuAST::Var::Dynamic.new('$*TOLERANCE')
        )   )   )   )   )   )   )
        !! Empty);

    my $exponential = format<type> eq 'scientific';
    my @modify-exponential := $exponential ?? rakuast-modify-exp()                                  !! Empty;
    my $declare-exp-power  := $exponential ?? rakuast-exp-power(format<exponential-power-multiple>) !! Empty;
    my @exponential        := $exponential ?? rast-exponential( format,symbols,number-system)       !! Empty;

    return  rast-magnitude(),           # determine $power
            $set-max-sig-digits,
           |@initial-n-adjust,          # adjust per max sig digits
            $declare-exp-power,
           |@modify-exponential,
            $integer-digits,            # format int digits
            $remove-integer-digits,     # reduce to fraction only
            $completed-digits-declare,  # declare completed digits
           |@int-grouping,              #
            $fraction-if,
           |@exponential,
            ;
}

#| my $rounding-quotient   = $n / $rounding-radix;
#| my $rounding-whole      = floor $rounding-quotient;
#| my $rounding-fraction   = $rounding-quotient - $rounding-whole;
#| $n = $rounding-radix * ($rounding-whole +
#|     ($fraction == 0.5 ?? ($rounding-quotient %% 2) !! ($fraction > 1))
#| )
#| # (this is half-even rounding, per IEEE / CLDR standards)
sub rast-round($round, :$calculated = False) {
    # For future maintainers, here's a generic solution for all rounding types:
    # multi sub round(Numeric() $number, Numeric() $radix = 1, :$mode!) {
    #    my $quotient   = floor $number / $radix;
    #    my $low        = $radix * $quotient;
    #    my $variance   = $number - $low;
    #    my $high       = $low + $radix;
    #    my $half-radix = $radix / 2;
    #
    #    if $variance < $half-radix { return $low  }
    #    if $variance > $half-radix { return $high }
    #
    #    do given $mode {
    #        when 'away-from-zero' { $number      > 0 ?? $high !! $low  } # Raku default
    #        when 'toward-zero'    { $number      > 0 ?? $low  !! $high }
    #        when 'half-even'      { $quotient   %% 2 ?? $low  !! $high } # IEEE default
    #        when 'half-odd'       { $quotient   %% 2 ?? $high !! $low  }
    #        when 'half-up'        { $half-radix  > 0 ?? $high !! $low  }
    #        when 'half-down'      { $half-radix  > 0 ?? $low  !! $high }
    #    }
    # }
    return Empty if !$calculated && $round == 0;
    my      \RADIX    = $calculated ?? RakuAST::Var::Lexical.new('$rounding-radix') !! RakuAST::RatLiteral.new($round.Rat);
    constant QUOTIENT = RakuAST::Var::Lexical.new('$rounding-quotient');
    constant WHOLE    = RakuAST::Var::Lexical.new('$rounding-whole');
    constant FRACTION = RakuAST::Var::Lexical.new('$rounding-fraction');
    my @statements;
    @statements.push: RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            name => '$rounding-radix',
            scope => 'my',
            initializer => RakuAST::Initializer::Assign.new($round)
        )
    ) if $calculated;

    @statements.append:
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            name => '$rounding-quotient',
            scope => 'my',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyInfix.new: :left(Var_N), :infix(Divide), :right(RADIX)
            )
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            name => '$rounding-whole',
            scope => 'my',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyPostfix.new(
                    postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('floor')),
                    operand => QUOTIENT
                )
            )
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
            name => '$rounding-fraction',
            scope => 'my',
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::ApplyInfix.new: :left(QUOTIENT), :infix(Minus), :right(WHOLE)
            )
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left  => Var_N,
            infix => Assign,
            right => RakuAST::ApplyInfix.new(
                left => RADIX,
                infix => Times,
                right => RakuAST::ApplyInfix.new(
                    left => QUOTIENT,
                    infix => Plus,
                    right => RakuAST::Ternary.new(
                        condition => RakuAST::ApplyInfix.new( :left(FRACTION), :infix(NumEqual), :right(RakuAST::RatLiteral.new: 0.5)),
                        then => RakuAST::ApplyInfix.new(:left(QUOTIENT), :infix(Divisible), :right(RakuAST::IntLiteral.new: 2  )),
                        else => RakuAST::ApplyInfix.new(:left(FRACTION), :infix(Greater), :right(RakuAST::RatLiteral.new: 0.5)),
                    )
                )
            )
        )
    );
    @statements
}

#| $n = $n * (10 ** (1-$exponential-power))
#| $power = $exponential-power - $power
sub rakuast-modify-exp is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => Var_N,
            infix => Assign,
            right => RakuAST::ApplyInfix.new(
                left => Var_N,
                infix => Times,
                right => RakuAST::ApplyInfix.new(
                    left => Ten,
                    infix => Power,
                    right => RakuAST::ApplyInfix.new(
                        left => Zero,
                        infix => Minus,
                        right => RakuAST::Var::Lexical.new('$exponential-power')
                    )
                )
            )
        )
    ),
    RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$power'),
            infix => Assign,
            right => RakuAST::ApplyInfix.new(
                right => RakuAST::Var::Lexical.new('$exponential-power'),
                infix => Minus,
                left => RakuAST::Var::Lexical.new('$power')
    )   )   )
}

# TODO check with a multiplier
#| my $exponential-power = MULTIPLIER < 2 ?? $power - 1 !! MULTI * ($power div MULTI)
sub rakuast-exp-power($multiplier) is pure {
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$exponential-power>,
            initializer => RakuAST::Initializer::Assign.new(
                $multiplier < 2
                    ?? RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$power'),
                        infix => Minus,
                        right => One)
                    !! RakuAST::ApplyInfix.new(
                        left => RakuAST::IntLiteral.new($multiplier),
                        infix => Times,
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::ApplyInfix.new(
                                left => RakuAST::Var::Lexical.new('$power'),
                                infix => Minus,
                                right => One),
                            infix => IntDivide,
                            right => RakuAST::IntLiteral.new($multiplier)
                        ),
                    )
            )
        )
    )
}


sub wrap-in-expressions(+@foo) is pure {
    @foo.map({RakuAST::Statement::Expression.new(expression => $^expression)}).list
}

sub rast-min-max-digits(\format,\symbols,\number-system) {
    my $reduce-for-max-integers := rast-max-int(format);
    my $set-magnitude := rast-magnitude;
    my $integer-digits := RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
            right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat),
                right => rast-transliterate-wrap(
                    RakuAST::Ternary.new(
                        condition => RakuAST::ApplyInfix.new(:left(RakuAST::Var::Lexical.new('$power')), :infix(Greater), :right(Zero)),
                        then => RakuAST::ApplyPostfix.new(
                            postfix => MakeStr,
                            operand => RakuAST::ApplyPostfix.new( postfix => FloorMethod, operand => Var_N)
                        ),
                        else => RakuAST::StrLiteral.new('0'),
                    ),
                    number-system
    )   )   )   );

    #| $result = ('0' x ($min-digs - $power)) ~ $result if $min-digs > $power
    my $pad-leading-zeros := format<minimum-integer-digits> > 1
        ?? RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                right => RakuAST::ApplyInfix.new( :right(Result), :infix(Concat),
                    left => RakuAST::ApplyInfix.new(
                        left => RakuAST::StrLiteral.new(%digit-offset{number-system}.chr), # it's only 0, so we can easily hardcode the localized one
                        infix => RakuAST::Infix.new('x'),
                        right => RakuAST::ApplyInfix.new( :right(RakuAST::Var::Lexical.new('$power')), :infix(Minus),
                            left => RakuAST::IntLiteral.new(format<minimum-integer-digits>)
            )   )   )   ),
            condition-modifier => RakuAST::StatementModifier::If.new(
                RakuAST::ApplyInfix.new(
                    :left(RakuAST::IntLiteral.new(format<minimum-integer-digits>)),
                    :infix(Greater),
                    :right(RakuAST::Var::Lexical.new('$power'))
        )   )   )
        !! Empty;

    # If show decimal is a requirement, we can just add it now without an conditionals
    # Otherwise, the decimal will be added if there's a fraction (and potentially clipped!)
    my $force-decimal := (format<show-decimal>
        ?? RakuAST::Statement::Expression.new( expression =>
            RakuAST::ApplyInfix.new(:left(Result), :infix(Assign),
                right => (:left(Result), :infix(Concat), :right(RakuAST::StrLiteral.new(symbols<decimal>)))
        )   )
        !! Empty
    );
    my @fraction-statements;
    my $fraction-statements;
    if format<maximum-fractional-digits> > 0 {
        if format<maximum-fractional-digits> === Inf {
            # my $local-max-digits = MIN-DIGS max abs(magnitude($*TOLERANCE));
            @fraction-statements.push: RakuAST::Statement::Expression.new(
                expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$local-max-digits>,
                    initializer => RakuAST::Initializer::Bind.new(
                        RakuAST::ApplyInfix.new(
                            left => RakuAST::IntLiteral.new(format<minimum-fractional-digits>),
                            infix => RakuAST::Infix.new('max'),
                            right => RakuAST::Call::Name.new( name => RakuAST::Name.from-identifier('abs'),
                                args => RakuAST::ArgList.new(
                                    RakuAST::ApplyInfix.new(:left(One), :infix(Minus), # because 0.1 = 0, not -1.  To be perfect, only adjust if < 1, but that (should) be the case for $*TOLERANCE
                                        right => RakuAST::Call::Name.new( name => RakuAST::Name.from-identifier('magnitude'),
                                            args => RakuAST::ArgList.new( RakuAST::Var::Dynamic.new('$*TOLERANCE') )
            )   )   )   )   )   )   )   );
        }

        @fraction-statements.push: RakuAST::Statement::Expression.new(
            expression => RakuAST::ApplyInfix.new(:left(Result), :infix(Assign),
                right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat), :right(RakuAST::StrLiteral.new(symbols<decimal>)))
            ),
        ) unless format<show-decimal>;

        # my result = $result ~ (($n - $n.floor) * 10 ** ((MAX-FRAC-DIGITS))).floor.Str
        @fraction-statements.append: RakuAST::Statement::Expression.new( expression =>
            RakuAST::ApplyInfix.new(:left(Result), :infix(Assign),
                right => RakuAST::ApplyInfix.new( :left(Result), :infix(Concat),
                        right => rast-transliterate-wrap(
                            RakuAST::ApplyPostfix.new(
                                postfix => MakeStr,
                                operand => RakuAST::ApplyPostfix.new(
                                    postfix => FloorMethod,
                                    operand => RakuAST::ApplyInfix.new(
                                        left => RakuAST::ApplyInfix.new(
                                            left => Var_N,
                                            infix => Minus,
                                            right => RakuAST::ApplyPostfix.new(
                                                postfix => FloorMethod,
                                                operand => Var_N,
                                        )   ),
                                        infix => Times,
                                        right => RakuAST::ApplyInfix.new(
                                            left => Ten,
                                            infix => Power,
                                            right => ( format<maximum-fractional-digits> === Inf
                                                ?? RakuAST::Var::Lexical.new('$local-max-digits')
                                                !! RakuAST::IntLiteral.new(format<maximum-fractional-digits>)
                            )   )   )   )   ),
                            number-system
            )   )   )   ),
        # my $fractional-clip = $result.chars; # TODO: optimize out if $min/$max guaranteed the same
        RakuAST::Statement::Expression.new(
            expression => RakuAST::VarDeclaration::Simple.new(:scope<my>, :name<$fractional-clip>,
                initializer => RakuAST::Initializer::Bind.new(
                    RakuAST::ApplyPostfix.new(
                        operand => Result,
                        postfix => RakuAST::Call::Method.new( name => RakuAST::Name.from-identifier('chars') )
        )   )   )   ),
        # for ^MAX-FRAC-DIGS {
        #   last if $result.substr($fractional-clip - 1, 1) ne ZERO;
        #   $fractional-clip -= 1;
        # }                      # TODO: optimize out if $min/$max guaranteed the same
        RakuAST::Statement::For.new(
            source => RakuAST::ApplyPrefix.new(
                :prefix(RakuAST::Prefix.new('^')),
                :operand( format<maximum-fractional-digits> === Inf
                    ?? RakuAST::Var::Lexical.new('$local-max-digits')
                    !! RakuAST::IntLiteral.new(format<maximum-fractional-digits>)
            )   ),
            body => block-blockoid-statement-list(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::Call::Name.new( name => RakuAST::Name.from-identifier('last') ),
                    condition-modifier => RakuAST::StatementModifier::If.new(
                        RakuAST::ApplyInfix.new(
                            right => RakuAST::StrLiteral.new(%digit-offset{number-system}.chr), # can cache it here
                            infix => RakuAST::Infix.new('ne'),
                            left => RakuAST::ApplyPostfix.new(
                                operand => Result,
                                postfix => RakuAST::Call::Method.new(
                                    name => RakuAST::Name.from-identifier('substr'),
                                    args => RakuAST::ArgList.new(
                                        RakuAST::ApplyInfix.new(
                                            left => RakuAST::Var::Lexical.new('$fractional-clip'),
                                            infix => Minus,
                                            right => One
                                        ),
                                        One
                )   )   )   )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$fractional-clip'),
                        infix => Assign,
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$fractional-clip'),
                            infix => Minus,
                            right => One,
        )   )   )   )   ),
        # $result = $result.substr(0, $result.chars - $fractional-clip == $maximum-fractional-digits ?? $fractional-clip - 1 !! $fractional-clip)
        RakuAST::Statement::Expression.new( expression =>
            RakuAST::ApplyInfix.new(
                left => Result,
                infix => Assign,
                right => RakuAST::ApplyPostfix.new(
                    operand => Result,
                    postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('substr'),
                        args => RakuAST::ArgList.new(
                            Zero,
                            RakuAST::Ternary.new(
                                condition => RakuAST::ApplyInfix.new(
                                    infix => NumEqual,
                                    right => (format<maximum-fractional-digits> === Inf
                                        ?? RakuAST::Var::Lexical.new('$local-max-digits')
                                        !! RakuAST::IntLiteral.new(format<maximum-fractional-digits>)
                                    ),
                                    left => RakuAST::ApplyInfix.new(
                                        left => RakuAST::ApplyPostfix.new(
                                            postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('chars')),
                                            operand => Result
                                        ),
                                        infix => Minus,
                                        right => RakuAST::Var::Lexical.new('$fractional-clip')
                                )   ),
                                then => RakuAST::ApplyInfix.new(:right(One), :infix(Minus),
                                    left => RakuAST::Var::Lexical.new('$fractional-clip')
                                ),
                                else => RakuAST::Var::Lexical.new('$fractional-clip')
                            )
        )   )   )   )   );
        $fraction-statements := RakuAST::Statement::If.new(
            condition => RakuAST::ApplyInfix.new(
                left => RakuAST::ApplyInfix.new( :left(Var_N), :right(One), :infix(RakuAST::Infix.new('%'))),
                infix => Greater,
                right => Zero),
            then => block-blockoid-statement-list(@fraction-statements)
        );
    } else {
        $fraction-statements := Empty;
    }
    my @integer-grouping := rast-integer-grouping(format,symbols);
    return  $reduce-for-max-integers,
            $set-magnitude,
            $integer-digits,
            $pad-leading-zeros,
           |@integer-grouping,
            $force-decimal,
            $fraction-statements,
    ;
}

#| Provides code to act on $result, assuming it only has integer digits
sub rast-integer-grouping( \format,\symbols) {
    # NOOP if $primary-grouping-size ≤ 0
    # ELSE
    #   my $grouping-location = PRIMARY-GROUPING-SIZE;
    #   while $grouping-location > 0 {
    #     $result = $result.substr(0,$grouping-location) ~ GROUP-SYMBOL ~ $result.substr($grouping-location))
    format<primary-grouping-size> > 0
        ?? (RakuAST::Statement::Expression.new(
                expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$grouping-location>,
                    initializer => RakuAST::Initializer::Bind.new(
                        RakuAST::ApplyInfix.new(
                            #left => RakuAST::Var::Lexical.new('$completed-digits'),
                            left => RakuAST::ApplyPostfix.new( :operand(Result),
                                postfix => RakuAST::Call::Method.new( name => RakuAST::Name.from-identifier('chars'))
                            ),
                            infix => Minus,
                            right => RakuAST::IntLiteral.new(format<primary-grouping-size>)
                        )
                    )
                )
            ),
            RakuAST::Statement::Loop::While.new(
                condition => RakuAST::ApplyInfix.new( :right(Zero), :infix(Greater),
                    left => RakuAST::Var::Lexical.new('$grouping-location')
                ),
                body => block-blockoid-statement-list(
                    RakuAST::Statement::Expression.new( expression =>
                        RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                            right => RakuAST::ApplyListInfix.new(
                                infix => Concat,
                                operands => (
                                    RakuAST::ApplyPostfix.new(operand => Result,
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('substr'),
                                            args => RakuAST::ArgList.new(Zero,RakuAST::Var::Lexical.new('$grouping-location'))
                                        )
                                    ),
                                    RakuAST::StrLiteral.new(symbols<group>),
                                    RakuAST::ApplyPostfix.new(operand => Result,
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('substr'),
                                            args => RakuAST::ArgList.new(RakuAST::Var::Lexical.new('$grouping-location'))
                )   )   )   )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyInfix.new(
                        left => RakuAST::Var::Lexical.new('$grouping-location'),
                        infix => Assign,
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$grouping-location'),
                            infix => Minus,
                            right => RakuAST::IntLiteral.new(format<secondary-grouping-size>)
        ))  )   )   )   )
        !! Empty;
}


sub rast-integer-grouping-FORSIG( \format,\symbols) {
#`<<<
    # NOOP if $primary-grouping-size ≤ 0
    # ELSE
    #   my $grouping-location = PRIMARY-GROUPING-SIZE;
    #   while $grouping-location > 0 {
    #     $result = $result.substr(0,$grouping-location) ~ GROUP-SYMBOL ~ $result.substr($grouping-location))
    format<primary-grouping-size> > 0
        ?? (RakuAST::Statement::Expression.new(
                expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$grouping-location>,
                    initializer => RakuAST::Initializer::Bind.new(
                        RakuAST::ApplyInfix.new(
                            #left => RakuAST::Var::Lexical.new('$completed-digits'),
                            left => RakuAST::ApplyPostfix.new( :operand(Result),
                                postfix => RakuAST::Call::Method.new( RakuAST::Name.from-identifier('chars'))
                            ),
                            infix => Minus,
                            right => RakuAST::IntLiteral.new(format<primary-grouping-size>)
                        )
                    )
                )
            ),
            RakuAST::Statement::Loop::While.new(
                condition => RakuAST::ApplyInfix.new( :right(One), :infix(LessThan),
                    left => RakuAST::Var::Lexical.new('$grouping-location')
                ),
                body => block-blockoid-statement-list(
                    RakuAST::Statement::Expression.new(
                        RakuAST::ApplyInfix.new( :left(Result), :infix(Assign),
                            right => RakuAST::ApplyListInfix.new(
                                infix => Concat,
                                operands => (
                                    RakuAST::ApplyPostfix.new(operand => Result,
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('substr'),
                                            args => RakuAST::ArgList.new((Zero,RakuAST::Var::Lexical.new('$grouping-location')))
                                        )
                                    ),
                                    RakuAST::StrLiteral.new(symbols<group>),
                                    RakuAST::ApplyPostfix.new(operand => Result,
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('substr'),
                                            args => RakuAST::ArgList.new(RakuAST::Var::Lexical.new('$grouping-location'))
                                        )
                                    ),

                                )
                                ))),
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyInfix.new(
                    left => RakuAST::ApplyInfix.new(
                        left => Result,
                        infix => Assign,
                        right => RakuAST::ApplyListInfix.new(
                            infix => Concat,
                            operands => (
                                RakuAST::ApplyPostfix.new(operand => Result,
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('substr'),
                                        args => RakuAST::ArgList.new((Zero,RakuAST::Var::Lexical.new('$grouping-location')))
                                    )
                                ),
                                RakuAST::StrLiteral.new(symbols.group),
                                RakuAST::ApplyPostfix.new(operand => Result,
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('substr'),
                                        args => RakuAST::ArgList.new(Zero)
                                    )
                                ),
                            ),
                        )
                    ),
                    infix => RakuAST::Infix.new('and'),
                    right => RakuAST::ApplyInfix.new(
                        left => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$grouping-location'),
                            infix => Assign,
                            right => RakuAST::ApplyInfix.new(
                                left => RakuAST::Var::Lexical.new('$grouping-location'),
                                infix => Minus,
                                right => RakuAST::IntLiteral.new(pattern<secondary-grouping-size>)
                            )
                        ),
                        infix => RakuAST::Infix.new('and'),
                        right => RakuAST::ApplyInfix.new(
                            left => RakuAST::Var::Lexical.new('$completed-digits'),
                            infix => Assign,
                            right => RakuAST::ApplyInfix.new( left =>RakuAST::Var::Lexical.new('$completed-digits'), :infix(Plus), :right(One))
                        )
                    )
                ),
                loop-modifier => RakuAST::StatementModifier::While.new(
                    RakuAST::ApplyInfix.new( :left(RakuAST::Var::Lexical.new: '$grouping-location'), :infix(LessThan), :right(Zero))
                )
            )>>>
        #)
        #!! Empty;
}

multi sub rast-transliterate ($?, :$ordhyper!) {
    # $result.ords.map(* + 49)>>.chr.join
    RakuAST::Sub.new( :scope<my>,
        name => RakuAST::Name.from-identifier('transliterate'),
        signature => RakuAST::Signature.new(
            parameters => RakuAST::Parameter.new(
                target => RakuAST::ParameterTarget::Var.new('$source'),
                traits => (RakuAST::Trait::Is.new( name => RakuAST::Name.from-identifier('raw')), )
        )   ),
        body => RakuAST::Blockoid.new( RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new( expression =>
                RakuAST::ApplyPostfix.new( postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('join')),
                    operand => RakuAST::ApplyPostfix.new(
                        postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('map'),
                            args => RakuAST::ArgList.new(
                                RakuAST::Block.new( body => RakuAST::Blockoid.new(
                                    RakuAST::StatementList.new(
                                        RakuAST::Statement::Expression.new(
                                            expression => RakuAST::ApplyPostfix.new(
                                                postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('chr')),
                                                operand => RakuAST::ApplyInfix.new( :infix(Plus),
                                                    left => RakuAST::Var::Lexical.new('$_'),
                                                    right => RakuAST::IntLiteral.new($*zero.ord - '0'.ord),
                        )   )   )), )   )   )   ),
                        operand => RakuAST::ApplyPostfix.new(
                            operand => RakuAST::Var::Lexical.new('$source'),
                            postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('ords'))
                        )))))))
}

# Creates a sub that transliterates a given string to a different decimal system
multi sub rast-transliterate($number-system, :$nqp!) {
    # This is currently the fastest (regardless of length) version I can come up with
    # It does require ALL codes to be in the convertable range.
    # The value passed into zero should indicate what the Unicode decimal zero is
    #
    # sub transliterate($source is raw) {
    #     my int32 @temp;
    #     nqp::strtocodes($source, nqp::const::NORMALIZE_NFC, @temp);
    #     my int32 i = nqp::elems(@temp);
    #     nqp::while(
    #         ($temp2 = nqp::sub_i($i,1)),
    #         {
    #             nqp::bindpos_i(
    #                 @temp, $i,
    #                 nqp::add_i(nqp::atpos_i(@temp,$i),$adj)
    #             )
    #         }
    #     )
    #     nqp::strfromcodes(@temp);
    # }
    return Empty if $number-system eq 'latn';
    my $adjust = %digit-offset{$number-system} - 48; # 48 is '0' in ASCII/Unicode
    use nqp;
    RakuAST::Statement::Expression.new( expression =>
        RakuAST::Sub.new(
            name => RakuAST::Name.from-identifier('transliterate'),
            signature => RakuAST::Signature.new(
                parameters =>  (
                    RakuAST::Parameter.new(
                        target =>  RakuAST::ParameterTarget::Var.new('$source'),
                        traits => (RakuAST::Trait::Is.new( name => RakuAST::Name.from-identifier('raw') ),)
            ),  )   ),
            body => blockoid-statement-list(
                RakuAST::Statement::Expression.new( expression =>
                    RakuAST::VarDeclaration::Simple.new(
                        name => '@temp',
                        type => RakuAST::Type::Simple.new( RakuAST::Name.from-identifier('int32') ),
                )   ),
                RakuAST::Statement::Expression.new( expression =>
                    RakuAST::Nqp.new(
                        'strtocodes',
                        RakuAST::Var::Lexical.new('$source'),
                        RakuAST::Nqp::Const.new('NORMALIZE_NFC'),
                        RakuAST::Var::Lexical.new('@temp'),
                )   ),
                RakuAST::Statement::Expression.new( expression =>
                    RakuAST::VarDeclaration::Simple.new(
                        name => '$i',
                        type => RakuAST::Type::Simple.new( RakuAST::Name.from-identifier('int32') ),
                        initializer => RakuAST::Initializer::Assign.new(
                            RakuAST::Nqp.new(
                                'elems',
                                RakuAST::Var::Lexical.new('@temp')
                )   )   )   ),
                RakuAST::Statement::Expression.new( expression =>
                    RakuAST::Nqp.new(
                        'while',
                        RakuAST::SemiList.new(
                            RakuAST::Statement::Expression.new( expression =>
                                RakuAST::ApplyInfix.new(
                                    left => RakuAST::Var::Lexical.new('$i'),
                                    infix => RakuAST::Infix.new('='),
                                    right => RakuAST::Nqp.new(
                                        'sub_i',
                                        RakuAST::Var::Lexical.new('$i'),
                                        RakuAST::IntLiteral.new(1),
                        )   )   )   ),
                        RakuAST::Nqp.new(
                            'bindpos_i',
                            RakuAST::Var::Lexical.new('@temp'),
                            RakuAST::Var::Lexical.new('$i'),
                            RakuAST::Nqp.new(
                                'add_i',
                                RakuAST::Nqp.new('atpos_i',RakuAST::Var::Lexical.new('@temp'),RakuAST::Var::Lexical.new('$i')),
                                RakuAST::IntLiteral.new($adjust), #RakuAST::Var::Lexical.new('$adj'),
                )   )   )   ),
                RakuAST::Statement::Expression.new( expression =>
                    RakuAST::Nqp.new(
                        'bindpos_i',
                        RakuAST::Var::Lexical.new('@temp'),
                        RakuAST::Var::Lexical.new('$i'),
                        RakuAST::Nqp.new(
                            'add_i',
                            RakuAST::Nqp.new('atpos_i',RakuAST::Var::Lexical.new('@temp'),Zero),
                            RakuAST::IntLiteral.new($adjust), #RakuAST::Var::Lexical.new('$adj'),
                )   )   ),
                RakuAST::Statement::Expression.new( expression =>
                    RakuAST::Call::Name.new(
                        name => RakuAST::Name.from-identifier('return'),
                        args => RakuAST::ArgList.new(RakuAST::Nqp.new('strfromcodes',RakuAST::Var::Lexical.new('@temp')))
    )   )   )   )   )
}

#| Declares a reusable sub to determine the magnitude of a number
sub rast-magnitude-declare is pure {
    # Equivalent to $x.log10.ceiling, but without the float inaccuracies that
    # are a major headache where $x ≈ 10ⁿ
    #
    # sub magnitude(Any $x is raw) {
    #     return 1 if $x == 0;
    #     my $n := $x;
    #     my $r := 1;
    #     my $c := 1;
    #     while $n <                1 { $r -= 8; $n *= 1_0000_0000 }
    #     while $n ≥ $c * 1_0000_0000 { $r += 8; $c *= 1_0000_0000 }
    #     while $n ≥ $c *      1_0000 { $r += 4; $c *=      1_0000 }
    #     while $n ≥ $c *          10 { $r += 1; $c *=          10 }
    #     return $r
    # }
    #
    # Magnitude is defined in $r which effectively tracks how many digits we've shifted
    # each way. Multiply until $n is over 1, reducing $r according, to shift us into the
    # positive. Then adjust a comparison value $c (multiplying $c is faster than dividing
    # $n) to until it's in the same magnitude as $n, adjusting $r accordingly.

    RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
            name      => RakuAST::Name.from-identifier("magnitude"),
            signature => RakuAST::Signature.new(
                parameters => (
                    RakuAST::Parameter.new(
                        type   => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("Any")),
                        target => RakuAST::ParameterTarget::Var.new('$x'),
                        traits => (RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier("raw")),)
            ),  )   ),
            body => blockoid-statement-list(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::Call::Name.new(
                        name => RakuAST::Name.from-identifier("return"),
                        args => RakuAST::ArgList.new(One)
                    ),
                    condition-modifier => RakuAST::StatementModifier::If.new(
                        RakuAST::ApplyInfix.new(
                            left  => Var_X,
                            infix => NumEqual,
                            right => Zero
                )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::VarDeclaration::Simple.new(
                        name        => '$n',
                        initializer => RakuAST::Initializer::Bind.new(Var_X)
                )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::VarDeclaration::Simple.new(
                        name        => '$r',
                        initializer => RakuAST::Initializer::Bind.new(One)
                )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::VarDeclaration::Simple.new(
                        name        => '$c',
                        initializer => RakuAST::Initializer::Bind.new(One)
                )   ),
                RakuAST::Statement::Loop::While.new(
                    condition => RakuAST::ApplyInfix.new(:left(Var_N),:infix(LessThan),:right(One)),
                    body      => block-blockoid-statement-list(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_R), :infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Minus),:right(RakuAST::IntLiteral.new(8)))
                        )   ),
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_N),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_N),:infix(Times),:right(RakuAST::IntLiteral.new(100000000)))
                )   )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyInfix.new(:left(Var_N),:infix(Assign),
                        right => RakuAST::ApplyPostfix.new(:operand(Var_N),:postfix(MakeInt))
                )   ),
                RakuAST::Statement::Loop::While.new(
                    condition => RakuAST::ApplyInfix.new(:left(Var_N),:infix(GreaterEqual),
                        right => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Times),:right(RakuAST::IntLiteral.new(100000000)))
                    ),
                    body => block-blockoid-statement-list(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Plus),:right(RakuAST::IntLiteral.new(8)))
                        )   ),
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Times),:right(RakuAST::IntLiteral.new(100000000)))
                )   )   )   ),
                RakuAST::Statement::Loop::While.new(
                    condition => RakuAST::ApplyInfix.new(:left(Var_N),:infix(GreaterEqual),
                        right => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Times),:right(RakuAST::IntLiteral.new(10000)))
                    ),
                    body => block-blockoid-statement-list(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Plus),:right(RakuAST::IntLiteral.new(4)))
                        )   ),
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Times),:right(RakuAST::IntLiteral.new(10000)))
                )   )   )   ),
                RakuAST::Statement::Loop::While.new(
                    condition => RakuAST::ApplyInfix.new(:left(Var_N),:infix(GreaterEqual),
                        right => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Times),:right(Ten))
                    ),
                    body => block-blockoid-statement-list(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_R),:infix(Plus),:right(One))
                        )   ),
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Assign),
                                right => RakuAST::ApplyInfix.new(:left(Var_C),:infix(Times),:right(Ten))
                )   )   )   ),
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::Call::Name.new(
                        name => RakuAST::Name.from-identifier("return"),
                        args => RakuAST::ArgList.new(Var_R)
    )   )   )   )   );
}

#| Calls the magnitude sub, storing results in `$power`
sub rast-magnitude is pure {
    # my $power = magnitude($n);
    RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new( :scope<my>, :name<$power>,
            initializer => RakuAST::Initializer::Assign.new(
                RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('magnitude'),
                    args => RakuAST::ArgList.new(Var_N)
    )   )   )   )
}
