#!/usr/bin/env perl6

use Intl::Format::Number;
my $time;


my @nums;
@nums.push: 100.rand for ^50;

my $a;
my $b;
my $c;
#sink format-number 100000;
my &intl = local-number-formatter(:language<en>);
$time = now;
$b ~= @nums.roll.Str for ^10000;
say "Done in {now - $time}";

$time = now;
$a ~= intl @nums.roll for ^10000;
say "Done in {now - $time}";

$time = now;
$c ~= @nums.roll.base(10) for ^10000;
say "Done in {now - $time}";
