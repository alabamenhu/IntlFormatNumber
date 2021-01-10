#!/usr/bin/env perl6

use Intl::Format::Number;
my $time;

#`<<<
my @nums;
@nums.push: floor 1000000000.rand for ^50;
say @nums;

my $a;
my $b;
sink format-number 100000;

$time = now;
$a ~= format-number @nums.roll for ^1000;
say "Done in {now - $time}";

$time = now;
$b ~= @nums.roll for ^1000;
say "Done in {now - $time}";
>>>
my $time2;

$time = now;
my &f = get-number-formatter 'en';# :show-decimal;
#my &g = get-number-formatter 'ar';# :show-decimal;
say f 1234567890123;
