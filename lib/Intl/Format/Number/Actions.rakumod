use v6.d;
unit class Actions;

# This is a very simple actions class that
# only returns the %*data from parsing.
#
# If someone else wishes to use the grammar,
# they may want to implement the full set of
# actions methods but the data that we need
# has for generating the formatter code is
# already available from the %data.

method TOP($/) {
    make %*data;
}