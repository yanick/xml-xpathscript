use strict;
use warnings;

use Test::More tests => 1;                      # last test to print

use XML::XPathScript;

# rename

my $xml = '<doc><foo>ttt</foo></doc>';
my $stylesheet = q#<%  $t->set( foo => { rename => 'bar' } ) %><%~ / %>#;

my $xps = XML::XPathScript->new;
is $xps->transform( $xml => $stylesheet ) 
    => '<doc><bar>ttt</bar></doc>';

