use strict;
use warnings;

use Test::More tests => 4;                      # last test to print

use XML::XPathScript;
use XML::XPathScript::Template qw/ DO_SELF_ONLY /;

my $xps = XML::XPathScript->new;
my $template = XML::XPathScript::Template->new;
my $processor = $xps->processor;
$processor->set_template( $template );

$template->set( 'foo' => { showtag => 1, 
                           insteadofchildren => '[ ... children ... ]', } );
$xps->set_xml( '<foo></foo>' );

is $processor->apply_templates( '//foo' ) => '<foo></foo>',
    'insteadofchildren without children';

$xps->set_xml( '<foo><bar/></foo>' );
$template->set( 'foo' => { showtag => 1,
                           insteadofchildren => '[ ... children ... ]' } );

is $processor->apply_templates( '//foo' ) 
            => '<foo>[ ... children ... ]</foo>',
            'insteadofchildren with children';

$template->set( 'foo' => { insteadofchildren => '[ ... children ... ]',
                           action => -1,
                           showtag => 1, } );

is $processor->apply_templates( '//foo' ) => '<foo></foo>',
    'precedence of "action"';

$template->set( foo => { showtag => 1, 
            action => 1, 
            insteadofchildren => sub { 
                my( $n, $t, $params ) = @_;
                my @children = $n->findnodes( 'child::*' );
                return 'node has '.@children.' children';
} } );

is $processor->apply_templates( '//foo' ) => '<foo>node has 1 children</foo>',
    'insteadofchildren as a function ref';

