use strict;
use warnings;
use Test::More qw/ no_plan /;

BEGIN {
    use_ok( 'XML::XPathScript' );
}

sub test_file {
    my $filename = shift;
    my $comment = shift;

    local $/ = undef;

    open my $xml, "t/testdocs/$filename.xml" or die;

    my $xps = XML::XPathScript->new(xml => join( '', <$xml> ), 
                                    stylesheetfile => "t/testdocs/$filename.xps" );

    my $doc;
    $xps->process( \$doc );

    open my $expected, "t/testdocs/$filename.expected" or die;

    is( $doc, <$expected>, $comment );
}

test_file( 'namespace', 'namespace' );
