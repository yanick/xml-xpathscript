use strict;
use warnings;
use Test::More;

plan(skip_all => "TomKit needed"), exit if
    ! eval { require Apache2::TomKit::Processor::AbstractProcessor };

plan tests => 1;

BEGIN {
    use_ok( 'Apache2::TomKit::Processor::XPathScript' );
}
