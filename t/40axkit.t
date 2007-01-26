use strict;
use warnings;
use Test::More;

plan(skip_all => "AxKit needed"), exit if
    ! eval { require Apache::AxKit::Provider };

plan tests => 1;

BEGIN {
    use_ok( 'Apache::AxKit::Language::YPathScript' );
}
