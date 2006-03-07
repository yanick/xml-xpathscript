#!/usr/bin/perl -w

use strict;
use Test;

=head1 NAME

20docbookxml2latex.t - Tests ../examples/docbookxml2latex.xps, a
Docbook-to-LaTeX stylesheet in XPathScript.

=head1 DESCRIPTION

This test simply checks that a sample Docbook document is converted
without errors. It doesn't attempt to do anything useful with the
resulting LaTeX file.

=head1 BUGS

The test document is in french :-)

=cut

BEGIN
{
	plan tests => 3, todo => [];
}


use XML::XPathScript;
use IO::File;
use File::Spec;

my $doc = new IO::File
    (File::Spec->catfile(qw(examples sample-docbook.xml)));
my $style = new IO::File
     (File::Spec->catfile(qw(examples docbook2latex.xps)));
ok(defined $doc);
ok(defined $style);

my $xps = new XML::XPathScript( xml => $doc, stylesheet => $style );

my $buf="";
do {
    ## Comment the following two lines to debug:
#    local *STDERR;
#    open(STDERR, ">", "/dev/null");
    $xps->process(sub {$buf .= shift});
};
ok($buf =~ m/\\begin\{document\}/);


