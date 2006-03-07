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
	plan tests => 1, todo => [];
}


use XML::XPathScript;
use IO::File;

my $doc = new IO::File("t/testdocs/docbook-testsuite.xml");
my $style = new IO::File("examples/docbook2latex.xml");
my $xps = new XML::XPathScript( xml => $doc, stylesheet => $style );

my $buf="";
$xps->process(sub {$buf .= shift});
ok(1);


