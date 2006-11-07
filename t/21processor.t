#!/usr/bin/perl

use strict;
use Test::More tests => 1;


use XML::XPathScript::Processor qw();
use XML::XPathScript::Template;

use XML::LibXML;  # TODO make this generic

my $processor = XML::XPathScript::Processor->new;
$processor->set_parser( 'XML::LibXML' );  #FIXME
$processor->set_doc( XML::LibXML->new->parse_string( <<'END_XML' ) );
<doc><foo>I am the walrus!</foo></doc>
END_XML

my $template = XML::XPathScript::Template->new;
$template->set( 'foo' => { rename => 'bar' } );

$processor->set_template( $template );

my $result = $processor->apply_templates( '//foo' );

is $result => '<bar>I am the walrus!</bar>';




