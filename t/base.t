#!/usr/bin/perl 

use strict;
use Test;

BEGIN 
{ 
	plan tests => 16, todo => [];
	unshift @INC, '../blib/arch', '../blib/lib';
}

use XML::YPathScript;
use Apache::AxKit::Language::YPathScript;

ok(1); 

sub test_xml
{
	my( $xml, $style, $result, $comment ) = @_;
    my $xps = new XML::YPathScript( xml => $xml, stylesheet => $style );
	my $buffer= $xps->process( 'return' );

	ok( $buffer, $result, $comment );
}


# empty run
test_xml( '<doc>dummy</doc>', 'working', 'working' );


# simple in/out
test_xml( '<doc>dummy</doc>', '<%= apply_templates() %>', '<doc>dummy</doc>' );

# a rogue print statement
test_xml( '<doc>dummy</doc>', '<% print "Hello!" %>', 'Hello!' );

# processing a comment
test_xml( '<doc><!-- hello world --></doc>', <<'EOT', "<doc>comment: hello world </doc>\n" );
<% $t->{'#comment'}{pre} = "comment:"; %><%= apply_templates() %>
EOT

# masking a comment
test_xml( '<doc><!-- hello world --></doc>', <<'EOT', "<doc></doc>\n" );
<% $t->{'#comment'}{testcode} = sub{ 0 } %><%= apply_templates() %>
EOT

# testing Interpolation

my $xml = "<doc><node color='blue'>Hello</node></doc>";
my $xps = <<'EOT';
<% $t->{node}{testcode} = sub{ my( $n, $t ) = @_; $t->{pre} = '{@color}'; return DO_SELF_ONLY() }; %>
<%= apply_templates() %>
EOT
test_xml( $xml, $xps, "\n<doc>{\@color}</doc>\n", 'Interpolation (disabled)'  );

$xps = <<'EOT';
<% 
	$XML::YPathScript::DoNotInterpolate = 0; 
	$t->{node}{testcode} = sub
	{ 
		my( $n, $t ) = @_; 
		$t->{pre} = '{@color}'; 
		return DO_SELF_ONLY() 
	}; %>
<%= apply_templates() %>
EOT

test_xml( $xml, $xps, "\n<doc>blue</doc>\n", 'Interpolation (enabled)'  );

test_xml( '<doc><apple/><banana/></doc>', <<'EOT', "<doc>!<apple></apple><banana></banana>?</doc>\n", 'Prechildren and Postchildren tags, with children' );
<%
	$t->{doc}{prechildren} = '!';
	$t->{doc}{postchildren} = '?';
	$t->{doc}{showtag} = 1;
%><%= apply_templates() %>
EOT

test_xml( '<doc></doc>', <<'EOT', "<doc></doc>\n", 'Prechildren and Postchildren tags, without children' );
<%
	$t->{doc}{prechildren} = '!';
	$t->{doc}{postchildren} = '?';
	$t->{doc}{showtag} = 1;
%><%= apply_templates() %>
EOT

test_xml( '<doc><apple/><banana/></doc>', <<'EOT', "<doc>!<apple></apple>?!<banana></banana>?</doc>\n", 'Prechild and Postchild tags' );
<%
	$t->{doc}{prechild} = '!';
	$t->{doc}{postchild} = '?';
	$t->{doc}{showtag} = 1;
%><%= apply_templates() %>
EOT

test_xml( '<doc>empty</doc>', '<!--#include file="t/include.xps" -->', "#include works!\n", '<!--#include -->' );

test_xml( '<doc>empty</doc>', '<!--#include file="t/include2.xps" -->', "#include works!\n\n", '2 levels of <!--#include -->' );

test_xml( '<doc>empty</doc>', '<!--#include file="t/recursive.xps" -->', "Ooops.\n", 'recursive <!--#include -->' );

test_xml( '<doc>empty</doc>', '<!--#include file="t/include3.xps" -->', "Ooops.\n\n", '2 levels of <!--#include --> + recursion' );

# override of printform
$xps = new XML::YPathScript( xml => '<doc/>', stylesheet => 'how about a shout-o-matic?' );
my $buffer;
$xps->process( sub{ $buffer .= uc shift } );

ok( $buffer, 'HOW ABOUT A SHOUT-O-MATIC?', 'override of printform' );
