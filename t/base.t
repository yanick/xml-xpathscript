#!/usr/bin/perl 

use strict;
use Test;

BEGIN 
{ 
	plan tests => 8, todo => [];
	unshift @INC, '../blib/arch';
	unshift @INC, '../blib/lib';

}

use XML::YPathScript;

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

