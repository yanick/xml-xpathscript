#!/usr/bin/perl 

use strict;
use Test;

BEGIN 
{ 
	plan tests => 6, todo => [];
	unshift @INC, '../blib/arch';
	unshift @INC, '../blib/lib';

}

use XML::YPathScript qw/ libxml /;

ok(1); 

sub test_xml
{
	my( $xml, $style, $result ) = @_;
    my $xps = new XML::YPathScript( xml => $xml, stylesheet => $style );
	my $buffer= $xps->process( 'return' );

	ok( $buffer, $result );
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

exit;

my $style = <<'EOT';
<%  
	$t->{turkey}{pre} =  'llama';
	$t->{chicken}{showtag} = 1;
	$t->{chicken}{testcode} = sub { return DO_SELF_ONLY() };
	$t->{'text()'}{testcode} = sub { $_[1]->{pre} = uc $_[0]->string_value; -1  };
%><%= apply_templates() %>
EOT

my $xml = <<'EOT';
<doc>
<chicken>
<egg>Pok</egg>
</chicken>
<turkey />
<ostrich>gloo?</ostrich>
</doc>
EOT

my $result = <<'EOT';
<doc>
<chicken></chicken>
llama
<ostrich>GLOO?</ostrich>
</doc>
EOT

my $xps = new XML::YPathScript( xml => $xml, stylesheet => $style );

my $buffer="";
$xps->process(sub {$buffer.=shift;});

ok $buffer, $result;

my( $style, $xml );
open $style, "t2.xps";
open $xml, "t1.xml";
my $xps = new XML::YPathScript( xml => $xml, stylesheet => $style );

ok $xps->process(), '<chicken></chicken>';

