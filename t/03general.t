use strict;
use Test;

BEGIN { 
	plan tests => 20, todo => [];
}

use XML::XPathScript;
use Apache::AxKit::Language::YPathScript;

ok(1); 

sub test_xml {
	my( $xml, $style, $result, $comment ) = @_;
    my $xps = new XML::XPathScript( xml => $xml, stylesheet => $style );
	my $buffer;
	$xps->process( \$buffer );

	ok( $buffer, $result, $comment );
}


test_xml( '<doc>dummy</doc>', 'working', 'working', 'empty run' );

test_xml( '<doc>dummy</doc>', '<%= apply_templates() %>', 
	'<doc>dummy</doc>', 'simple in/out');

test_xml( '<doc>dummy</doc>', '<% print "Hello!" %>', 'Hello!', 
	'rogue print statement' );

test_xml( '<doc><!-- hello world --></doc>', <<'EOT', "<doc>comment: hello world </doc>\n", 'processing a comment' );
<% $t->{'#comment'}{pre} = "comment:"; %><%= apply_templates() %>
EOT

test_xml( '<doc><!-- hello world --></doc>', <<'EOT', "<doc></doc>\n", 'masking a comment' );
<% $t->{'#comment'}{testcode} = sub{ 0 } %><%= apply_templates() %>
EOT

############################################################
# Interpolation

my $xml = "<doc><node color='blue'>Hello</node></doc>";
my $xps = <<'EOT';
<% 
	$XML::XPathScript::DoNotInterpolate = 1;
	$t->{node}{testcode} = sub { 
		my( $n, $t ) = @_; 
		$t->{pre} = '{@color}'; 
		return DO_SELF_ONLY() 
	}; 
%>
<%= apply_templates() %>
EOT
test_xml( $xml, $xps, "\n<doc>{\@color}</doc>\n", 'Interpolation (disabled)'  );

$xps = <<'EOT';
<% 
	$XML::XPathScript::DoNotInterpolate = 0; 
	$t->{node}{testcode} = sub
	{ 
		my( $n, $t ) = @_; 
		$t->{pre} = '{@color}'; 
		return DO_SELF_ONLY() 
	}; %>
<%= apply_templates() %>
EOT

test_xml( $xml, $xps, "\n<doc>blue</doc>\n", 'Interpolation (enabled)'  );

############################################################
# double interpolation 

$xps = <<'EOT';
<% 
	$XML::XPathScript::DoNotInterpolate = 0; 
	$t->{node}{testcode} = sub
	{ 
		my( $n, $t ) = @_; 
		$t->{pre} = '{@color}:{@color}'; 
		return DO_SELF_ONLY() 
	}; %>
<%= apply_templates() %>
EOT
test_xml( $xml, $xps, "\n<doc>blue:blue</doc>\n", 'Double interpolation'  );

############################################################
# interpolation regex

test_xml( '<doc arg="stuff" />', <<'XPS' , "stuff\n", 'interpolation regex' );
<%
	$XML::XPathScript::current->{interpolation_regex} = qr/\[\[(.*?)\]\]/;
	$t->{doc}{pre} = '[[@arg]]';
%><%= apply_templates() %>
XPS


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
$xps = new XML::XPathScript( xml => '<doc/>', stylesheet => 'how about a shout-o-matic?' );
my $buffer;
$xps->process( sub{ $buffer .= uc shift } );

ok( $buffer, 'HOW ABOUT A SHOUT-O-MATIC?', 'override of printform' );


test_xml( '<doc><a/><b/><c/></doc>', <<'EOXPS', "only b: <b></b>\n", 'xpath testcode return statement' );
<%
    $t->{doc}{pre} = 'only b: ';	
	$t->{doc}{testcode} = sub{ 'b'; }
%><%= apply_templates() %>
EOXPS


# encoding
#test_xml( '<doc>&#1000;</doc>', '<%= apply_templates() %>', '', 'Encoding' ); 

# testing for proper STDOUT management
{
	my $xps = new XML::XPathScript( xml => '<blah>hello</blah>', stylesheet => '<%= apply_templates()%>' );
	my $output_file = 't/output.xml';
	local *STDOUT;
	die "file $output_file shouldn't be there" if -f $output_file;
	open STDOUT, ">$output_file" or die $!;
	$xps->process;
	close STDOUT;
	open FILE, $output_file or die "$!";
	ok( <FILE>, '<blah>hello</blah>', 'STDOUT management');
	close FILE;
	unlink $output_file or die $!;
}

