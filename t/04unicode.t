#!/usr/bin/perl -w

use strict;
use Test;
use XML::XPathScript::Processor;

BEGIN
{
	plan tests => 10, todo => [];
}

ok( ! is_utf8_tainted(" ") );

my $utf8=do { use utf8; "Ã©" }; # literal e acute in UTF-8
ok ($utf8 eq "é");

ok( is_utf8_tainted($utf8) );

# Regression
ok(!is_utf8_tainted("Documentation d<39>administration de IDX<45>ReverseProxyé"));


ok (is_utf8_tainted("\x{263A}"));

use XML::XPathScript;

my $isostring = do {
	no utf8;
	"Où qu'il réside, à Nîmes ou même Capharnaüm,".
		" tout Français inscrit au rôle payera son dû dès avant Noël,".
			" qu'il soit naïf ou râleur"
};

ok(! is_utf8_tainted($isostring));

my $style = <<'STYLE';
<%
XML::XPathScript->current()->binmode();
sub utf8tolatin1 {
	my $orig=shift;
	$orig=$orig->string_value() if (ref($orig) =~ m/^XML::/);

	return pack("C*",grep {$_<255} (unpack("U*",$orig)));
}

$t->{convertok}->{testcode}=sub {
    my ($self, $t)=@_;
    $t->{pre}=utf8tolatin1(findvalue("text()",$self));
    return DO_SELF_ONLY;
};

$t->{convertfail}->{testcode}=sub {
    my ($self, $t)=@_;
    $t->{pre}=findvalue("text()",$self);
    return DO_SELF_ONLY;
};
%><%= apply_templates() %>
STYLE

my $xps = new XML::XPathScript(xml => <<"XML", stylesheet => $style);
<?xml version="1.0" encoding="iso-8859-1" ?>
<convertok>$isostring</convertok>
XML

my $result="";

$xps->process(\$result);
ok(! is_utf8_tainted($result));
ok($result eq $isostring."\n") or warn $result;

$xps = new XML::XPathScript(xml => <<"XML", stylesheet => $style);
<?xml version="1.0" encoding="iso-8859-1" ?>
<convertfail>$isostring</convertfail>
XML

$result="";
ok(! eval {$xps->process(\$result); 1}) or warn $result;
ok($@ =~ m/taint/i);

# Dying while STDOUT is butchered by process() is fatal in Perl 5.6.1, so
# please do not add any tests below :-/
