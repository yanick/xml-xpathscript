#!/usr/bin/perl

use strict;
use Test;

BEGIN
{
	plan tests => 2, todo => [];
}

use XML::XPathScript;

do {
	my $xps = new XML::XPathScript( xml => <<'XML',
<doc>
<linkinter url="/some/where"><blah/></linkinter>
<link url="/some/where"><blah/></link>
</doc>
XML
									stylesheet => <<'STYLE' );
<%
   $t->{'linkinter'}{pre} = '<a href="{@url}">';
   $t->{'linkinter'}{post} = '</a>';
   $t->{'link'}{testcode} = sub {
      my ($currentnode, $t) = @_;
      my $url = findvalue('@url', $currentnode);
      $t->{pre}="<a href=\"$url\">";
      $t->{post}='</a>';
      return 1;
   };
%><%= apply_templates() %>
STYLE

	my $buf="ABC";
	$xps->process(\$buf);

	ok($buf, <<'EXPECTED');
<doc>
<a href="/some/where"><blah></blah></a>
<a href="/some/where"><blah></blah></a>
</doc>
EXPECTED
};


do {
	my $xps = new XML::XPathScript( xml => <<'XML',
<doc>
<link url="/some/where"><blah/></link>
</doc>
XML
									stylesheet => <<'STYLE' );
<%
   die unless (XML::XPathScript->current()->interpolating());
   die unless (XML::XPathScript->current()->interpolating(1));
   die unless (XML::XPathScript->current()->interpolating());

   die unless (XML::XPathScript->current()->interpolating(0));

   die if (XML::XPathScript->current()->interpolating());
   die if (XML::XPathScript->current()->interpolating(0));
   die if (XML::XPathScript->current()->interpolating());

   $t->{'link'}{pre} = '<a href="{@url}">';
   $t->{'link'}{post} = '</a>';
%><%= apply_templates() %>
STYLE

	my $buf="";
	$xps->process(\$buf);

	ok($buf, <<'EXPECTED');
<doc>
<a href="{@url}"><blah></blah></a>
</doc>
EXPECTED
};
