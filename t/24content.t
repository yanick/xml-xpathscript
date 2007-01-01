use strict;
use warnings;

use Test::More tests => 4;                      # last test to print

use XML::XPathScript;

my $xps = XML::XPathScript->new();

$xps->set_xml( '<doc><foo type="strange">one<bar>zwei</bar><baz/>'
              .'trois</foo></doc>' );
$xps->set_stylesheet( <<'END_STYLESHEET' );
<%
    $template->set( foo => { content => '<{@type}><%~ bar %></{@type}>' } );
%>
<%-~ /doc -%>
END_STYLESHEET

is $xps->transform => '<doc><strange><bar>zwei</bar></strange></doc>';


$xps->set_xml( <<'END_XML' );
<track track_id="13">
    <title>White and Nerdy</title>
    <artist>Weird Al Yankovic</artist>
    <lyrics> ... </lyrics>
</track>
END_XML

$xps->set_stylesheet( <<'END_STYLESHEET' );
<%-
$template->set( track => { content => <<'END_CONTENT' } );
<%-#  will turn

<track track_id="13">
<title>White and Nerdy</title>
<artist>Weird Al Yankovic</artist>
<lyrics> ... </lyrics>
</track>

into 
                
<song title="White and Nerdy">
<artist>Weird Al Yankovic</artists>
<note>lyrics available</note>
<song>
-%>
<song title="{title/text()}">
    <%-~ artist -%>
    <% if ( findnodes( 'lyrics' ) ) { -%>
    <note>lyrics available</note>
    <%- } -%></song><%- -%>
END_CONTENT
-%>
<%-~ //track -%>
END_STYLESHEET

is $xps->transform => '<song title="White and Nerdy"><artist>Weird Al Yankovic</artist><note>lyrics available</note></song>';

$xps->set_xml( '<doc><foo/></doc>' );
$xps->interpolation( 0 );
$xps->set_stylesheet( <<'END_STYLESHEET' );
<%-  $template->set( foo => { content => '<%= $params{myattr} %>' } );
-%><%= apply_templates( '//foo' => { myattr => 'bar' } ) -%>
END_STYLESHEET

is $xps->transform => 'bar';

# messing with $self shouldn't mess up with the processor
$xps->set_stylesheet( <<'END_STYLESHEET' );
<%-  $template->set( foo => { content => '<%= $self = "mouahaha" %>' } );
-%><%= apply_templates( '//foo' ) -%>
END_STYLESHEET

is $xps->transform => 'mouahaha';
