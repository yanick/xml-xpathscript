package XML::XPathScript;

use strict;
use warnings;

# $Revision$ - $Date$

=pod "

=head1 NAME

XML::XPathScript - a Perl framework for XML stylesheets

=head1 SYNOPSIS

  use XML::XPathScript;
  my $xps = XML::XPathScript->new(xml => $xml, stylesheet => $stylesheet);

  # The short way:

  $xps->process();

  # The long way (caching the compiled stylesheet for reuse and
  # outputting to multiple files):

  my $compiled = XML::XPathScript->new(stylesheetfile => $filename)
         ->compile('$r');

  foreach my $xml (@xmlfiles) {
     use IO::File;

     my $currentIO=new IO::File(shift @outputfiles);

     XML::XPathScript->new(xml => $xml, compiledstylesheet=>$compiled)
         ->process(sub {$currentIO->print(shift)});
  };

  # Making extra variables available to the stylesheet dialect:

  my $handler=$xps->compile('$r');

  &$handler($xmltree,&Apache::print,Apache->request());

=head1 DESCRIPTION

This is the I<XML::XPathScript> stylesheet framework, part of the
AxKit project at http://axkit.org/.

XPathScript is a stylesheet language similar in many ways to XSLT (in
concept, not in appearance), for transforming XML from one format to
another (possibly HTML, but XPathScript also shines for non-XML-like
output).

Like XSLT, XPathScript offers a dialect to mix verbatim portions of
documents and code. Also like XSLT, it leverages the powerful
``templates/apply-templates'' and ``cascading stylesheets'' design
patterns, that greatly simplify the design of stylesheets for
programmers. The availability of the I<XPath> query language inside
stylesheets promotes the use of a purely document-dependent,
side-effect-free coding style. But unlike XSLT which uses its own
dedicated control language with an XML-compliant syntax, XPathScript
uses Perl which is terse and highly extendable.

The result of the merge is an extremely powerful tool for rendering
complex XML documents into other formats. Stylesheets written in
XPathScript are very easy to create, extend and reuse, even if they
manage hundreds of different XML tags.

=head1 STYLESHEET WRITER DOCUMENTATION

=head2 Creating stylesheets

See http://axkit.org/docs/xpathscript/guide.dkb for a head start.
There you will learn how to markup the embedded dialect and fill in
the template hash $t.

=head2 xpathscript Invocation

This CPAN module is bundled with an "xpathscript" shell tool that
is to be invoked like this:

   xpathscript mydocument.xml mystylesheet.xps

It will produce the resulting document on standard output. For more
options, refer to xpathscript's man page.

=head2 XPathScript methods available from within the stylesheet

A number of callback functions are available from the stylesheet
proper.  They apply against the current document and template hash,
which are transparently passed back and forth as global variables (see
L</Global variables>). They are defined in the
L<XML::XPathScript::Processor> package, which is implicitly imported into
all code written in the embedded stylesheet dialect.

The following methods are also available to peek at the internal state
of the XPathScript engine from within the stylesheet. Although C<<
XML::XPathScript->current()->whatever() >> may be called from anywhere
within the stylesheet (except a BEGIN or END block or similar), it is
B<most unwise> to alter the state of the interpreter from within a
C<testcode> block as the order of evaluation of the XML nodes is not
specified. Better tweak the stylesheet globals (e.g. L</binmode>) once
and for all at the beginning of the stylesheet.

=over

=item I<current()>

This class method (e.g. C<< XML::XPathScript->current() >>) returns
the stylesheet object currently being applied. This can be called from
anywhere within the stylesheet, except a BEGIN or END block or
similar. B<Beware though> that using the return value for altering (as
opposed to reading) stuff from anywhere except the stylesheet's top
level is unwise.

=cut "

sub current {
    unless (defined $XML::XPathScript::current) {
	use Carp;
	Carp::croak "Wrong context for calling current()";
    };
    return $XML::XPathScript::current;
}

=item I<interpolating()>

=item I<interpolating($boolean)>

Gets (first call form) or sets (second form) the XPath interpolation
boolean flag. If true, values set in C<< $template->{pre} >> and
similar may contain expressions within braces, that will be
interpreted as XPath expressions and substituted in place: for
example, when interpolation is on, the following code

   $t->{'link'}{pre} = '<a href="{@url}">';
   $t->{'link'}{post} = '</a>';

is enough for rendering a C<< <link> >> element as an HTML hyperlink.
The interpolation-less version is slightly more complex as it requires a
C<testcode>:

   $t->{'link'}{testcode} = sub {
      my ($currentnode, $t) = @_;
      my $url = findvalue('@url', $currentnode);
      $t->{pre}="<a href='$url'>";
      $t->{post}='</a>';
	  return DO_SELF_AND_KIDS();
   };

Interpolation is on by default. A (now undocumented) global variable
used to change the default to off, but don't do that.

=cut "

sub interpolating {
    my $self=shift;

    $self->{interpolating}=shift if (@_);

    return exists $self->{interpolating} ?
			$self->{interpolating} :
		    !$XML::XPathScript::DoNotInterpolate; # Obsolete, for compatibility:
}

=pod "

=item I<binmode()>

Declares that the stylesheet output is B<not> in UTF-8, but instead in
an (unspecified) character encoding embedded in the stylesheet source
that neither Perl nor XPathScript should have any business dealing
with. Calling C<< XML::XPathScript->current()->binmode() >> is an
B<irreversible> operation with the consequences outlined in L</The
Unicode mess>.

=cut "

sub binmode {
    my ($self)=@_;
    $self->{binmode}=1;
    binmode ORIGINAL_STDOUT if (! defined $self->{printer});
}

=pod "

=back

=head2 Stylesheet Guidelines

Here are a few things to watch out for when coding stylesheets.

=head3 The Unicode mess

Unicode is a balucitherian character numbering standard, that strives
to be a superset of all character sets currently in use by humans and
computers. Going Unicode is therefore the way of the future, as it
will guarantee compatibility of your applications with every character
set on planet Earth: for this reason, all XML-compliant APIs
(XML::XPathScript being no exception) should return Unicode strings in
all their calls, regardless of the charset used to encode the XML
document to begin with.

The gotcha is, the brave Unicode world sells itself in much the same
way as XML when it promises that you'll still be able to read your
data back in 30 years: that will probably turn out to be true, but
until then, you can't :-)

Therefore, you as a stylesheet author will more likely than not need
to do some wrestling with Unicode in Perl, XML::XPathScript or
not. Here is a primer on how.

=head4 Unicode, UTF-8 and Perl

Unicode is B<not> a text file format: UTF-8 is. Perl, when doing
Unicode, prefers to use UTF-8 internally.

Unicode is a character numbering standard: that is, an abstract
registry that associates unique integer numbers to a cast of thousands
of characters. For example the "smiling face" is character number
0x263a, and the thin space is 0x2009 (there is a URL to a Unicode
character table in L</SEE ALSO>). Of course, this means that the
8-bits- (or even, Heaven forbid, 7-bits-?)-per-character idea goes
through the window this instant. Coding every character on 16 bits in
memory is an option (called UTF-16), but not as simple an idea as it
sounds: one would have to rewrite nearly every piece of C code for
starters, and even then the Chinese aren't quite happy with "only"
65536 character code points.

Introducing UTF-8, which is a way of encoding Unicode character
numbers (of any size) in an ASCII- and C-friendly way: all 127 ASCII
characters (such as "A" or or "/" or ".", but I<not> the ISO-8859-1
8-bit extensions) have the same encoding in both ASCII and UTF-8,
including the null character (which is good for strcpy() and
friends). Of course, this means that the other characters are rendered
using I<several> bytes, for example "é" is "Ã©" in UTF-8. The result
is therefore vaguely intelligible for a Western reader.

=head4 Output to UTF-8 with XPathScript

The programmer- and C-friendly characteristics of UTF-8 have made it
the choice for dealing with Unicode in Perl. The interpreter maintains
an "UTF8-tainted" bit on every string scalar it handles (much like
what L<perlsec> does for untrusted data). Every function in
XML::XPathScript returns a string with such bit set to true:
therefore, producing UTF-8 output is straightforward and one does not
have to take any special precautions in XPathScript.

=head4 Output to a non-UTF-8 character set with XPathScript

When L</binmode> is invoked from the stylesheet body, it signals that
the stylesheet output should I<not> be UTF-8, but instead some
user-chosen character encoding that XML::XPathScript cannot and will
not know or care about. Calling C<<
XML::XPathScript->current()->binmode() >> has the following
consequences:

=over 2

=item *

presence of this "UTF-8 taint" in the stylesheet output is now a fatal
error. That is, whenever the result of a template evaluation is marked
internally in Perl with the "this string is UTF-8" flag (as opposed to
being treated by Perl as binary data without character meaning, see
L</perlunicode>), L<XML::XPathScript::Processor/translate_node> will
croak;

=item *

the stylesheet therefore needs to build an "unicode firewall". That
is, C<testcode> blocks have to take input in UTF-8 (as per the XML
standard, UTF-8 indeed is what will be returned by
L<XML::XPathScript::Processor/findvalue> and such) and provide output in
binary (in whatever character set is intended for the output), lest
I<translate_node()> croaks as explained above. The L<Unicode::String>
module comes in handy to the stylesheet writer to cast from UTF-8 to
an 8-bit-per-character charset such as ISO 8859-1, while laundering
Perl's internal UTF-8-string bit at the same time;

=item *

the appropriate voodoo is performed on the output filehandle(s) so
that a spurious, final charset conversion will not happen at print()
time under any locales, versions of Perl, or phases of moon.

=back


=head3 XPath scalar return values considered harmful

XML::XPath calls such as I<findvalue()> return objects in an object
class designed to map one of the types mandated by the XPath spec (see
L<XML::XPath> for details). This is often not what a Perl programmer
comes to expect (e.g. strings and numbers cannot be treated the
same). There are some work-arounds built in XML::XPath, using operator
overloading: when using those objects as strings (by concatenating
them, using them in regular expressions etc.), they become strings,
through a transparent call to one of their methods such as I<<
->value() >>. However, we do not support this for a variety of reasons
(from limitations in L</overload> to stylesheet compatibility between
XML::XPath and XML::LibXML to Unicode considerations), and that is why
our L</findvalue> and friends return a real Perl scalar, in violation
of the XPath specification.

On the other hand, L</findnodes> does return a list of objects in list
context, and an I<XML::XPath::NodeSet> or I<XML::LibXML::NodeList>
instance in scalar context, obeying the XPath specification in
full. Therefore you most likely do not want to call I<findnodes()> in
scalar context, ever: replace

   my $attrnode = findnodes('@url',$xrefnode); # WRONG!

with

   my ($attrnode) = findnodes('@url',$xrefnode);


=head3 Do not use DOM method calls, for they make stylesheets non-portable

The I<findvalue()> such functions described in
L<XML::XPathScript::Processor> are not the only way of extracting bits from
the XML document. Objects passed as the first argument to the I<<
->{testcode} >> templates and returned by I<findnodes()> in array
context are of one of the I<XML::XPath::Node::*> classes, and they
feature some data extraction methods by themselves, conforming to the
DOM specification.

However, the names of those methods are not standardized even among
DOM parsers (the accessor to the C<childNodes> property, for example,
is named C<childNodes()> in I<XML::LibXML> and C<getChildNodes()> in
I<XML::XPath>!). In order to write a stylesheet that is portable
between L<XML::libXML> and L<XML::XPath> used as back-ends to
L<XML::XPathScript>, one should refrain from doing that. The exact
same data is available through appropriate XPath formulae, albeit more
slowly, and there are also type-checking accessors such as
C<is_element_node()> in L<XML::XPathScript::Processor>.


=head1 TECHNICAL DOCUMENTATION

The rest of this POD documentation is B<not> useful to programmers who
just want to write stylesheets; it is of use only to people wanting to
call existing stylesheets or more generally embed the XPathScript
engine into some wider framework.

I<XML::XPathScript> is an object-oriented class with the following features:

=over

=item *

an I<embedded Perl dialect> that allows the merging of the stylesheet
code with snippets of the output document. Don't be afraid, this is
exactly the same kind of stuff as in I<Text::Template>, I<HTML::Mason>
or other similar packages: instead of having text inside Perl (that
one I<print()>s), we have Perl inside text, with a special escaping
form that a preprocessor interprets and extracts. For XPathScript,
this preprocessor is embodied by the I<xpathscript> shell tool (see
L</xpathscript Invocation>) and also available through this package's
API;

=item *

a I<templating engine>, that does the apply-templates loop, starting
from the top XML node and applying templates to it and its subnodes as
directed by the stylesheet.

=back

When run, the stylesheet is expected to fill in the I<template hash>
$t, which is a lexically-scoped variable made available to it at
preprocess time.

=head2 Dependencies

Although XPathScript is a core component of AxKit, which depends on
this module to be able to process XPathScript stylesheets, there is
plenty of motivation for doing stylesheets outside of a WWW
application server and so I<XML::XPathScript> is also distributed as a
standalone CPAN module.  The AxKit XPathScript component inherits from
this class and provides the coupling with the application framework by
overloading and adding some methods.

I<XML::XPathScript> requires the following Perl packages:

=over

=item I<Symbol>

For loading files from anonymous filehandles. I<Symbol> is bundled
with Perl.

=item I<File::Basename>

For fetching stylesheets from system files. One may provide other
means of fetching stylesheets through object inheritance (this is what
AxKit does). I<File::Basename> is bundled with Perl.

=item I<XML::Parser>

=item I<XML::XPath>

For the XML parser and XPath interpreter, obviously needed. Plans are
to support the I<XML::libXML> package as an alternative, which does
the same as the above in C (and hence an order of magnitude faster).

=back

=head2 Global variables

Due to the peculiar syntax allowed in the embedded dialect for
accessing the template hash, the stylesheet is not reentrant and
cannot (yet) transform several documents at once. However, one should
not rely on those variables existing forever.

=over

=item I<$XML::XPathScript::xp>

The XML::XPath object that holds the whole document (created by
L<XML::XPath/new>)

=item I<$XML::XPathScript::trans>

The template hash currently in use (known as $t in the AxKit
documentation). Its keys are element names, and its values are
the matching templates (as hash references).

=back

=cut "


use vars qw( $VERSION $XML_parser $DoNotInterpolate $debug_level );

use Symbol;
use File::Basename;
use XML::XPathScript::Processor;

$VERSION = '0.16';

$XML_parser = 'XML::LibXML';

# By default, we interpolate
$DoNotInterpolate = 0;

# internal variable for debugging information. 
# 0 is total silence and 10 is complete verbiage
$debug_level = 0;

sub import
{
	my $self = shift @_;

	if( grep $_ eq 'XML::XPath', @_ )
	{
		$XML::XPathScript::XML_parser = 'XML::XPath';
	}
	elsif( grep $_ eq 'XML::LibXML', @_ )
	{
		$XML::XPathScript::XML_parser = 'XML::LibXML';
	}
}

INIT
{
	if( $XML_parser eq 'XML::XPath' )
	{
		eval <<EOT;
			use XML::XPath 1.0;
			use XML::XPath::XMLParser;
			use XML::XPath::Node;
			use XML::XPath::NodeSet;
			use XML::Parser;
EOT
		die $@ if $@;
	}
	else
	{
		eval 'use XML::LibXML';
		die $@ if $@;
	}

}

=pod "

=head2 Methods and class methods

=over

=item I<< new(key1=>value1,key2=>value2,...) >>

Creates a new XPathScript translator. The recognized named arguments are

=over

=item xml => $xml

$xml is a scalar containing XML text, or a reference to a filehandle
from which XML input is available, or an I<XML::XPath> or
I<XML::libXML> object (support for the latter object class is very
poor for now, as it involves unparsing and parsing back into
I<XML::XPath>).

An XML::XPathscript object without an I<xml> argument
to the constructor is only able to compile stylesheets (see
L</SYNOPSIS>).

=item stylesheet => $stylesheet

$stylesheet is a scalar containing the stylesheet text, or a reference
to a filehandle from which the stylesheet text is available.  The
stylesheet text may contain unresolved C<< <!--#include --> >>
constructs, which will be resolved relative to ".".

=item stylesheetfile => $filename

Same as I<stylesheet> but let I<XML::XPathScript> do the loading
itself.  Using this form, relative C<< <!--#include --> >>s in the
stylesheet file will be honored with respect to the dirname of
$filename instead of "."; this provides SGML-style behaviour for
inclusion (it does not depend on the current directory), which is
usually what you want.

=item compiledstylesheet => $function

Re-uses a previous return value of I<compile()> (see L</SYNOPSIS> and
L</compile>), typically to apply the same stylesheet to several XML
documents in a row.

=item interpolation_regex => $regex

Sets the interpolation regex to be $regex. Whatever is
captured in $1 will be used as the xpath expression. 
Defaults to qr/{(.*?)}/.

=back

=cut "

sub new {
    my $class = shift;
    die "Invalid hash call to new" if @_ % 2;
    my %params = @_;
    my $self = \%params;
	$self->{interpolation_regex} ||= qr/{(.*?)}/;
    bless $self, $class;
}


=pod "

=item I<process()>

=item I<process($printer)>

=item I<process($printer,@varvalues)>

Processes the document and stylesheet set at construction time, and
prints the result to STDOUT by default. If $printer is set, it must be
either a reference to a filehandle open for output, or a reference to
a string, or a reference to a subroutine which does the output, as in

   my $buffer="";
   $xps->process(sub {$buffer.=shift;});

or

   $xps->process(sub {print ANOTHERFD (shift);});

(not that the latter would be any good, since C<<
$xps->process(\*ANOTHERFD) >> would do exactly the same, only faster)

If the stylesheet was I<compile()>d with extra I<varname>s, then the
calling code should call I<process()> with a corresponding number of
@varvalues. The corresponding lexical variables will be set
accordingly, so that the stylesheet code can get at them (looking at
L</SYNOPSIS>) is the easiest way of getting the meaning of this
sentence).

=cut "

sub process {
    my ($self, $printer, @extravars) = @_;

    do { $$printer="" } if (UNIVERSAL::isa($printer, "SCALAR"));
    $self->{printer}=$printer if $printer;


    my $xpath;

	#warn "Entering process...";

	if( $XML_parser eq 'XML::XPath' )
	{
		eval <<EOT;
			use XML::XPath 1.0;
			use XML::XPath::XMLParser;
			use XML::XPath::Node;
			use XML::XPath::NodeSet;
			use XML::Parser;
EOT
		die $@ if $@;
	}
	else
	{
		eval 'use XML::LibXML';
		die $@ if $@;
	}

	#warn "Eval'uation done...";


	# a third option should be auto, for which we
	# would use the already-defined object
	if( $XML_parser eq 'auto' )
	{
		if (UNIVERSAL::isa($self->{xml},"XML::XPath")) 
		{
			$xpath=$self->{xml};
			$XML_parser = 'XML::XPath';
		}
		elsif(UNIVERSAL::isa($self->{xml},"XML::LibXML" ))
		{
			$xpath=$self->{xml};
			$XML_parser = 'XML::LibXML';
		}
	}

    if (UNIVERSAL::isa($self->{xml},"XML::XPath")) 
	{
		if( $XML_parser eq 'XML::XPath' or $XML_parser eq 'auto' )
		{
			$xpath=$self->{xml};
			$XML_parser = 'XML::XPath';
		}
		else 		# parser if XML::LibXML
		{
			$xpath = XML::LibXML->parse_string( $self->{xml}->toString )->documentElement;
		}
    } 
	elsif (UNIVERSAL::isa($self->{xml},"XML::libXML")) 
	{
		if( $XML_parser eq 'XML::LibXML' or $XML_parser eq 'auto' )
		{
			$xpath=$self->{xml};
			$XML_parser = 'XML::LibXML';
		}
		else 		# parser if xpath
		{
			$xpath = new XML::XPath( xml => $self->{xml}->toString );
		}
    } 
	else
	{
		$XML_parser = 'XML::LibXML' if $XML_parser eq 'auto';

		if (ref($self->{xml})) 
		{
			$xpath= ( $XML_parser eq 'XML::LibXML' ) ? 
			    XML::LibXML->new->parse_fh( $self->{xml} )->documentElement :
				XML::XPath->new(ioref => $self->{xml})
		} 
		else 
		{
			$xpath= ( $XML_parser eq 'XML::LibXML' ) ? 
			    XML::LibXML->new->parse_string( $self->{xml} )->documentElement :
				XML::XPath->new( xml => $self->{xml});
		};
	}

	$self->{dom} = $xpath;

	$xpath->ownerDocument->setEncoding( "UTF-8" ) 
		if $XML_parser eq 'XML::LibXML';

	{
		local *ORIGINAL_STDOUT;
		*ORIGINAL_STDOUT = *STDOUT;
   		local *STDOUT;

		# Perl 5.6.1 dislikes closed but tied descriptors (causes SEGVage)
   		*STDOUT = *ORIGINAL_STDOUT if $^V lt v5.7.0; 

	   	tie *STDOUT, 'XML::XPathScript::StdoutSnatcher';
	   	my $retval = $self->compile()->( $self, @extravars );
	   	untie *STDOUT;
	   	return $retval;
	}
}

=pod "

=item I<extract($stylesheet)>

=item I<extract($stylesheet,$filename)>

=item I<extract($stylesheet,@includestack)> # from include_file() only

The embedded dialect parser. Given $stylesheet, which is either a
filehandle reference or a string, returns a string that holds all the
code in real Perl. Unquoted text and C<< <%= stuff %> >> constructs in
the stylesheet dialect are converted into invocations of I<<
XML::XPathScript->current()->print() >>, while C<< <% stuff %> >>
constructs are transcripted verbatim.

C<< <!-- #include --> >> constructs are expanded by passing their
filename argument to L</include_file> along with @includestack (if any)
like this:

   $self->include_file($includefilename,@includestack);

@includestack is not interpreted by I<extract()> (except for the first
entry, to create line tags for the debugger). It is only a bandaid for
I<include_file()> to pass the inclusion stack to itself across the
mutual recursion existing between the two methods (see
L</include_file>).  If I<extract()> is invoked from outside
I<include_file()>, the last invocation form should not be used.

This method does a purely syntactic job. No special framework
declaration is prepended for isolating the code in its own package,
defining $t or the like (L</compile> does that). It may be overriden
in subclasses to provide different escape forms in the stylesheet
dialect.

=cut "

sub extract {
    my ($self,$stylesheet,@includestack) = @_;

    my $filename=$includestack[0] || "stylesheet";

    my $contents = $self->read_stylesheet( $stylesheet );

    my $script="#line 1 $filename\n",
    my $line = 1;

    while ($contents =~ /\G(.*?)(<!--#include|<%[#=]?)/gcs) {
        my ($text, $type) = ($1, $2);
        $line += $text =~ tr/\n//; # count \n's in text
        $text =~ s/\|/\\\|/g;
        $script .= "print(q|$text|);";
        $script .= "\n#line $line $filename\n";
        if ($type eq '<%=') {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
            $script .= "print( $perl );\n";
            $line += $perl =~ tr/\n//;
        }
        elsif ($type eq '<!--#include') {
            my %params;
            while ($contents =~ /\G(\s+(\w+)\s*=\s*(["'])([^\3]*?)\3|\s*-->)/gcs) {
                last if $1 eq '-->';
                $params{$2} = $4 if (defined $2);
            }

			die "No matching file attribute in #include at line $line"
				unless $params{file};

            $script .= $self->include_file($params{file},@includestack);
        }
        else {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
	    if( $type ne '<%#' ) {
		    $perl =~ s/;?$/;/s; # add on ; if its missing. As in <% $foo = 'Hello' %>
		    $script .= $perl;
	    }
            $line += $perl =~ tr/\n//;
        }
    }

    if ($contents =~ /\G(.+)/gcs) {
        my $text = $1;
        $text =~ s/\|/\\\|/g;
        $script .= "print(q|$text|);";
    }

    return $script;
}

=pod "

=item $string = I<read_stylesheet( $stylesheet )>

Read the $stylesheet (which can be a filehandler or a string). 
Used by I<extract> and exists such that it can be overloaded in
I<Apache::AxKit::Language::YPathScript>.

=cut

sub read_stylesheet
{
	my( $self, $stylesheet ) = @_;
	
	# $stylesheet can be a filehandler
	# or a string
    if( ref($stylesheet) ) {
        local $/;
        return <$stylesheet>;
    }
    else {
        return $stylesheet;
    }
	
}

=pod "

=item I<include_file($filename)>

=item I<include_file($filename,@includestack)>

Resolves a C<< <!--#include file="foo" --> >> directive on behalf of
I<extract()>, that is, returns the script contents of
I<$filename>. The return value must be de-embedded too, which means
that I<extract()> has to be called recursively to expand the contents
of $filename (which may contain more C<< <!--#include --> >>s etc.)

$filename has to be slash-separated, whatever OS it is you are using
(this is the XML way of things). If $filename is relative (i.e. does
not begin with "/" or "./"), it is resolved according to the basename
of the stylesheet that includes it (that is, $includestack[0], see
below) or "." if we are in the topmost stylesheet. Filenames beginning
with "./" are considered absolute; this gives stylesheet writers a way
to specify that they really really want a stylesheet that lies in the
system's current working directory.

@includestack is the include stack currently in use, made up of all
values of $filename through the stack, lastly added (innermost)
entries first. The toplevel stylesheet is not in @includestack
(that is, the outermost call does not specify an @includestack).

This method may be overridden in subclasses to provide support for
alternate namespaces (e.g. ``axkit://'' URIs).

=cut "

sub include_file {
    my ($self, $filename, @includestack) = @_;

    if ($filename !~ m|^\.?/|) {
	my $reldir;
	# We guarantee that all values we insert into @includestack begin
	# either with "/" or "./". This allows us to do the relative
	# directory thing, and at the same time we get to safely ignore
	# bizarre URIs inserted by inheriting classes.

	if ($includestack[0] && $includestack[0] =~ m|^\.?/|) {
	    $reldir=dirname($includestack[0]);
	} else {
	    $reldir=".";
	};
	$filename = "$reldir/$filename";
    }
	
	# are we going recursive?
	return '' if grep $_ eq $filename, @includestack;

    my $sym = gensym;
    open($sym, $filename) || do {
	use Carp;
	Carp::croak "Can't read include file '$filename': $!";
    };
    return $self->extract($sym, $filename, @includestack);
}

=pod "

=item I<compile()>

=item I<compile(varname1, varname2,...)>

Compiles the stylesheet set at I<new()> time and returns an anonymous
CODE reference. $stylesheet shall be written in the unparsed embedded
dialect (in other words C<< ->extract($stylesheet) >> will be called
first inside I<compile()>).

I<varname1>, I<varname2>, etc. are extraneous arguments that will be
made available to the stylesheet dialect as lexically scoped
variables. L</SYNOPSIS> shows a way to use this feature to pass the
Apache handler to AxKit XPathScript stylesheets, which explains this
feature better than a lengthy paragraph would do.

The return value is an opaque token that encapsulates a compiled
stylesheet.  It should not be used, except as the
I<compiledstylesheet> argument to I<new()> to initiate new objects and
amortize the compilation time.  Subclasses may alter the type of the
return value, but will need to overload I<process()> accordingly of
course.

The I<compile()> method is idempotent. Subsequent calls to it will
return the very same token, and calls to it when a
I<compiledstylesheet> argument was set at I<new()> time will return
said argument.

=cut "

# Internal documentation: the return value is an anonymous sub whose
# prototype is
#     &$compiledfunc($xpathscriptobj, $val1, $val2,...);

sub compile {
    my ($self,@extravars) = @_;

	return $self->{compiledstylesheet}
		if defined $self->{compiledstylesheet};

    my $stylesheet;

    if (exists $self->{stylesheet}) {
		$stylesheet=$self->{stylesheet};
    } 
	elsif (exists $self->{stylesheetfile}) {
		# This hack fails if $self->{stylesheetfile} contains
		# double quotes.  I think we can ignore this and get
		# away.
		$stylesheet=qq:<!--#include file="$self->{stylesheetfile}" -->:;
    } 
	else {
		die "Cannot compile without a stylesheet\n";
    };

    my $script = $self->extract($stylesheet);

    my $package=gen_package_name();

	my $extravars = join ',', @extravars;
	
	my $eval = <<EOT;
		    package $package;
		    no strict;   # Don't moan on sloppyly
		    no warnings; # written stylesheets
			
			use $XML_parser;  
		    BEGIN {XML::XPathScript::Processor->import;}
		    sub {
		    	my (\$self, $extravars ) = \@_;
				local \$XML::XPathScript::current=\$self;
		    	my \$t = \$XML::XPathScript::current->{t} = {};
				local \$XML::XPathScript::trans = \$t; # Yes,
				# this does the sharing! Perl is a bizarre and
				# wonderful language.
				local \$XML::XPathScript::xp=\$self->{dom};
				$script
		    }
EOT

	#warn "script ready for compil: $eval";
    local $^W;
	$self->debug( 10, "Compiling code:\n $eval" );
    my $retval = eval $eval;
    die $@ unless defined $retval;

    return $self->{compiledstylesheet} = $retval;
}




=item I<print($text)>

Outputs a chunk of text on behalf of the stylesheet. The default
implementation is to use the second argument to L</process>, which was
stashed in C<< $self->{printer} >> by said function. Overloading this
method in a subclass provides yet another method to redirect output.

=cut "

sub print {
    my ($self, @text)=@_;
    my $printer=$self->{printer};
    if (!defined $printer) {
	print ORIGINAL_STDOUT @text;
    } elsif (ref($printer) eq "CODE") {
	$printer->(@text);
    } elsif (UNIVERSAL::isa($printer, "SCALAR")) {
	$$printer.= join '', @text;
    } else {
	local $\=undef;
	print $printer @text;
    };
}


#  $self->debug( $level, $message )
#	Display debugging information

sub debug {
	warn $_[2] if $_[1] <= $debug_level;
}

=pod "

=back

=head2 Utility functions

The functions below are not methods.

=over

=item I<gen_package_name()>

Generates a fresh package name in which we would compile a new
stylesheet. Never returns twice the same name.

=cut "

do {
my $uniquifier;
sub gen_package_name {
    $uniquifier++;
    return "XML::XPathScript::STYLESHEET$uniquifier";
}
};

=item  $nodeset = $xps->document( $uri )

	Reads XML given in $uri, parses it and returns it in a nodeset.

=cut

sub document {
    # warn "Document function called\n";
    my( $self, $uri ) = @_;
	  
	my( $results, $parser );	
	if( $XML_parser eq 'XML::XPath' ) {
		my $xml_parser = XML::Parser->new(
				ErrorContext => 2,
				Namespaces => $XML::XPath::VERSION < 1.07 ? 1 : 0,
				# ParseParamEnt => 1,
				);
	
		$parser = XML::XPath::XMLParser->new(parser => $xml_parser);
		$results = XML::XPath::NodeSet->new();
	} 
	elsif ( $XML_parser eq 'XML::LibXML' ) {
		$parser = XML::LibXML->new;
		$results = XML::LibXML::Document->new;
	}
	else {
		$self->die( "xml parser not valid: $XML_parser" );
	}

	
    my $newdoc;
	# TODO: must handle axkit: scheme a little more cleverly
    if ($uri =~ /^\w\w+:/ and $uri !~ /^axkit:/ ) { # assume it's scheme://foo uri
        eval {
         	$self->debug( 5, "trying to parse $uri" );
			eval "use LWP::Simple";
            $newdoc = $parser->parse_string( LWP::Simple::get( $uri ) );
            $self->debug( 5, "Parsed OK into $newdoc\n" );
        };
        if (my $E = $@) {
			$self->debug("Parse of '$uri' failed: $E" );
        }
    }
    else {
        $self->debug(3, "Parsing local: $uri\n");
		if( $XML_parser eq 'XML::LibXML' ) {
        	$newdoc = $parser->parse_file( $uri );
		} elsif( $XML_parser eq 'XML::XPath' ) {
			$newdoc = XML::XPath->new( filename => $uri );
		}
		else { die "invalid parser: $XML_parser\n"; }
    }

	if( $newdoc ) {
		if( $XML_parser eq 'XML::LibXML' ) {
			$results = $newdoc->documentElement();
		} 
		elsif( $XML_parser eq 'XML::XPath' ) {
			$results = $newdoc->findnodes('/')->[0]->getChildNodes->[0];
		}
	}
	
    $self->debug(8, "XPathScript: document() returning");
    return $results;
}

1;

# small package to catch print statements within
# the stylesheets
package XML::XPathScript::StdoutSnatcher;

sub TIEHANDLE { my $self = ''; bless \$self, $_[0] }
sub PRINT {
	my $self = shift;
	XML::XPathScript::current()->print( @_ );
}
sub BINMODE {
    XML::XPathScript::current()->binmode( @_ );
}

'end of XML::XPathScript::StdoutSnatcher' ;

__END__

=back



=head1 AUTHORS

Created by Matt Sergeant <matt@sergeant.org>

Improvements and feature merge with
Apache::AxKit::Language::XPathScript by Yanick Champoux
<yanick@babyl.dyndns.org> and Dominique Quatravaux <dom@idealx.com>

=head1 LICENSE

This is free software. You may distribute it under the same terms as
Perl itself.

=head1 SEE ALSO

The XPathScript Guide at

  http://axkit.org/wiki/view/AxKit/XPathScriptGuide

XPath documentation from W3C:

  http://www.w3.org/TR/xpath

Unicode character table:

  http://www.unicode.org/charts/charindex.html

=cut

# Local Variables:
# mode:cperl
# tab-width:8
# End:
