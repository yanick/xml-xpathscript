package XML::YPathScript;
# Revision: $Rev: 12 $
# Last Changed: $Date: 2004-07-11 16:37:16 -0400 (Sun, 11 Jul 2004) $   

use strict;
use warnings;

=pod

=head1 NAME

XML::YPathScript - a Perl framework for XML stylesheets

=head1 SYNOPSIS

  use XML::YPathScript;
  my $xps = XML::YPathScript->new(xml => $xml, stylesheet => $stylesheet);

  # The short way:

  $xps->process();

  # The long way (caching the compiled stylesheet for reuse and
  # outputting to multiple files):

  my $compiled = XML::YPathScript->new(stylesheetfile => $filename)
         ->compile('$r');

  foreach my $xml (@xmlfiles) 
  {
     use IO::File;

     my $currentIO=new IO::File(shift @outputfiles);

     XML::YPathScript->new(xml => $xml, compiledstylesheet=>$compiled)
         ->process(sub {$currentIO->print(shift)});
  };

  # Making extra variables available to the stylesheet dialect:

  my $handler=$xps->compile('$r');

  &$handler($xmltree,&Apache::print,Apache->request());

=head1 DESCRIPTION

I<XML::YPathScript> is a fork of I<XML::XPathScript>, 
itself a fork of Matt Sergeant's I<XPathScript>, and
unless where specified otherwise, it is meant to provide
the same functionality and behaviors than the original XPathScript. 

Notable differences with XPS are:

	* YPS uses either XML::libXML or XML::XPath as its parser.
	
	* YPS's interpolation is controlled by the variable 
		$XML::YPathScript::DoNotInterpolate. By default
		YPS does not interpolate.
		
	* the pre/postchildren template sub-keys are only used if
		the node actually has children 
		
	* the template supports a #comment key. 
	  E.g.:
	  	
		$t->{'#comment'}{pre}  = 'This is a comment {';
		$t->{'#comment'}{post} = "}\n";
	

		


YPathScript is a stylesheet language similar in many ways to XSLT (in
concept, not in appearance), for transforming XML from one format to
another format (possibly HTML, but YPathScript also shines for
non-XML-like output).

Like XSLT, YPathScript uses the powerful ``templates/apply-templates''
and ``cascading stylesheets'' design patterns, that greatly simplifies
the design of stylesheets for programmers. The availability of the
I<XPath> query language inside stylesheets promotes the use of a purely
document-dependent, side-effect-free coding style. Unlike XSLT which
uses its own dedicated control language with an XML-compliant syntax,
YPathScript uses Perl which is terse and highly extendable.

The result of the merge is an extremely powerful environment for
development tasks that involve rendering complex XML documents to
other formats. Stylesheets written in YPathScript are very easy to
create, extend and reuse, even if they treat hundreds of different
XML tags.

=head1 STYLESHEET WRITER DOCUMENTATION

=head2 Creating stylesheets

See http://axkit.org/docs/xpathscript/guide.dkb for a head start.
There you will learn how to markup the embedded dialect and fill in
the template hash $t.

=head2 xpathscript Invocation

This module is bundled with an "yps" shell tool that
is to be invoked like this:

   yps mydocument.xml mystylesheet.xps

It will produce the resulting document on standard output. For more
options, refer to yps's man page.

=head2 Functions and global variables available in the stylesheet

A number of callback functions are available from the stylesheet
proper.  They apply against the current document and template hash,
which are transparently passed back and forth as global variables (see
L</Global variables>). They are defined in the
I<XML::YPathScript::Toys> package, which is implicitly imported into
all code written in the embedded stylesheet dialect.

=over

=cut 


=pod "

=back

=head1 TECHNICAL DOCUMENTATION

The rest of this POD documentation is B<not> useful to programmers who
just want to write stylesheets; it is of use only to people wanting to
call existing stylesheets or more generally embed the YPathScript
motor into some wider framework.

I<XML::YPathScript> is an object-oriented class with the following features:

=over

=item *

an I<embedded Perl dialect> that allows the merging of the stylesheet
code with snippets of the output document. Don't be afraid, this is
exactly the same kind of stuff as in I<Text::Template>, I<HTML::Mason>
or other similar packages: instead of having text inside Perl (that
one I<print()>s), we have Perl inside text, with a special escaping
form that a preprocessor interprets and extracts. For YPathScript,
this preprocessor is embodied by the I<xpathscript> shell tool (see
L</xpathscript Invocation>);

=item *

a I<templating engine>, that does the apply-templates loop, starting
from the top XML node and applying templates to it and its subnodes as
directed by the stylesheet.

=back

When run, the stylesheet is expected to fill in the I<template hash>
$t, which is a lexically-scoped variable made available to it at
preprocess time.

=head2 Dependencies

Although YPathScript is a core component of AxKit, which will not work
without this module, there is plenty of motivation for doing
stylesheets outside of a WWW application server and so
I<XML::YPathScript> is also distributed as a standalone CPAN module.
The AxKit YPathScript component inherits from this class and provides
the coupling with the application framework by overloading and adding
some methods.

I<XML::YPathScript> requires the following Perl packages:

=over

=item I<Symbol>

For generating a separate namespace in which code from the embedded
dialect will run. I<Symbol> is bundled with Perl.

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

=item I<$XML::YPathScript::xp>

The XML::XPath object that holds the whole document (created by
L<XML::XPath/new>)

=item I<$XML::YPathScript::trans>

The template hash currently in use (known as $t in the AxKit
documentation). Its keys are element names, and its values are
the matching templates (as hash references).

=back

=cut "


use vars qw( $VERSION $XML_parser $DoNotInterpolate );

use Symbol;
use File::Basename;
use XML::YPathScript::Toys;

$VERSION = '0.20';

$XML_parser = 'XML::LibXML';

# By default, no interpolation
$DoNotInterpolate = 1;

# internal variable for debugging information. 
# 0 is total silence and 10 is complete verbiage
my $debug_level = 0;

sub import
{
	my $self = shift @_;

	if( grep $_ eq 'XML::XPath', @_ )
	{
		$XML::YPathScript::XML_parser = 'XML::XPath';
	}
	elsif( grep $_ eq 'XML::LibXML', @_ )
	{
		$XML::YPathScript::XML_parser = 'XML::LibXML';
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

Creates a new YPathScript translator. The recognized named arguments are

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

Same as I<stylesheet> but let I<XML::YPathScript> do the loading
itself.  Using this form, relative C<< <!--#include --> >>s in the
stylesheet file will be honored with respect to the dirname of
$filename instead of "."; this provides SGML-style behaviour for
inclusion (it does not depend on the current directory), which is
usually what you want.

=item compiledstylesheet => $function

Re-uses a previous return value of I<compile()> (see L</SYNOPSIS> and
L</compile>), typically to apply the same stylesheet to several XML
documents in a row.

=back

=cut "

sub new {
    my $class = shift;
    die "Invalid hash call to new (param: ".join(':',@_).")" if @_ % 2;
    my %params = @_;
    my $self = \%params;
    bless $self, $class;
}


=pod "

=item I<process()>

=item my $output = I<process( 'return' )>

=item I<process( $stdout_tie, @varvalues )>

Processes the document and stylesheet set at construction time, and
prints the result to STDOUT by default. If $stdout_tie is set, it
must be either the name of a class that will be tied to STDOUT, 
or the string 'return', in which case STDOUT will be tied to the 
class XML::YPathScript::Blah::Buffer and will be returned by the function. 

If the stylesheet was I<compile()>d with extra I<varname>s, then the
calling code should call I<process()> with a corresponding number of
@varvalues. The corresponding lexical variables will be set
accordingly, so that the stylesheet code can get at them (looking at
L</SYNOPSIS>) is the easiest way of getting the meaning of this
sentence).

=cut "

sub process {
    my ($self,$printsub,@varvalues) = @_;

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

	#warn "XML: $xpath";
	#warn "preparing to compile";

	# special case for 'return'
	if( $printsub eq 'return' )
	{
		local *STDOUT;
		tie *STDOUT, 'XML::YPathScript::FileHandle::Buffer';
		$self->compile()->( $xpath );
		my $t = (tied *STDOUT)->get_contents;
		untie *STDOUT;
		return $t;
   } 
   
   return $self->compile()->( $xpath, $printsub );
}

=pod "

=item I<extract($stylesheet)>

=item I<extract($stylesheet,$printform)>

=item I<extract($stylesheet,$printform,$filename)>

=item I<extract($stylesheet,$printform,@includestack)> # from include_file() only

The embedded dialect parser. Given $stylesheet, which is either a
filehandle reference or a string, returns a string that holds all
the code in real Perl. Unquoted text and C<< <%= stuff %> >>
constructs in the stylesheet dialect is converted into invocations of
I<$printform>, which must be a function-like Perl form ( ``print'' by
default), while C<< <% stuff %> >> are transcripted verbatim.

C<< <!-- #include --> >> constructs are expanded by passing their
filename argument to L</include_file> along with @includestack (if any)
like this:

   $self->include_file($includefilename,@includestack);

@includestack is not interpreted by I<extract()> (except for the first
entry, to create line tags for the debugger). It is only a bandaid for
I<include_file()> to pass the inclusion stack to itself across the
mutual recursion existing between the two methods (see
L</include_file>).  If I<extract()> is invoked from outside
I<include_file()>, @includestack should be either empty or of size one.

This method does a purely syntactic job. No special framework
declaration is prepended for isolating the code in its own package,
defining $t or the like (L</compile> does that). It may be overriden
in subclasses to provide different escape forms in the stylesheet
dialect.

=cut "

sub extract 
{
    my ( $self, $stylesheet, $printform, @includestack ) = @_;

	# if not specified, we just use a simple 'print'
    $printform ||= "print";

    my $filename = $includestack[0] || "stylesheet";

    my $contents;

	# $stylesheet can be a filehandler
	# or a string
    if( ref($stylesheet) ) 
	{
        local $/;
        $contents = <$stylesheet>;
    }
    else 
	{
        $contents = $stylesheet;
    }

    my $script="#line 1 $filename\n",
    my $line = 1;

    while ($contents =~ /\G(.*?)(<!--#include|<%=?)/gcs) {
        my ($text, $type) = ($1, $2);
        $line += $text =~ tr/\n//; # count \n's in text
        $text =~ s/\|/\\\|/g;
        $script .= "$printform(q|$text|);";
        $script .= "\n#line $line $filename\n";
        if ($type eq '<%=') {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perlcode = $1;
            $script .= "$printform( $perlcode );\n";
            $line += $perlcode =~ tr/\n//;
        }
        elsif ($type eq '<!--#include') {
            my %params;
            while ($contents =~ /\G(\s+(\w+)\s*=\s*(["'])([^\3]*?)\3|\s*-->)/gcs) {
                last if $1 eq '-->';
                $params{$2} = $4 if (defined $2);
            }

			die "No matching file attribute in #include at line $line"
				unless $params{file};

            $script .= $self->include_file($printform, $params{file},@includestack);
        }
        else {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
            $perl =~ s/;?$/;/s; # add on ; if its missing. As in <% $foo = 'Hello' %>
            $script .= $perl;
            $line += $perl =~ tr/\n//;
        }
    }

    if( $contents =~ /\G(.+)/gcs ) 
	{
        my $text = $1;
        $text =~ s/\|/\\\|/g;
        $script .= "$printform(q|$text|);";
    }

    return $script;
}

=pod "

=item I<include_file($print_form,$filename)>

=item I<include_file($print_form,$filename,@includestack)>

Resolves a C<< <!--#include file="foo" --> >> directive on behalf of
I<extract()>, that is, returns the script contents of
I<$filename>. The return value must be de-embedded too, which means
that I<extract()> has to be called recursively to expand the contents
of $filename (which may contain more C<< <!--#include --> >>s etc.)

If $filename is relative (does not begin with "/" or "./"), it is
resolved according to the basename of the stylesheet that includes it
(that is, $includestack[0], see below) or "." if we are in the topmost
stylesheet. Filenames beginning with "./" are considered absolute;
this gives stylesheet writers a way to specify that they really really
want stylesheets that are in the system's current working directory.

@includestack is the include stack currently in use, made up of all
values of $filename through the stack, lastly added (innermost)
entries first. The toplevel stylesheet is not in @includestack
(e.g. the outermost call does not specify an @includestack).

$print_form is an opaque payload for I<extract()> which is passed down
to it across the mutual recursion that exists between both methods.

This method may be overridden in subclasses to provide support for
alternate namespaces (e.g. ``axkit://'' URIs).

=cut "

sub include_file {
    my ($self, $print_form, $filename, @includestack) = @_;

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
    return $self->extract($sym,$print_form,$filename,@includestack);
}

=pod "

=item I<compile()>

=item I<compile(varname1, varname2,...)>

Compiles the stylesheet set at I<new()> time and returns an anonymous
CODE reference. $stylesheet shall be written in the unparsed embedded
dialect (e.g. C<< ->extract($stylesheet) >> will be called first
inside I<compile()>).

I<varname1>, I<varname2>, etc. are extraneous arguments that will be
made available to the stylesheet dialect as lexically scoped
variables. L</SYNOPSIS> shows a way to use this feature to pass the
Apache handler to AxKit YPathScript stylesheets, which explains this
feature better than a lengthy paragraph would do.

The return value is an opaque token that encapsulates a compiled
stylesheet.  It should not be used, except as the
I<compiledstylesheet> argument to I<new()> to initiate new objects and
amortize the compilation time.  Subclasses may overload the type of
the return value, but will need to overload I<process()> accordingly
of course.

The I<compile()> method is idempotent. Subsequent calls to it will
return the very same function, and calls to it when a
I<compiledstylesheet> argument was set at I<new()> time will return
said argument.

=cut

# Internal documentation: the return value is an anonymous sub whose
# prototype is
#     &$compiledfunc($xpath,$outputfunc,$val1,$val2,...);

sub compile {
    my ($self,@extravars) = @_;

    return $self->{compiledstylesheet} if defined $self->{compiledstylesheet};

    my $stylesheet;

    if (exists $self->{stylesheet}) {
	$stylesheet=$self->{stylesheet};
    } elsif (exists $self->{stylesheetfile}) {
	# This hack fails if $self->{stylesheetfile} contains
	# double quotes.  I think we can ignore this and get
	# away.
	$stylesheet=qq:<!--#include file="$self->{stylesheetfile}" -->\n:;
    } else {
	die "Cannot compile without a stylesheet";
    };

    my $script = $self->extract($stylesheet,'$_[1]->');

    my $package=gen_package_name();

	my $extravars = join ',', @extravars;

	my $eval = <<EOT;
		    package $package;
		    no strict;   # Don't moan on sloppyly
		    no warnings; # written stylesheets
			
			use $XML_parser;  
		    XML::YPathScript::Toys->import;
		    sub {
		    	my (undef,undef, $extravars ) = \@_;
		    \$_[1]=sub {print shift} if (!defined \$_[1]);
		    my \$t = {};
		    local \$XML::YPathScript::trans = \$t; # Yes,
		    # this does the sharing! Perl is a bizarre and
		    # wonderful language.
		    local \$XML::YPathScript::xp=\$_[0];
		    $script
		    }
EOT

	#warn "script ready for compil: $eval";
    local $^W;
	$self->debug( 10, "Compiling code:\n $eval" );
    my $retval = eval $eval;
    die $@ unless defined $retval;

    return ( $self->{compiledstylesheet} = $retval );
}

sub debug
{
	warn $_[2] if $_[1] <= $debug_level;
}


=pod "

=back

=head2 Utility functions

The functions below are not methods.

=over

=item I<XML::XPath::Function::document>

An XPath function made available to XPath expressions in the
stylesheet. Should be removed?

=cut "

sub XML::XPath::Function::document 
{
    my $self = shift;
    my ($node, @params) = @_;

    die "document: Function takes 1 parameter\n" unless @params == 1;

    my $parser = XML::XPath::XMLParser->new();

    my $results = XML::XPath::NodeSet->new();
    my $newdoc;
    my $sym = gensym;
    my $file = $params[0];
    open($sym, $file) || die "Cannot open document() file '$file': $!";
    $newdoc = $parser->parse( ioref => $sym );
    $results->push($newdoc) if $newdoc;
    return $results;
}


=pod "

=item I<gen_package_name()>

Generates a fresh package name in which we would compile a new
stylesheet. Never returns twice the same name.

=cut "

do 
{
	my $uniquifier;
	sub gen_package_name 
	{
    	$uniquifier++;
    	return 'XML::YPathScript::STYLESHEET'.$uniquifier;
	}
};

1;

package XML::YPathScript::FileHandle::Buffer;
# shamelessly stolen  from Tie::FileHandle::Buffer

sub TIEHANDLE { my $self = ''; bless \$self, $_[0] }

sub PRINT { ${$_[0]} .= $_[1] }

sub get_contents { ${$_[0]} }

sub clear { ${$_[0]} = '' }

'end of XML::YPathScript::FileHandle::Buffer';


__END__

=back

=head1 AUTHOR

XPathScript was created by Matt Sergeant <matt@sergeant.org>

And it came to pass that
XPathScript begat XML::XPathScript, 
by the actions of Dominique Quatravaux <dom@ideax.com> and Yanick Champoux <yanick@babyl.dyndns.org>,
offering improvements and feature merges with Apache::AxKit::Language::XPathScript.

And, by the will of a rather fork-happy Yanick, XML::XPS in turn begat XML::YPS. 
We can only hope that the madness will stop there.

=head1 LICENSE

This is free software. You may distribute it under the same terms as
Perl itself.

=head1 SEE ALSO

The XPathScript Guide at http://axkit.org/wiki/view/AxKit/XPathScriptGuide

=cut
