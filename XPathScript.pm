#!/usr/bin/perl -w

use strict;

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
another format (possibly HTML, but XPathScript also shines for
non-XML-like output).

Like XSLT, XPathScript has a dialect to mix up verbatim document
portions and code. Also like XSLT, it leverages the powerful
``templates/apply-templates'' and ``cascading stylesheets'' design
patterns, that greatly simplify the design of stylesheets for
programmers. The availability of the I<XPath> query language inside
stylesheets promotes the use of a purely document-dependent,
side-effect-free coding style. But unlike XSLT which uses its own
dedicated control language with an XML-compliant syntax, XPathScript
uses Perl which is terse and highly extendable.

The result of the merge is an extremely powerful environment for
development tasks that involve rendering complex XML documents to
other formats. Stylesheets written in XPathScript are very easy to
create, extend and reuse, even if they manage hundreds of different
XML tags.

=head1 STYLESHEET WRITER DOCUMENTATION

=head2 Creating stylesheets

See http://axkit.org/docs/xpathscript/guide.dkb for a head start.
There you will learn how to markup the embedded dialect and fill in
the template hash $t.

=head2 xpathscript Invocation

This CPAN module is bundled with an "xpathscript" shell tool that
is to be invoked like this:

   xpathscript mydocument.xml mystylesheet.xps

It will produce the resulting document on standard output. More
options will be added later (select output file, handle multiple
output files, pass parameters to the stylesheet etc.).

=head2 Functions and global variables available in the stylesheet

A number of callback functions are available from the stylesheet
proper.  They apply against the current document and template hash,
which are transparently passed back and forth as global variables (see
L</Global variables>). They are defined in the
I<XML::XPathScript::Toys> package, which is implicitly imported into
all code written in the embedded stylesheet dialect.

=over

=cut "

package XML::XPathScript::Toys;

use XML::XPath::Node;

use vars '@ISA', '@EXPORT';
use Exporter;
@ISA = ('Exporter');
@EXPORT = qw(
        findnodes
        findvalue
        findvalues
        findnodes_as_string
        apply_templates
	call_template
        matches
        set_namespace
		DO_SELF_AND_KIDS
		DO_SELF_ONLY
		DO_NOT_PROCESS
        );

=pod "

=item I<DO_SELF_AND_KIDS>

=item I<DO_SELF_ONLY>

=item I<DO_NOT_PROCESS>

Symbolic constants evaluating respectively to 1, -1 and 0, to be used
as mnemotechnic return values in C<testcode> routines instead of the
numeric values which are harder to remember.

=cut "

    sub DO_SELF_AND_KIDS () { return  1 }
    sub DO_SELF_ONLY     () { return -1 }
    sub DO_NOT_PROCESS   () { return  0 }

# quieten warnings when compiling this module
sub apply_templates (;$@);

=pod "

=item I<findnodes($path)>

=item I<findnodes($path, $context)>

Returns a list of nodes found by XPath expression $path, optionally
using $context as the context node (default is the root node of the
current document).  In scalar context returns a NodeSet object.

=cut "

sub findnodes {
    $XML::XPathScript::current->{xp}->findnodes(@_);
}

=pod "

=item  I<findvalue($path)>

=item  I<findvalue($path, $context)>

Evaluates XPath expression $path and returns the result, as either a
"XML::XPath::Literal", a "XML::XPath::Boolean" or a
"XML::XPath::Number" object. If the path returns a NodeSet,
$nodeset->to_literal is called automatically for you (and thus a
"XML::XPath::Literal" is returned). Note that for each of the objects
stringification is overloaded, so you can just print the value found,
or manipulate it in the ways you would a normal perl value (e.g. using
regular expressions) - just beware that the result of such
stringification will be UTF8-encoded (see L<perlunicode>), as just
about everything under the XML sun is.

=cut "

sub findvalue {
    $XML::XPathScript::current->{xp}->findvalue(@_);
}

=pod "


=item  I<findvalues($path)>

=item  I<findvalues($path, $context)>

Evaluates XPath expression $path as a nodeset expression, just like
L</findnodes> would, but returns a list of UTF8-encoded XML strings
instead of node objects.

=cut "

sub findvalues {
    my @nodes = findnodes(@_);
    map { findvalue('.', $_) } @nodes;
}

=pod "

=item I<findnodes_as_string($path)>

=item I<findnodes_as_string($path, $context)>

Similar to L</findvalues> but concatenates the XML snippets.  The
result is not guaranteed to be valid XML though.

=cut "

sub findnodes_as_string {
    $XML::XPathScript::current->{xp}->findnodes_as_string(@_);
}

=pod "

=item I<matches($node, $path)>

=item I<matches($node, $path, $context)>

Returns true if the node matches the path (optionally in context $context)

=cut "

sub matches {
    $XML::XPathScript::current->{xp}->matches(@_);
}

sub set_namespace {
    eval {
        $XML::XPathScript::current->{xp}->set_namespace(@_);
    };
    if ($@) {
        warn "set_namespace failed: $@";
    }
}

=pod "

=item I<apply_templates()>

=item I<apply_templates($xpath)>

=item I<apply_templates($xpath, $context)>

=item I<apply_templates(@nodes)>

This is where the whole magic in XPathScript resides: recursively
applies the stylesheet templates to the nodes provided either
literally (last invocation form) or through an XPath expression
(second and third invocation forms), and returns a string
concatenation of all results. If called without arguments at all,
renders the whole document.

Calls to I<apply_templates()> may occur both implicitly (at the top of
the document, and for rendering subnodes when the templates choose not
to handle that by themselves), and explicitly (from C<testcode>
routines).

If appropriate care is taken in all templates (especially the
C<testcode> routines and the I<text()> template), the string result of
I<apply_templates> need not be UTF-8 (see L<perlunicode>): it is thus
possible using XPathScript to produce output in any character set
without an extra translation pass.

=cut "

sub apply_templates (;$@) {
    unless (@_) {
        return apply_templates(findnodes('/'));
    }

    my ($arg1, @args) = @_;

    if (!ref($arg1)) {
        # called with a path to find
#         warn "apply_templates with path '$arg1'\n";
		if( my $nodes = findnodes($arg1, @args) )
		{
        	return apply_templates($nodes);
		}
		else{ return }
    }

    my $retval = '';
    if ($arg1->isa('XML::XPath::NodeSet')) {
        foreach my $node ($arg1->get_nodelist) {
            $retval .= translate_node($node);
        }
    }
    else {
        $retval .= translate_node($arg1);
        foreach my $node (@args) {
            $retval .= translate_node($node);
        }
    }

    return $retval;
}

=pod "

=item I<call_template($node, $t, $templatename)>

B<EXPERIMENTAL> - allows C<testcode> routines to invoke a template by
name, even if the selectors do not fit (e.g. one can apply template B
to an element node of type A). Returns the stylesheeted string
computed out of $node just like L</apply_templates> would.

=cut "

sub call_template {
    my ($self,$t,$template)=@_;

    if (defined(my $sub=$template->{testcode})) {
	return &$sub($self,$t);
    } elsif (exists $t->{prechild} || exists $t->{prechildren} ||
	     exists $t->{postchild} || exists $t->{postchildren}) {
	warn "XML::XPathScript::Toys::call_template: cannot handle this sort of templates yet";
	# Attempt to recover
	$t->{pre}="";
	$t->{post}="";
	return 1;
    } else {
	$t->{pre}=$template->{pre};
	$t->{post}=$template->{post};
	return 1;
    };
}


sub _apply_templates {
    my @nodes = @_;

    my $retval = '';
    foreach my $node (@nodes) {
        $retval .= translate_node($node);
    }

    return $retval;
}

sub translate_node {
    my $node = shift;

    my $translations = $XML::XPathScript::current->{t};

    # Find appropriate template for node.
    my $trans;
    if ($node->isElementNode) {
	my $node_name = $node->getName;
	$trans = $translations->{$node_name} if (defined $node_name);
	$trans ||= $translations->{'*'};
    } elsif ($node->isTextNode) {
	$trans = $translations->{"text()"};
    } elsif ($node->isCommentNode) {
	$trans = $translations->{"comment()"};
    } elsif ($node->isPINode) {
	unless (defined($trans=$translations->{"processing-instruction()"})) {
	    # Default handling of Processing Instructions:
	    # don't output top-level ones, but leave children alone
	    # (falling thru the no-template case).
	    eval { $node->getParentNode->getParentNode } || return '';
	};
    } else { # Unknown node type, leave it alone.
        return $node->toString;
    }

    if (!$trans) { # Node has no template.
	if ($node->isElementNode) {
	    return start_tag($node) .
	      _apply_templates($node->getChildNodes) .
                end_tag($node);
	} else {
	    return $node->toString;
	}
    }


    my $dokids = 1;
    my $search;

    if ($trans->{testcode}) {
	$trans={%$trans}; # Performs shallow copy...
	my $t={};         # for modifying it in place according to how $t is
	# set by testcode sub.

#         warn "Evalling testcode\n";
        my $result = $trans->{testcode}->($node, $t);

	if ($result !~ m/^-?\d+/) {
            $dokids = 0;
            $search = $result;
	}
	elsif ($result == DO_NOT_PROCESS) {
            # don't process anything.
            return;
        }
	elsif ($result == DO_SELF_ONLY) {
            # -1 means don't do children.
            $dokids = 0;
        }
        elsif ($result == DO_SELF_AND_KIDS) {
            # do kids
        } else {
	    use Carp;
	    Carp::croak "Unknown return value in testcode: $result";
	}

	if (%$t) {
	    foreach my $key (keys %$t) {
		$trans->{$key} = $t->{$key};
	    }
	}
    }

    # default: process children too.
    my $pre = interpolate($node, $trans->{pre}) .
            ($trans->{showtag} ? start_tag($node) : '') .
            interpolate($node, $trans->{prechildren});

    my $post = interpolate($node, $trans->{postchildren}) .
            ($trans->{showtag} ? end_tag($node) : '') .
            interpolate($node, $trans->{post});

    if ($dokids) {
        my $middle = '';
        for my $kid ($node->getChildNodes()) {
            if ($kid->isElementNode) {
                $middle .= interpolate($node, $trans->{prechild}) .
                        _apply_templates($kid) .
                        interpolate($node, $trans->{postchild});
            }
            else {
                $middle .= _apply_templates($kid);
            }
        }
        return $pre . $middle . $post;
    }
    elsif ($search) {
        my $middle = '';
        for my $kid (findnodes($search, $node)) {
            if ($kid->isElementNode) {
                $middle .= interpolate($node, $trans->{prechild}) .
                        _apply_templates($kid) .
                        interpolate($node, $trans->{postchild});
            }
            else {
                $middle .= _apply_templates($kid);
            }
        }
        return $pre . $middle . $post;
    }
    else {
        return $pre . $post;
    }
}

sub start_tag {
    my ($node) = @_;

    my $name = $node->getName;
    return '' unless $name;

    my $string = "<" . $name;

    foreach my $ns ($node->getNamespaceNodes) {
        $string .= $ns->toString;
    }

    foreach my $attr ($node->getAttributeNodes) {
        $string .= $attr->toString;
    }

    $string .= ">";

    return $string;
}

sub end_tag {
    my ($node) = @_;

    if (my $name = $node->getName) {
        return "</" . $name . ">";
    }
    else {
        return '';
    }
}

sub interpolate {
    my ($node, $string) = @_;
    return '' if (!defined $string);
    return $string if (! $XML::XPathScript::current->interpolating());
    my $new = '';
    while ($string =~ m/\G(.*?)\{(.*?)\}/gcs) {
        my ($pre, $path) = ($1, $2);
        $new .= $pre;
        $new .= $node->findvalue($path);
    }
    $string =~ /\G(.*)/gcs;
    $new .= $1 if defined $1;
    return $new;
}

1;


=pod "

=back

=head1 TECHNICAL DOCUMENTATION

The rest of this POD documentation is B<not> useful to programmers who
just want to write stylesheets; it is of use only to people wanting to
call existing stylesheets or more generally embed the XPathScript
motor into some wider framework.

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

=cut "

package XML::XPathScript;

use vars qw($VERSION);

use XML::XPath 1.0;
use XML::XPath::XMLParser;
use XML::XPath::Node;
use XML::XPath::NodeSet;
use XML::Parser;
use Symbol;
use File::Basename;

$VERSION = '0.11';

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

=back

=cut "

sub new {
    my $class = shift;
    die "Invalid hash call to new" if @_ % 2;
    my %params = @_;
    my $self = \%params;
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

    my ($xdom);
    if (UNIVERSAL::isa($self->{xml},"XML::XPath")) {
	$xdom=$self->{xml};
    } elsif (UNIVERSAL::isa($self->{xml},"XML::libXML")) {
	# Transcode to XML::XPath - OUCH ! Huge performance hit.
	# In the future we will be able to manage both kinds of
	# XML trees at once.
	$xdom=XML::XPath->new(xml => $self->{xml}->toString);
    } elsif (ref($self->{xml})) {
	$xdom=XML::XPath->new(ioref => $self->{xml})
    } else {
	$xdom=XML::XPath->new(xml => $self->{xml});
    };
    $self->{xp}=$xdom;

    do { $$printer="" } if (UNIVERSAL::isa($printer, "SCALAR"));
    $self->{printer}=$printer;

    $self->compile()->($self, @extravars);

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

    my $contents;
    if (ref($stylesheet)) {
        local $/;
        $contents = <$stylesheet>;
    }
    else {
        $contents = $stylesheet;
    }

    my $script="#line 1 $filename\n",
    my $line = 1;

    while ($contents =~ /\G(.*?)(<!--#include|<%=?)/gcs) {
        my ($text, $type) = ($1, $2);
        $line += $text =~ tr/\n//; # count \n's in text
        $text =~ s/\|/\\\|/g;
        $script .= "\$_[0]->print(q|$text|);";
        $script .= "\n#line $line $filename\n";
        if ($type eq '<%=') {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
            $script .= "\$_[0]->print( $perl );\n";
            $line += $perl =~ tr/\n//;
        }
        elsif ($type eq '<!--#include') {
            my %params;
            while ($contents =~ /\G(\s+(\w+)\s*=\s*(["'])([^\3]*?)\3|\s*-->)/gcs) {
                last if $1 eq '-->';
                $params{$2} = $4 if (defined $2);
            }

            if (!$params{file}) {
                die "No matching file attribute in #include at line $line";
            }

            $script .= $self->include_file($params{file},@includestack);
        }
        else {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
            $perl =~ s/;?$/;/s; # add on ; if its missing. As in <% $foo = 'Hello' %>
            $script .= $perl;
            $line += $perl =~ tr/\n//;
        }
    }

    if ($contents =~ /\G(.*)/gcs) {
        my ($text) = ($1);
        $text =~ s/\|/\\\|/g;
        $script .= "\$_[0]->print(q|$text|);";
    }

    return $script;
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
(this is the XML way of things). If $filename is relative (e.g. does
not begin with "/" or "./"), it is resolved according to the basename
of the stylesheet that includes it (that is, $includestack[0], see
below) or "." if we are in the topmost stylesheet. Filenames beginning
with "./" are considered absolute; this gives stylesheet writers a way
to specify that they really really want a stylesheet that lies in the
system's current working directory.

@includestack is the include stack currently in use, made up of all
values of $filename through the stack, lastly added (innermost)
entries first. The toplevel stylesheet is not in @includestack
(e.g. the outermost call does not specify an @includestack).

This method may be overridden in subclasses to provide support for
alternate namespaces (e.g. ``axkit://'' URIs).

=cut "

sub include_file {
    my ($self, $filename, @includestack) = @_;

    # should maybe check for circular includes here...
#warn "INCLUDE: $filename\n";

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
dialect (e.g. C<< ->extract($stylesheet) >> will be called first
inside I<compile()>).

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
      if (defined $self->{compiledstylesheet});

    my $stylesheet;

    if (exists $self->{stylesheet}) {
	$stylesheet=$self->{stylesheet};
    } elsif (exists $self->{stylesheetfile}) {
	# This hack fails if $self->{stylesheetfile} contains
	# double quotes.  I think we can ignore this and get
	# away.
	$stylesheet=qq:<!--#include file="$self->{stylesheetfile}" -->:;
    } else {
	die "Cannot compile without a stylesheet";
    };

    my $script = $self->extract($stylesheet);

    my $package=gen_package_name();
    my $eval = join("\n",
		    "package $package;",
		    'no strict;',   # Don't moan on sloppyly
		    'no warnings;', # written stylesheets
		    "use XML::XPath::Node;",
		    'XML::XPathScript::Toys->import;',
		    'sub {',
		    'my (undef,'.join('',@extravars).') = @_;',
		    'local $XML::XPathScript::current=$_[0];',
		    'my $t = ($XML::XPathScript::current->{t} = {});',
		    $script,
		    "}",
		   );

    local $^W;
#warn "Compiling: $eval\n";
    my $retval=eval $eval;
    die $@ if (!defined $retval);
    return ($self->{compiledstylesheet}=$retval);
}

=pod "

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
      $t->{pre}="<a href=\"$url\">";
      $t->{post}='</a>';
   };

Interpolation is on by default. A (now undocumented) global variable
used to change the default to off, but don't do that.

=cut "

sub interpolating {
    my $self=shift;
    my $retval=(exists $self->{interpolating} ?
		$self->{interpolating} :
		# Obsolete, for compatibility:
		(! $XPathScript::DoNotInterpolate));

    $self->{interpolating}=shift if (@_);
    return $retval;
}

=pod "

=item I<print($text)>

Outputs a chunk of text on behalf of the stylesheet. The default
implementation is to use the second argument to L</process>, which was
stashed in C<< $self->{printer} >> by said function. Overloading this
method in a subclass provides yet another method to redirect output.


=cut "

sub print {
    my ($self, $text)=@_;
    my $printer=$self->{printer};
    if (!defined $printer) {
	print $text;
    } elsif (ref($printer) eq "CODE") {
	$printer->($text);
    } elsif (UNIVERSAL::isa($printer, "SCALAR")) {
	$$printer.=$text;
    } else {
	local $\=undef;
	print $printer $text;
    };
}

=pod "

=item I<current()>

This class method (e.g. C<< XML::XPathScript->current() >>) returns
the stylesheet object currently being applied. This can be called from
anywhere within the stylesheet, except a BEGIN or END block or
similar.

=cut "

sub current {
    unless (defined $XML::XPathScript::current) {
	use Carp;
	Carp::croak "Wrong context for calling current()";
    };
    return $XML::XPathScript::current;
}

=pod "

=back

=head2 Utility functions

The functions below are not methods.

=over

=item I<XML::XPath::Function::document>

An XPath function made available to XPath expressions in the
stylesheet, that takes one parameter (a system filename) and returns a
nodeset consisting of the root node of a foreign document that is
parsed from said filename. This feature can be used to process several
input documents at once with one stylesheet, even if their respective
DTDs are unrelated.

=cut "

sub XML::XPath::Function::document {
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

do {
my $uniquifier;
sub gen_package_name {
    $uniquifier++;
    return "XML::XPathScript::STYLESHEET$uniquifier";
}
};

1;

__END__

=back

=head1 BUGS

Due to the peculiar syntax allowed in the embedded dialect for
accessing the template hash, this package is not reentrant and
thus cannot transform several documents at once.

=head1 AUTHORS

Created by Matt Sergeant <matt@sergeant.org>

Improvements and feature merge with
Apache::AxKit::Language::XPathScript by Yanick Champoux
<yanick@babyl.dyndns.org> and Dominique Quatravaux <dom@ideax.com>

=head1 LICENSE

This is free software. You may distribute it under the same terms as
Perl itself.

=cut

# Local Variables:
# mode:cperl
# tab-width:8
# End:

