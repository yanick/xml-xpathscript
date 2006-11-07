=head1 NAME

XML::XPathScript::Processor - the XML transformation engine in XML::XPathScript

=head1 SYNOPSIS

In a stylesheet C<< ->{testcode} >> sub for e.g. Docbook's 
C<< <ulink> >> tag:

      my $url = findvalue('@url',$self);
      if (findnodes("node()", $self)) {
         # ...
		$t->set({ pre  => "<a href='$url'>", 
                  post => '</a>'             });
		return DO_SELF_AND_KIDS;
      } 
      else {
        $t->set({  pre  => "<a href='$url'>$url</a>",
                   post => ''                          });
		return DO_SELF_ONLY;
      };

At the stylesheet's top-level one often finds:

   <%= apply_templates() %>

=head1 DESCRIPTION

The I<XML::XPathScript> distribution offers an XML parser glue, an
embedded stylesheet language, and a way of processing an XML document
into a text output. This package implements the latter part: it takes
an already filled out C<< $template >> template object and an already parsed
XML document (which come from L<XML::XPathScript> behind the scenes),
and provides a simple API to implement stylesheets. In particular, the
L</apply_templates> function triggers the recursive expansion of
the whole XML document when used as shown in L</SYNOPSIS>.

=head1 XPATHSCRIPT LANGUAGE FUNCTIONS

All of these functions are intended to be called solely from within
the C<< ->{testcode} >> templates or C<< <% %> >> or C<< <%= %> >>
blocks in XPathScript stylesheets. They are automatically exported to
both these contexts.

=cut

package XML::XPathScript::Processor;

use strict;
use warnings;
use Carp;

use base 'Class::Exporter';

use XML::XPathScript::Template;

our $VERSION = '1.46';

our @EXPORT = qw(
        processor
        findnodes
        findvalue
        findvalues
        findnodes_as_string
		xpath_to_string
        apply_templates
        matches
        set_namespace
		is_element_node
		is_text_node
        is_comment_node
		is_pi_node
		is_nodelist
		is_utf8_tainted
		get_xpath_of_node
		DO_SELF_AND_KIDS
		DO_SELF_ONLY
		DO_NOT_PROCESS
		DO_TEXT_AS_CHILD
        set_doc
        get_doc
        set_parser
        get_parser
        set_binmode
        get_binmode
        set_template
        get_template
        set_interpolation
        get_interpolation
        set_interpolation_regex
        get_interpolation_regex
        );

use constant DO_TEXT_AS_CHILD =>  2;
use constant DO_SELF_AND_KIDS =>  1;
use constant DO_SELF_ONLY     => -1;
use constant DO_NOT_PROCESS   =>  0;


sub new {
    my $class = shift;
    my $self = {};
    bless $self, $class;
    # $XML::XPathScript::xp => {doc}
    #                          {parser}
    #                          {binmode}
    #                          {template}
    # $XML::XPathScript::current->interpolating() {is_interpolating}
    # $XML::XPathScript::current->{interpolation_regex}  {interpolation_regex}
}

sub processor {
    return $_[0];
}

sub set_doc { $_[0]->{doc} = $_[1] }
sub get_doc { $_[0]->{doc} }
sub set_parser { $_[0]->{parser} = $_[1] }
sub get_parser { $_[0]->{parser} }
sub set_binmode { $_[0]->{binmode} = $_[1] }
sub get_binmode { $_[0]->{binmode} }
sub set_template { $_[0]->{template} = $_[1] }
sub get_template { $_[0]->{template} }
sub set_interpolation { $_[0]->{is_interpolating} = $_[1] }
sub get_interpolation { $_[0]->{is_interpolating} }
sub set_interpolation_regex { $_[0]->{interpolation_regex} = $_[1] }
sub get_interpolation_regex { $_[0]->{interpolation_regex} }



=head2 findnodes

   ( @nodes ) = findnodes( $path )
   ( @nodes ) = findnodes( $path, $context ) 

Returns a list of nodes found by XPath expression $path, optionally
using $context as the context node (default is the root node of the
current document).  In scalar context returns a NodeSet object (but
you do not want to do that, see L<XML::XPathScript/XPath scalar return
values considered harmful>).

=cut "

sub findnodes {
    my $self = shift;

	if ($self->{parser} eq 'XML::XPath' ) {
		return $self->{doc}->findnodes(@_);
	}

	my ($path, $context) = @_;
	$context = $self->{doc} if (!defined $context);
	return $context->findnodes($path);
}

=head2 findvalue

    $value = findvalue( $path )
    $value = findvalue( $path, $context )

Evaluates XPath expression $path and returns the resulting value. If
the path returns one of the "Literal", "Numeric" or "NodeList" XPath
types, the stringification is done automatically for you using
L</xpath_to_string>.

=cut "

sub findvalue {
    my $self = shift;
	if ( $self->{parser} eq 'XML::XPath' ) {
		return $self->xpath_to_string(scalar $self->{doc}->findvalue(@_));
	}

	my ($path, $context) = @_;
	$context = $self->{doc} if (!defined $context);
	return $self->xpath_to_string($context->findvalue($path));
}

=head2 xpath_to_string

    $string = xpath_to_string( $blob )

Converts any XPath data type, such as "Literal", "Numeric",
"NodeList", text nodes, etc. into a pure Perl string (UTF-8 tainted
too - see L</is_utf8_tainted>). Scalar XPath types are interpreted in
the straightforward way, DOM nodes are stringified into conform XML,
and NodeList's are stringified by concatenating the stringification of
their members (in the latter case, the result obviously is not
guaranteed to be valid XML).

See L<XML::XPathScript/XPath scalar return values considered harmful>
on why this is useful.

=cut "

sub xpath_to_string {
    my $self = shift;
	my ($blob)=@_;
	return $blob if (! ref($blob));
	# Was simply C<< return "$blob" >> but Perl 5.6.1 seems to have
	# issues with UTF8-flag used in overloaded stringification :-(
	return $blob->can("data") ? $blob->data() :
		$blob->can("value") ? $blob->value() :
			$blob->string_value();
}

=head2 findvalues

    @values = findvalues( $path )
    @values = findvalues( $path, $context )

Evaluates XPath expression $path as a nodeset expression, just like
L</findnodes> would, but returns a list of UTF8-encoded XML strings
instead of node objects or node sets. See also
L<XML::XPathScript/XPath scalar return values considered harmful>.

=cut "
sub findvalues {
    my $self = shift;
    my @nodes = $self->findnodes(@_);
    map { $self->findvalue('.', $_) } @nodes;
}

=head2 findnodes_as_string

    @nodes = findnodes_as_string( $path )
    @nodes = findnodes_as_string( $path, $context )

Similar to L</findvalues> but concatenates the XML snippets.  The
result obviously is not guaranteed to be valid XML.

=cut "

sub findnodes_as_string {
    my $self = shift;
	$self->{doc}->findnodes_as_string( @_ ) 
}

=head2 matches

    $bool = matches( $node, $path )
    $bool = matches( $node, $path, $context )

Returns true if the node matches the path (optionally in context $context)

=cut "

sub matches {
    my $self = shift;
	$self->{doc}->matches(@_)
}

sub set_namespace
{
    my $self = shift;
	eval { $self->{doc}->set_namespace(@_) };
	warn "set_namespace failed: $@" if $@;
}

=head2 apply_templates

    $transformed = apply_templates()
    $transformed = apply_templates( $xpath )
    $transformed = apply_templates( $xpath, $context )
    $transformed = apply_templates( @nodes )

This is where the whole magic in XPathScript resides: recursively
applies the stylesheet templates to the nodes provided either
literally (last invocation form) or through an XPath expression
(second and third invocation forms), and returns a string
concatenation of all results. If called without arguments at all,
renders the whole document (same as C<< apply_templates("/") >>).

Calls to I<apply_templates()> may occur both implicitly (at the top of
the document, and for rendering subnodes when the templates choose to
handle that by themselves), and explicitly (because C<testcode>
routines require the XML::XPathScript::Processor to
L</DO_SELF_AND_KIDS>).

If appropriate care is taken in all templates (especially the
C<testcode> routines and the I<text()> template), the string result of
I<apply_templates> need not be UTF-8 (see
L<XML::XPathScript/binmode>): it is thus possible to use XPathScript
to produce output in any character set without an extra translation
pass.

=cut "

sub apply_templates {
    my $self = shift;
	# catch the calls to apply_templates() 
	return $self->apply_templates( $self->findnodes('/') ) unless @_;

    my ($arg1, @args) = @_;

    unless( ref($arg1) ) { # called with a path to find

		my $nodes = $self->findnodes($arg1, @args);
		return $nodes ? $self->apply_templates($nodes) : undef;
    }

    my $retval = '';
	if ( $self->is_nodelist($arg1))
	{
        foreach my $node ($arg1->get_nodelist) {
            $retval .= $self->translate_node($node);
        }
    }
    else {
        $retval .= $self->translate_node($arg1);
        foreach my $node (@args) {
            $retval .= $self->translate_node($node);
        }
    }

    return $retval;
}

=head2 call_template

    call_template( $node, $t, $templatename )
    
B<EXPERIMENTAL> - allows C<testcode> routines to invoke a template by
name, even if the selectors do not fit (e.g. one can apply template B
to an element node of type A). Returns the stylesheeted string
computed out of $node just like L</apply_templates> would.

=cut "

sub call_template {
    my ($self,$node,$t,$template)=@_;

    if (defined(my $sub=$template->{testcode})) {
	return &$sub($node,$t);
    } elsif (exists $t->{prechild} || exists $t->{prechildren} ||
	     exists $t->{postchild} || exists $t->{postchildren}) {
	warn "XML::XPathScript::Processor::call_template: cannot handle this sort of templates yet";
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
    my $self = shift;
	no warnings 'uninitialized';
	return join '', map $self->translate_node($_), @_;
}

=head2  is_element_node 

    $bool = is_element_node( $object )

Returns true if $object is an element node, false otherwise.

=cut

sub is_element_node {
    my $self = shift;
	UNIVERSAL::isa( $_[0], 'XML::XPath::Node::Element' ) or
		UNIVERSAL::isa( $_[0], 'XML::LibXML::Element' );
}

=head2 is_text_node

    $bool = is_text_node( $object )

Returns true if $object is a "true" text node (B<not> a comment node),
false otherwise.

=cut "

sub is_text_node {
    my $self = shift;
	UNIVERSAL::isa($_[0], 'XML::XPath::Node::Text') or
	# little catch: XML::LibXML::Comment is a
	# XML::LibXML::Text
		( UNIVERSAL::isa($_[0], 'XML::LibXML::Text') &&
		  ! UNIVERSAL::isa($_[0], 'XML::LibXML::Comment') );
}

=head2 is_comment_node

    $bool = is_comment_node ( $object )

Returns true if $object is an XML comment node, false otherwise.

=cut "

sub is_comment_node {
    my $self = shift;
		UNIVERSAL::isa( $_[0], 'XML::LibXML::Comment' ) or
			UNIVERSAL::isa( $_[0], 'XML::XPath::Node::Comment' );
}

=head2 is_pi_node

    $bool = is_pi_node( $object )

Returns true iff $object is a processing instruction node.

=cut "

sub is_pi_node {
    my $self = shift;
	UNIVERSAL::isa($_[0], "XML::LibXML::PI") ||
		UNIVERSAL::isa($_[0], "XML::XPath::Node::PI");
}

=head2 is_nodelist

    $bool = is_nodelist( $object )

Returns true if $node is a node list (as returned by L</findnodes> in
scalar context), false otherwise.

=cut "

sub is_nodelist {
    my $self = shift;
	UNIVERSAL::isa($_[0], 'XML::XPath::NodeSet') or
		UNIVERSAL::isa($_[0], 'XML::LibXML::NodeList');
}

=head2 is_utf_tainted

    $bool = is_utf8_tainted( $string )

Returns true if Perl thinks that $string is a string of characters (in
UTF-8 internal representation), and false if Perl treats $string as a
meaningless string of bytes.

The dangerous part of the story is when concatenating a non-tainted
string with a tainted one, as it causes the whole string to be
re-interpreted into UTF-8, even the part that was supposedly
meaningless character-wise, and that happens in a nonportable fashion
(depends on locale and Perl version). So don't do that - and use this
function to prevent that from happening.

=cut "

sub is_utf8_tainted {
    my $self = shift;
    my ($string) = @_;

    my $ghost = ($string x 0) .
        "ab"; # Very quick and conserves UTF-8 flag (and taintedness)

    $ghost .= do { use bytes; "\xc3" };
    $ghost .= do { use bytes; "\xa9" };
    my $charlength = do { no bytes; length($ghost) };
    my $bytelength = do { use bytes; length($ghost) };

    if ($charlength == 3) {
        # The two bytes we added got lumped in core into a single
        # UTF-8 char. This is a Perl bug (arising e.g. because $string
        # is tainted, see t/04unicode.t) but we recover gracefully.
        return 1;
    } elsif ($charlength == 4 && $bytelength == 4) {
        return 0;
    } elsif ($charlength == 4 && $bytelength == 6) {
        return 1; # The bytes were upgraded
    } else {
        die "is_utf8_tainted assertion check failed".
            " (charlength = $charlength, bytelength=$bytelength)";
    }
}

=head2 get_xpath_of_node

 $xpath = get_xpath_of_node( $node )

Returns an XPath string that points to $node, from the root. Useful to
create error messages that point at some location in the original XML
document.

=cut "

	sub get_xpath_of_node {
		my $self =shift;
        my $node = shift;

		# ugly hacks all over in this function, because the quirky
		# refcount-proof aliasing (i.e. XML::XPath::Element versus
		# XML::XPath::ElementImpl) in XML::XPath gets in the way badly
		$node = $$node if
			$node->isa( 'XML::XPath::Node::Element' ) and not $self->isa( 'XML::XPath::Node::ElementImpl' );

		my $parent = ( $node->can("parentNode") ?
					$node->parentNode() :
					$node->getParentNode() );

		return "" unless defined $parent;

		my $name;
		if ($self->is_element_node($node)) {
			$name = $node->findvalue('name()');
		} elsif ($self->is_text_node($node)) {
			$name = "text()";
	    } elsif ($self->is_comment_node($node)) {
			$name = "comment()";
		} elsif ($self->is_pi_node($node)) {
			$name = "processing-instruction()";
		} else {
			# YKYBPTMNW...
			return $self->get_xpath_of_node($parent)."/strange-node()";
		}

		# ugly hack, part II
		my @brothers = map{ ($_->isa( 'XML::XPath::Node::Element' ) ) ? $$_ : $_ } $parent->findnodes("./$name");

		# Short-cut for nodes that have an ID. FIXME: not all DTDs use
		# attribute named "id" as the SGML ID!
	if ($self->is_element_node($node) && (my $id=$self->findvalue('@id',$node))) {
		return $self->get_xpath_of_node($parent).sprintf('/%s[@id="%s"]', $name, $id);
	}

	# Bug: the matches() function from XML::XPath is hosed, and only
	# works towards ancestors. We resort to comparing references for
	# identity. See above for details on the $$self quirk.
	my $theself=($node =~ m/SCALAR/?$$node:$node);

	for my $i ( 0..$#brothers ) {
		my $thebrother=($brothers[$i] =~ m/SCALAR/?
						${$brothers[$i]}:$brothers[$i]);

		return sprintf '%s/%s[%d]', $self->get_xpath_of_node($parent), $name, $i+1 
			if $theself eq $thebrother;
	};

	return $self->get_xpath_of_node($parent)."/$name"."[?]";
}


########################## End of exportable stuff ####################

sub translate_node {
    my $self = shift;
    my $node = shift;

	if( UNIVERSAL::isa($node,"XML::LibXML::Document") ) 
	{
		$node = $node->documentElement;
	}

	my $retval;
	if ( $self->is_comment_node($node) ) {
		$retval = $self->translate_comment_node( $node );
	} elsif ( $self->is_text_node($node) )
	{
		$retval = $self->translate_text_node( $node );
	} elsif ( $self->is_element_node( $node )) {
		$retval = $self->translate_element_node( $node );
	} elsif ( $self->is_pi_node($node) ) {
		# don't output top-level PI's
		$retval = eval {
			if ($node->getParentNode->getParentNode) {
				return $node->toString;
			} else { '' }
		} || '';
	} else {
		$retval = $node->toString;
	};

	if ( $self->{binmode} &&
		$self->is_utf8_tainted($retval)) {
		use Carp qw(confess);  # TODO remove this
		confess("Wrong translation by stylesheet".
				" (result is Unicode-tainted) at ".$self->get_xpath_of_node($node).
				"\n$retval\n");
	}

	return $retval;
}


sub translate_text_node {
    my $self = shift;	
	my $node = shift;
    my $translations = $self->{template};

	my $trans = $translations->{'#text'} || $translations->{'text()'};

	return $node->toString unless $trans;

	my $middle = '';

    my $action = $trans->{action};

    my $t = new XML::XPathScript::Template::Tag;
    $t->{$_} = $trans->{$_} for keys %{$trans};
	if (my $code = $trans->{testcode}) 
	{

		$action = $code->($node, $t);
    }
		
	no warnings 'uninitialized';
    return if defined($action) and $action == DO_NOT_PROCESS;

    $middle = $node->toString if defined($action) 
                                    and $action == DO_TEXT_AS_CHILD;

	return $t->{pre} . $middle . $t->{post};
}

sub translate_element_node {
    my $self = shift;
	my $node = shift;
    my $translations = $self->{template};

    my $node_name = 
        $self->{parser} eq 'XML::LibXML' ? $node->localname
      : $self->{parser} eq 'XML::XPath'  ? 
                                 # nasty hack to get around that 
                                 # the root has no name 
                                 ( $node->getName && $node->getLocalName )
      :           croak "unsupported parser:  $self->{parser}"
      ;

    my $namespace;
    if( $self->{parser} eq 'XML::LibXML' ) {
        my $ns = $node->getNamespaces();
        $namespace = $ns ? $ns->getData() : undef ;
    }
    elsif( $self->{parser} eq 'XML::XPath' ) {
        if( my $prefix = $node->getPrefix ) {
            $namespace = $node->getNamespace( $prefix )->getExpanded();
        }
    }
    else {
        croak "unsupported parser:  $self->{parser}"
    }

    my $trans = XML::XPathScript::Template::resolve( $translations, 
                                                $namespace, $node_name );

    unless( $trans ) {
        # no specific and no generic? Okay, okay, return as is...
        no warnings qw/ uninitialized /;
        return $self->start_tag($node) . 
                $self->_apply_templates( ( $self->{parser} eq 'XML::LibXML' ) ? 
                                    $node->childNodes : $node->getChildNodes) .
                $self->end_tag($node);	
		
    }

    my $dokids = 1;  # by default we do the kids
    my $search;
    my $t = new XML::XPathScript::Template::Tag;
    $t->{$_} = $trans->{$_} for keys %{$trans};

    my $action = $trans->{action};
    
	if ($trans->{testcode}) {
        $action = $trans->{testcode}->($node, $t);
    }

	no warnings 'uninitialized';
    if( defined( $action) and $action !~ /^-?\d+/ ) {
        # ah, an xpath expression
        $dokids = 0;
        $search = $action;
    }
    elsif ( defined($action) and $action == DO_NOT_PROCESS ) {
        return;
    }
    elsif ($action == DO_SELF_ONLY ) {
        $dokids = 0;
    }

    # default: process children too.
	my $has_kids = $self->{parser} eq 'XML::LibXML' ? 
						$node->hasChildNodes() : $node->getFirstChild();
	
    my $pre = $self->interpolate($node, $t->{pre});
	$pre .= $self->start_tag( $node , $t->{rename}) if $t->{showtag};
	$pre .= $t->{intro};
	$pre .= $self->interpolate($node, $t->{prechildren}) if $has_kids;
	
	my $post;
	$post .= $self->interpolate($node, $t->{postchildren}) if $has_kids;
	$post .= $t->{extro};
	$post .= $self->end_tag( $node, $t->{rename} ) if  $t->{showtag};
	$post .= $self->interpolate($node, $t->{post});

	my $middle;
	my @kids = $dokids ? $node->getChildNodes()
			 : $search ? $node->findnodes($search)
			 : ();
	for my $kid ( @kids ) 
	{
		$middle .= $self->interpolate($node, $trans->{prechild}) 
			if $self->is_element_node( $kid );

		$middle .= $self->_apply_templates($kid);

		$middle .= $self->interpolate($node, $trans->{postchild})
			if $self->is_element_node( $kid );
	}
        
	return $pre . $middle . $post
}

sub translate_comment_node {
    my $self = shift;
	my $node = shift;
    my $translations = $self->{template};

	my $trans = $translations->{'#comment'} || $translations->{'comment()'};

	return $node->toString unless $trans;

	my $middle = $self->{parser} eq 'XML::LibXML' ?
					$node->textContent : $node->getData;

	if (my $code = $trans->{testcode}) 
	{
		my $t = {};
		my $retval = $code->($node, $t);
		if ($retval and %$t) {
			foreach my $tkey (keys %$t) {
				$trans->{$tkey} = $t->{$tkey};
			}
		}

		return if $retval == DO_NOT_PROCESS;
		$middle = '' if $retval == DO_SELF_ONLY;
	}
	
	no warnings 'uninitialized';
	return $trans->{pre}. $middle. $trans->{post};
}

sub start_tag {
    my( $self, $node, $name ) = @_;

    $name ||= $node->getName or return '';

    my $string = '<'.$name;

	# do we need this for libXML?
	if( $self->{parser} eq 'XML::XPath' )
	{
    	$string .= $_->toString for $node->getNamespaceNodes;
	}

    for my $attr ( ( $self->{parser} eq 'XML::LibXML' ) ? 
						$node->attributes : $node->getAttributeNodes) 
	{
	  
	  	
		if( $self->{parser} eq 'XML::XPath' )
	   	{
	   		$string .= $attr->toString;
	   	}
	   	else
	   	{
			#my $att = $attr->toString( 0, 1 );
		    	#$att =~ s/'/&quot;/g;
            if( $attr->isa( 'XML::LibXML::Namespace' ) ) {
                $string .= ' xmlns:'.$attr->getName().'="'.$attr->value().'" ';
            } 
            else {
                $string .= $attr->toString( 0, 1 );
            }
		}
    }

    $string .= '>';

    return $string;
}

sub end_tag {
    my $self = shift;
    if (my $name = $_[1] || $_[0]->getName) {
        return "</$name>";
    }
	return '';
}

sub interpolate {
    my ($self, $node, $string) = @_;
	
	# if string is empty or no interpolation,
	# we return
    return( $string || '' ) unless 
		defined( $string ) and 
        $self->{is_interpolating};

	my $regex = $self->{interpolation_regex};
	$string =~ s/$regex/ $node->findvalue($1) /egs;
	
	no warnings 'uninitialized';
    return $string;
}

=pod

=back

=head1 BUGS

Right now I<XML::XPathScript::Processor> is just an auxillary module
to L<XML::XPathScript> which should not be called directly: in other
words, XPathScript's XML processing engine is not (yet) properly
decoupled from the stylesheet language parser, and thus cannot stand
alone.

Please send bug reports to <bug-xml-xpathscript@rt.cpan.org>,
or via the web interface at 
http://rt.cpan.org/Public/Dist/Display.html?Name=XML-XPathScript .

=head1 AUTHORS

Yanick Champoux <yanick@cpan.org> 
and Dominique Quatravaux <dom@idealx.com>

=cut

1;
