package XML::XPathScript::Toys;

use strict;

use Exporter;
use vars '@ISA', '@EXPORT';
@ISA = ('Exporter');

@EXPORT = qw(
        findnodes
        findvalue
        findvalues
        findnodes_as_string
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
        );

=pod "

=over 4

=item I<DO_SELF_AND_KIDS>

=item I<DO_SELF_ONLY>

=item I<DO_NOT_PROCESS>

Symbolic constants evaluating respectively to 1, -1 and 0, to be used
as mnemotechnic return values in C<testcode> routines instead of the
numeric values which are harder to remember.

=cut "

use constant DO_SELF_AND_KIDS =>  1;
use constant DO_SELF_ONLY     => -1;
use constant DO_NOT_PROCESS   =>  0;

# quieten warnings when compiling this module
sub apply_templates (;$@);

=pod "

=item I<findnodes($path)>

=item I<findnodes($path, $context)>

Returns a list of nodes found by XPath expression $path, optionally
using $context as the context node (default is the root node of the
current document).  In scalar context returns a NodeSet object (but
you do not want to do that, see L<XML::XPathScript/XPath scalar return
values considered harmful>).

=cut "

sub findnodes {
	$XML::XPathScript::xp->findnodes(@_)
}

=pod "

=item  I<findvalue($path)>

=item  I<findvalue($path, $context)>

Evaluates XPath expression $path and returns the resulting value. If
the path returns a set of nodes, the stringification is done
automatically for you (see L<XML::XPathScript/XPath scalar return
values considered harmful>). The result is an UTF-8 string.

=cut "

sub findvalue {
	my $blob = $XML::XPathScript::xp->findvalue(@_);
	return $blob if (! ref($blob));
	# Was "$blob" but Perl 5.6.1 seems to have issues with
	# UTF8-flag used in overloaded stringification :-(
	return $blob->value();
}

=pod "


=item  I<findvalues($path)>

=item  I<findvalues($path, $context)>

Evaluates XPath expression $path as a nodeset expression, just like
L</findnodes> would, but returns a list of UTF8-encoded XML strings
instead of node objects or node sets. See also
L<XML::XPathScript/XPath scalar return values considered harmful>.

=cut "
sub findvalues {
    my @nodes = findnodes(@_);
    map { findvalue('.', $_) } @nodes;
}

=pod "

=item I<findnodes_as_string($path)>

=item I<findnodes_as_string($path, $context)>

Similar to L</findvalues> but concatenates the XML snippets.  The
result obviously is not guaranteed to be valid XML.

=cut "

sub findnodes_as_string {
	$XML::XPathScript::xp->findnodes_as_string( @_ ) 
}

=pod "

=item I<matches($node, $path)>

=item I<matches($node, $path, $context)>

Returns true if the node matches the path (optionally in context $context)

=cut "

sub matches {
	$XML::XPathScript::xp->matches(@_)
}

sub set_namespace
{
	eval { $XML::XPathScript::xp->set_namespace(@_) };
	warn "set_namespace failed: $@" if $@;
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
I<apply_templates> need not be UTF-8 (see
L<XML::XPathScript/binmode>): it is thus possible to use XPathScript
to produce output in any character set without an extra translation
pass.

=cut "

sub apply_templates (;$@) {
	# catch the calls to apply_templates() 
	return apply_templates( findnodes('/') ) unless @_;
	
    my ($arg1, @args) = @_;

    if (!ref($arg1)) {
        # called with a path to find
		if( my $nodes = findnodes($arg1, @args) )
		{
        	return apply_templates($nodes);
		}
		else { 
			return 
		}
    }

    my $retval = '';
	if (is_nodelist($arg1))
	{
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
	return join( '', map { translate_node($_) or "" } @_ );
}

=item  is_element_node ( $object )

Returns true if $object is an element node, false otherwise.

=cut

sub is_element_node {
	UNIVERSAL::isa( $_[0], 'XML::XPath::Node::Element' ) or
		UNIVERSAL::isa( $_[0], 'XML::LibXML::Element' );
}

=pod "

=item  is_text_node ( $object )

Returns true if $object is a "true" text node (B<not> a comment node),
false otherwise.

=cut "

sub is_text_node {
	UNIVERSAL::isa($_[0], 'XML::XPath::Node::Text') or
	# little catch: XML::LibXML::Comment is a
	# XML::LibXML::Text
		( UNIVERSAL::isa($_[0], 'XML::LibXML::Text') &&
		  ! UNIVERSAL::isa($_[0], 'XML::LibXML::Comment') );
}

=pod "

=item  is_comment_node ( $object )

Returns true if $object is an XML comment node, false otherwise.

=cut "

sub is_comment_node {
		UNIVERSAL::isa( $_[0], 'XML::LibXML::Comment' ) or
			UNIVERSAL::isa( $_[0], 'XML::XPath::Node::Comment' );
}

=pod "

=item  is_pi_node ( $object )

Returns true iff $object is a processing instruction node.

=cut "

sub is_pi_node {
	UNIVERSAL::isa($_[0], "XML::LibXML::PI") ||
		UNIVERSAL::isa($_[0], "XML::XPath::Node::PI");
}

=pod "

=item  is_nodelist ( $object )

Returns true if $node is a node list (as returned by L</findnodes> in
scalar context), false otherwise.

=cut "

sub is_nodelist {
	UNIVERSAL::isa($_[0], 'XML::XPath::NodeSet') or
		UNIVERSAL::isa($_[0], 'XML::LibXML::NodeList');
}

=pod "

=item I<is_utf8_tainted($string)>

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

# This implementation is vulnerable to the "é" (e acute) getting
# crushed when source code gets converted e.g. to EBCDIC. Oh well.
sub is_utf8_tainted {
	my ($string)=@_;
	my $maybe_autopromoted = do { no bytes; no utf8; "é"  . $string};
	use bytes;
	return ( length($string) + 1 < length($maybe_autopromoted) );
}

# Getting an XPath that leads to some node (e.g. for warning messages)

=pod "

=item I<get_xpath_of_node($node)>

Returns an XPath string that points to $node, from the root. Useful to
create error messages that point at some location in the original XML
document.

=cut "

sub get_xpath_of_node {
	my $self =shift;

	# ugly hack to get aroudn XML::XPath::Element and XML::XPath::ElementImp
	# quirkiness
	$self = $$self if 
		$self->isa( 'XML::XPath::Node::Element' ) and not $self->isa( 'XML::XPath::Node::ElementImpl' );

	my $parent = ( $self->can("parentNode") ?
				$self->parentNode() :
				$self->getParentNode() );

	return "" unless defined $parent;

	my $name;
	if (is_element_node($self)) {
		$name = $self->findvalue('name()');
	} elsif (is_text_node($self)) {
		$name = "text()";
    } elsif (is_comment_node($self)) {
		$name = "comment()";
	} elsif (is_pi_node($self)) {
		$name = "processing-instruction()";
	} else {
		return get_xpath_of_node($parent)."/bizarre-node()";
	}

	# ugly hack, part II
	my @brothers = map{ ($_->isa( 'XML::XPath::Node::Element' ) ) ? $$_ : $_ } $parent->findnodes("./$name");

	# Short-cut for nodes that have an ID. FIXME: not all DTDs use
	# attribute named "id" as the SGML ID!
	if (is_element_node($self) && (my $id=findvalue('@id',$self))) {
		return get_xpath_of_node($parent).sprintf('/%s[@id="%s"]', $name, $id);
	}

	# Bug: the matches() function from XML::XPath is hosed, and only
	# works towards ancestors. We resort to comparing references for
	# identity, but even then the clever refcount-proof aliasing in
	# XML::XPath gets in the way badly.
	my $theself=($self =~ m/SCALAR/?$$self:$self);

	for my $i ( 0..$#brothers ) {
		my $thebrother=($brothers[$i] =~ m/SCALAR/?
						${$brothers[$i]}:$brothers[$i]);

		return sprintf '%s/%s[%d]', get_xpath_of_node($parent), $name, $i+1 
			if $theself eq $thebrother;
	};

	return get_xpath_of_node($parent)."/$name"."[?]";
}


########################## End of exportable stuff ####################

sub translate_node {
    my $node = shift;

	if( UNIVERSAL::isa($node,"XML::LibXML::Document") ) 
	{
		$node = $node->documentElement;
	}

	my $retval;
	if ( is_comment_node($node) ) {
		$retval = translate_comment_node( $node );
	} elsif ( is_text_node($node) )
	{
		$retval = translate_text_node( $node );
	} elsif (is_element_node( $node )) {
		$retval = translate_element_node( $node );
	} elsif ( is_pi_node($node) ) {
		# don't output top-level PI's
		$retval = eval {
			if ($node->getParentNode->getParentNode) {
				return $node->toString;
			} else { '' }
		} || '';
	} else {
		$retval = $node->toString;
	};

	if (XML::XPathScript->current()->{binmode} &&
		is_utf8_tainted($retval)) {
		use Carp qw(confess);
		confess("Wrong translation by stylesheet".
				" (result is Unicode-tainted) at ".get_xpath_of_node($node).
				"\n$retval\n");
	}

	return $retval;
}


sub translate_text_node {
	
	my $node = shift;
    my $translations = $XML::XPathScript::trans;

	my $trans = $translations->{'#text'} || $translations->{'text()'};

	return $node->toString unless $trans;

	my $middle = $node->toString;
	my $retval;

	if (my $code = $trans->{testcode}) 
	{
		my $t = {};
		$retval = $code->($node, $t);
		return if $retval == DO_NOT_PROCESS;

		if ($retval and %$t) 
		{
			$trans->{$_} = $t->{$_} for keys %$t;
		}
			
		$middle = '' if $retval == DO_SELF_ONLY;
	}

	# not pretty, but keep warning mollified
	return ($trans->{pre} ||''). 
		   ($middle       ||'').
		   ($trans->{post}||'');
}

sub translate_element_node {

	my $node = shift;
    my $translations = $XML::XPathScript::trans;

    my $node_name = $node->getName;
    my $trans;
	$trans = $translations->{$node_name} if defined $node_name;

	# no specific transformation? use the generic '*'
    unless( $trans ) 
	{
        $node_name = '*';
        
		unless( $trans = $translations->{$node_name} )
		{
			# no specific and no generic? Okay, okay, return as is...
			return start_tag($node) . 
                	_apply_templates( ( $XML::XPathScript::XML_parser eq 'XML::LibXML' ) ? 
										$node->childNodes : $node->getChildNodes) .
                	end_tag($node);	
		}
    }

    my $dokids = 1;  # by default we do the kids
    my $search;
    my $t = {};
    
	if ($trans->{testcode}) 
	{
        my $result = $trans->{testcode}->($node, $t);

		if( $result !~ /^-?\d+/ ) {
			# ah, an xpath expression
            $dokids = 0;
            $search = $result;
		}
		elsif ($result == DO_NOT_PROCESS ) {
			return;
		}
        elsif ($result == DO_SELF_ONLY ) {
            $dokids = 0;
        }
        # any number beside 0 and -1 will do the kids
    }

    local $translations->{$node_name};
    # copy old values in
    %{$translations->{$node_name}} = %$trans;

    if (%$t) 
	{
        $translations->{$node_name}{$_} = $t->{$_} for keys %$t;
        $trans = $translations->{$node_name};
    }

    # default: process children too.
	my $has_kids = $XML::XPathScript::XML_parser eq 'XML::LibXML' ? 
						$node->hasChildNodes() : $node->getFirstChild();
	
    my $pre = interpolate($node, $trans->{pre});
	$pre .= start_tag( $node ) if $trans->{showtag};
	$pre .= interpolate($node, $trans->{prechildren}) if $has_kids;
	

    my $post = '';
	$post .= interpolate($node, $trans->{postchildren}) if $has_kids;
	$post .= end_tag( $node ) if  $trans->{showtag};
	$post .= interpolate($node, $trans->{post});

    if ($dokids) 
	{
        my $middle;
        for my $kid ($node->getChildNodes()) 
		{
			$middle .= interpolate($node, $trans->{prechild})
				if is_element_node( $kid );

			$middle .= _apply_templates($kid);

			$middle .= interpolate($node, $trans->{postchild})
				if is_element_node( $kid );
        }
        
		return ($pre||'') . ($middle||'') . ($post||'');
    }
	
    if($search) 
	{
        my $middle = '';
        for my $kid (findnodes($search, $node)) {

			$middle .= interpolate($node, $trans->{prechild}) if is_element_node( $kid );
			$middle .= _apply_templates($kid);
			$middle .= interpolate($node, $trans->{postchild}) if is_element_node( $kid );

        }
        return $pre . $middle . $post;
    }
    
	
	return $pre . $post;

}

sub translate_comment_node {

	my $node = shift;
    my $translations = $XML::XPathScript::trans;

	my $trans = $translations->{'#comment'} || $translations->{'comment()'};

	return $node->toString unless $trans;

	my $middle = $XML::XPathScript::XML_parser eq 'XML::LibXML' ?
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
	
	$trans->{pre} ||= '';
	$trans->{post} ||= '';

	return $trans->{pre}. $middle. $trans->{post};
}

sub start_tag {
    my $node = shift;

    my $name = $node->getName or return '';

    my $string = '<'.$name;

	# do we need this for libXML?
	if( $XML::XPathScript::XML_parser eq 'XML::XPath' )
	{
    	$string .= $_->toString for $node->getNamespaceNodes;
	}

    for my $attr ( ( $XML::XPathScript::XML_parser eq 'XML::LibXML' ) ? 
						$node->attributes : $node->getAttributeNodes) 
	{
	   
		if( $XML::XPathScript::XML_parser eq 'XML::XPath' )
	   	{
	   		$string .= $attr->toString;
	   	}
	   	else
	   	{
		   my $value = $attr->value;
		   $value =~ s/'/&quot;/g;
			$string .= ' ' . $attr->name . "='$value' ";
		}
    }

    $string .= '>';

    return $string;
}

sub end_tag {
    if (my $name = shift->getName) {
        return "</$name>";
    }
	return '';
}

sub interpolate {
    my ($node, $string) = @_;

	# if string is empty or no interpolation,
	# we return
    return( $string || '' ) unless 
		defined( $string ) and 
		$XML::XPathScript::current->interpolating();

	my $regex = $XML::XPathScript::current->{interpolation_regex};
	$string =~ s/$regex/ $node->findvalue($1) /egs;
    return $string || '';
}

1;
