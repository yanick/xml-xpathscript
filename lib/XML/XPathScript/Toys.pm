package XML::XPathScript::Toys;

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

use constant
{
	DO_SELF_AND_KIDS =>  1,
	DO_SELF_ONLY     => -1,
	DO_NOT_PROCESS   =>  0
};

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
	$XML::XPathScript::xp->findnodes(@_) 
}

=pod "

=item  I<findvalue($path)>

=item  I<findvalue($path, $context)>

Evaluates XPath expression $path and returns the resulting value.
. If the path returns a set of nodes, the stringification
is done automatically for you. 
=cut "

sub findvalue {
	$XML::XPathScript::xp->findvalue(@_) 
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
result is not guaranteed to be valid XML.

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
I<apply_templates> need not be UTF-8 (see L<perlunicode>): it is thus
possible using XPathScript to produce output in any character set
without an extra translation pass.

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
	if ( $arg1->isa('XML::XPath::NodeSet') or $arg1->isa('XML::LibXML::NodeList') ) 
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
    my @nodes = @_;

    my $retval = '';
    foreach my $node (@nodes) {
		# the || is there just to quiet the warnings
        $retval .= translate_node($node) || '';
    }

    return $retval;
}

=item  $boolean = is_element( $node )

Returns true if $node is an element node, false otherwise.

=cut

sub is_element_node {
	my $node = shift;

    return ( $XML::XPathScript::XML_parser eq 'XML::LibXML' ) ? 
		UNIVERSAL::isa( $node, 'XML::LibXML::Element' ) : 
		$node->isElementNode;
}

sub translate_node {
    my $node = shift;

    my $translations = $XML::XPathScript::trans;

	if( $XML::XPathScript::XML_parser eq 'XML::LibXML' and 
		UNIVERSAL::isa($node,"XML::LibXML::Document") ) 
	{
		$node = $node->documentElement;
	}

	if( $XML::XPathScript::XML_parser eq 'XML::LibXML' and
		UNIVERSAL::isa( $node, 'XML::LibXML::Comment' ) )
	{
		my $trans = $translations->{'#comment'} || $translations->{'comment()'};

		return $node->toString unless $trans;

		my $middle = $node->textContent;

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

	if ( ( $XML::XPathScript::XML_parser eq 'XML::LibXML' ) ? UNIVERSAL::isa( $node, 'XML::LibXML::Text' )
	                                               : $node->isTextNode) 
	{
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

    unless( is_element_node( $node ) ) 
	{
        # don't output top-level PI's
		# could this be it?
        if ($XML::XPathScript::XML_parser eq 'XML::XPath' and $node->isPINode) {
            return try {
                if ($node->getParentNode->getParentNode) {
                    return $node->toString;
                }
                return '';
            } catch Error with {
                return '';
            };
        }
        return $node->toString;
    }

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
        else { 	# any number beside 0 and -1 will do the kids
            $dokids = 1;
        }
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
	

    my $post;
	$post .= interpolate($node, $trans->{postchildren}) if $has_kids;
	$post .= end_tag( $node ) if  $trans->{showtag};
	$post .= interpolate($node, $trans->{post});

    if ($dokids) 
	{
        my $middle = '';
        for my $kid ($node->getChildNodes()) 
		{
            if ( is_element_node( $kid ) ) 
			{
                $middle .= interpolate($node, $trans->{prechild}) .
                        	_apply_templates($kid) .
                        	interpolate($node, $trans->{postchild});
            }
            else 
			{
                $middle .= _apply_templates($kid);
            }
        }
        
		return $pre . $middle . $post;
    }
	
    if($search) 
	{
        my $middle = '';
        for my $kid (findnodes($search, $node)) {
            if ( is_element_node( $kid ) ) {
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
    
	
	return $pre . $post;
}

sub start_tag {
    my ($node) = @_;

    my $name = $node->getName;
    return '' unless $name;

    my $string = "<" . $name;

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
			$string .= ' '.$attr->name."='".$value."' ";
		}
    }

    $string .= '>';

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
    return '' unless defined $string;
    return $string unless $XML::XPathScript::current->interpolating();

    my $new = '';
	$string =~ s/\{(.*?)\}/ $node->findvalue($1) /egs;
    return $string;
}

1;
