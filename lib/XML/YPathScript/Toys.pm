package XML::YPathScript::Toys;

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

use constant
{
	DO_SELF_AND_KIDS => 1,
	DO_SELF_ONLY => -1,
	DO_NOT_PROCESS => 0
};

# quieten warnings when compiling this module
sub apply_templates (;$@);

sub findnodes { $XML::YPathScript::xp->findnodes(@_) }

sub findvalue { $XML::YPathScript::xp->findvalue(@_) }

sub findvalues {
    my @nodes = findnodes(@_);
    map { findvalue( '.', $_ ) } @nodes;
}

sub findnodes_as_string { $XML::YPathScript::xp->findnodes_as_string( @_ ) }

sub matches { $XML::YPathScript::xp->matches(@_) }

sub set_namespace 
{
	eval { $XML::YPathScript::xp->set_namespace(@_) };
	warn "set_namespace failed: $@" if $@;
}

sub apply_templates (;$@) 
{
	# catch teh calls to apply_templates() 
	return apply_templates( findnodes('/') ) unless @_;
	
    my ($arg1, @args) = @_;

    if (!ref($arg1)) 
	{
        # called with a path to find
		if( my $nodes = findnodes($arg1, @args) )
		{
        	return apply_templates($nodes);
		}
		else
		{ 
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

sub _apply_templates {
    my @nodes = @_;

    my $retval = '';
    foreach my $node (@nodes) 
	{
		# the || is there just to quiet the warnings
        $retval .= translate_node($node) || '';
    }

    return $retval;
}


# $boolean = is_element_node( $node )
# returns true if $node is an element node, false otherwise
sub is_element_node
{
	my $node = shift;

    return ( $XML::YPathScript::XML_parser eq 'XML::LibXML' ) ? 
		UNIVERSAL::isa( $node, 'XML::LibXML::Element' ) : 
		$node->isElementNode;
}

sub translate_node 
{
    my $node = shift;

    my $translations = $XML::YPathScript::trans;

	if( $XML::YPathScript::XML_parser eq 'XML::LibXML' and 
		UNIVERSAL::isa($node,"XML::LibXML::Document") ) 
	{
		$node = $node->documentElement;
	}

	if( $XML::YPathScript::XML_parser eq 'XML::LibXML' and
		UNIVERSAL::isa( $node, 'XML::LibXML::Comment' ) )
	{
		my $trans = $translations->{'#comment'};

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

	if ( ( $XML::YPathScript::XML_parser eq 'XML::LibXML' ) ? UNIVERSAL::isa( $node, 'XML::LibXML::Text' )
	                                               : $node->isTextNode) 
	{
		my $trans = $translations->{'#text'};

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

		return $trans->{pre} . $middle . $trans->{post};
	}

    unless( is_element_node( $node ) ) 
	{
        # don't output top-level PI's
		# could this be it?
        if ($XML::YPathScript::XML_parser eq 'XML::XPath' and $node->isPINode) {
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
                	_apply_templates( ( $XML::YPathScript::XML_parser eq 'XML::LibXML' ) ? 
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

		return if $result == DO_NOT_PROCESS;

        if ($result == DO_SELF_ONLY ) 
		{
            $dokids = 0;
        }
        elsif ($result == DO_SELF_AND_KIDS ) 
		{
            $dokids = 1;
        }
        else # search pattern returned 
		{
            $dokids = 0;
            $search = $result;
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
	my $has_kids = $XML::YPathScript::XML_parser eq 'XML::LibXML' ? 
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
	if( $XML::YPathScript::XML_parser eq 'XML::XPath' )
	{
    	$string .= $_->toString for $node->getNamespaceNodes;
	}

    for my $attr ( ( $XML::YPathScript::XML_parser eq 'XML::LibXML' ) ? 
						$node->attributes : $node->getAttributeNodes) 
	{
	   
		if( $XML::YPathScript::XML_parser eq 'XML::XPath' )
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

sub end_tag 
{
    my ($node) = @_;

    if (my $name = $node->getName) 
	{
        return "</" . $name . ">";
    }
    else 
	{
        return '';
    }
}

sub interpolate {
    my ($node, $string) = @_;
    return '' unless defined $string;
    return $string if $XML::YPathScript::DoNotInterpolate;

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
