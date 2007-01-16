use strict;
use warnings;

package XML::XPathScript::Processor::LibXML;

use base qw/ XML::XPathScript::Processor /;

sub get_namespace { 
        my $ns = $_[1]->getNamespaces();
        return $ns ? $ns->getData() : () ;
}

sub is_text_node { 
	# little catch: XML::LibXML::Comment is a
	# XML::LibXML::Text
        $_[1]->isa( 'XML::LibXML::Text' ) 
    && !$_[1]->isa( 'XML::LibXML::Comment' );
}

sub get_attributes   { $_[1]->attributes }
sub get_text_content { $_[1]->textContent }
sub get_child_nodes  { $_[1]->childNodes }
sub get_node_name    { $_[1]->localname }
sub is_element_node  { $_[1]->isa( 'XML::LibXML::Element' ); }
sub is_comment_node  { $_[1]->isa( 'XML::LibXML::Comment' ); }
sub is_pi_node       { $_[1]->isa( "XML::LibXML::PI" ); }
sub is_nodelist      { $_[1]->isa( 'XML::LibXML::NodeList' ); }

sub get_attribute {
    return $_[1]->isa( 'XML::LibXML::Namespace' )
         ?  ' xmlns:'.$_[1]->getName() . '="' . $_[1]->value() . '" '
         : $_[1]->toString( 0, 1 )
         ;
}

1;
