use strict;
use warnings;

package XML::XPathScript::Processor::B;

use base qw/ XML::XPathScript::Processor /;

sub findnodes { 
    my ( $self, $xpath, $context ) = @_;
    $context ||= $self->{dom};

    return $context->match( $xpath );
}

sub to_string {
    my ( $self, $node ) = @_;

    my $string = $self->start_tag( $node );
    $string .= $self->to_string( $_ ) for $node->get_children;
    $string .= $self->end_tag( $node );
   
    return $string;
}

sub get_name {
    my ( $self, $node ) = @_;
    return $node->get_name;
   # $node->can( 'name' ) ? $node->name : 
    #        $node->can( 'get_name' ) ? $node->get_name :
     #       'UNKNOWN';
}

sub get_attribute_names {
    if ( $_[1]->can( 'get_attr_names' ) ) {
        return $_[1]->get_attr_names;
    }
    else {
        return;
    }
}

sub get_attribute_value {
    return $_[1]->get_attr_value( $_[2] );
}

1;

