package XML::XPathScript::Template::Tag;

use strict;
use warnings;

use Carp;
use List::MoreUtils qw/ none /;
use Scalar::Util qw/ reftype /;
use Perl6::Export::Attrs;
use Data::Dumper;


{

our @ALLOWED_ATTRIBUTES =  qw/ pre post testcode showtag
                               intro extro prechildren postchildren /;

sub new {
   my( $class ) = @_;

   my $self = {};
   bless $self, $class;

   return $self;
}

sub set {
	my( $self, $attribute_ref ) = @_;

	for my $key ( keys %{$attribute_ref} ) {
        croak "attribute $key not allowed" 
            if none { $key eq $_ } @ALLOWED_ATTRIBUTES;

        $self->{$key} = $attribute_ref->{$key};
	}

	return;
}

'end of XML::XPathScript::Template::Tag';
}

__END__

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Module Documentation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

=head1 NAME

XML::XPathScript::Template - XML::XPathScript transformation template 

=head1 SYNOPSIS

    <%
        $t->set( 'important' => { 'pre' => '<blink>', 
                                  'post' => '</blink>',
                                  'prechild' => '<u>',
                                  'postchild' => '</u>',
                                  } );

        # urgent and annoying share the 'pre' and 'post'
        # of important
        $t->copy( 'important' => [ qw/ urgent annoying / ], 
                    [ qw/ pre post / ] );

        # redHot is a synonym of important
        $t->alias( 'important' => 'redHot' );

     %>
     <%= apply_templates() %>

=head1 DESCRIPTION

The XML::XPathScript::Tag class is used to represent tags 
within an XPathScript template. 

=head2 As Argument to the testcode Functions

Typically, the only time you'll be exposed to those objects is
via the testcode functions, which receive as arguments a reference
to the current node and its associated template entry. 

Note that changing any of the tag's attributes only impacts the current
node and doesn't change the tag entry in the template. To modify the 
template, you'll have to access I<$template> directly.

Example:

    <%
        $t->set( 'foo' => { testcode => \&frumble  } );

        sub frumble {
            my( $n, $t ) = @_;

            if( $n->findvalue( './@bar' ) eq 'whonk' ) {
                # we've been whonk'ed! This foo must
                # blink
                $t->set({ 'pre' => '<blink>', 'post' => '</blink>' });
                
                # and the next foos will be in italic
                $template->set( foo => { pre => '<i>', post => '</i>' } );
            }
            return $DO_SELF_AND_CHILDREN;
        }
    %>

=head1 METHODS

=over

=item set

    $t->set( \%attributes );

Updates the tag's attributes with the values given in \%attributes

Example:

    $t->set({ pre => '<a>', post => '</a>' });

=back

=head1 BACKWARD COMPATIBILITY

As for XML::XPathScript::Template, the tags within the template of a
stylesheet were not objects but simple hash references. Modifications
to the tag attributes were done by manipulating the hash directly.

    <%
        $t->{foo}{testcode} = sub {  
            my( $n, $t ) = @_;
            $t->{pre} = '<a>';
            $t->{post} = '</a>';

            return DO_SELF_AND_CHILDREN;
        };
    %>

Don't tell anyone, but as an XML::XPathScript::Tag is
a blessed hash reference this way of doing things will 
still work. However, direct manipulation of the tag's hash
is deprecated. Instead, it is recommended to use the object's 
access methods.

    <%
        $t->{foo}{testcode} = sub {  
            my( $n, $t ) = @_;
            $t->set({ pre => '<a>', post => '</a>' });

            return $DO_SELF_AND_CHILDREN;
        };
    %>

=head1 BUGS AND LIMITATIONS

There are no known bugs in this module.

Please report problems to Yanick Champoux (yanick@cpan.org)

Patches are welcome.

=head1 AUTHOR

Yanick Champoux (yanick@cpan.org)

=cut

