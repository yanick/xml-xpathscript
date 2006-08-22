package XML::XPathScript::Template::Tag;

use strict;
use warnings;

use Carp;
use Scalar::Util qw/ reftype /;

our $VERSION = '1.45';

our @ALLOWED_ATTRIBUTES =  qw/ pre post testcode showtag
                               intro extro prechildren postchildren
                               prechild postchild action rename /;

sub new {
   my( $class ) = @_;

   my $self = {};
   bless $self, $class;

   return $self;
}

sub get {
    my( $self, @attributes ) = @_;

    return map { $self->{$_} } @attributes;
}

sub set {
	my( $self, $attribute_ref ) = @_;

	for my $key ( keys %{$attribute_ref} ) {
        croak "attribute $key not allowed" 
            if ! grep { $key eq $_ } @ALLOWED_ATTRIBUTES;

        $self->{$key} = $attribute_ref->{$key};

        # renaming implies showing the tag
        $self->{showtag} = 1 if $key eq 'rename';
	}

	return;
}

'end of XML::XPathScript::Template::Tag';

__END__

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Module Documentation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

=head1 NAME

XML::XPathScript::Template::Tag - XPathScript Template Element 

=head1 SYNOPSIS

    <%
        $tag->set( 'foo' => { testcode => \&frumble  } );

        sub frumble {
            my( $n, $t ) = @_;

            $t->set({ 'pre' =>  '<bar>' });

            return DO_SELF_AND_CHILDREN();

        }
     %>
     <%= apply_templates() %>

=head1 DESCRIPTION

The XML::XPathScript::Tag class is used to represent tags 
within an XPathScript template. 

=head1 CALLED AS ARGUMENT TO THE TESTCODE FUNCTIONS

Typically, the only time you'll be exposed to those objects is
via the testcode functions, which receive as arguments a reference
to the current node and its associated template entry. 

Note that changing any of the tag's attributes only impacts the current
node and doesn't change the tag entry in the template. To modify the 
template, you'll have to access I<$template> directly.

Example:

    <%
        $template->set( 'foo' => { testcode => \&frumble  } );

        sub frumble {
            my( $n, $t ) = @_;

            if( $n->findvalue( './@bar' ) eq 'whonk' ) {
                # we've been whonk'ed! This foo must
                # blink
                $t->set({ 
                    'pre' => '<blink>', 'post' => '</blink>' 
                });

                # and the next foos will be in italic
                $template->set( foo => { 
                    pre => '<i>', post => '</i>' 
                } );
            }
            return DO_SELF_AND_CHILDREN();
        }
    %>

=head1 METHODS

=head2 new

    $tag = XML::XPathScript::Template::Tag->new

Creates a new, empty tag.

=head2 set

    $t->set( \%attributes )

Updates the tag's attributes with the values given in \%attributes

Example:

    $t->set({ pre => '<a>', post => '</a>' });

=head2 get

    @values = $tag->get( @attributes )

Returns the values of @attributes.

Example:

    @values = $tag->get( 'pre', 'post' );

=head1 BACKWARD COMPATIBILITY

As for XML::XPathScript::Template, prior to release 1.0 of XPathScript, 
the tags within the template of a
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

Don't tell anyone, but as an XML::XPathScript::Template::Tag is
a blessed hash reference this way of doing things will 
still work. However, direct manipulation of the tag's hash
is deprecated. Instead, it is recommended to use the object's 
access methods.

    <%
        $template->set( foo => { testcode => \&tc_foo } );
        sub tc_foo {  
            my( $n, $t ) = @_;
           
            $t->set({ 
                pre => '<a>', post => '</a>' 
            });
            
            return DO_SELF_AND_CHILDREN;
        };
    %>

=head1 BUGS 

Please send bug reports to <bug-xml-xpathscript@rt.cpan.org>,
or via the web interface at 
http://rt.cpan.org/Public/Dist/Display.html?Name=XML-XPathScript .

=head1 AUTHOR

Yanick Champoux <yanick@cpan.org>

=cut

