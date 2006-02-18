package XML::XPathScript::Template;

use strict;
use warnings;

use Carp;
use List::MoreUtils qw/ none /;
use Scalar::Util qw/ reftype /;
use Data::Dumper;
use XML::XPathScript::Template::Tag;


{

sub new {
   my( $class ) = @_;

   my $self = {};
   bless $self, $class;

   return $self;
}


sub set {
	my( $self, $tag, $attribute_ref ) = @_;

    my $type = reftype $tag;
    my @templates =         # templates to change
            !$type           ? $self->{$tag} 
                                    ||= new XML::XPathScript::Template::Tag
          : $type eq 'ARRAY' ?  map { $self->{$_} 
                                    ||= new XML::XPathScript::Template::Tag 
                                    } @$tag
          : croak "tag cannot be of type $type"
          ;

    $_->set( $attribute_ref ) for @templates;

	return;
}

sub copy { 
    my( $self, $src, $copy, $attributes_ref ) = @_;

    croak "tag $src not found in template"
        unless $self->{$src};

    my %attributes = %{ $self->{$src} };
    %attributes = map { $_ => $attributes{ $_ } }@$attributes_ref 
            if $attributes_ref;
   
   $self->set( $copy, \%attributes );

   return;
}

sub alias {
    my( $self, $src, $copy ) = @_;

    $self->{$_} = $self->{$src} for ref( $copy ) ? @$copy : $copy;

    return;
}


sub dump {
    my( $self, @tags ) = @_;
    
    my %template = %{$self};
    
    @tags = keys %template unless @tags;
    
    %template = map { $_ => $template{ $_ } } @tags;
    
    return Data::Dumper->Dump( [ \%template ], [ 'template' ] );
}

sub clear {
    my( $self, $tags ) = @_;

    delete $self->{ $_ } for $tags ? @$tags : keys %$self;
}

sub is_alias {
    my( $self, $tag ) = @_;

    my $id = $self->{$tag};

    my @aliases = grep { $_ ne $tag and $self->{$_} eq $id } keys %{$self};

    return @aliases;
}

sub unalias {
    my( $self, $tag ) = @_;

    my $fresh = new XML::XPathScript::Template::Tag;

    $fresh->set( $self->{$tag} );

    $self->{$tag} = $fresh;

    return;
}

sub namespace {
    my( $self, $namespace ) = @_;

    return $self->{ ":$namespace" } ||= new XML::XPathScript::Template;
}

sub resolve {
    my $template = shift;
    my( $namespace, $tag ) = @_ == 2 ? @_ : ( undef, @_ ); 

    no warnings qw/ uninitialized /;
    $namespace = ':'.$namespace;

    return ( ( $template->{$namespace} && 			# selection order
                (  $template->{$namespace}{$tag} 	# foo:bar
                || $template->{$namespace}{'*'} ) ) # foo:*
                || $template->{$tag}                # bar
                || $template->{'*'} );              # *  
                                                    # (and undef if nothing)
}

'end of XML::XPathScript::Template';
}

__END__

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  Module Documentation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

=head1 NAME

XML::XPathScript::Tag - Tag within a XML::XPathScript transformation template 

=head1 SYNOPSIS

    <%
        $t->set( 'foo' => { testcode => \&frumble  } );

        sub frumble {
            my( $n, $t ) = @_;

            $t->set({ 'pre' =>  '<bar>' });

            return $DO_SELF_AND_CHILDREN;

        }
     %>
     <%= apply_templates() %>

=head1 DESCRIPTION


A stylesheet's template defines the transformations and actions that 
are performed on the tags of a document as they are processed.

The template of a stylesheet is aliased by default to the variables 
I<$t> and I<$template>.


=head1 METHODS

=over

=item  set

    $template->( $tag, \%attributes )
    $template->set_template( \@tags , \%attributes )

Update the tag(s) $tag or @tags in the template with the 
given %attributes.

Example:

    $template->set( 'foo' => { pre => '<a>', post => '</a>' } );

=item copy

    $template->copy( $original_tag, $copy_tag );
    $template->copy( $original_tag, $copy_tag, \@attributes );
    $template->copy( $original_tag, \@copy_tags );
    $template->copy( $original_tag, \@copy_tags, \@attributes );

Copies all attributes (or a subset of them if \@attributes is given)
of $original_tag to $copy_tag.

Note that subsequent modifications of the original tag will not
affect the copies. To bind several tags to the same behavior, see
L<alias>.

Example:

    # copy the attributes 'pre' and 'post' of important 
    # to 'urgent' and 'redHot'
    $template->copy( 'important' => [ qw/ urgent redHot / ], 
                        [ qw/ pre post / ] );

=item alias

    $template->alias( $original_tag => $alias_tag );
    $template->alias( $original_tag => \@alias_tags );

Makes the target tags aliases to the original tag. Further
modifications that will be done on any of these tags will 
be reflected on all others. 

Example:

    $template->alias( 'foo' => 'bar' );
    $template->set( 'bar' => { pre => '<u>' } );  # also modify 'foo'


=item is_alias

    @aliases = $template->is_alias( $tag )

Returns all tags that are aliases to $tag. 

=item unalias

    $template->unalias( $tag )

Unmerge $tag of its aliases, if it has any. Further modifications to
$tag will not affect the erstwhile aliases, and vice versa.

Example:

    $template->alias( 'foo' => [ qw/ bar baz / ] );
    $template->set( 'foo' => { pre => '<a>' } );    # affects foo, bar and baz
    $template->unalias( 'bar' );
    $template->set( 'bar' => { pre => '<c>' } );    # affects only bar
    $template->set( 'baz' => { pre => '<b>' } );    # affects foo and baz

=item dump

    $template->dump()
    $template->dump( \@tags )

Returns a pretty-printed dump of the templates. If @tags are
specified, only return their templates.

Example:

    <%= $template->dump( 'foo' ) %>
    
    # will yield something like

    # $template = {
    #                'foo' => {
    #                            'post' => '</bar>',
    #                            'pre' => '<bar>'
    #                        }
    #             };

=cut

=back


=head1 BACKWARD COMPATIBILITY

Prior to version FIXME of XML::XPathScript, the template of a
stylesheet was not an object but a simple hash reference. Modifications
to the template were done by manipulating the hash directly.

    <%
        # pre-FIXME way of manipulating the template
        $t->{important}{pre}  = '<blink>';
        $t->{important}{post} = '</blink>';
    
        for my $tag ( qw/ urgent redHot / ) {
            for my $attr ( qw/ pre post / ) {
                $t->{$tag}{$attr} = $t->{important}{$attr};
            }
        }

        $t->{ alert } = $t->{ important };
    %>

Don't tell anyone, but as an XML::XPathScript::Template is
a blessed hash reference this way of doing things will 
still work. However, direct manipulation of the template's hash
is deprecated. Instead, it is recommended to use the object's 
access methods.

    <%
        # correct way to manipulate the template
        $t->set( important => { pre => '<blink>', 
                                post => '</blink>',
                                showtag => 1
                                } );

        $t->copy( important => [ qw/ urgent redHot / ], [ qw/ pre post / ] );

        $t->alias( important => alert );
    %>

=head1 BUGS AND LIMITATIONS

There are no known bugs in this module.

Please report problems to Yanick Champoux (yanick@cpan.org)

Patches are welcome.

=head1 AUTHOR

Yanick Champoux (yanick@cpan.org)

=cut

