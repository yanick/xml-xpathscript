package Apache::AxKit::Language::YPathScript;

use strict;
use vars qw( @ISA $VERSION $stash );

@ISA = qw/ Apache::AxKit::Language XML::XPathScript /;

=head1 NAME

Apache::AxKit::Language::YPathScript - An XML Stylesheet Language

=head1 SYNOPSIS

  AxAddStyleMap "application/x-xpathscript => \
        Apache::AxKit::Language::YPathScript"

=head1 DESCRIPTION

YPathScript is a fork of the original AxKit's XPathScript using 
XML::XPathScript as its transforming engine. 

As it is mostly 
backward compatible with the classic Axkit XPathScript module, 
the definitive reference for 
XPathScript, located at http://axkit.org/docs/xpathscript/guide.dkb,
also applies to YPathScript, excepts for the differences listed in the
sections below.

=head1 PRE-DEFINED STYLESHEET VARIABLES 

=item $r

A copy of the Apache::AxKit::request object -- which is itself a wrapper around
the Apache::request object -- tied to the current document.

	<%	$location = $r->blah() %>
	<p>URL: <%= $location %></p>

=back 

=cut

use Apache;
use Apache::File;
use Apache::AxKit::Provider;
use Apache::AxKit::Language;
use Apache::AxKit::Cache;
use Apache::AxKit::Exception;
use Apache::AxKit::CharsetConv;
use XML::XPathScript; 

$VERSION = 1.4;

=head2 Functions

=item $xps = new Apache::AxKit::Language::YPathScript($xml_provider, $style_provider)

Constructs a new YPathScript language interpreter out of the provided
providers.

=cut

sub new
{
	my( $class, $xml_provider, $style_provider ) = @_;
	
	my $self = XML::XPathScript::new( $class, 
							xml_provider => $xml_provider, 
	                        style_provider => $style_provider );

	return $self;
}

=item	$rc = handler( $class, $request, $xml_provider, $style_provider )

	The function called by Apache. Does all cache-managing magic.
	Not present in X::X.


=cut

sub handler 
{
	AxKit::Debug( 10, "this is Apache::AxKit::Language::YPathScript version $VERSION" );
	
    my ( $class, $r, $xml_provider, $style_provider) = @_;

	my $xps = new Apache::AxKit::Language::YPathScript( $xml_provider, $style_provider );

    $xps->{local_ent_handler} = $xml_provider->get_ext_ent_handler();

    AxKit::Debug(6, "YPathScript: Getting XML Source");
  
	# try to find the XML document
	$xps->{xml} = $r->pnotes('dom_tree')                       # dom_tree is an XML::LibXML DOM
	   	     	|| $r->pnotes('xml_string')
		     	|| get_source_tree($xml_provider);
   
    AxKit::Debug(7, "XML retrieved: $xps->{xml}\n");

	# $xpath->set_context($source_tree);   what does this do?

	AxKit::Debug( 6, "Recompiling stylesheet\n" );
	$xps->{stylesheet} = get_source_tree($style_provider);
	
	#$xps->get_stylesheet( $style_provider );

    AxKit::Debug(7, "Running YPathScript script\n");
    local $^W;
	$xps->compile( '$r' );
	return $xps->process( '', $r );
}

=item $file_content = I<include_file( $filename )>

=item $file_content = I<include_file( $filename, @includestack )>

Overloaded from XML::XPathScript in order to provide URI-based
stylesheet inclusions: $filename may now be any AxKit URI.  The AxKit
language class drops support for plain filenames that exists in the
ancestor class: this means that include directives like

   <!-- #include file="/some/where.xps" -->

in existing stylesheets should be turned into

   <!-- #include file="file:///some/where.xps" -->

in order to work with AxKit.

=cut

sub include_file 
{
    my ($self, $filename, @includestack) = @_;
	
	my $provider = $self->{xml_provider};

    AxKit::Debug(10, "YPathScript: entering include_file ($filename)");

    # return if already included
    my $key = $provider->key();
    return '' if grep $_ eq $filename, @{$stash->{$key}{includes}};

    push @{$stash->{$key}{includes}}, $filename;
    
    my $apache = $provider->apache_request;
    my $sub = $apache->lookup_uri( $filename );
    local $AxKit::Cfg = Apache::AxKit::ConfigReader->new( $sub );
    
    my $inc_provider = Apache::AxKit::Provider->new_style_provider( $sub );
	
	AxKit::Debug( 10, "File: $filename, Sub: $sub, $inc_provider: $inc_provider" );
    
    my $contents;
    eval 
	{ 
        my $fh = $inc_provider->get_fh();
        local $/;
        $contents = <$fh>;
    };
    if ($@) 
	{
		eval{ $contents = ${ $inc_provider->get_strref() } };
		if( $@ ){ AxKit::Debug( 10, "couldn't include $filename" ) }
    }
    
    my $r = AxKit::Apache->request();
    if (my $charset = $r->dir_config('AxOutputCharset')) {
        
        AxKit::Debug(8, "XPS: got charset: $charset");
        
        my $map = Apache::AxKit::CharsetConv->new($charset, "utf-8") 
					or die "No such charset: $charset";

        $contents = $map->convert($contents);
    }
    
    $stash->{$key}{includes} = [];
    
    AxKit::Debug(10, "YPathScript: extracting from '$key' contents: $contents\n");

	return $self->extract( $inc_provider, @includestack );
}

=item 	$doc = get_source_tree( $xml_provider  )

	Reads an XML document from the provider.
	Return the doc as a string.

	Not present in X::X

=cut

sub get_source_tree 
{
    my $provider = shift;
    my $xml;

    AxKit::Debug(7, "YPathScript: reparsing file");

    eval 
	{
        my $fh = $provider->get_fh();
        local $/;
        $xml = <$fh>;
        close $fh;
    };
   
	# didn't work? try get_strref
	$xml = $provider->get_strref() if $@;
    
    AxKit::Debug(7, "YPathScript: Returning source tree");
    return $xml;
}

=item $string = I<read_stylesheet( $stylesheet )>

Read the $stylesheet (which can be a filehandler or a string). 
Used by I<extract>.

Overrides the XML::XPathScript method

=cut

sub read_stylesheet
{
	my ( $self, $stylesheet ) = @_;
	my $contents;

	if( ref( $stylesheet ) ) {
    	eval {	 
			my $fh = $stylesheet->get_fh();
			local $/;
			$contents = <$fh>;
		};
		$self->debug( 7, "wasn't able to extract $stylesheet: $@" ) if $@;
	}
	else {
		$contents = $stylesheet;  # it's a string
	}
    
    my $r = AxKit::Apache->request();
    if (my $charset = $r->dir_config('AxOutputCharset')) {
        
        $self->debug(8, "XPS: got charset: $charset");
        
        my $map = Apache::AxKit::CharsetConv->new($charset, "utf-8") 
			or $self->die( "No such charset: $charset" );
        $contents = $map->convert($contents);
    }

	return $contents;	
}

=item $self->debug( $level, $message )

Prints $message if the requested debug level 
is equal or smaller than $level.

=cut

sub debug{ shift; AxKit::Debug( @_ ) }

=item $self->die( $suicide_note )

=cut

sub die{ die @_ }

__END__



=item  $nodeset = XML::XPath::Function::document( $node, $uri )

	Reads XML given in $uri, parses it and returns it in a nodeset.

	Pretty similar to the one in X::X, except for details like inclusion
	of scheme axkit://.

=cut

sub XML::XPath::Function::document {
    # warn "Document function called\n";
    return unless $Apache::AxKit::Language::YPathScript::local_ent_handler;
    my $self = shift;
    my ($node, @params) = @_;
    die "document: Function takes 1 parameter\n" unless @params == 1;

    my $xml_parser = XML::Parser->new(
            ErrorContext => 2,
            Namespaces => $XML::XPath::VERSION < 1.07 ? 1 : 0,
            # ParseParamEnt => 1,
            );

    my $parser = XML::XPath::XMLParser->new(parser => $xml_parser);

    my $results = XML::XPath::NodeSet->new();
    my $uri = $params[0];
    my $newdoc;
    if ($uri =~ /^axkit:/) {
        $newdoc = $parser->parse( AxKit::get_axkit_uri($uri) );
    }
    elsif ($uri =~ /^\w\w+:/) { # assume it's scheme://foo uri
        eval {
            # warn "Trying to parse $params[0]\n";
            $newdoc = $parser->parse(
                    $Apache::AxKit::Language::YPathScript::local_ent_handler->(
                        undef, undef, $uri
                    )
                );
            # warn "Parsed OK into $newdoc\n";
        };
        if (my $E = $@) {
            if ($E->isa('Apache::AxKit::Exception::IO')) {
                AxKit::Debug(2, $E);
            }
            else {
                throw Apache::AxKit::Exception::Error(-text => "Parse of '$uri' failed: $E");
            };
        }
    }
    else {
        AxKit::Debug(3, "Parsing local: $uri\n");
        
        # create a subrequest, so we get the right AxKit::Cfg for the URI
        my $apache = AxKit::Apache->request;
        my $sub = $apache->lookup_uri($uri);
        local $AxKit::Cfg = Apache::AxKit::ConfigReader->new($sub);
        
        my $provider = Apache::AxKit::Provider->new_content_provider($sub);
        
        $newdoc = $parser->parse( xml => get_source_tree($provider) );
        undef $provider;
        undef $apache;
        undef $sub;
    }

    $results->push($newdoc) if $newdoc;
    #AxKit::Debug(8, "YPathScript: document() returning");
    return $results;
}

'Apache::AxKit::Language::YPathScript';
