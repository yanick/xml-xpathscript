package Apache::AxKit::Language::YPathScript;

use strict;
use vars qw( @ISA $VERSION $stash );

@ISA = qw/ Apache::AxKit::Language XML::XPathScript /;

=pod

=head1 Apache::AxKit::Language::YPathScript

Sub-class of Apache::AxKit::Language and XML::YPathScript.

=head2 global variables

$VERSION 

$stash - Hash table of stylesheets

=head2 Imported modules

=over

=item Apache

=item Apache::File

=item XML::XPath 1.00

=item XML::XPath::XMLParser

=item XML::XPath::Node

=item XML::XPath::NodeSet

=item XML::Parser

=item Apache::AxKit::Provider

=item Apache::AxKit::Language

=item Apache::AxKit::Cache

=item Apache::AxKit::Exception

=item Apache::AxKit::CharsetConv

=item X::X::Toys

=back

=cut

use Apache;
use Apache::File;
#use XML::XPath 1.00;
#use XML::XPath::XMLParser;
#use XML::XPath::Node;
#use XML::XPath::NodeSet;
#use XML::Parser;
use Apache::AxKit::Provider;
use Apache::AxKit::Language;
use Apache::AxKit::Cache;
use Apache::AxKit::Exception;
use Apache::AxKit::CharsetConv;
use XML::XPathScript; # qw/ libxml /;

$VERSION = '1.0';

=head2 Functions

=item $xps = new Apache::AxKit::Language::YPathScript($xml_provider, $style_provider)

Constructs a new XPathScript language interpreter out of the provided
providers.

=cut

sub new
{
	my( $class, $xml_provider, $style_provider ) = @_;

	my $self = XML::XPathScript::new( $class, xml_provider => $xml_provider, 
	                                          style_provider => $style_provider );

	return $self;
}

=item	$rc = handler( $class, $request, $xml_provider, $style_provider )

	The function called by Apache. Does all cache-managing magic.
	Not present in X::X.


=cut

sub handler {
    my ( $class, $r, $xml_provider, $style_provider) = @_;

	my $xps = Apache::AxKit::Language::YPathScript->new( $xml_provider, $style_provider );

    $xps->{local_ent_handler} = $xml_provider->get_ext_ent_handler();

    AxKit::Debug(6, "YPathScript: Getting XML Source");
  
	# try to find the XML document
	$xps->{xml} = $r->pnotes('dom_tree')                       # dom_tree is an XML::LibXML DOM
	   	     	|| $r->pnotes('xml_string')
		     	|| get_source_tree($xml_provider);
   
    AxKit::Debug(7, "XML retrieved: $xps->{xml}\n");

	# $xpath->set_context($source_tree);   what does this do?

	$xps->debug(6, "Recompiling stylesheet\n");
	$xps->{stylesheet} = get_source_tree($style_provider);
	
	#$xps->get_stylesheet( $style_provider );

    AxKit::Debug(7, "Running YPathScript script\n");
    local $^W;
	return $xps->process();

	# no output? Try apply_templates
	print XML::XPathScript::Toys::apply_templates()
		unless $r->pnotes('xml_string') or $r->dir_config('XPSNoApplyTemplatesOnEmptyOutput');
}

=item	$file_content = include_file( $filename )

Overloaded from XML::XPathScript in order to provide URI-based
stylesheet inclusions: $filename may now be any AxKit URI.  The AxKit
language class drops support for plain filenames that exists in the
ancestor class: this means that include directives like

   <!-- #include file="/some/where.xps" -->

in existing stylesheets should be turned into

   <!-- #include file="file:///some/where.xps" -->

in order to work with AxKit.

=cut

sub include_file {
    my ($self, $filename, @includestack) = @_;

	my $provider = $self->{xml_provider};

    AxKit::Debug(10, "YPathScript: entering include_file");

    # return if already included
    my $key = $provider->key();
    return '' if grep $_ eq $filename, @{$stash->{$key}{includes}};

    push @{$stash->{$key}{includes}}, $filename;
    
    my $apache = $provider->apache_request;
    my $sub = $apache->lookup_uri( $filename );
    local $AxKit::Cfg = Apache::AxKit::ConfigReader->new( $sub );
    
    my $inc_provider = Apache::AxKit::Provider->new_style_provider( $sub );
    
    
    my $contents;
    eval 
	{ 
        my $fh = $inc_provider->get_fh();
        local $/;
        $contents = <$fh>;
    };
    if ($@) 
	{
        $contents = ${ $inc_provider->get_strref() };
    }
    
    my $r = AxKit::Apache->request();
    if (my $charset = $r->dir_config('YPathScriptCharset')) {
        
        AxKit::Debug(8, "XPS: got charset: $charset");
        
        my $map = Apache::AxKit::CharsetConv->new($charset, "utf-8") 
					or die "No such charset: $charset";

        $contents = $map->convert($contents);
    }
    
    $stash->{$key}{includes} = [];
    
    AxKit::Debug(10, "YPathScript: extracting from '$key' contents: $contents\n");
    

	return $self->XML::XPathScript::extract($contents, $filename, @includestack);
}

=item 	$doc = get_source_tree( $xml_provider  )

	Reads an XML document from the provider.
	Return the doc as a string.

	Not present in X::X

=cut

sub get_source_tree {
    my($provider) = shift;

    my $xml;

    AxKit::Debug(7, "YPathScript: reparsing file");

    eval {
        my $fh = $provider->get_fh();
        local $/;
        $xml = <$fh>;
        close($fh);
    };
   
	# didn't work? try get_strref
	$xml = $provider->get_strref() if $@;
    
    AxKit::Debug(7, "YPathScript: Returning source tree");
    return $xml;
}


=item $self->debug( $level, $message )

Prints $message if the requested debug level 
is equal or smaller than $level.

=cut

sub debug{ AxKit::Debug( @_ ) }

=item $self->die( $suicide_note )

=cut

sub die{ die @_ }

__END__

=item $compiled_package = $self->get_stylesheet( $stylesheet_provider )

=cut


sub get_stylesheet
{
	my ( $self, $provider ) = @_;
    
    my $mtime = $provider->mtime();
    my $style_key = $provider->key();
    my $package = $self->gen_package_name();
    
    $self->debug(6, "Checking stylesheet mtime: $mtime\n");

    if ( 0 and $stash->{$style_key}
		and exists($stash->{$style_key}{mtime})
		and !$provider->has_changed($stash->{$style_key}{mtime})
		and check_inc_mtime($stash->{$style_key}{mtime}, $provider, $stash->{$style_key}{includes})) {
        # cached... just exec.
        $self->debug(7, "Using stylesheet cache\n");
		$self->{compiledstylesheet} = $stash->{$style_key};
    }
    else {
        # recompile stylesheet.
        $self->debug(6, "Recompiling stylesheet: $style_key\n");
		eval { 
			my $fh = $provider->get_fh();
			local $/;
			$self->{stylesheet} = <$fh>;
		};
		if ($@) {
			$self->{stylesheet} = ${ $provider->get_strref() };
		}
        $stash->{$style_key} = $self->compile($package, $provider);
        $stash->{$style_key}{mtime} = $self->get_mtime( undef, $provider);
    }

	return $package;
}


=item check_inc_mtime( $mtime, $provider, \@includes )

	Check the modified time of included files

	Use Apache::Axkit::Provider

	Return 0 if recompile is required, 1 otherwise (?)

	Not in X::X.

=cut

sub check_inc_mtime {
    my ($mtime, $provider, $includes) = @_;
    
    my $apache = $provider->apache_request;
    
    for my $inc (@$includes) {
#        warn "Checking mtime for $inc\n";
        my $sub = $apache->lookup_uri($inc);
        local $AxKit::Cfg = Apache::AxKit::ConfigReader->new($sub);
        
        my $inc_provider = Apache::AxKit::Provider->new_style_provider($sub);
        
        if ($inc_provider->has_changed($mtime)) {
#            warn "$inc newer (" . $inc_provider->mtime() . ") than last compile ($mtime) causing recompile\n";
            return;
        }
    }
    return 1;
}


=item	$stylesheet = extract( $provider, $scalar_output )

Read the stylesheet from the $provider and
transform it into an executable perl script.
the stylesheet <!--#include-->s are dealt with here.

If $scalar_output is true, the compiled stylesheet's output will be piped
to $__OUTPUT.

Exist in X::X, but with some differences


=cut

sub extract {
    my ($self, $provider,$scalar_output) = @_;
    
    my $contents;
    eval { 
        my $fh = $provider->get_fh();
        local $/;
        $contents = <$fh>;
    };
    if ($@) {
        $contents = ${ $provider->get_strref() };
    }
    
    my $r = AxKit::Apache->request();
    if (my $charset = $r->dir_config('YPathScriptCharset')) {
        
        AxKit::Debug(8, "XPS: got charset: $charset");
        
        my $map = Apache::AxKit::CharsetConv->new($charset, "utf-8") || die "No such charset: $charset";
        $contents = $map->convert($contents);
    }
    
    my $key = $provider->key();
    $stash->{$key}{includes} = [];
    
    AxKit::Debug(10, "YPathScript: extracting from '$key' contents: $contents\n");
    
    my $script;
    
    my $line = 1;
    
    while ($contents =~ /\G(.*?)(<!--\#include|<%=?)/gcs) {
        my ($text, $type) = ($1, $2);
        $line += $text =~ tr/\n//;
        $text =~ s/\|/\\\|/g;
        if($scalar_output) {
            $script .= "\$__OUTPUT.=q|$text|;";
        } else {
            $script .= "print q|$text|;";
        }
        $script .= "\n#line $line $key\n";
        if ($type eq '<%=') {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line ($key)";
            my $perl = $1;
            if(!$scalar_output) {
                $script .= "print(do { $perl });\n";
            } else {
                $script .= "\$__OUTPUT.=join('',(do { $perl }));\n";
            }
            $line += $perl =~ tr/\n//;
        }
        elsif ($type eq '<!--#include') {
            my %params;
            while ($contents =~ /\G(\s+(\w+)\s*=\s*(["'])([^\3]*?)\3|\s*-->)/gcs) {
                last if $1 eq '-->';
                $params{$2} = $4;
            }
            
            if (!$params{file}) {
                die "No matching file attribute in #include at line $line ($key)";
            }
            
            AxKit::Debug(10, "About to include file $params{file}");
            $script .= include_file($params{file}, $provider, $scalar_output);
            AxKit::Debug(10, "include done");
        }
        else {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line ($key)";
            my $perl = $1;
            $perl =~ s/;?$/;/s; # add on ; if its missing. As in <% $foo = 'Hello' %>
            $script .= $perl;
            $line += $perl =~ tr/\n//;
        }
    }
    
    if ($contents =~ /\G(.*)/gcs) {
        my ($text) = ($1);
        $text =~ s/\|/\\\|/g;
        if ($scalar_output) {
            $script .= "\$__OUTPUT.=q|$text|;";
        } else {
            $script .= "print q|$text|;";
        }
    }
    
    return $script;
}

=item compile( $package, $provider )

Compile the XPS stylesheet given in $provider into the package
$package and wrap it in the function &handler of that package.

Pretty much the same as for X::X


sub compile {
    my ($self, $package, $provider) = @_;

    $self->debug( 5, 'Compiling package...' );

    my $script = $self->extract($provider);
    
    my $eval = join('',
            'package ',
            $package,
            '; use Apache qw(exit);',
            'use XML::XPath::Node;',
            'Apache::AxKit::Language::YPathScript::Toys->import;',
            'sub handler {',
            'my ($r, $xp, $t) = @_;',
            "\n#line 1 " . $provider->key() . "\n",
            $script,
            ";\n",
            'return Apache::Constants::OK;',
            "\n}",
            );

    local $^W;

    AxKit::Debug(10, "Compiling script:\n$eval\n");
    eval $eval;
    if ($@) {
        AxKit::Debug(1, "Compilation failed: $@");
        throw $@;
    }
}

=cut

=item	$file_content = include_file( $filename, $provider, $scalar_output )

Wrapper around extract. Verify if $filename hasn't already been
extracted before.

Exists in X::X in a simplified version.

=cut

sub include_file {
    my ($self, $filename, $provider, $script_output) = @_;

    # return if already included
    my $key = $provider->key();
    return '' if grep {$_ eq $filename} @{$stash->{$key}{includes}};

    push @{$stash->{$key}{includes}}, $filename;
    
    my $apache = $provider->apache_request;
    my $sub = $apache->lookup_uri($filename);
    local $AxKit::Cfg = Apache::AxKit::ConfigReader->new($sub);
    
    my $inc_provider = Apache::AxKit::Provider->new_style_provider($sub);
    
    return $self->extract($inc_provider, $script_output);
}

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

=item   $mtime =  get_mtime( $class, $provider )

Returns the mtime of $provider. 

$class is not used.

The global $stash is modified as a side-effect.

Does not exist in X::X.

=cut

sub get_mtime {

	return 0;
	my $self = shift;
    my $class = shift;
    my ($provider) = @_;

    my $mtime = $provider->mtime();
    my $filename = $provider->key();

    if (!$stash->{$filename}) {
        # compile stylesheet
        $self->compile( $self->get_package_name($filename), $provider);
    
        $stash->{$filename}{mtime} = $mtime;
        return 0;
    }

    my $apache = $provider->apache_request;
    
    for my $inc (@{$stash->{$filename}{includes}}) {
        
        my $sub = $apache->lookup_uri($inc);
        local $AxKit::Cfg = Apache::AxKit::ConfigReader->new($sub);
        
        my $inc_provider = Apache::AxKit::Provider->new_style_provider(
                $sub, 
                # uri => $inc,
                );
        
#        warn "Checking mtime of $inc\n";
        if ($inc_provider->has_changed($mtime)) {
            $mtime = $inc_provider->mtime();
        }
    }
    
    return $mtime;
}


1;
__END__

=head1 NAME

Apache::AxKit::Language::YPathScript - An XML Stylesheet Language

=head1 SYNOPSIS

  AxAddStyleMap "application/x-xpathscript => \
        Apache::AxKit::Language::YPathScript"

=head1 DESCRIPTION

This documentation has been removed. The definitive reference for 
YPathScript is now at http://axkit.org/docs/xpathscript/guide.dkb
in DocBook format.

=cut
