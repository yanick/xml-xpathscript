package XML::XPathScript;

use strict;
use vars qw($VERSION);

use XML::XPath 1.0;
use XML::XPath::XMLParser;
use XML::XPath::Node;
use XML::XPath::NodeSet;
use XML::Parser;
use Symbol;
use File::Basename;

$VERSION = '0.03';

sub new {
    my $class = shift;
    die "Invalid hash call to new" if @_ % 2;
    my %params = @_;
    my $self = \%params;
    bless $self, $class;
}

sub process {
    my $self = shift;
    
    my ($refxml, $refstylesheet);
    if (ref($self->{xml})) {
        $refxml++;
    }
    
    if (ref($self->{stylesheet})) {
        $refstylesheet++;
    }
    
    my $xpath = $refxml ? 
            XML::XPath->new(ioref => $self->{xml})
            :
            XML::XPath->new(xml => $self->{xml});
    
    my $package = gen_package_name($self->{package_name});
    
    if (!exists $XML::XPathScript::CompiledPackages{$package}) {
        # needs compiling
        compile($package, $self->{stylesheet});
        $XML::XPathScript::CompiledPackages{$package}++;
    }
    
    no strict 'refs';
    my $cv = \&{"$package\::handler"};

    $XML::XPathScript::xp = $xpath;
    my $t = {};
    $XML::XPathScript::trans = $t;
    
    local $^W;
    $cv->($xpath, $t);
        
#    if (!$r->notes('xml_string')) { # no output? Try apply_templates
#        print Apache::AxKit::Language::XPathScript::Toys::apply_templates();
#    }
    
#    warn "Run\n";

    $XML::XPathScript::xp = undef;
    $XML::XPathScript::trans = undef;
}

sub gen_package_name {
    my $package_prefix = shift || 'dingleberry';
    # Escape everything into valid perl identifiers
    $package_prefix =~ s/([^A-Za-z0-9_\/])/sprintf("_%2x",unpack("C",$1))/eg;

    # second pass cares for slashes and words starting with a digit
    $package_prefix =~ s{
                  (/+)       # directory
                  (\d?)      # package's first character
                 }[
                   "::" . (length $2 ? sprintf("_%2x",unpack("C",$2)) : "")
                  ]egx;

    return "XML::XPathScript::ROOT$package_prefix";
}

sub extract {
    my ($stylesheet) = @_;
    
    my $contents;
    if (ref($stylesheet)) {
        local $/;
        $contents = <$stylesheet>;
    }
    else {
        $contents = $stylesheet;
    }

    my $script;
    my $line = 1;
    
    while ($contents =~ /\G(.*?)(<!--#include|<%=?)/gcs) {
        my ($text, $type) = ($1, $2);
        $line += $text =~ tr/\n//; # count \n's in text
        $text =~ s/\|/\\\|/g;
        $script .= "print q|$text|;";
        $script .= "\n#line $line stylesheet\n";
        if ($type eq '<%=') {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
            $script .= "print( $perl );\n";
            $line += $perl =~ tr/\n//;
        }
        elsif ($type eq '<!--#include') {
            my %params;
            while ($contents =~ /\G(\s+(\w+)\s*=\s*(["'])([^\3]*?)\3|\s*-->)/gcs) {
                last if $1 eq '-->';
                $params{$2} = $4;
            }
            
            if (!$params{file}) {
                die "No matching file attribute in #include at line $line";
            }
            
            $script .= include_file($params{file});
        }
        else {
            $contents =~ /\G(.*?)%>/gcs || die "No terminating '%>' after line $line";
            my $perl = $1;
            $perl =~ s/;?$/;/s; # add on ; if its missing. As in <% $foo = 'Hello' %>
            $script .= $perl;
            $line += $perl =~ tr/\n//;
        }
    }
    
    if ($contents =~ /\G(.*)/gcs) {
        my ($text) = ($1);
        $text =~ s/\|/\\\|/g;
        $script .= "print q|$text|;";
    }
    
    return $script;
}

sub include_file {
    my ($filename) = @_;
    
    # should maybe check for circular includes here...
#warn "INCLUDE: $filename\n";
    
    chdir dirname($filename);
    
    my $sym = gensym;
    open($sym, basename($filename)) || die "Can't read include file '$filename': $!";
    return extract($sym);
}

sub compile {
    my ($package, $stylesheet) = @_;
    
    my $script = extract($stylesheet);
    
    my $eval = join('',
            'package ',
            $package,
            ';use XML::XPath::Node;',
            'XML::XPathScript::Toys->import;',
            'sub handler {',
            'my ($xp, $t) = @_;',
            "\n#line 1 stylesheet\n",
            $script,
            "\n}",
            );
    
    local $^W;
#warn "Compiling: $eval\n";    
    eval $eval;
    die $@ if $@;
}

sub set_xml {
    my $self = shift;
    $self->{xml} = shift;
}

sub set_stylesheet {
    my $self = shift;
    $self->{stylesheet} = shift;
}

sub set_package_name {
    my $self = shift;
    $self->{package_name} = shift;
}

sub XML::XPath::Function::document {
    my $self = shift;
    my ($node, @params) = @_;
    die "document: Function takes 1 parameter\n" unless @params == 1;

    my $parser = XML::XPath::XMLParser->new();

    my $results = XML::XPath::NodeSet->new();
    my $newdoc;
    my $sym = gensym;
    my $file = $params[0];
    open($sym, $file) || die "Cannot open document() file '$file': $!";
    $newdoc = $parser->parse( ioref => $sym );
    $results->push($newdoc) if $newdoc;
    return $results;
}

{
    package XML::XPathScript::Toys;
    
    use XML::XPath::Node;

    use vars '@ISA', '@EXPORT';
    use Exporter;
    @ISA = ('Exporter');
    @EXPORT = qw(
            findnodes 
            findvalue
            findvalues
            findnodes_as_string
            apply_templates
            matches
            set_namespace
            );

    sub findnodes {
        $XML::XPathScript::xp->findnodes(@_);
    }

    sub findvalue {
        $XML::XPathScript::xp->findvalue(@_);
    }
    
    sub findvalues {
        my @nodes = findnodes(@_);
        map { findvalue('.', $_) } @nodes;
    }

    sub findnodes_as_string {
        $XML::XPathScript::xp->findnodes_as_string(@_);
    }
    
    sub matches {
        $XML::XPathScript::xp->matches(@_);
    }
    
    sub set_namespace {
        eval {
            $XML::XPathScript::xp->set_namespace(@_);
        };
        if ($@) {
            warn "set_namespace failed: $@";
        }
    }
    
    sub apply_templates (;$@) {
        unless (@_) {
            return apply_templates(findnodes('/'));
        }
        
        my ($arg1, @args) = @_;

        if (!ref($arg1)) {
            # called with a path to find
#            warn "apply_templates with path '$arg1'\n";
            return apply_templates(findnodes($arg1, @args));
        }
        
        my $retval = '';
        if ($arg1->isa('XML::XPath::NodeSet')) {
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
        foreach my $node (@nodes) {
            $retval .= translate_node($node);
        }
        
        return $retval;
    }
    
    sub translate_node {
        my $node = shift;
        
        local $^W;
                
        my $translations = $XML::XPathScript::trans;
        
        if (!$node->isElementNode) {
            # don't output top-level PI's
            if ($node->isPINode) {
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
        
#        warn "translate_node: ", $node->getName, "\n";
        
        my $node_name = $node->getName;
        my $trans = $translations->{$node_name};

        if (!$trans) {
            $node_name = '*';
            $trans = $translations->{$node_name};
        }
        
        if (!$trans) {
            return start_tag($node) . 
                    _apply_templates($node->getChildNodes) .
                    end_tag($node);
        }
        
        local $^W;
        
        my $dokids = 1;
        my $search;

        my $t = {};
        if ($trans->{testcode}) {
#            warn "Evalling testcode\n";
            my $result = $trans->{testcode}->($node, $t);
            if ($result eq "0") {
                # don't process anything.
                return;
            }
            if ($result eq "-1") {
                # -1 means don't do children.
                $dokids = 0;
            }
            elsif ($result eq "1") {
                # do kids
            }
            else {
                $dokids = 0;
                $search = $result;
            }
        }
        
        local $translations->{$node_name};
        # copy old values in
        %{$translations->{$node_name}} = %$trans;
        
        if (%$t) {
            foreach my $key (keys %$t) {
                $translations->{$node_name}{$key} = $t->{$key};
            }
            $trans = $translations->{$node_name};
        }
        
        # default: process children too.
        my $pre = interpolate($node, $trans->{pre}) . 
                ($trans->{showtag} ? start_tag($node) : '') .
                interpolate($node, $trans->{prechildren});
        
        my $post = interpolate($node, $trans->{postchildren}) .
                ($trans->{showtag} ? end_tag($node) : '') .
                interpolate($node, $trans->{post});
        
        if ($dokids) {
            my $middle = '';
            for my $kid ($node->getChildNodes()) {
                if ($kid->isElementNode) {
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
        elsif ($search) {
            my $middle = '';
            for my $kid (findnodes($search, $node)) {
                if ($kid->isElementNode) {
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
        else {
            return $pre . $post;
        }
    }
    
    sub start_tag {
        my ($node) = @_;
        
        my $name = $node->getName;
        return '' unless $name;
        
        my $string = "<" . $name;
        
        foreach my $ns ($node->getNamespaceNodes) {
            $string .= $ns->toString;
        }
        
        foreach my $attr ($node->getAttributeNodes) {
            $string .= $attr->toString;
        }

        $string .= ">";
        
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
        return $string if $XPathScript::DoNotInterpolate;
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
}

1;
__END__

=head1 NAME

XML::XPathScript - Stand alone XPathScript

=head1 SYNOPSIS

  use XML::XPathScript;
  my $xps = XML::XPathScript->new(xml => $xml, stylesheet => $stylesheet);
  $xps->process;

=head1 DESCRIPTION

This is a standalone version of XPathScript, originally part of the AxKit
project at http://axkit.org/

XPathScript is a stylesheet language similar in many ways to XSLT (in
concept, not in appearance), for transforming XML from one format to
another format (possibly HTML).

The documentation for the XPathScript language is available on the
AxKit web site at the URL above.

=head1 USAGE

TODO

=head1 BUGS

Currently output is to STDOUT. I'd like to fix this, though my time on
this module is limited as I do not personally have a use for this module,
since I mainly just use AxKit itself. Patches welcome.

=head1 AUTHOR

Matt Sergeant, matt@sergeant.org

=head1 LICENSE

This is free software. You may distribute it under the same terms as
Perl itself.

=cut
