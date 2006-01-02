package Apache2::TomKit::Processor::XPathScript;

use strict;
use warnings;
use Carp;

use base qw( Apache2::TomKit::Processor::AbstractProcessor XML::XPathScript );

use Apache2::TomKit::Processor::DefinitionProvider::FileSystemProvider;

sub new {
    my $class  = shift;
    my $logger = shift;
    my $config = shift;
    my $processordef   = shift;
    my $getMappingType = shift;

    my $this = $class->SUPER::new($logger,$config);

    $this->{processordef}   = new Apache2::TomKit::Processor::DefinitionProvider::FileSystemProvider( $logger, $config, $processordef );
    $this->{stylesheet}     = undef;
    $this->{getMappingType} = $getMappingType;

    return $this;
}


sub init { }

sub setUp {
    my $this = shift;

    return if( $this->{stylesheet} );

	# FIXME
    if( $this->{processordef}->isFile() ) {
    	$this->{stylesheet} = $this->{processordef}->getInstructions();
    } else {
    	$this->{stylesheet} = $this->{processordef}->getInstructions();
    }
    
        
    open my $STYLESHEET, $this->{processordef}->getInstructions();
    $this->{stylesheet} = "";
    $this->{stylesheet} .= $_ while <$STYLESHEET>;
    
    $this->{logger}->debug( 0, "XPathScript: stylesheet is $this->{stylesheet}" );

}

sub process {
    my $this = shift;
    my $input = shift;

    $this->{logger}->debug(0,"XPathScript: Is processing the source with stylesheet: " . $this->{processordef} );

	# FIXME
	$this->{xml} = $input;
	$this->{logger}->debug( 0, "XPathScript: source is $input" );
	
	my $output;
	$this->{printer} = \$output;
	$this->{dom} = $this->{xml};

	{
		local *ORIGINAL_STDOUT;
		*ORIGINAL_STDOUT = *STDOUT;
   		local *STDOUT;

		# Perl 5.6.1 dislikes closed but tied descriptors (causes SEGVage)
   		*STDOUT = *ORIGINAL_STDOUT if $^V lt v5.7.0; 

	   	tie *STDOUT, 'XML::XPathScript::StdoutSnatcher';
	   	my $retval = $this->compile()->( $this );
	   	untie *STDOUT;
	}
	
	$this->{logger}->debug( 0, "XPathScript: output is $output" );
	
	use XML::LibXML;
	my $parser = XML::LibXML->new();
	return $parser->parse_string( $output );
	#return $output;

	
    # return $this->{stylesheet}->transform( $input );
}

sub getMTime {
    my $this = shift;
    return $this->{processordef}->getMTime();
}

sub createsXML {
    0;
}

#sub getKey {
#    return $_[0]->{processordef}->getMD5Key();
#}

sub createsDom {
    1;
}

sub getContentType {
    return "text/html";
}

sub getMappingType {
    return $_[0]->{getMappingType}
}

sub getProcessorDefinition {
    return $_[0]->{processordef}->getKey();
}


1;