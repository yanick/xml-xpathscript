use ExtUtils::MakeMaker;

my $XML_parser;
if( join( ' ', @ARGV ) =~ /XML_PARSER=(\S*)/ )
{
	$XML_parser = $1;
	unless( grep $XML_parser eq $_, qw/ XML::XPath XML::libXML / )
	{
		die "XML parser $XML_parser not supported\nplease choose XML::XPath or XML::libXML\n";
	}
	
	print "XML parser configured to be $XML_parser\n";
	
}
else
{	
	print "detecting XML parser...\n";
	
	
	print "checking if XML::LibXML is present.. ";
	eval "use XML::LibXML";
	unless( $@ )
	{ 
		print "XML::LibXML detected, good!\nsetting XML parser to XML::LibXML";
		$XM_parser = 'libxml';
	}
	else
	{
		print "XML::LibXML not detected, drat\n";
		print "checking if XML::XPath is present.. ";
		eval "use XML::XPath";
		unless( $@ )
		{
			print "XML::XPath detected\nsetting XML parser to XML::XPath\n";
			$XML_parser = 'xpath';
		}
		else
		{
			print "XML::XPath not found\n";
			die "XML::YPathScript needs either XML::LibXML or XML::XPath to be installed\n";
		}
	}
}

print $XML_parser;

open YPS, "YPathScript.pm" or die "can't open YPathScript.pm: $!\n";
my $module;
while( <YPS> )
{
	s/^(\$XML_parser = )'.*?';$/$1='$XML_parser';/;
	$module .= $_;
}
open YPS, ">YPathScript.pm" or die "can't overwrite YPathScript.pm: $!\n";
print YPS $module;
close YPS;


WriteMakefile(
    'NAME'		=> 'XML::YPathScript',
    'VERSION_FROM'	=> 'YPathScript.pm', 
	'EXE_FILES' => [ qw( script/yps ) ],
    ( $] >= 5.005 ?    
      (ABSTRACT_FROM => 'YPathScript.pm', 
       AUTHOR     => 'Yanick Champoux <yanick@babyl.dyndns.org>') : ()),
);
