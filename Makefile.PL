use ExtUtils::MakeMaker;
use Getopt::Long;

use strict;
use warnings;

my $XML_parser;
GetOptions( 'parser=s' => \$XML_parser );

if( $XML_parser )
{
	unless( grep $XML_parser eq $_, qw/ XML::XPath XML::LibXML / )
	{
		die "XML parser $XML_parser not supported\nplease choose XML::XPath or XML::LibXML\n";
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
		print "XML::LibXML detected, good!\nsetting XML parser to XML::LibXML\n";
		$XML_parser = 'XML::LibXML';
	}
	else
	{
		print "XML::LibXML not detected, drat\n";
		print "checking if XML::XPath is present.. ";
		eval "use XML::XPath";
		unless( $@ )
		{
			print "XML::XPath detected\nsetting XML parser to XML::XPath\n";
			$XML_parser = 'XML::XPath';
		}
		else
		{
			print "XML::XPath not found\n";
			die "XML::XPathScript needs either XML::LibXML or XML::XPath to be installed\n";
		}
	}
}

open YPS, "XPathScript.pm" or die "can't open XPathScript.pm: $!\n";
my $module;
while( <YPS> )
{
	$module .= $_;
}
open YPS, ">XPathScript.pm" or die "can't overwrite XPathScript.pm: $!\n";
print YPS $module;
close YPS;

my @pmlibdirs = ( 'lib', '$(BASEEXT)' );

eval "use AxKit";
unless( $@ ) {
	print "AxKit detected, adding Apache::Axkit::Language::YPathScript to installation\n";
	push @pmlibdirs, 'lib_axkit';
}


eval "use Apache2::TomKit";
unless( $@ ) {
	print "TomKit detected, adding Apache2::TomKit::Processor::XPathScript to installation\n";
	push @pmlibdirs, 'lib_tomkit';
}

WriteMakefile(
    NAME		=> 'XML::XPathScript',
    AUTHOR     => 'Yanick Champoux <yanick@babyl.dyndns.org>, Dominique Quatravaux <dom@idealx.com> and Matt Sergeant <matt@sergeant.org>',
    VERSION_FROM	=> 'XPathScript.pm',
	EXE_FILES => [ qw( script/xpathscript ) ],
	PMLIBDIRS => \@pmlibdirs,
    PREREQ_PM => {
            "Carp" => 0,
            "Symbol" => 0,
	        "File::Basename" => 0,
            "IO::File" => 0,
	        "Data::Dumper" => 0,
            "Scalar::Util" => 0,
                 },
     ABSTRACT_FROM => 'XPathScript.pm',
   # Quoting fest!
    PM_FILTER => "$^X dist/rewrite-default-xml-parser $XML_parser",
);

