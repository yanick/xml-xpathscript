use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
require 5.005;

WriteMakefile(
    'NAME'	=> 'XML::XPathScript',
    'VERSION_FROM' => 'XPathScript.pm', # finds $VERSION
    'PREREQ_PM' => { XML::XPath => 1.0 },
    'EXE_FILES' => ['scripts/xpathscript'],
);
