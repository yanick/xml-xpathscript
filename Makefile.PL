use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
    'NAME'		=> 'XML::YPathScript',
    'VERSION_FROM'	=> 'YPathScript.pm', # finds $VERSION
	'EXE_FILES' => [ qw( script/yps ) ],
    'PREREQ_PM'		=> {}, # e.g., Module::Name => 1.1
    ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
      (ABSTRACT_FROM => 'YPathScript.pm', # retrieve abstract from module
       AUTHOR     => 'Yanick Champoux <yanick@babyl.dyndns.org>') : ()),
);
