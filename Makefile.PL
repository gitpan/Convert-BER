# This -*- perl -*- script makes the Makefile
# $Id: Makefile.PL,v 1.3 1997/03/19 20:20:15 gbarr Exp $

use 5.004;
use ExtUtils::MakeMaker;

#--- MY package

sub MY::libscan
{
 my($self,$path) = @_;

 return '' 
	if($path =~ m:/(RCS|CVS|SCCS)/: ||
	   $path =~ m:[~%]$: ||
	   $path =~ m:\.(orig|rej|nfs[A-F0-9]+|b[ac]k)$:
	  );
 
 $path;
}

#--- Write the Makefile

my @clean = qw(*% *.html *.b[ac]k *.old);

push @clean, map { "*/$_" } @clean;

WriteMakefile(
    VERSION_FROM  => "lib/Convert/BER.pm",
    NAME          => 'Convert::BER',
    'dist'        => {
	              COMPRESS     => 'gzip -9f',
	              SUFFIX	   => '.gz',
	              DIST_DEFAULT => 'all tardist',
	             },

    'clean'       => {FILES => join(" ", @clean)},
);

