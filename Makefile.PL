# This -*- perl -*- script makes the Makefile
# $Id: //depot/ber/Makefile.PL#4 $

use 5.004;
use ExtUtils::MakeMaker;

my @ppd;

if ($] >= 5.00503) {
  @ppd = (
    AUTHOR    => 'Graham Barr <gbarr@pobox.com>',
    ABSTRACT  => 'Error/exception handling in an OO-ish way'
  );
}

WriteMakefile(
  VERSION_FROM  => 'BER.pm',
  NAME          => 'Convert::BER',
  @ppd
);

sub MY::postamble {

  return '' unless $] >= 5.00503;

<<'ESQ';

dist : ppd

ESQ
}


