# This -*- perl -*- script makes the Makefile
# $Id: //depot/ber/Makefile.PL#5 $

use 5.004;
use ExtUtils::MakeMaker;

my @ppd;

if ($] >= 5.00503) {
  @ppd = (
    AUTHOR    => 'Graham Barr <gbarr@pobox.com>',
    ABSTRACT  => 'Encode/decoing of ASN.1 usinf BER rules'
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


