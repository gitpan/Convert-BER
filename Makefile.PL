# This -*- perl -*- script makes the Makefile
# $Id: //depot/ber/Makefile.PL#6 $

use 5.004;
use ExtUtils::MakeMaker;


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



