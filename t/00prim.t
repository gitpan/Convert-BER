#!/usr/local/bin/perl

#
# Test that the primitive operators are working
#

use Convert::BER;

print "1..42\n";

$test = 1;


##
## NULL (tests 1 - 3)
##

$ber = Convert::BER->new->encode( NULL => 0 );

    print "# NULL\n";

if($ber) {
    print "ok ",$test++,"\n";

    my $result = pack("C*", 0x05, 0x00);

    print "not "
	unless $ber->buffer eq $result;
    print "ok ",$test++,"\n";

    my $null = undef;

    print "not "
	unless $ber->decode(NULL => \$null) && $null;

    print "ok ",$test++,"\n";
}

print "not ok",$test++,"\n"
	while($test <= 3);

##
## BOOLEAN (tests 4 - 12)
##

foreach $val (0,1,-99) {
    print "# BOOLEAN $val\n";
    $ber = Convert::BER->new->encode( BOOLEAN => $val);

    if($ber) {
	print "ok ",$test++,"\n";

	my $result = pack("C*", 0x01, 0x01, $val ? 0xFF : 0);

	print "not "
	    unless $ber->buffer eq $result;

	print "ok ",$test++,"\n";

	my $bool = undef;

	print "not "
	    unless $ber->decode( BOOLEAN => \$bool)
	    && defined($bool)
	    && (!$bool == !$val);

	print "ok ",$test++,"\n";
    }
}

print "not ok ",$test++,"\n"
	while($test <= 12);

##
## INTEGER (tests 13 - 21)
##

my %INTEGER = (
    0		=> pack("C*", 0x02, 0x01, 0x00),
    0x667799	=> pack("C*", 0x02, 0x03, 0x66, 0x77, 0x99),
    -457	=> pack("C*", 0x02, 0x02, 0xFE, 0x37),
);

while(($val,$result) = each %INTEGER) {
    print "# INTEGER $val\n";
    $ber = Convert::BER->new->encode( INTEGER => $val);

    if($ber) {
	print "ok ",$test++,"\n";

	print "not "
	    unless $ber->buffer eq $result;

	print "ok ",$test++,"\n";

	my $int = undef;

	print "not "
	    unless $ber->decode( INTEGER => \$int)
	    && defined($int)
	    && ($int == $val);

	print "ok ",$test++,"\n";
    }
}

print "not ok ",$test++,"\n"
	while($test <= 21);

##
## STRING ( tests 22 - 27)
##

my %STRING = (
    ""		=> pack("C*",   0x04, 0x00),
    "A string"	=> pack("CCa*", 0x04, 0x08, "A string"),
);

while(($val,$result) = each %STRING) {
    print "# STRING '$val'\n";
    $ber = Convert::BER->new->encode( STRING => $val);

    if($ber) {
	print "ok ",$test++,"\n";

	print "not "
	    unless $ber->buffer eq $result;

	print "ok ",$test++,"\n";

	my $str = undef;

	print "not "
	    unless $ber->decode( STRING => \$str)
	    && defined($str)
	    && ($str eq $val);

	print "ok ",$test++,"\n";
    }
}

print "not ok ",$test++,"\n"
	while($test <= 27);

##
## OBJECT_ID (tests 28 - 33)
##

my %OBJECT_ID = (
    ".1.2.3.4.5" => pack("C*", 0x06, 0x04, 0x2A, 0x03, 0x04, 0x05),
    ".2.5.457"   => pack("C*", 0x06, 0x03, 0x55, 0x83, 0x49),
);


while(($val,$result) = each %OBJECT_ID) {
    print "# OBJECT_ID $val\n";
    $ber = Convert::BER->new->encode( OBJECT_ID => $val);

    if($ber) {
	print "ok ",$test++,"\n";

	print "not "
	    unless $ber->buffer eq $result;

	print "ok ",$test++,"\n";

	my $oid = undef;

	print "not "
	    unless $ber->decode( OBJECT_ID => \$oid)
	    && defined($oid)
	    && ($oid eq $val);

	print "ok ",$test++,"\n";
    }
}

print "not ok ",$test++,"\n"
	while($test <= 33);

##
## ENUM (tests 34 - 42)
##

my %ENUM = (
    0		=> pack("C*", 0x0A, 0x01, 0x00),
    -99		=> pack("C*", 0x0A, 0x01, 0x9D),
    6573456	=> pack("C*", 0x0A, 0x03, 0x64, 0x4D, 0x90),
);

while(($val,$result) = each %ENUM) {
    print "# ENUM $val\n";
    $ber = Convert::BER->new->encode( ENUM => $val);

    if($ber) {
	print "ok ",$test++,"\n";

	print "not "
	    unless $ber->buffer eq $result;

	print "ok ",$test++,"\n";

	my $enum = undef;

	print "not "
	    unless $ber->decode( ENUM => \$enum)
	    && defined($enum)
	    && ($enum == $val);

	print "ok ",$test++,"\n";
    }
}

print "not ok ",$test++,"\n"
	while($test <= 42);

