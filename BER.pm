# Convert::BER.pm
#
# Copyright (c) 1995-8 Graham Barr <gbarr@pobox.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package Convert::BER;

use 5.004;

use vars qw($VERSION @ISA);
use Exporter ();
use strict;
use vars qw($VERSION @ISA @EXPORT_OK);

BEGIN {
    $VERSION = "1.15";

    @ISA = qw(Exporter);
    
    @EXPORT_OK = qw(
	BER_BOOLEAN
	BER_INTEGER
	BER_BIT_STR
	BER_OCTET_STR
	BER_NULL
	BER_OBJECT_ID
	BER_SEQUENCE
	BER_SET

	BER_UNIVERSAL
	BER_APPLICATION
	BER_CONTEXT
	BER_PRIVATE

	BER_PRIMITIVE
	BER_CONSTRUCTOR

	BER_LONG_LEN
	BER_EXTENSION_ID
	BER_BIT
    );
}

##
## Constants
##

sub BER_BOOLEAN 	() { 0x01 }
sub BER_INTEGER 	() { 0x02 }
sub BER_BIT_STR 	() { 0x03 }
sub BER_OCTET_STR 	() { 0x04 }
sub BER_NULL 		() { 0x05 }
sub BER_OBJECT_ID 	() { 0x06 }
sub BER_ENUMERATED	() { 0x0A }
sub BER_SEQUENCE 	() { 0x10 }
sub BER_SET 		() { 0x11 }

sub BER_UNIVERSAL 	() { 0x00 }
sub BER_APPLICATION 	() { 0x40 }
sub BER_CONTEXT 	() { 0x80 }
sub BER_PRIVATE		() { 0xC0 }

sub BER_PRIMITIVE	() { 0x00 }
sub BER_CONSTRUCTOR	() { 0x20 }

sub BER_LONG_LEN	() { 0x80 }
sub BER_EXTENSION_ID	() { 0x1F }
sub BER_BIT 		() { 0x80 }

# This module is used a lot so performance matters. For that reason it
# is implemented as an ARRAY instead of a HASH.
# inlined constants for array indices

sub _BUFFER () { 0 }
sub _POS    () { 1 }
sub _INDEX  () { 2 }
sub _ERROR  () { 3 }

INIT: {
  Convert::BER->define(
    ##
    ## Syntax operator
    ##

    [ BER          => undef, undef ],
    [ ANY          => undef, undef ],
    [ CONSTRUCTED  => undef, undef ],
    [ OPTIONAL     => undef, undef ],

    ##
    ## Primitive operators
    ##

    [ BOOLEAN     => undef, BER_UNIVERSAL | BER_PRIMITIVE   | BER_BOOLEAN   ],
    [ INTEGER     => undef, BER_UNIVERSAL | BER_PRIMITIVE   | BER_INTEGER   ],
    [ STRING      => undef, BER_UNIVERSAL | BER_PRIMITIVE   | BER_OCTET_STR ],
    [ NULL        => undef, BER_UNIVERSAL | BER_PRIMITIVE   | BER_NULL	    ],
    [ OBJECT_ID   => undef, BER_UNIVERSAL | BER_PRIMITIVE   | BER_OBJECT_ID ],

    [ SEQUENCE    => undef, BER_UNIVERSAL | BER_CONSTRUCTOR | BER_SEQUENCE  ],
    [ SEQUENCE_OF => undef, BER_UNIVERSAL | BER_CONSTRUCTOR | BER_SEQUENCE  ],
  );

  ##
  ## $INTEGER and $SEQUENCE will be defined by the above ->define() call
  ##

  use vars qw($INTEGER $SEQUENCE);

  Convert::BER->define(
    ##
    ## Sub-classed primitive operators
    ##

    [ ENUM => $INTEGER,  BER_UNIVERSAL | BER_PRIMITIVE   | BER_ENUMERATED ],
    [ SET  => $SEQUENCE, BER_UNIVERSAL | BER_CONSTRUCTOR | BER_SET        ],
  );
}

# only load Carp when needed

sub croak {
    require Carp;
    goto &Carp::croak;
}

##
## define:
##	does all the hard work of dynamically building the BER class
##	and BER-type classes
##

sub define {
    my $pkg = shift;

    no strict 'refs'; # we do some naughty stuff here :-)

    $pkg = ref($pkg) || $pkg;

    while(@_) {
	my($name,$isa,$tag) = @{ $_[0] }; shift;
	my $subpkg = $pkg . "::" . $name;

	croak("Redefinition of tag '$name'")
		if($pkg->can('_' . $name));

	croak("Bad tag name '$name'")
		if($name =~ /\A(?:DESTROY|VERSION)\Z/);

	if(defined $isa) {
	    my $isapkg = $pkg->can('_' . $isa) or
		croak "Unknown BER tag type '$isa'";

	    @{$subpkg . "::ISA"} = ( $isapkg->() )
		unless @{$subpkg . "::ISA"};

	    $tag = $subpkg->tag
		unless defined $tag;
	}

	if(defined &{$subpkg . "::tag"}) {
	    croak "tags for '$name' do not match "
		unless $subpkg->tag == $tag;
	}
	else {
	    *{$subpkg . "::tag"} = sub { $tag };
	}
	push(@{$pkg . "::EXPORT_OK"}, '$' . $name);

	*{$pkg . "::_" . $name} = sub { $subpkg };
	*{$pkg . "::"  . $name} = \$name;
    }
}

use strict;

sub new {
    my $package = shift;
    my $class = ref($package) || $package;

    my $self = bless [
	@_ == 1 ? shift : "",
	0,
	ref($package) ? $package->[ Convert::BER::_INDEX() ] : [],
    ], $class;

    @_ ? $self->encode(@_) : $self;
}

##
## Some basic subs for packing/unpacking data
## These methods would be called by the BER-type classes
##

sub num_length {

    return 1 if ( ($_[0] &     0xff) == $_[0]);
    return 2 if ( ($_[0] &   0xffff) == $_[0]);
    return 3 if ( ($_[0] & 0xffffff) == $_[0]);
    return 4;
}

sub pos {
    my $ber = shift;
    @_ ? ($ber->[ Convert::BER::_POS() ] = shift) : $ber->[ Convert::BER::_POS() ];
}

sub pack {
    my $ber = shift;
    $ber->[ Convert::BER::_BUFFER() ] .= $_[0];
    1;
}

sub unpack {
    my($ber,$len) = @_;
    my $pos = $ber->[ Convert::BER::_POS() ];

    die "Buffer empty"
	if(($pos + $len) > length($ber->[ Convert::BER::_BUFFER() ]));

    $ber->[ Convert::BER::_POS() ] += $len;

    substr($ber->[ Convert::BER::_BUFFER() ],$pos,$len);
}

sub pack_tag {
    my($ber,$tag) = @_;

    # small tag number are more common, so check $tag size in reverse order
    unless($tag & ~0xff) {
	$ber->[ 0 ] .= chr( $tag );
	return 1;
    }

    unless($tag & ~0xffff) {
	$ber->[ 0 ] .= pack("n",$tag);
	return 2;
    }

    unless($tag & ~0xffffff) {
	$ber->[ 0 ] .= pack("nc",($tag >> 8),$tag);
	return 3;
    }

    $ber->[ 0 ] .= pack("N",$tag);
    return 4;
}

sub unpack_tag {
    my($ber,$expect) = @_;
    my $pos = $ber->[ Convert::BER::_POS() ];
    my $len = length($ber->[ Convert::BER::_BUFFER() ]);

    die "Buffer empty"
	if($pos >= $len);

    my $tag = unpack("C",substr($ber->[ Convert::BER::_BUFFER() ],$pos++,1));

    if(($tag & 0x1f) == 0x1f) {
	my $b;

	do {
	    die "Buffer empty"
		if($pos >= $len);
	    $b = unpack("C",substr($ber->[ Convert::BER::_BUFFER() ],$pos++,1));
	    $tag = ($tag << 7) | ($b & 0x7f);
	} while($b & 0x80);
    }

    die sprintf("Expecting tag 0x%x, found 0x%x",$expect,$tag)
	if(defined($expect) && ($tag != $expect));

    $ber->[ Convert::BER::_POS() ] = $pos;

    $tag
}

sub pack_length {
    my($ber,$len) = @_;

    if($len & ~0x7f) {
	my $lenlen = num_length($len);

	$ber->[ Convert::BER::_BUFFER() ] .= pack("C", $lenlen | 0x80) . substr(pack("N",$len), 0 - $lenlen);

	return $lenlen + 1;
    }

    $ber->[ Convert::BER::_BUFFER() ] .= pack("C", $len);
    return 1;
}

sub unpack_length {
    my $ber = shift;
    my $pos = $ber->[ Convert::BER::_POS() ];

    die "Buffer empty"
	if($pos >= length($ber->[ Convert::BER::_BUFFER() ]));

    my $len = unpack("C", substr($ber->[ Convert::BER::_BUFFER() ],$pos++,1));

    if($len & 0x80) {
	my $buf;

	$len &= 0x7f;

	die "Buffer empty"
	    if(($pos+$len) > length($ber->[ Convert::BER::_BUFFER() ]));

	my $tmp = "\0" x (4 - $len) . substr($ber->[ Convert::BER::_BUFFER() ],$pos,$len);

	$pos += $len;

	$len = unpack("N",$tmp);
    }

    $ber->[ Convert::BER::_POS() ] = $pos;

    $len;
}

##
## User interface (public) method
##

sub error {
    my $ber = shift;
    $ber->[ Convert::BER::_ERROR() ];
}


sub tag {
    my $ber = shift;
    my $pos = $ber->[ Convert::BER::_POS() ];
    my $tag = eval { $ber->unpack_tag() } or return undef;
    $ber->[ Convert::BER::_POS() ] = $pos;
    $tag;
}

sub length {
    my $ber = shift;

    length($ber->[ Convert::BER::_BUFFER() ]);
}

sub buffer {
    my $ber = shift;
    if(@_) {
	$ber->[ Convert::BER::_POS() ] = 0;
	$ber->[ Convert::BER::_BUFFER() ] = "" . shift;
    }
    $ber->[ Convert::BER::_BUFFER() ];
}

##
## just for debug :-)
##

sub _hexdump {
    my($buf,$lead) = @_;

    my @x = unpack("C*", $buf);

    print "\n";

    while(@x) {
	my @y = splice(@x,0,16);
	print $lead;
	$lead = " " x length($lead);
	printf "%02X " x @y, @y;
	printf "   " x (16 - @y)
	    if @y != 16;
	print " ";
	foreach (@y) {
	    my $ch = chr($_);
	    $ch = "." if($_ < 32 || $_ >+ 127);
	    print $ch;
	}
	print "\n";
    }
}

sub dump {
    my $ber = shift;
    my $pos = 0;
    my $indent = "";
    my @seqend = ();
#    $ber->[ Convert::BER::_POS() ] = 0;

    while($ber->[ Convert::BER::_POS() ] < length($ber->[ Convert::BER::_BUFFER() ])) {
        while(@seqend && $ber->[ Convert::BER::_POS() ] >= $seqend[0]) {
	    $indent = substr($indent,2);
	    shift @seqend;
	}

	my $tag = $ber->unpack_tag();
	printf "%sTag   : 0x%X",$indent, $tag;

	if(($tag & 0xC0) == BER_APPLICATION) {
	    print " APPLICATION";
	}
	elsif(($tag & 0xC0) == BER_CONTEXT) {
	    print " CONTEXT";
	}
	elsif(($tag & 0xC0) == BER_PRIVATE) {
	    print " PRIVATE";
	}
	else {
	    print " UNIVERSAL";
	}

	if(($tag & 0x20) == BER_CONSTRUCTOR) {
	    print " CONSTRUCTOR";
	}
	else {
	    print " PRIMITIVE";
	}

	printf " 0x%X\n", $tag & ~0xE0;
	my $len = $ber->unpack_length();
	printf "%sLength: 0x%X\n",$indent, $len;

	if($tag & BER_CONSTRUCTOR) {
	    unshift(@seqend, $ber->[ Convert::BER::_POS() ] + $len);
	    $indent .= "  ";
	    next;
	}

	_hexdump($ber->unpack($len),$indent . "Data  : ");
    } continue { 
	print "-" x 70,"\n";
    }
}

sub hexdump {
    my $ber = shift;
    _hexdump($ber->[ Convert::BER::_BUFFER() ],"");
    print "\n";
}

##
## And now the real guts of it, the encoding and decoding routines
##

sub encode {
    my $ber = shift;

    $ber->[ Convert::BER::_INDEX() ] = [];

    return $ber
	if eval { $ber->_encode(\@_) };

    $ber->[ Convert::BER::_ERROR() ] = $@;

    undef;
}

sub _encode {
    my $ber = shift;
    my $desc = shift;
    my $i = 0;

    while($i < @$desc ) {
	my $type = $desc->[$i++];
	my $arg  = $desc->[$i++];
	my $tag  = undef;

	($type,$tag) = @$type
	    if(ref($type) eq 'ARRAY');

	my $can = $ber->can('_' . $type);

	die "Unknown element '$type'"
	    unless $can;

	my $pkg = &$can();

	$tag = $pkg->tag
	    unless defined $tag;

	$arg = $arg->(@{$ber->[ Convert::BER::_INDEX() ]})
	    if(ref($arg) eq 'CODE');

	if(ref($arg) eq 'ARRAY') {
	    if($can = $pkg->can('pack_array')) {
		$ber->pack_tag($tag)
		    if defined $tag;

		$pkg->pack_array($ber,$arg);
	    }
	    else {
		my $a;
		foreach $a (@$arg) {
		    $ber->pack_tag($tag)
			if defined $tag;

		    $pkg->pack($ber,$a);
		}
	    }
	}
	else {
	    $ber->pack_tag($tag)
		if defined $tag;

	    $pkg->pack($ber,$arg);
	}
    }

    1;
}

sub decode {
    my $ber = shift;
    my $pos = $ber->[ Convert::BER::_POS() ];

    $ber->[ Convert::BER::_INDEX() ] = [];

    return $ber
	if eval { $ber->_decode(\@_) };

    $ber->[ Convert::BER::_ERROR() ] = $@;
    $ber->[ Convert::BER::_POS() ]   = $pos;

    undef;
}

sub _decode {
    my $ber = shift;
    my $desc = shift;
    my $i = 0;

    my $argc;

TAG:
    for($argc = @$desc ; $argc > 0 ; $argc -= 2) {
	my $type = $desc->[$i++];
	my $arg  = $desc->[$i++];
	my $tag  = undef;

	($type,$tag) = @$type
	    if(ref($type) eq 'ARRAY');

	my $can = $ber->can('_' . $type);

	die "Unknown element '$type'"
	    unless $can;

	my $pkg = &$can();

	$tag = $pkg->tag
	    unless defined $tag;

	$arg = $arg->(@{$ber->[ Convert::BER::_INDEX() ]})
	    if(ref($arg) eq 'CODE');

	if(ref($arg) eq 'ARRAY') {
	    if($pkg->can('unpack_array')) {

		$ber->unpack_tag($tag)
		    if(defined $tag);

		$pkg->unpack_array($ber,$arg);
	    }
	    else {
		@$arg = ();
		while(length($ber->[ Convert::BER::_BUFFER() ]) > $ber->[ Convert::BER::_POS() ]) {
		    if(defined $tag) {
			next TAG
			    unless $ber->unpack_tag($tag);
		    }

		    push @$arg, undef;
		    $pkg->unpack($ber,\$arg->[-1]);
		}
	    }
	}
	else {
	    eval {
		$ber->unpack_tag($tag)
		    if(defined $tag);

		$pkg->unpack($ber,$arg);
		1;
	    } or ($$arg = undef, die);
	}
    }

   1;
}

##
## a couple of routines to interface to a file descriptor.
##

sub read {
    my $ber = shift;
    my $io  = shift;

    $ber = $ber->new unless ref($ber);
    $ber->[ Convert::BER::_BUFFER() ] = "";
    $ber->[ Convert::BER::_POS() ] = 0;

    $io->sysread($ber->[ Convert::BER::_BUFFER() ],1) or
	return eval { die "I/O Error $!" };

    my $ch = unpack("C",$ber->[ Convert::BER::_BUFFER() ]);

    if(($ch & 0x1f) == 0x1f) {
	do {
	    $io->sysread($ber->[ Convert::BER::_BUFFER() ],1,length $ber->[ Convert::BER::_BUFFER() ]) or
		return eval { die "I/O Error $!" };

	    $ch = unpack("C",substr($ber->[ Convert::BER::_BUFFER() ],-1));

	} while($ch & 0x80);
    }

    $io->sysread($ber->[ Convert::BER::_BUFFER() ],1,length $ber->[ Convert::BER::_BUFFER() ]) or
	return eval { die "I/O Error $!" };

    $ch = unpack("C",substr($ber->[ Convert::BER::_BUFFER() ],-1));

    if($ch & 0x80) {
	$io->sysread($ber->[ Convert::BER::_BUFFER() ],$ch & 0x7f,length $ber->[ Convert::BER::_BUFFER() ]) or
	    return eval { die "I/O Error" };
    }

    my $pos = 0;

    $ber->[ Convert::BER::_POS() ] = 0;
    $ber->unpack_tag();

    my $len = $ber->unpack_length();
    my $got;
    $ber->[ Convert::BER::_POS() ] = 0;

    while($len) {
	return eval { die "I/O Error $!" }
	    unless( $got = $io->sysread($ber->[ Convert::BER::_BUFFER() ],$len,length $ber->[ Convert::BER::_BUFFER() ]) );

	$len -= $got;
    }

    $ber;
}

sub write {
    my $ber = shift;
    my $io = shift;

    my $togo = length($ber->[ Convert::BER::_BUFFER() ]);
    my $pos = 0;

    while($togo) {
	my $len = $io->syswrite($ber->[ Convert::BER::_BUFFER() ],$togo,$pos) or
	    return eval { die "I/O Error" };
	$togo -= $len;
	$pos += $len;
    }

    1;
}

sub send {
    my $ber = shift;
    my $sock = shift;
    $sock->send($ber->[ Convert::BER::_BUFFER() ],0,@_);
}

sub recv {
    my $ber = shift;
    my $sock = shift;

    require Socket; # for Socket::MSG_PEEK

    $ber = $ber->new unless ref($ber);
    $ber->[ Convert::BER::_BUFFER() ] = "";
    $ber->[ Convert::BER::_POS() ] = 0;

    # We do not know the size of the datagram, so we have to PEEK --GMB
    my $n = 0;
    do {
	$n += 1024;
	$sock->recv($ber->[ Convert::BER::_BUFFER() ],$n,Socket::MSG_PEEK());
    } while($n == length($ber->[ Convert::BER::_BUFFER() ]));

    # now we know the size, get it again but without MSG_PEEK
    # this will cause the kernel to remove the datagram from it's queue

    $sock->recv($ber->[ Convert::BER::_BUFFER() ],$n);

    $ber;
}

##
## The primitive packages
##

package Convert::BER::BER;

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->[ Convert::BER::_BUFFER() ] .= $arg->[ Convert::BER::_BUFFER() ];

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    my $len = length($ber->[ Convert::BER::_BUFFER() ]) - $ber->[ Convert::BER::_POS() ];

    $$arg = $ber->new($ber->unpack($len));

    1;
}

package Convert::BER::ANY;

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->[ Convert::BER::_BUFFER() ] .= $arg->[ Convert::BER::_BUFFER() ];

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    my $pos = $ber->[ Convert::BER::_POS() ];
    my $tag = $ber->unpack_tag();
    my $len = $ber->unpack_length() + $ber->[ Convert::BER::_POS() ] - $pos;
    $ber->[ Convert::BER::_POS() ] = $pos;

    $$arg = $ber->new($ber->unpack($len));

    1;
}

##
##
##

package Convert::BER::BOOLEAN;

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->pack_length(1);
    $ber->[ Convert::BER::_BUFFER() ] .= pack("c", $arg ? 0xff : 0x00);

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    my $len = $ber->unpack_length();

    $$arg = CORE::unpack("c", $ber->unpack($len)) ? 1 : 0;

    1;
}

##
##
##

package Convert::BER::INTEGER;

##
## Math::BigInt support
##

sub pack_bigint {
    my($self,$ber,$arg) = @_;

    require Math::BigInt;

    my $neg = ($arg < 0) ? 1 : 0;
    my @octet = ();
    my $num = new Math::BigInt(abs($arg));

    $num -= 1 if $neg;
    while($num > 0) {
	my($i,$y) = $num->bdiv(256);
	$num = new Math::BigInt($i);
	$y = $y ^ 0xff if $neg;
	unshift(@octet,$y);
    }
    @octet = (0) unless @octet;

    my $msb = ($octet[0] & 0x80) ? 1 : 0;

    unshift(@octet,$neg ? 0xff : 0x00)
	if($neg != $msb);

    $ber->pack_length(scalar @octet);

    $ber->[ Convert::BER::_BUFFER() ] .= pack("C*",@octet);

    1;
}

sub unpack_bigint {
    my($self,$ber,$arg) = @_;

    require Math::BigInt;

    my $len = $ber->unpack_length();
    my @octet = unpack("C*",$ber->unpack($len));
    my $neg = ($octet[0] & 0x80) ? 1 : 0;
    my $val = $$arg = 0;

    while(@octet) {
	my $oct = shift @octet;
	$oct = $oct ^ 0xff
	    if $neg;
	$val *= (1<<8);
	$val += $oct;
    }

    $val = -1 - $val
	if $neg;

    1;
}

##
## Math::BigInteger support
##

sub pack_biginteger {
    my($self,$ber,$arg) = @_;

    my($len,$data);
    my $offset = 0;

    require Math::BigInteger;
    # save has no concept of +/-
    my $v = $arg->cmp(new Math::BigInteger(0));

    if($v) {
	if($v < 0) {
	    my $b = $arg->bits + 8;
	    $b -= $b % 8;
	    my $tmp = new Math::BigInteger(1);
	    $tmp->lshift(new Math::BigInteger(1), $b);
	    $arg = $tmp + $arg;
	}

	$data = $arg->save;
	$len = length($data);

	my $c = ord(substr($data,0,1));

	if($c == 0) {
	    for( ; $len > 1 ; $len--, $offset++) {
		my $ch = ord(substr($data,$offset,1));
		if($ch & 0xff) {
		    if($ch & 0x80) {
			$len++;
			$offset--;
		    }
		    last;
		}
	    }
	}
	elsif($c == 0xff) {
	    for( ; $len > 1 ; $len--, $offset++) {
		my $ch = ord(substr($data,$offset,1));
		unless($ch == 0xff) {
		    unless($ch & 0x80) {
			$len++;
			$offset--;
		    }
		    last;
		}
	    }
	}
    }
    else {
	$len = 1;
	$data = pack("C",0);
    }

    $ber->pack_length($len);
    $ber->[ Convert::BER::_BUFFER() ] .= substr($data,$offset);

    return 1;
}

sub unpack_biginteger {
    my($self,$ber,$arg) = @_;

    require Math::BigInteger;

    my $len = $ber->unpack_length();
    my $data = $ber->unpack($len);
    my $int = restore Math::BigInteger $data;

    # restore has no concept of +/-
    if(ord(substr($data,0,1)) & 0x80) {
	my $tmp = new Math::BigInteger;
	$tmp->lshift(new Math::BigInteger(1), $len * 8);
	$tmp = new Math::BigInteger(0) - $tmp;
	$int = $tmp + $int;
    }
    $$arg = $int;

    return 1;
}

##
##
##

sub pack {
    my($self,$ber,$arg) = @_;

    if(ref $arg) {
	goto &pack_bigint
	    if UNIVERSAL::isa($arg,'Math::BigInt');

	goto &pack_biginteger
	    if UNIVERSAL::isa($arg,'Math::BigInteger');
    }

    my $neg = ($arg < 0) ? 1 : 0;

    my $len = Convert::BER::num_length($neg ? ~ $arg : $arg);

    my $msb = $arg & (0x80 << (($len - 1) * 8));

    $len++
	if(($msb && not($neg)) || ($neg && not($msb)));

    $ber->pack_length($len);
    $ber->[ Convert::BER::_BUFFER() ] .= substr(pack("N",$arg), 0 - $len);

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    if( ref($arg) && ref($$arg) ) {
	goto &unpack_bigint
	    if UNIVERSAL::isa($$arg,'Math::BigInt');

	goto unpack_biginteger
	    if UNIVERSAL::isa($$arg,'Math::BigInteger');
    }

    my $len = $ber->unpack_length();
    my $tmp = "\0" x (4 - $len) . $ber->unpack($len);
    my $val = CORE::unpack("N",$tmp);

    $val -=  0x1 << ($len * 8)
	if($val & (0x1 << (($len * 8) - 1)));

    $$arg = $val;

    1;
}

##
##
##

package Convert::BER::NULL;

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->pack_length(0);
}

sub unpack {
    my($self,$ber,$arg) = @_;

    $ber->unpack_length();

    $$arg = 1;
}

##
##
##

package Convert::BER::STRING;

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->pack_length(length($arg));
    $ber->[ Convert::BER::_BUFFER() ] .= $arg;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    my $len = $ber->unpack_length();
    $$arg = $ber->unpack($len);

    1;
}

##
##
##

package Convert::BER::SEQUENCE;

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->pack_length(length($arg->[ Convert::BER::_BUFFER() ]));
    $ber->[ Convert::BER::_BUFFER() ] .= $arg->[ Convert::BER::_BUFFER() ];

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    my $len = $ber->unpack_length();
    $$arg = $ber->new($ber->unpack($len));

    1;
}

sub pack_array {
    my($self,$ber,$arg) = @_;

    my $ber2 = $ber->new;

    return undef
	unless defined($ber2->_encode($arg));

    $ber->pack_length(length($ber2->[ Convert::BER::_BUFFER() ]));
    $ber->[ Convert::BER::_BUFFER() ] .= $ber2->[ Convert::BER::_BUFFER() ];

    1;
}

sub unpack_array {
    my($self,$ber,$arg) = @_;

    my $ber2;

    $self->unpack($ber,\$ber2);

    $ber2->_decode($arg);

    die "Sequence buffer not empty"
	if length($ber2->[ Convert::BER::_BUFFER() ]) != $ber2->[ Convert::BER::_POS() ];

    1;
}

##
##
##

package Convert::BER::OBJECT_ID;

sub pack {
    my($self,$ber,$arg) = @_;
    my @data = ($arg =~ /(\d+)/g);

    if(@data < 2) {
	@data = (0);
    }
    else {
	my $first = $data[1] + ($data[0] * 40);
	splice(@data,0,2,$first);
    }

    @data = map {
	my @d = ($_);
	if($_ >= 0x80) {
	    @d = ();
	    my $v = 0 | $_; # unsigned
	    while($v) {
		unshift(@d, 0x80 | ($v & 0x7f));
		$v >>= 7;
	    }
	    $d[-1] &= 0x7f;
	}
	@d;
    } @data;

    my $data = pack("C*", @data);

    $ber->pack_length(length($data));
    $ber->[ Convert::BER::_BUFFER() ] .=  $data;

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;

    my $len = $ber->unpack_length();
    my @ch = unpack("C*",$ber->unpack($len));
    my @data = ();
    my $val = 0;
    while(@ch) {
	my $ch = shift @ch;
	$val = ($val << 7) | ($ch & 0x7f);
	unless($ch & 0x80) {
	    push @data, $val;
	    $val = 0;
	}
    }
    if(@data) {
	my $first = shift @data;
	unshift @data, $first % 40;
	unshift @data, int($first / 40);
	unshift @data, "";
    }
    $$arg = join(".",@data);
    1;
}

##
##
##

package Convert::BER::CONSTRUCTED;

BEGIN {
    # Cannot call import here as Convert::BER has not been initialized
    *BER_CONSTRUCTOR = *Convert::BER::BER_CONSTRUCTOR
}

sub pack {
    my($self,$ber,$arg) = @_;

    $ber->pack_tag($arg->tag | BER_CONSTRUCTOR);
    $ber->pack_length(length($arg->[ Convert::BER::_BUFFER() ]));
    $ber->[ Convert::BER::_BUFFER() ] .= $arg->[ Convert::BER::_BUFFER() ];

    1;
}

sub unpack {
    my($self,$ber,$arg) = @_;
    my $tag = $ber->unpack_tag();

    die "Not constructed"
	unless $tag & BER_CONSTRUCTOR;

    my $len = $ber->unpack_length();
    my $buf = $ber->new( $ber->unpack($len));

    die $ber->(0,"Bad construction")
	unless( ($buf->tag | BER_CONSTRUCTOR) == $tag);

    $$arg = $buf;

    1;
}

sub pack_array {
    my($self,$ber,$arg) = @_;

    $self->_encode($arg);
}

sub unpack_array {
    my($self,$ber,$arg) = @_;

    my $ber2;

    $self->unpack($ber,\$ber2);

    $ber2->_decode($arg);
}

##
##
##

package Convert::BER::OPTIONAL;

sub pack_array {
    my($self,$ber,$arg) = @_;
    $ber->_encode($arg);
}

sub unpack_array {
    my($self,$ber,$arg) = @_;
    my $pos = $ber->[ Convert::BER::_POS() ];

    eval { $ber->_decode($arg) } or
	$ber->[ Convert::BER::_POS() ] = $pos;

    1;
}

##
##
##

package Convert::BER::SEQUENCE_OF;

sub pack_array {
    my($self,$ber,$arg) = @_;
    my($n,@desc) = @$arg;
    my $i;

    $n = $n->(@{$ber->[ Convert::BER::_INDEX() ]})
	if ref($n) eq 'CODE';

    push(@{$ber->[ Convert::BER::_INDEX() ]},0);

    my $b = $ber->new;

    if(ref($n) eq 'HASH') {
	my $v;
	foreach $v (keys %$n) {
	    $ber->[ Convert::BER::_INDEX() ][-1] = $v;
	    $b->_encode(\@desc);
	}
    }
    elsif(ref($n) eq 'ARRAY') {
	my $v;
	foreach $v (@$n) {
	    $ber->[ Convert::BER::_INDEX() ][-1] = $v;
	    $b->_encode(\@desc);
	}
    }
    else {
	while($n--) {
	    $b->_encode(\@desc);
	    $ber->[ Convert::BER::_INDEX() ][-1] += 1;
	}
    }

    pop @{$ber->[ Convert::BER::_INDEX() ]};

    $ber->pack_length(length($b->[ Convert::BER::_BUFFER() ]));
    $ber->[ Convert::BER::_BUFFER() ] .= $b->[ Convert::BER::_BUFFER() ];

    1;
}

sub unpack_array {
    my($self,$ber,$arg) = @_;
    my($nref,@desc) = @$arg;

    push(@{$ber->[ Convert::BER::_INDEX() ]},0);

    my $len = $ber->unpack_length();
    my $b   = $ber->new($ber->unpack($len));
    my $pos = $ber->[ Convert::BER::_POS() ];
    my $n;

    while(length($b->[ Convert::BER::_BUFFER() ]) > $b->[ Convert::BER::_POS() ]) {
	$b->_decode(\@desc);
	$ber->[ Convert::BER::_INDEX() ][-1] += 1;
    }

    $$nref = pop @{$ber->[ Convert::BER::_INDEX() ]};
    1;
}

1;
