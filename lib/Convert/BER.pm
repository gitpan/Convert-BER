# Convert::BER.pm
#
# Copyright (c) 1995 Graham Barr <gbarr@ti.com>. All rights reserved.
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

package Convert::BER;

use 5.004;

use Carp;
use vars qw($VERSION @ISA);
use Error;
use IO::Handle;
use IO::Select;
use Exporter;
use strict;
use vars qw($VERSION @ISA @EXPORT_OK %EXPORT_TAGS @EXPORT %EXPORT);

BEGIN {
    $VERSION = "1.08";

    @ISA = qw(Exporter Error::Source);

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

INIT: {
  Convert::BER->define(
    ##
    ## Syntax operator
    ##

    [ BER          => undef, undef ],
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
		if($pkg->can($name));

	croak("Bad tag name '$name'")
		if($name =~ /\A(?:DESTROY|VERSION)\Z/);

	if(defined $isa) {
	    my $isapkg = $pkg->can($isa) or
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

	*{$pkg . "::"       . $name} = sub { $subpkg };
	*{$pkg . "::"       . $name} = \$name;
    }
}

use strict;

sub new {
    my $package = shift;
    my $class = ref($package) || $package;

    my $buf = @_ == 1 ? shift : "";

    my $self = bless \$buf, $class;

    $self = $self->encode(@_)
	if @_;

    $self;
}

##
## Some basic subs for packing/unpacking data
## These methods would be called by the BER-type classes
##

sub num_length {
    my $tag = shift;

    return 4 if $tag & 0xff000000;
    return 3 if $tag & 0x00ff0000;
    return 2 if $tag & 0x0000ff00;
    return 1;
}

sub pack {
    my $ber = shift;
    $$ber .= shift;
    1;
}

sub unpack {
    my($ber,$posref,$len) = @_;

    my $p = $$posref;
    $$posref += $len;

    substr($$ber,$p,$len);
}

sub pack_tag {
    my($ber,$tag) = @_;

    $$ber .= chr( ($tag >> 24) & 0xff )
	if($tag > 0xffffff);

    $$ber .= chr( ($tag >> 16) & 0xff )
	if($tag > 0xffff);

    $$ber .= chr( ($tag >>  8) & 0xff )
	if($tag > 0xff);

    $$ber .= chr( $tag  & 0xff  );
    1;
}

sub unpack_tag {
    my($ber,$posref, $expect) = @_;
    my $pos = $$posref;
    my $tag = unpack("C",substr($$ber,$pos++,1));

    if(($tag & 0x1f) == 0x1f) {
	my $b;

	do {
	    $b = unpack("C",substr($$ber,$pos++,1));
	    $tag = ($tag << 7) | ($b & 0x7f);
	} while($b & 0x80);
    }

    if(defined($expect) && ($tag != $expect)) {
	undef $tag;
    }
    else {
	$$posref = $pos;
    }

    $tag;
}

sub pack_length {
    my($ber,$len) = @_;
    my $lenlen = 1;

    if($len < 0x80) {
	$$ber .= pack("C", $len);
    }
    else {
	$lenlen = num_length($len) | 0x80;

	$$ber .= pack("C", $lenlen);
	$$ber .= substr(pack("N",$len), 0 - $lenlen);

	$lenlen++;
    }

    $lenlen;
}

sub unpack_length {
    my($ber,$posref) = @_;

    my $len = unpack("C", substr($$ber,$$posref++,1));

    if($len & 0x80) {
	$len &= 0x7f;
	my $tmp = "\0" x (4 - $len) . $ber->unpack($posref,$len);
	$len = unpack("N",$tmp);
    }

    $len;
}

##
## User interface (public) method
##

sub tag {
    my $pos = 0;
    shift->unpack_tag(\$pos);
}

sub clear {
    my $ber = shift;

    $$ber = "";
}

sub length {
    my $ber = shift;

    length($$ber);
}

sub buffer {
    my $ber = shift;

    $$ber;
}

##
## just for debug :-)
##

sub _hexdump {
    my($ber,$pos,$len,$lead) = @_;

    my @x = unpack("C*", substr($$ber,$pos,$len));

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

    while($pos < length($$ber)) {
        while(@seqend && $pos >= $seqend[0]) {
	    $indent = substr($indent,2);
	    shift @seqend;
	}
	my $tag = $ber->unpack_tag(\$pos);
	printf "%sTag   : 0x%X\n",$indent, $tag;
	my $len = $ber->unpack_length(\$pos);
	printf "%sLength: 0x%X\n",$indent, $len;

	if($tag & BER_CONSTRUCTOR) {
	    unshift(@seqend,$pos+$len);
	    $indent .= "  ";
	    next;
	}

	_hexdump($ber,$pos,$len,$indent . "Data  : ");
	$pos += $len;
    } continue { 
	print "-" x 70,"\n";
    }
}

sub hexdump {
    my $ber = shift;
    _hexdump($ber,0,length($$ber),"");
    print "\n";
}

##
## And now the real guts of it, the encoding and decoding routines
##

sub encode { shift->_encode([],\@_); }

sub _encode {
    my $ber = shift;
    my $index = shift || [];
    my $desc = shift;
    my $i = 0;

    while($i < @$desc ) {
	my $type = $desc->[$i++];
	my $arg  = $desc->[$i++];
	my $tag  = undef;

	($type,$tag) = @$type
	    if(ref($type) eq 'ARRAY');

	my $can = $ber->can($type);

	return $ber->Error(1,"unknown element '$type'")
	    unless $can;

	my $pkg = &$can();

	$tag = $pkg->tag
	    unless defined $tag;

	$arg = $arg->(@$index)
	    if(ref($arg) eq 'CODE');

	if(ref($arg) eq 'ARRAY' && !UNIVERSAL::can($arg,'can')) {
	    if($can = $pkg->can('pack_array')) {
		$ber->pack_tag($tag)
		    if defined $tag;
		$pkg->pack_array($ber,$arg,$index);
	    }
	    else {
		my $a;
		foreach $a (@$arg) {
		    $ber->pack_tag($tag)
			if defined $tag;
		    $pkg->pack($ber,$a,$index);
		}
	    }
	}
	else {
	    $ber->pack_tag($tag)
		if defined $tag;
	    $pkg->pack($ber,$arg,$index);
	}
    }

    $ber;
}

sub decode {
    my $ber = shift;
    my $pos = $ber->_decode(0,[],\@_);

    substr($$ber,0,$pos) = ""
	if $pos;

   $pos;
}

sub _decode {
    my $ber = shift;
    my $pos = shift;
    my $index = shift;
    my $desc = shift;
    my $i = 0;

    my $all = 0;
    if(@$desc % 2) {
	return $ber->Error(1,"Odd number of arguments'")
	    if(defined $desc->[-1]);
	$all = 1;
    }

    my $argc = @$desc;
    $argc >>= 1;

TAG:
    while($argc--) {
	my $type = $desc->[$i++];
	my $arg  = $desc->[$i++];
	my $tag  = undef;

	($type,$tag) = @$type
	    if(ref($type) eq 'ARRAY');

	my $can = $ber->can($type);

	return $ber->Error(1,"unknown element '$type'")
	    unless $can;

	my $pkg = &$can();

	$tag = $pkg->tag
	    unless defined $tag;

	$arg = $arg->(@$index)
	    if(ref($arg) eq 'CODE');

	if(ref($arg) eq 'ARRAY') {
	    if($pkg->can('unpack_array')) {

		if(defined $tag) {
		    return undef
			unless $ber->unpack_tag(\$pos,$tag);
		}

		$pkg->unpack_array($ber,\$pos,$arg,$index);
	    }
	    else {
		@$arg = ();
		while(1) {
		    if(defined $tag) {
			next TAG
			    unless $ber->unpack_tag(\$pos,$tag);
		    }

		    push @$arg, "";
		    $pkg->unpack($ber,\$pos,\$arg->[-1],$index);
		}
	    }
	}
	else {
	    $$arg = undef;
	    if(defined $tag) {
		return undef
		    unless $ber->unpack_tag(\$pos,$tag);
	    }
	    $pkg->unpack($ber,\$pos,$arg,$index);
	}
    }

    return $ber->Error(0,"Buffer not empty")
	if $all && $pos < $ber->length;

   $pos;
}

##
## a couple of routines to interface to a file descriptor.
##

sub read {
    my $ber = shift;
    my $io  = shift;

    my $sel = IO::Select->new($io);

    return $ber->Error(3,"Timeout")
	unless $sel->can_read();
    $$ber = "";
    $io->sysread($$ber,1) or
	return $ber->Error(4,"I/O Error $!");

    my $ch = unpack("C",$$ber);

    if(($ch & 0x1f) == 0x1f) {
	do {
	    return $ber->Error(3,"Timeout")
		unless $sel->can_read();

	    $io->sysread($$ber,1,length $$ber) or
		return $ber->Error(4,"I/O Error $!");

	    $ch = unpack("C",substr($$ber,-1));

	} while($ch & 0x80);
    }

    return $ber->Error(3,"Timeout")
	unless $sel->can_read();

    $io->sysread($$ber,1,length $$ber) or
	return $ber->Error(4,"I/O Error $!");
    $ch = unpack("C",substr($$ber,-1));

    if($ch & 0x80) {
	return $ber->Error(3,"Timeout")
	    unless $sel->can_read();
	$io->sysread($$ber,$ch & 0x7f,length $$ber) or
	    return $ber->Error(4,"I/O Error");
    }

    my $pos = 0;

    $ber->unpack_tag(\$pos);

    my $len = $ber->unpack_length(\$pos);
    my $got;

    while($len) {
	return $ber->Error(3,"Timeout")
	    unless $sel->can_read;
	return $ber->Error(4,"I/O Error $!")
	    unless( $got = $io->sysread($$ber,$len,length $$ber) );
	$len -= $got;
    }

    $ber;
}

sub write {
    my $ber = shift;
    my $io = shift;

    my $togo = $ber->length;
    my $pos = 0;

    while($togo) {
	my $len = $io->syswrite($$ber,$togo,$pos) or
	    return $ber->Error(4,"I/O Error");
	$togo -= $len;
	$pos += $len;
    }

    1;
}

##
## The primitive packages
##

package Convert::BER::BER;

sub pack {
    my($self,$ber,$arg,$index) = @_;

    $ber->pack($arg->buffer);

    1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;
    $$arg = $ber->new($ber->unpack($posref,$ber->length - $$posref));
    1;
}

##
##
##

package Convert::BER::BOOLEAN;

sub pack {
    my($self,$ber,$arg,$index) = @_;

    $ber->pack_length(1);
    $ber->pack( pack("c", $arg ? 0xff : 0x00) );

    1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;

    my $len = $ber->unpack_length($posref);
    $$arg = CORE::unpack("c", $ber->unpack($posref,$len)) ? 1 : 0;

    1;
}

##
##
##

package Convert::BER::INTEGER;

sub pack_bigint {
    my($self,$ber,$arg,$index) = @_;

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

    my $len = @octet;

    $ber->pack_length($len);
    $ber->pack( pack("C*",@octet));

   1;
}

sub unpack_bigint {
    my($self,$ber,$posref,$arg,$index) = @_;

    require Math::BigInt;

    my $len = $ber->unpack_length($posref);
    my @octet = unpack("C*",$ber->unpack($posref,$len));
    my $val = new Math::BitInt(0);
    my $neg = ($octet[0] & 0x80) ? 1 : 0;

    while(@octet) {
	my $oct = shift @octet;
	$oct = $oct ^ 0xff
	    if $neg;
	$val *= (1<<8);
	$val += $oct;
    }

    $val = -1 - $val
	if $neg;

    $$arg = $val;

    1;
}

sub pack {
    my($self,$ber,$arg,$index) = @_;

    goto &pack_bigint
	if ref($arg) && UNIVERSAL::isa($arg,'Math::BigInt');

    my $neg = ($arg < 0) ? 1 : 0;

    my $len = Convert::BER::num_length($neg ? ~ $arg : $arg);

    my $msb = $arg & (0x80 << (($len - 1) * 8));

    $len++
	if(($msb && not($neg)) || ($neg && not($msb)));

    $ber->pack_length($len);
    $ber->pack( substr(pack("N",$arg), 0 - $len) );

   1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;

    goto &unpack_bigint
	if ref($arg) && UNIVERSAL::isa($arg,'Math::BigInt');

    my $len = $ber->unpack_length($posref);
    my $tmp = "\0" x (4 - $len) . $ber->unpack($posref,$len);
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
    my($self,$ber,$arg,$index) = @_;

    $ber->pack_length(0);
    1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;

    $ber->unpack_length($posref);
    $$arg = 1;

    1;
}

##
##
##

package Convert::BER::STRING;

sub pack {
    my($self,$ber,$arg,$index) = @_;

    $ber->pack_length(length($arg));
    $ber->pack( $arg );

    1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;

    my $len = $ber->unpack_length($posref);
    $$arg = $ber->unpack($posref,$len);

    1;
}

##
##
##

package Convert::BER::SEQUENCE;

sub pack {
    my($self,$ber,$arg,$index) = @_;

    $ber->pack_length($arg->length);
    $ber->pack( $arg->buffer );

    1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;

    my $len = $ber->unpack_length($posref);
    $$arg = $ber->new($ber->unpack($posref,$len));

    1;
}

sub pack_array {
    my($self,$ber,$arg,$index) = @_;

    my $ber2 = $ber->new;

    $ber2->_encode($index,$arg);

    $self->pack($ber,$ber2);
}

sub unpack_array {
    my($self,$ber,$posref,$arg,$index) = @_;

    my $ber2;

    $self->unpack($ber,$posref,\$ber2);

    return $ber2->_decode(0,$index,$arg);
}

##
##
##

package Convert::BER::OBJECT_ID;

sub pack {
    my($self,$ber,$arg,$index) = @_;
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
    $ber->pack( $data );

    1;
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;

    my $len = $ber->unpack_length($posref);
    my @ch = unpack("C*",$ber->unpack($posref,$len));
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
    my($self,$ber,$arg,$index) = @_;

    $ber->pack_tag($arg->tag | BER_CONSTRUCTOR);
    $ber->pack_length($arg->length);
    $ber->pack( $arg->buffer);
}

sub unpack {
    my($self,$ber,$posref,$arg,$index) = @_;
    my $pos = $$posref;
    my $tag = $ber->unpack_tag(\$pos);

    return undef
	unless $tag & BER_CONSTRUCTOR;

    my $len = $ber->unpack_length(\$pos);
    my $buf = $ber->new( $ber->unpack(\$pos, $len));

    return undef
	unless( ($buf->tag | BER_CONSTRUCTOR) == $tag);

    $$arg = $buf;
    $$posref = $pos;

    1;
}

sub pack_array {
    my($self,$ber,$arg,$index) = @_;

    my $b = $ber->new;
    $b->_encode($index,$arg) or
	return undef;

    $self->pack($ber,$b);

    1;
}

sub unpack_array {
    my($self,$ber,$posref,$arg,$index) = @_;

    my $ber2;
    $self->unpack($ber,$posref,\$ber2) or
	return undef;

    $ber2->_decode(0,$index,$arg);
}

##
##
##

package Convert::BER::OPTIONAL;

sub pack_array {
    my($self,$ber,$arg,$index) = @_;
    $ber->_encode($index,$arg);
}

sub unpack_array {
    my($self,$ber,$posref,$arg,$index) = @_;
    my $len;

    $$posref = $len
	if($len = $ber->_decode($$posref, $index, $arg));

    1;
}

##
##
##

package Convert::BER::SEQUENCE_OF;

sub pack_array {
    my($self,$ber,$arg,$index) = @_;
    my($n,@desc) = @$arg;
    my $i;
    push(@$index,0);

    my $b = $ber->new;
    if(ref($n) eq 'HASH') {
	my $v;
	foreach $v (keys %$n) {
	    $index->[-1] = $v;
	    $b->_encode($index,\@desc);
	}
    }
    elsif(ref($n) eq 'ARRAY') {
	my $v;
	foreach $v (@$n) {
	    $b->_encode($index,\@desc);
	}
    }
    else {
	while($n--) {
	    $b->_encode($index,\@desc);
	    $index->[-1]++;
	}
    }

    pop @$index;

    $ber->pack_length($b->length);
    $ber->pack( $b->buffer );

}

sub unpack_array {
    my($self,$ber,$posref,$arg,$index) = @_;
    my($nref,@desc) = @$arg;

    push(@$index,0);
    $$nref = 0;

    my $len = $ber->unpack_length($posref);
    my $b = $ber->new($ber->unpack($posref,$len));
    my $pos = 0;
    my $n;

    while($n = $b->_decode($pos,$index,\@desc)) {
	$$nref++;
	$pos = $n;
	$index->[-1]++;
    }

    pop @$index;
    1;
}

1;

__END__
