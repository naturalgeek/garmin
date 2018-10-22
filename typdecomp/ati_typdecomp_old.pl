#!/usr/bin/perl
#
# Copyright (c) 2007, misch (http://ati.land.cz/)
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of the <organization> nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY misch (http://ati.land.cz/) ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# Author: misch (http://ati.land.cz/)
# Major version: 1.20
# Minor version: 3 (2008-01-02)
#
# Usage: 
# 	./typ_decompiler input.typ

use strict;
use utf8;	# script is written in utf-8
use Fcntl qw(:seek);

our $VERSION = 1.20;

# Decompile($file_name);
sub Decompile($) {
	my ($fname) = @_;

	my $t = '';

	my $buf;
	open (F, '<', $fname);
	binmode(F);

	# 0x0:
	sysseek(F, 0, SEEK_SET);
	sysread(F, $buf, 0x0c);
	die "Not a Garmin TYP file: >".unpack('H*', $buf)."<" if unpack('H*', $buf) !~ /^(6e|5b)004741524d494e20545950$/; # '['.chr(0).'GARMIN_TYP'

	# 0x0c:
	sysseek(F, 0x0c, SEEK_SET);
	sysread(F, $buf, 0x0b);
	# 01 00 YYYY MM-1 DD hh mi ss E4 04
	my (undef, undef, $yyyy, $mm, $dd, $hh, $mi, $ss, undef) = unpack("CCSCCCCCS", $buf);
	$t .= sprintf("; Original TYP was created %d.%d.%04d at %02d:%02d:%02d\n", $dd, $mm+1, (($yyyy <= 200) ? (1900+$yyyy) : $yyyy), $hh, $mi, $ss);
	$t .= "\n";

	# Rest of header
	my ($fid, $pid);
	sysseek(F, 0x17, SEEK_SET);
	sysread(F, $buf, 4+4+4+4+4+4+2+2+4+2+4+4+2+4+4+2+4+4+2+4);
	my (@header) = unpack("LLLLLLSSLSLLSLLSLLSL", $buf);
	my %header = (
		point => {
			dataofs => $header[0],
			datalen => $header[1],
			arrayofs => $header[8],
			arraymod => $header[9],
			arraysize => $header[10],
		},
		line => {
			dataofs => $header[2],
			datalen => $header[3],
			arrayofs => $header[11],
			arraymod => $header[12],
			arraysize => $header[13],
		},
		polygon => {
			dataofs => $header[4],
			datalen => $header[5],
			arrayofs => $header[14],
			arraymod => $header[15],
			arraysize => $header[16]
		}
	);
	my ($DRAWORDER_POS, $DRAWORDER_MODULO, $DRAWORDER_LENGTH) = @header[17 .. 20];
	($fid, $pid) = ($header[6], $header[7]);
	$t .= "[_id]\n";
	$t .= sprintf "ProductCode=0x%x\n", $pid;
	$t .= sprintf "FID=0x%x\n", $fid;
	$t .= "[end]\n";
	$t .= "\n";

	{
		die "DLMOD!=5" if $DRAWORDER_MODULO != 5;
		die "DL%5!=0" if ($DRAWORDER_LENGTH % 5) != 0;
		
		sysseek(F, $DRAWORDER_POS, SEEK_SET);
		sysread(F, $buf, $DRAWORDER_LENGTH)==$DRAWORDER_LENGTH || die;
		my $_prvku = $DRAWORDER_LENGTH/5;
		my $_dcnt=0;
		my @_do = ();
		for (my $i=1; $i<=$_prvku; $i++) {
			my $tmp = substr($buf, 5*($i-1), 5);
			my ($typ, $foo1, $foo2) = unpack('SSC', $tmp);	# ushort, ushort, char?
			if ($typ == 0) {
				# separator. move to next level.
				$_dcnt++;
			} else {
				push @{$_do[$_dcnt]}, $typ;
			};
		};

		my %by_type = ();
		for (my $i=0; $i<=$#_do; $i++) {
			foreach (@{$_do[$i]}) {
				$by_type{$_} = $i+1;
			};
		};

		# text priorities are 1-8:
		$t .= "[_drawOrder]\n";
		foreach (sort {$a <=> $b} keys %by_type) {
			$t .= sprintf "Type=0x%02x,%d\n", $_, $by_type{$_};
		};
		$t .= "[end]\n";
		$t .= "\n";
	};

	
	# And now real data:
	foreach my $kind (qw(polygon point line)) {
		my %h = %{$header{$kind}};
		# all of them together:
		sysseek(F, $h{dataofs}, SEEK_SET);
		sysread(F, $buf, $h{datalen});
		my $full_content = $buf;	# copy

		# navigation to data elements:
		my @elements = ();
		if ($h{arraysize} > 0) {
			sysseek(F, $h{arrayofs}, SEEK_SET);
			sysread(F, $buf, $h{arraysize});
			my $num_of_elements = $h{arraysize}/$h{arraymod};
			for (my $i=0; $i<$num_of_elements; $i++) {
				my $tmp = substr($buf, $h{arraymod}*$i, $h{arraymod});

				my ($otyp, $ofs);
				if ($h{arraymod} == 4) {
					($otyp, $ofs) = unpack('SS', $tmp);
				} elsif ($h{arraymod} == 3) {
					($otyp, $ofs) = unpack('SC', $tmp);
				} else {
					die "unknown arraymod length: $h{arraymod}";
				};

				# i don't know why is it so strangely rotated:
				my $wtyp = ($otyp >> 5) | (($otyp & 0x1F) << 11);	
				my $typ = ($wtyp & 0xFF);
				my $subtyp = ($wtyp >> 8); 
				$subtyp = ($subtyp >> 3) | (($subtyp & 0x07) << 5);

				push @elements, {
					typ	=> $typ,
					subtyp => $subtyp,
					ofs	=> $ofs,
					puvodni_poradi => $i
				};
			};
		};

		# Elements must be sorted by offset. Some TYPs contain unsored data
		# and without sorting it would not be possible to compute their
		# lengths.
		@elements = sort {$a->{ofs} <=> $b->{ofs}} @elements;

		# Compute length:
		for (my $i=0; $i<=$#elements; $i++) {
			my $p = $elements[$i];
			my $block_length = undef;
			if ($i < ($#elements)) {
				# nasleduje za nim dalsi, je to jednoduche (snad, doufam ze maji vzestupne adresy):
				$block_length = $elements[$i+1]->{ofs} - $p->{ofs};
			} else {
				# je to posledni prvek:
				$block_length = $h{datalen} - $p->{ofs};
			};
			$p->{len} = $block_length;
			$p->{content} = substr($full_content, $p->{ofs}, $p->{len});
		};

		for (my $i=0; $i<=$#elements; $i++) {
			my $p = $elements[$i];
			my $info = sprintf("typ=%02x/%02x, ofs?=%04x, len=%04x", $p->{typ}, $p->{subtyp}, $p->{ofs}, $p->{len});
			$t .= "[_$kind]\n";
			$t .= sprintf "Type=0x%x\n", $p->{typ};
			if ($p->{subtyp}) {
				$t .= sprintf "SubType=0x%x\n", $p->{subtyp};
			};

			if ($kind eq 'polygon') {
				my $tmp = $p->{content};
				my $x = substr($tmp, 0, 1, '');	# pryc s nim
				$x = unpack("C", $x);
				if ($x & 0x10) {
					# has localization
				};
				my $color_type = ($x & 0x0f);

				my (@c);
				my $bitmap = undef;
				if ($color_type == 6) {
					# pouze 1 barva, bez bitmapy
					@c = (
						get_rgb_from_buffer($tmp), 
						undef
					);
					$c[1] = $c[0];
				} elsif ($color_type == 7) {
					# pouze 2 barvy (noc a den), bez bitmapy
					@c = (
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp)
					);
				} elsif ($color_type == 8) {
					@c = (
						get_rgb_from_buffer($tmp),
						get_rgb_from_buffer($tmp)
					);
					$bitmap = substr($tmp, 0, 128, '');
				} elsif ($color_type == 0xf) {
					@c = (
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp)
					);
					$c[1] = undef;	# override (musim ji sebrat z bufferu, proto az tady)
					$bitmap = substr($tmp, 0, 128, '');
				} elsif (($color_type == 9)) {
					@c = (
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp),
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp)
					);
					# Neprohazovat, jinak bude preview sice spravne, ale cGPSmapper
					# to zkompiluje tak ze budou barvy prohozene:
					#@c = ($c[1], $c[0], $c[3], $c[2]);	# prohodit popredi a pozadi
					$bitmap = substr($tmp, 0, 128, '');
				} elsif ($color_type == 0x0b) {
					# 20d72c32eab3cb12f1b79e14061bf0d13a0771e9.typ
					# 1 bitmapa, a k ni 3 ruzne barvy.
					# Zrejme bude jedna z barev pruhledna? Ne, to je u polygonu asi pitomost
					@c = (
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp),
						get_rgb_from_buffer($tmp), 
						undef
					);
					$bitmap = substr($tmp, 0, 128, '');
				} elsif ($color_type == 0x0e) {
					# Asi podobne jako 6, vyskytuje se v bbc8ace3f24c685564d44d8b99196fd7821becac.typ,
					# ale contentuje bitmapu!
					# Tzn. jen jedna definovana barva, bez bitmapy. Ze by pruhledna? To je u polygonu asi blbost.
					@c = (
						get_rgb_from_buffer($tmp), 
						undef
					);
					#$c[1] = $c[0];	# ??
					$bitmap = substr($tmp, 0, 128, '');
				} else {
					die "unknown color type: $color_type";
				};
				if (defined $bitmap) {
					$bitmap ^= (chr(0xff) x length($bitmap));	# inverze
				};

				# Zobrazim definici bitmapy:
				$t .= DecodeBitmap($bitmap, 32, 32, 1, \@c);

				$t .= DecodeLocalizationStrings($tmp);
			} elsif ($kind eq 'line') {
				my $tmp = $p->{content};
				my ($a, $b) = unpack("CC", substr($tmp, 0, 2, ''));	# pryc s nim

				my $color_flag = ($a & 0x07);
				my (undef) = (($a & 0x01) ? 1 : 0);	# neznamy flag
				my (undef) = (($a & 0x04) ? 1 : 0);	# neznamy flag
				my $rows = ($a >> 3);
				my ($localization) = (($b & 0x01) ? 1 : 0);
				my ($orientation) = (($b & 0x02) ? 1 : 0);

				my @c = ();
				my $real_color_count = 0;
				if ($color_flag == 0x01) {
					# 001
					# 4 barvy: 2 pro den, 2 pro noc (pozadi neni pruhledne)
					# SPRAVNE. Akorat je to mozna noc a den (tzn. naopak).
					# Pravdepodobne jsou to tyto barvy:
					@c = (
						get_rgb_from_buffer($tmp), # pixel '3',
						get_rgb_from_buffer($tmp), # pixel 
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp)
					);
				} elsif ($color_flag == 0x03) { 
					# 011
					# 2 barvy (den:popredi+pozadi), noc je stejna jako den
					# Treti barva se da (asi) ignorovat?
					# 100% funkcni
					@c = (
						get_rgb_from_buffer($tmp), # pixel '1'
						get_rgb_from_buffer($tmp), # pixel '0'
						get_rgb_from_buffer($tmp)	# kupodivu jsou tam vzdy 3, ale tahle se nikdy nepouzije
					);
					@c = @c[0 .. 1];	# zrusim 3. barvu, at to pri dekodovani vypise jen dve
				} elsif ($color_flag == 0x07) { 
					# 111
					# 2 barvy, ale kazda pouze pro popredi noci a dne,
					# protoze pozadi je vzdy pruhledne???
					# 100% funkcni
					#
					# Bitmapa vypada tak, ze 1=pixel, 0=pozadi.
					# Barvy by tedy v poli 
					@c = (
						undef,
						get_rgb_from_buffer($tmp),
						undef,
						get_rgb_from_buffer($tmp)
					);
				} elsif ($color_flag == 0) {
					# 000
					# Bez bitmapy. Jsou tam pak dve barvy (den:popredi+pozadi).
					# Noc je stejna jako den
					@c = (
						get_rgb_from_buffer($tmp), 
						get_rgb_from_buffer($tmp)
					);
				} elsif ($color_flag == 6) {
					# 110
					# Jen jedna barva? Divne. A s bitmapou nebo bez?
					@c = (
						get_rgb_from_buffer($tmp)
					);
				} else {
					die "unknown color flag: $color_flag";
				};
				if ($rows == 0) {
					# ÄÃ¡ra bez bitmapy, jen s definicÃ­ barev
					$t .= DecodeBitmap('', 32, 0, 1, \@c);
					my ($lsize, $totalsize) = unpack("CC", substr($tmp, 0, 2, ''));	# pryc s nim
					$t .= "UseOrientation=" . ($orientation ? 'N' : 'Y') . "\n";	# 100% OK
					$t .= sprintf "LineWidth=%d\n", $lsize;
					$t .= sprintf "BorderWidth=%d\n", ($totalsize - $lsize) / 2;
				} else {
					$t .= "UseOrientation=" . ($orientation ? 'N' : 'Y') . "\n";	# 100% OK
					my $bitmap = substr($tmp, 0, 4*$rows, '');
					if (($color_flag == 0x01) || ($color_flag == 0x03)) { 
						# Pozor, po dekompilaci vznikaji spravne barvy, ale inverzni bitmapa.
						# Timhle to spravim:
						$bitmap ^= (chr(0xff) x length($bitmap));	# inverze
					};
					# ÄÃ¡ra mÃ¡ vÅ¾dy 1 BPP,bez ohledu na noÄnÃ­ a dennÃ­ barvy:
					$t .= DecodeBitmap($bitmap, 32, $rows, 1, \@c);
				};
				$t .= DecodeLocalizationStrings($tmp);	# if $localization;
			} elsif ($kind eq 'point') { # {{{
				my $tmp = $p->{content};
				my ($a, $w, $h, $colors, $x3) = unpack("CCCCC", substr($tmp, 0, 5, ''));	# pryc s nim
				#$colors++;	# v definici je o 1 min. A nebo ne? Ne!

				# Kolik bitÅ¯ zabÃ­rÃ¡ jeden pixel? To zjistÃ­m jen a pouze z poÄtu barev.
				my ($bpp, $w_bytes) = BppAndWidthInBytes($colors, $w, $x3);

				#die if $w != 0x10;
				#die if $h != 0x10;
				#die $t . "\n$x3 != 0x10" if $x3 != 0x10;

				if (($a == 5) || ($a == 1) || ($a == 0x0d) || ($a == 0x0b) || ($a == 0x09)) {
					my @c = ();
					if ($x3 == 0x20) {	# barvy se 4-bitovym alfakanalem
						my $bytes = $colors * 3.5;
						if ($bytes != int($bytes)) {
							# zaokrouhleni nahoru
							$bytes = int($bytes) + 1;
						};

						my $c = substr($tmp, 0, $bytes, '');	# raw
						# prevedu $c na bitstring:
						$c = unpack('b*', $c);
						for (my $i=1; $i<=$colors; $i++) {
							#$t .= "; i=$i; c=$c\n";
							my $barva = substr($c, 0, 3*8, '');	# 3x8 bitu (8 pro kazdou barvu)
							my ($b, $g, $r) = unpack("CCC", pack('b*', $barva));
							my $alfa = unpack("C", pack('b*', substr($c, 0, 4, ''))); # 4 bity na alfakanal nebo na co?
							# FIXME neresim tu skutecnou pruhlednost,
							# jen falesnou (0=plna, 15=pruhledna)
							if ($alfa == 0) {
								push @c, (($r << 16) | ($g << 8) | $b);
							} else {
								push @c, undef;	# pruhledna?
							};
						};
					} elsif ($x3 == 0x10) {
						for (my $i=1; $i<=$colors; $i++) {
							push @c,  get_rgb_from_buffer($tmp);
						};
					} elsif ($x3 == 0) {
						# x3=0:
						# ... pri typu 9 a poctu barev 3 to znamena:
						#  "pouze 2 pixely na barvu v bitmape!"
						# ... pri typu 9 a poctu barev 32 to znamena:
						#    beze zmeny (8 bpp)
						for (my $i=1; $i<=$colors; $i++) {
							push @c,  get_rgb_from_buffer($tmp);
						};
						if ($bpp == 4) {
							$bpp /= 2;
							$w_bytes /= 2;
						};
					} else {
						$t .= "; unknown x3: $x3\n";
						warn "unknown x3: $x3 (a=$a)";
					};

					my $bitmap = substr($tmp, 0, $h*$w_bytes, '');
					$t .= DecodeBitmap($bitmap, $w, $h, $bpp, \@c, 'Day');
				} elsif ($a == 7) {
					# obe XPM (Day i Night)
					my @c = ();
					for (my $i=1; $i<=$colors; $i++) {
						push @c,  get_rgb_from_buffer($tmp);
					};
					my $bitmap = substr($tmp, 0, $h*$w_bytes, '');
					$t .= DecodeBitmap($bitmap, $w, $h, $bpp, \@c, 'Day');

					# NightXPM vypada takto:
					# $colors, $x3:
					($colors, $x3) = unpack("CC", (substr($tmp, 0, 2, '')));
					#die if $x3 != 0x10;
					($bpp, $w_bytes) = BppAndWidthInBytes($colors, $w);
					@c = ();
					for (my $i=1; $i<=$colors; $i++) {
						push @c,  get_rgb_from_buffer($tmp);
					};
					$bitmap = substr($tmp, 0, $h*$w_bytes, '');
					$t .= DecodeBitmap($bitmap, $w, $h, $bpp, \@c, 'Night');
				} else {
					die "unknown bitmap type for [_point]: $a";
				};
				#printf "; a=%02x, b=%02x\n", $a, $b;
				$t .= DecodeLocalizationStrings($tmp); # if $localization;
				# }}}
			} else {
				die;
			};

			$t .= "[end]\n";
			$t .= "\n";
		};
	};

	close F;
	return ($t, $fid, $pid);
};

# meni buffer!!!!!!!!
sub get_rgb_from_buffer($) {
	my $raw = substr($_[0], 0, 3, '');	# vyriznout primo z predaneho parametru!
	my ($b, $g, $r) = unpack("CCC", $raw);
	return ($r << 16) | ($g << 8) | $b;
};

sub DecodeLocalizationStrings($) {
	my ($buf) = @_;
	if (length($buf) == 0) {
		return;
	};

	my $ret = '';
	my $delka = unpack("C", substr($buf, 0, 1, ''));
	# a ted postupne:
	my $cnt=1;
	# slow, but working (who needs speed, anyway?)
	while (length($buf) > 0) {
		my ($lang) = unpack("C", substr($buf, 0, 1, ''));
		my $text = '';
		C:
		while (length($buf)>0) {
			my $c = substr($buf, 0, 1, '');
			last C if ord($c) == 0;
			$text .= $c;
		};
		$ret .= sprintf("String%d=0x%02x,%s\n", $cnt, $lang, $text);
		$cnt++;
	};
	return $ret;
};

sub DecodeBitmap($$$$$;$) {
	my ($bmap, $w, $h, $bpp, $barvy, $xpm_prefix) = @_;
	my @color_name = ();
	my @out = ();

	if ($bmap eq '') {
		$w = 0;
		$h = 0;
	} else {
		# Sirka jednoho radku v bajtech. Protoze se provadi zaokrouhleni na cele bajty, 
		# je to trochu komplikovanejsi:
		my $w_bytes = ($w * $bpp) / 8;
		if ($w_bytes > int($w_bytes)) {
			# zaokrouhleni nahoru
			$w_bytes = int($w_bytes) + 1;
		};

		for (my $y=0; $y<$h; $y++) {
			my @l = ();
			my $line = substr($bmap, 0, $w_bytes, '');
			my $b = unpack("b*", $line);
			for (my $x=0; $x<$w; $x++) {
				my $partbits = substr($b, 0, $bpp, '');
				my $colornumber = unpack('C', pack('b*', $partbits));
				push @l, $colornumber;

				if ($colornumber > $#{$barvy}) {
					for (my $i=1+$#{$barvy}; $i<=$colornumber; $i++) {
						$barvy->[$i] = undef;	# prazdna
						AssigncolorName($i, $barvy->[$i], \@color_name);
					};
				};
				AssigncolorName($colornumber, $barvy->[$colornumber], \@color_name);
			};
			push @out, \@l;
		};
	};

	my $ret = '';
	$ret .= sprintf("${xpm_prefix}XPM=\"%d %d %d %d\",\n", $w, $h, 1+$#{$barvy}, 1);
	for (my $i=0; $i<=$#{$barvy}; $i++) {
		# nouzove prideleni nazvu barvy, pokud vlastne nebyla nikde pouzita:
		AssigncolorName($i, $barvy->[$i], \@color_name);
		if (defined $barvy->[$i]) {
			$ret .= "\"$color_name[$i]  c #" . sprintf('%06x', $barvy->[$i]). "\",";
		} else {
			$ret .= "\"$color_name[$i]  c none\",";
		};
		$ret .= "\n";
	};
	for (my $y=0; $y<=$#out; $y++) {
		$ret .= "\"";
		my $x=0;
		foreach (@{$out[$y]}) {
			$ret .= $color_name[$_];
			# ano, je to x,y, ne obracene:
			$x++;
		};
		if ($y == $#out) {
			$ret .= qq{"\n};
			$ret .= "};\n";
		} else {
			$ret .= qq{",\n};
		};
	};
	return $ret;
};

sub AssigncolorName($$$) {
	my ($colornumber, $rgb, $ref_nazvy) = @_;
	return $ref_nazvy->[$colornumber] if defined $ref_nazvy->[$colornumber];

	my $nazev = undef;
	if ((!defined $rgb) && ($colornumber == 0)) {
		$nazev = '.';
	} else {
		$nazev = substr('X=*#%1234567890ABCDEFGHIJKLMNOPQRSTUVWYZabcdefghijklmnopqrstuvwxyz!@$^&()-+/<>,:;~', $colornumber, 1);
	};

	$ref_nazvy->[$colornumber] = $nazev;
	return $nazev;
};

# How many bits is needed by one pixel? 
sub BppAndWidthInBytes($$;$) {
	my ($colors, $w, $specialflag) = @_;	# number of colors, width in pixels
	my $bpp = undef;
	if ($colors >= 16) {
		# 16, 17, ...
		$bpp = 8;
	} elsif ($colors >= 3) {
		# 3-15 colors
		if (($colors == 3) && ($specialflag == 0x20)) {
			$bpp = 2;	
		} else {
			$bpp = 4;
		};
	} else {
		# 1-2 colors
		$bpp = 2;
	};
	# round to whole bytes
	my $w_bytes = ($w * $bpp) / 8;
	if ($w_bytes > int($w_bytes)) {
		# round up
		$w_bytes = int($w_bytes) + 1;
	};
	return ($bpp, $w_bytes);
};

# main
my ($text, undef, undef) = Decompile($ARGV[0]);
print $text;
