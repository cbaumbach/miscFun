#!/usr/bin/env perl
# gzhead.pl
use warnings;
use strict;
use Getopt::Long;
use IO::Uncompress::Gunzip qw($GunzipError);

Getopt::Long::Configure(qw(no_auto_abbrev no_ignore_case_always bundling));

# Print the first logical line from a gzip-compressed file.

sub usage {
    print STDERR "Usage: $0 [-nK] file[s]\n";
    exit 1;
}

my $nlines = 1;

GetOptions('n=i' => \$nlines) or usage();

usage() unless @ARGV;

my $multiple_files = @ARGV >= 2;
while (my $file = shift @ARGV) {

    my $z = new IO::Uncompress::Gunzip $file
        or die "IO::Uncompress::Gunzip failed: $GunzipError\n";

    print "==> $file <==\n" if $multiple_files;
    print scalar(<$z>) for 1..$nlines;
    print "\n" if @ARGV;
}
