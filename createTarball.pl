#!/usr/bin/perl -w
use strict;
use File::Path;
use Config;
my $perlpath = $Config{perlpath};

open I, 'Changes'
    or die "Couldn't open Changes";
my $version = <I>;
close I;
chomp $version;
$version =~ /^Version (.*):$/
    or die "Unexpected version line: $version";
$version = $1;

# Test that this version number is the same as that in the .pm
use lib 'lib';
use Parallel::Loops;
die sprintf ("Version mismatch: Changes: '%s', pm: '%s'",
             $version, $Parallel::Loops::VERSION)
    if ($version ne $Parallel::Loops::VERSION);
if (-e "Parallel-Loops-$version.tar.gz") {
    die "Parallel-Loops-$version.tar.gz already exists - remove it first"
}
sub safeSystem {
    system(@_);
    die sprintf( "system call '%s' failed: %d",
                 join(" ", @_),
                 $?
               )
        if $?;
}
safeSystem('pod2text lib/Parallel/Loops.pm > README');
safeSystem('git diff --exit-code README > /dev/null');
safeSystem('git', 'archive', 'HEAD', "--prefix=Parallel-Loops-$version/",
           '-o', "Parallel-Loops-$version.tar");
safeSystem('gzip', "Parallel-Loops-$version.tar");
safeSystem('tar', '-zxvf', "Parallel-Loops-$version.tar.gz");
chdir "Parallel-Loops-$version";
safeSystem($perlpath, 'Makefile.PL');
safeSystem('make');
safeSystem('make', 'test');
chdir "..";
rmtree("Parallel-Loops-$version");
