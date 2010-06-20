#!/usr/bin/perl -w
use strict;

=head1 Testing Parallel Loops

A simple test here. We run a foreach loop and in it, we send the results back.
We test the result structure and make sure it is what we expect. Then we do the same for a while loop

=cut

use Test::More tests => 23;
BEGIN { use_ok( 'Parallel::Loops' ); }

my $maxProcs = 2;
my $pl = new Parallel::Loops($maxProcs);

my @iterations = ( 0 .. 4 );

my %output;
$pl->tieOutput( \%output );

my @pids;
$pl->tieOutput( \@pids );

sub checkResults {
    my @seenPids;
    my $expectedStruct = {
        foo => 'FOO',
        bar => 'BAR'
    };
    foreach (@iterations) {
        my $out = $output{$_};
        if (defined $out->{pid} && $out->{pid} != $$) {
            pass("pid from child defined and good");
        } else {
            fail("pid from child has error");
        }
        push @seenPids, $out->{pid};
        is_deeply($out->{struct}, $expectedStruct, "Testing data transfer");
    }
    @seenPids = sort @seenPids;
    @pids = sort @pids;
    is_deeply(\@seenPids, \@pids, "Pids registered");
}

$pl->foreach(
    \@iterations,
    sub {
        $output{$_} = {
            pid => $$,
            struct => {
                foo => 'FOO',
                bar => 'BAR'
            }
        };
        push @pids, $$;
    }
);

checkResults();

%output = (); @pids = ();

my $i = -1;
$pl->while (
    sub { ++$i < scalar(@iterations) },
    sub {
        $output{$i} = {
            pid => $$,
            struct => {
                foo => 'FOO',
                bar => 'BAR'
            }
        };
        push @pids, $$;
    }
);
checkResults();

