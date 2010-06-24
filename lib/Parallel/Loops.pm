package Parallel::Loops;

our $VERSION='0.01';

=head1 NAME

Parallel::Loops - Execute loops using parallel forked subprocesses

=encoding utf-8

=head1 SYNOPSIS

    use Parallel::Loops;

    my $maxProcs = 5;
    my $pl = new Parallel::Loops($maxProcs);

    my @input = ( 0 .. 9 );

    # We want to perform some hefty calculation for each @input and store each
    # calculation's result in %output. For that reason, we "tie" %output, so
    # that changes to %output in any child process (see below) are
    # automatically transfered and updated in the parent also.

    my %output;
    $pl->tieOutput( \%output );

    $pl->foreach(
        \@input,
        sub {
            # This sub "magically" executed in parallel forked child
            # processes

            # Lets just create a simple example, but this could be a
            # massive calculation that will be parallelized, so that
            # $maxProcs different processes are calculating sqrt
            # simultaneously for different values of $_ on different CPUs

            $output{$_} = sqrt($_);
        }
    );
    foreach (@input) {
        printf "i: %d sqrt(i): %f\n", $_, $output{$_};
    }
  
You can also use @arrays instead of %hashes, and/or while loops
instead of foreach:

    my @output;
    $pl->tieOutput(\@output);

    my $i = 0;
    $pl->while (
        sub { $i++ < 10 },
        sub {

            # This sub "magically" executed in parallel forked
            # child processes

            push @output, [ $i, sqrt($i) ];
        }
    );

=head1 DESCRIPTION

Often a loop performs calculations where each iteration of the loop
does not depend on the previous iteration, and the iterations really
could be carried out in any order.

This module allows you to run such loops in parallel using all the
CPUs at your disposal.

Output is automatically transfered from children to parents via
%hashes or @arrays, that have explicitly been configured for that sort
of monitoring via $pl->tieOutput(). Hashes will transfer keys that are
set in children (but not cleared or unset), and elements that are
pushed to @arrays in children are pushed to the parent @array too (but
note that the order is not guaranteed to be the same as it would have
been if done all in one process, since there is no way of knowing
which child would finish first!)

If you can see past the slightly weird syntax, you're basically
getting foreach and while loops that can run in parallel without
having to bother with fork, pipes, signals etc. This is all handled
for you by this module.

=head2 foreach loop

    $pl->foreach($arrayRef, $childBodySub)

Runs $childBodySub->() with $_ set foreach element in @$arrayRef,
except that $childBodySub is run in a forked child process to obtain
parallelism. Essentially, this does something conceptually similar to:

    foreach(@$arrayRef) {
        $childBodySub->();
    }

Any setting of hash keys or pushing to arrays that have been set with
$pl->tieOutput() will automagically appear in the hash or array in the
parent process.

If you like loop variables, you can run it like so:

    $pl->foreach( \@input, sub {
            my $i = $_;
            .. bla, bla, bla ... $output{$i} = sqrt($i);
        }
    );

=head2 while loop

  $pl->while($conditionSub, $childBodySub)

Essentially, this does something conceptually similar to:

  while($conditionSub->()) {
      $childBodySub->();
  }

except that $childBodySub->() is executed in a forked child process.
Output is transfered via tieOutput() like in L</foreach loop> above.

=head3 while loops must affect condition outside $childBodySub

Note that incrementing $i in the $childBodySub like in this example
B<will not work>:

   $pl->while( sub { $i < 5 },
               sub { 
                   $output{$i} = sqrt($i);
                   # Won't work!
                   $i++ 
               }
             );

Because $childBodySub is executed in a child, and so while $i would
be incremented in the child, that change would not make it to the
parent, where $conditionSub is evaluated. The changes that make
$conditionSub return false eventually I<must> take place outside
the $childBodySub so it is executed in the parent. (Adhering to
the parallel principle that one iteration may not affect any other
iterations - including whether to run them or not) 

=head2 tieOutput

  $pl->tieOutput(\%output, \@output, ...)

Each of the arguments to tieOutput() are instrumented, so that when a
hash key is set or array element pushed in a child, this is transfered
to the parent's hash or array automatically when a child is finished.

B<Note the limitation> Only keys being set like C<$hash{'key'} =
'value'> and arrays elements being pushed like C<push @array, 'value'>
will be transfered to the parent. Unsetting keys, or setting
particluar array elements with $array[3]='value' will be lost if done
in the children. In the parent process all the %hashes and @arrays are
full-fledged, and you can used all operations. But only these
operations in the child processes make it back to the parent.

=head3 Array element sequence not defined

Note that when using tieOutput() for @output arrays, the sequence of
elements in @output is not guaranteed to be the same as you'd see with
a normal sequential while or foreach loop, since the calculations are
done in parallel and the children may end in an unexpected sequence.
But if you don't really care about the order of elements in the
@output array then tieOutput-ing an array can be useful and fine.

If you need to be able to determine which iteration generated what output, use
a hash instead.

=head2 Recursive forking is possible

Note that no check is performed for recursive forking: If the main
process encouters a loop that it executes in parallel, and the
execution of the loop in child processes also encounters a parallel
loop, these will also be forked, and you'll essentially have
$maxProcs^2 running processes. It wouldn't be too hard to implement
such a check (either inside or outside this package) but maybe this is
what a programmer actually wants, so it isn't implemented.

=head1 SEE ALSO

This module uses fork(). ithreads could have been possible too, but was not
chosen. You may want to check out:

When to use forks, when to use threads ...?
L<http://www.perlmonks.org/index.pl?node_id=709061>

The forks module (not used here)
L<http://search.cpan.org/search?query=forks>

threads in perlthrtut
L<http://perldoc.perl.org/perlthrtut.html>

=head1 DEPENDENCIES

I believe this is the only dependency that isn't part of core perl:

    use Parallel::ForkManager;

These should all be in perl's core:

    use Data::Dumper;
    use IO::Handle;
    use Tie::Array;
    use Tie::Hash;
    use UNIVERSAL qw(isa);

=head1 BUGS / ENHANCEMENTS

No bugs are known at the moment. Send any reports to peter@morch.com.

Enhancements:

Use Storable instead of Data::Dumper - its probably faster and better suited to
these needs. L<http://www.unix.com.ua/orelly/linux/dbi/ch02_05.htm>

Maybe use function prototypes (see Prototypes under perldoc perlsub). 

Then we could do something like

    pl_foreach @input {
        yada($_);
    };
or

    pl_foreach $pl @input {
        yada($_);
    };

instead of

    $pl->foreach(\@input, sub {
        yada($_);
    });

and so on, where the first suggestion above means global variables (yikes!).
Unfortunately, methods aren't supported by prototypes, so this will never be
posssible:

    $pl->foreach @input {
        yada($_);
    };

An alternative pointed out by the perlmonks chatterbox could be to use
L<Devel::Declare|http://search.cpan.org/perldoc?Devel::Declare> "if I can stand
pain".

=head1 COPYRIGHT

Copyright (c) 2008 Peter Valdemar Mørch <peter@morch.com>

All right reserved. This program is free software; you can redistribute it 
and/or modify it under the same terms as Perl itself.

=head1 AUTHOR

  Peter Valdemar Mørch <peter@morch.com>

=cut

use strict;
use warnings;

use IO::Handle;
use Data::Dumper;
use Parallel::ForkManager;
use UNIVERSAL qw(isa);

sub new {
    my ($class, $maxProcs) = @_;
    my $self = { maxProcs => $maxProcs, tieOutputNr => 0 };
    return bless $self, $class;
}

sub tieOutput {
    my ($self, @tieRefs) = @_;
    foreach my $ref (@tieRefs) {
        if (ref $ref && isa $ref, 'HASH') {
            # $storage will point to the Parallel::Loops::TiedHash object
            my $storage;
            tie %$ref, 'Parallel::Loops::TiedHash', $self, \$storage;
            push @{$$self{tieObjects}}, $storage;
            push @{$$self{tieHashes}}, [$$self{tieOutputNr}, $ref];
        } elsif (ref $ref && isa $ref, 'ARRAY') {
            # $storage will point to the Parallel::Loops::TiedArray object
            my $storage;
            tie @$ref, 'Parallel::Loops::TiedArray', $self, \$storage;
            push @{$$self{tieObjects}}, $storage;
            push @{$$self{tieArrays}}, [$$self{tieOutputNr}, $ref];
        } else {
            die "Only hash and array refs are supported by tieOutput";
        }
        $$self{tieOutputNr}++;
    }
}

sub in_child {
    my ($self) = @_;
    return $$self{forkManager} && $$self{forkManager}{in_child};
}

sub readChangesFromChild {
    my ($self, $childRdr) = @_;
    my $childOutput = '';
    while (<$childRdr>) {
        $childOutput .= $_;
    }
    my @output;
    eval $childOutput;
    foreach my $set (@{$$self{tieHashes}}) {
        my ($outputNr, $h) = @$set;
        foreach my $k (keys %{$output[$outputNr]}) {
            $$h{$k} = $output[$outputNr]{$k};
        }
    }
    foreach my $set (@{$$self{tieArrays}}) {
        my ($outputNr, $a) = @$set;
        foreach my $v (@{$output[$outputNr]}) {
            push @$a, $v;
        }
    }
}

sub printChangesToParent {
    my ($self, $parentWtr) = @_;
    my $outputNr = 0;
    foreach (@{$$self{tieObjects}}) {
       print $parentWtr $_->dumpChildInfo($outputNr++); 
    }
}

sub foreach {
    my ($self, $varRef, $arrayRef, $sub);
    if (ref $_[1] eq 'ARRAY') {
        ($self, $arrayRef, $sub) = @_;
    } else {
        ($self, $varRef, $arrayRef, $sub) = @_;
    }
    my %childHandles;
    my $fm = Parallel::ForkManager->new($$self{maxProcs});
    $$self{forkManager} = $fm;
    $fm->run_on_finish( sub {
        my ($pid) = @_;
        my $childRdr = $childHandles{$pid};
        $self->readChangesFromChild($childRdr);
    });
    my $childCounter = 0;
    foreach my $i (@$arrayRef) {
        # Setup pipes so the child can send info back to the parent about
        # output data.
        my $parentWtr = IO::Handle->new();
        my $childRdr  = IO::Handle->new();
        pipe( $childRdr, $parentWtr )
            or die "Couldn't open a pipe";
        $parentWtr->autoflush(1);
        
        my $pid = $fm->start( ++$childCounter );

        if ($pid) {
            # We're running in the parent...
            close $parentWtr;
            $childHandles{$pid} = $childRdr;
            next; 
        }

        # Setup either $varRef or $_, if no such given before calling $sub->()
        if ($varRef) {
            $$varRef = $i;
        } else {
            $_ = $i;
        }

        # We're running in the child
        $sub->();

        $self->printChangesToParent($parentWtr);
        close $parentWtr;

        $fm->finish($childCounter);    # pass an exit code to finish
    }
    $fm->wait_all_children;
    delete $$self{forkManager};
}

sub while {
    my ($self, $continueSub, $bodySub) = @_;
    my %childHandles;
    my $fm = Parallel::ForkManager->new($$self{maxProcs});
    $$self{forkManager} = $fm;
    $fm->run_on_finish( sub {
        my ($pid) = @_;
        my $childRdr = $childHandles{$pid};
        $self->readChangesFromChild($childRdr);
    });
    my $childCounter = 0;
    while ($continueSub->()) {
        # Setup pipes so the child can send info back to the parent about
        # output data.
        my $parentWtr = IO::Handle->new();
        my $childRdr  = IO::Handle->new();
        pipe( $childRdr, $parentWtr )
            or die "Couldn't open a pipe";
        $parentWtr->autoflush(1);
        
        my $pid = $fm->start( ++$childCounter );

        if ($pid) {
            # We're running in the parent...
            close $parentWtr;
            $childHandles{$pid} = $childRdr;
            next; 
        }

        # We're running in the child
        $bodySub->();

        $self->printChangesToParent($parentWtr);
        close $parentWtr;

        $fm->finish($childCounter);    # pass an exit code to finish
    }
    $fm->wait_all_children;
    delete $$self{forkManager};
}

package Parallel::Loops::TiedHash;
use Tie::Hash;
use base 'Tie::ExtraHash';

sub TIEHASH {
    my ( $class, $loops, $storageRef ) = @_;
    my $storage = bless [ {}, { loops => $loops, childKeys => {} } ], $class;
    $$storageRef = $storage;
    return $storage;
}

sub STORE {
    my ( $data, $key, $value ) = @_;

    my $hash  = $$data[0];
    my $extra = $$data[1];
    my $loops = $$extra{loops};

    if ( $loops->in_child() ) {
        $$extra{childKeys}{$key} = $value;
    }

    # warn sprintf "Setting $key to $value";
    $$hash{$key} = $value;
}

sub dumpChildInfo {
    my ($self, $outputNr) = @_;
    my $extra = $$self[1];
    my $dumper = Data::Dumper->new([$extra->{childKeys}],["output[$outputNr]"]);
    return $dumper->Dump();
}

package Parallel::Loops::TiedArray;
use Tie::Array;
use base 'Tie::Array';

sub TIEARRAY {
    my ( $class, $loops, $storageRef ) = @_;
    my $storage = bless { arr => [], loops => $loops, childArr => [] }, $class;
    $$storageRef = $storage;
    return $storage;
}

sub FETCHSIZE { scalar @{ $_[0]->{arr} } }
sub STORESIZE { $#{ $_[0]->{arr} } = $_[1] - 1 }
sub STORE     { $_[0]->{arr}->[ $_[1] ] = $_[2] }
sub FETCH     { $_[0]->{arr}->[ $_[1] ] }
sub CLEAR     { @{ $_[0]->{arr} } = () }
sub POP       { pop( @{ $_[0]->{arr} } ) }
sub SHIFT     { shift( @{ $_[0]->{arr} } ) }
sub UNSHIFT   { my $o = shift; unshift( @{ $o->{arr} }, @_ ) }
sub EXISTS    { exists $_[0]->{arr}->[ $_[1] ] }
sub DELETE    { delete $_[0]->{arr}->[ $_[1] ] }

sub PUSH {
    my $self = shift;

    if ( $$self{loops}->in_child() ) {
        push( @{ $self->{childArr} }, @_ );
    }

    push( @{ $self->{arr} }, @_ );
}

sub dumpChildInfo {
    my ($self, $outputNr) = @_;
    my $dumper = Data::Dumper->new([$self->{childArr}],["output[$outputNr]"]);
    return $dumper->Dump();
}

1;
