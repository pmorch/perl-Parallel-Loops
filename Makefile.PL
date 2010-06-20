use ExtUtils::MakeMaker;

WriteMakefile(
    NAME            => 'Parallel::Loops',
    VERSION_FROM    => 'lib/Parallel/Loops.pm',
    PREREQ_PM       => {
        "Test::More" => 0,
        "Parallel::ForkManager" => 0,
    },
);
