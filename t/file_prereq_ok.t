package main;

use 5.010;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();
use Test::Prereq::Meta;

my $tpm = Test::Prereq::Meta->new();

$tpm->file_prereq_ok( 'lib/Test/Prereq/Meta.pm' );

$tpm->file_prereq_ok( 't/basic.t' );

$tpm->file_prereq_ok( 't/data/hello_world.PL' );

done_testing();

1;

# ex: set textwidth=72 :