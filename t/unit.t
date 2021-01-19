package main;

use 5.010;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();
use Test::Prereq::Meta;

{
    my $rslt;

    TODO: {
	local $TODO = 'Deliberately-failing test';
	$rslt = Test::Prereq::Meta->new(
	    meta_file	=> 't/data/accept/META.json',
	    name	=> 'Unlisted prereq: %f uses %m',
	)->all_prereq_ok( 't/data/accept/lib' );
    }

    ok( ! $rslt, 'Got failure when expected' );
}

Test::Prereq::Meta->new(
    meta_file	=> 't/data/accept/META.json',
    name	=> 'Unlisted core prereq: %f uses %m',
    perl_version	=> 'this',
)->all_prereq_ok( 't/data/accept/lib' );

Test::Prereq::Meta->new(
    accept	=> [ qw{ strict } ],
    meta_file	=> [ qw{
	t/data/accept/some-non-existent-file.yml
	t/data/accept/META.json
	} ],
    name	=> 'Unlisted-but-accepted prereq: %f uses %m',
)->all_prereq_ok( 't/data/accept/lib' );

Test::Prereq::Meta::file_prereq_ok( 't/data/rogue_require' );

done_testing;

1;

# ex: set textwidth=72 :
