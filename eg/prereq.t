package main;

use 5.010;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();

BEGIN {
    eval {
	require Test::Prereq::Meta;
	Test::Prereq::Meta->import( qw{ all_prereq_ok } );
	1;
    } or plan skip_all => 'Test::Prereq::Meta not available';
}

all_prereq_ok();

done_testing;

1;

# ex: set textwidth=72 :
