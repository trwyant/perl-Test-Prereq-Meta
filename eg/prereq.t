package main;

use 5.010;

use strict;
use warnings;

use Test::More 0.88;	# Because of done_testing();
use Test::Prereq::Meta qw{ all_prereq_ok };

all_prereq_ok();

done_testing;

1;

# ex: set textwidth=72 :
