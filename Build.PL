use strict;
use warnings;

use 5.010;

use lib qw{ inc };

use My::Module::Build;

my $bldr = My::Module::Build->new();

$bldr->create_build_script ();
