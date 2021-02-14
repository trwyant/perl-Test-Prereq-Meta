package My::Module::MakeMaker;

use 5.010;

use strict;
use warnings;

use ExtUtils::MakeMaker;
use My::Module::Meta;

our $VERSION = '0.000_918';

sub write_make_file {

    ( my $mmv = ExtUtils::MakeMaker->VERSION ) =~ s/_//g;

    my $meta = My::Module::Meta->new();

    my %args = (
	ABSTRACT	=> $meta->abstract(),
	AUTHOR		=> $meta->author(),
	DISTNAME	=> $meta->dist_name(),
	NAME		=> $meta->module_name(),
	# VERSION_FROM	=> 'lib/Test/CPAN/Meta/Links.pm',
	PREREQ_PM	=> $meta->requires(),
	PL_FILES => {},	# Prevent old MakeMaker from running Build.PL
	realclean	=> {
	    FILES => 'cover_db xt/author/optionals',
	},
    );

    $mmv >= 6.31
	and $args{LICENSE} = $meta->license();

    if ( $mmv >= 6.4501 ) {
	$args{META_ADD} = {
	    no_index => $meta->no_index(),
	};
	$args{META_MERGE} = $meta->meta_merge( $meta->provides() );
    }

    $mmv >= 6.4701
	and $args{MIN_PERL_VERSION} = $meta->requires_perl();


    if ( $mmv >= 6.52 ) {
	$args{BUILD_REQUIRES} = $meta->build_requires();
	$args{CONFIGURE_REQUIRES} = $meta->configure_requires();
    } elsif ( $mmv >= 6.5501 ) {
	$args{BUILD_REQUIRES} = $meta->build_requires();
	$args{META_MERGE}{configure_requires} = $meta->configure_requires();
    } elsif ( $mmv >= 6.4501 ) {
	$args{META_MERGE}{build_requires} = $meta->build_requires();
	$args{META_MERGE}{configure_requires} = $meta->configure_requires();
    } else {
	foreach my $method ( qw{ configure_requires build_requires } ) {
	    my $req = $meta->$method();
	    foreach my $key ( keys %{ $req } ) {
		exists $args{PREREQ_PM}{$key}
		    or $args{PREREQ_PM}{$key} = $req->{$key};
	    }
	}
    }

    WriteMakefile( %args );
}

sub MY::postamble {
#   my ( $self, @args ) = @_;
    my ( $self ) = @_;

    my $authortest = $self->test_via_harness(
	'$(FULLPERLRUN)', '$(AUTHORTEST_FILES)' );
    $authortest =~ s/ \s+ \z //smx;
    $authortest =~ s/ \A \s+ //smx;
    chomp $authortest;

    return <<"EOD";

AUTHORTEST_FILES = t/*.t xt/author/*.t

authortest :: pure_all
	AUTHOR_TESTING=1 $authortest

testcover :: pure_all
	cover -test
EOD

}

1;

__END__

=head1 NAME

My::Module::MakeMaker - <<< replace boilerplate >>>

=head1 SYNOPSIS

<<< replace boilerplate >>>

=head1 DESCRIPTION

<<< replace boilerplate >>>

=head1 METHODS

This class supports the following public methods:

=head1 ATTRIBUTES

This class has the following attributes:


=head1 SEE ALSO

<<< replace or remove boilerplate >>>

=head1 SUPPORT

Support is by the author. Please file bug reports at
L<https://github.com/trwyant/perl-Test-CPAN-Meta-Links/issues>, or in
electronic mail to the author.

=head1 AUTHOR

Thomas R. Wyant, III F<wyant at cpan dot org>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2021 by Thomas R. Wyant, III

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl 5.10.0. For more details, see the full text
of the licenses in the directory LICENSES.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut

# ex: set textwidth=72 :
