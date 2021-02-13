package My::Module::Build;

use strict;
use warnings;

use Module::Build;
our @ISA = qw{ Module::Build };

use My::Module::Meta;

sub new {
    my ( $class ) = @_;

    ( my $mbv = Module::Build->VERSION() ) =~ s/_//g;

    my $meta = My::Module::Meta->new();

    my %args = (
	add_to_cleanup	=> [ qw{ cover_db xt/author/optionals } ],
	build_requires	=> $meta->build_requires(),
	configure_requires	=> $meta->configure_requires(),
	dist_abstract	=> $meta->abstract(),
	dist_author	=> $meta->author(),
	dist_name	=> $meta->dist_name(),
	license		=> $meta->license(),
	module_name	=> $meta->module_name(),
	requires	=> $meta->requires(
	    perl	=> $meta->requires_perl(),
	),
    );

    if ( $mbv >= 0.28 ) {
	$args{meta_merge} = $meta->meta_merge();
	$args{meta_add} = {
	    no_index	=> $meta->no_index(),
	};
    }

    $mbv >= 0.34
	and $args{auto_configure_requires} = 0;	# Don't require Module::Build

    return $class->SUPER::new( %args );
}

sub ACTION_authortest {
    my ( $self, @args ) = @_;

    local $ENV{AUTHOR_TESTING} = 1;

    -e 'META.json'
	or $self->depends_on( 'distmeta' );

    $self->depends_on( 'build' );

    $self->test_files( qw{ t xt/author } );
    $self->depends_on( 'test' );

    return;
}

sub ACTION_test {
    my ( $self, @args ) = @_;

    -e 'META.json'
	or $self->depends_on( 'distmeta' );

    $self->depends_on( 'build' );

    return $self->SUPER::ACTION_test( @args );
}

1;

__END__

=head1 NAME

My::Module::Build - Extend Module::Build for this module

=head1 SYNOPSIS

 perl Build.PL
 ./Build
 ./Build test
 ./Build authortest # supplied by this module
 ./Build install

=head1 DESCRIPTION

This extension of L<Module::Build|Module::Build> adds the following
action to those provided by L<Module::Build|Module::Build>:

  authortest

=head1 ACTIONS

This module provides the following actions:

=head2 authortest

This action runs not only those tests which appear in the F<t>
directory, but those that appear in the F<xt> directory. The F<xt> tests
are provided for information only, since some of them (notably
F<xt/critic.t> and F<xt/pod_spelling.t>) are very sensitive to the
configuration under which they run.

Some of the F<xt> tests require modules that are not named as
requirements. These should disable themselves if the required modules
are not present.

This action is sensitive to the C<verbose=1> argument, but not to the
C<--test_files> argument.

This action also creates the F<META.*> files if needed.

=head2 test

This action overrides the core C<test> action to create the F<META.*>
files if needed.

=head1 SUPPORT

Support is by the author. Please file bug reports at
L<https://github.com/trwyant/perl-Test-Prereq-Meta/issues>, or in
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
