package Test::Prereq::Meta;

use 5.010;	# because Module::Extract::Use has this.

use strict;
use warnings;

use Carp;
use CPAN::Meta;
use Exporter qw{ import };
use File::Find;
use File::Spec;
use Module::Extract::Use;
use Module::CoreList;
use Scalar::Util ();
use Test::More 0.88;

our $VERSION = '0.000_008';

our @EXPORT_OK = qw{ all_prereq_ok file_prereq_ok prereq_ok };
our %EXPORT_TAGS = (
    all	=> \@EXPORT_OK,
);

use constant DEFAULT_PATH_TYPE	=> do {
    ( my $path_type = $File::Spec::ISA[0] ) =~ s/ .* :: //smx;
    $path_type;
};

use constant REF_ARRAY	=> ref [];

sub new {
    my ( $class, %arg ) = @_;

    # NOTE that {path_type} is unsupported, and may change or be
    # retracted without warning. I thought I needed it to support
    # argument {prune}, which is itself experimental.
    $arg{name} //= 'Prereq test: %f uses %m';
    $arg{path_type} //= DEFAULT_PATH_TYPE;
    $arg{per_file_note} //= '%f';
    $arg{perl_version} //= 'none';
    $arg{skip_name} //= 'Prereq test: %f does not use any modules';

    state $default = {
	accept	=> [],
	meta_file	=> [ qw{
	    MYMETA.json MYMETA.yml META.json META.yml } ],
	prune	=> [],
    };
    foreach my $name ( keys %{ $default } ) {
	$arg{$name} //= $default->{$name};
	my $code = __PACKAGE__->can( "__validate_$name" ) ||
	    __PACKAGE__->can( '__validate_' . ref $default->{$name} ) ||
	    sub { $_[0] };
	$code->( $name, \%arg );
    }

    my $core_modules;
    {
	local $Module::CoreList::version{none} = {};
	local $Module::CoreList::version{this} =
	    $Module::CoreList::version{$]};
	$core_modules = $Module::CoreList::version{$arg{perl_version}}
	    or croak( "Unknown 'perl_version' $arg{perl_version}" );
    }

    # The below is pretty much verbatim from the CPAN::Meta synopsis

    my $meta_data = $arg{_meta_file};

    my %requires;

    my $prereqs = $meta_data->effective_prereqs();
    foreach my $phase ( qw{ configure build test runtime } ) {
	my $reqs = $prereqs->requirements_for( $phase, 'requires' );
	foreach my $module ( $reqs->required_modules() ) {
	    $requires{$module} = 1;
	}
    }

    # The above is pretty much verbatim from the CPAN::Meta synopsis

    my $provides = $meta_data->provides() || do {
	require Module::Metadata;
	Module::Metadata->provides(
	    version	=> 2,
	    dir		=> 'blib/lib',
	);
    };

    my %has = map { $_ => 1 }
	@{ $arg{accept} },
	keys %{ $core_modules },
	keys %{ $provides },
	keys %requires,
	;
    delete $has{perl};

    return bless {
	# accept		=> $arg{accept},
	# core_modules	=> $core_modules,
	has		=> \%has,
	meta_file	=> $arg{meta_file},
	meta_data	=> $meta_data,
	name		=> $arg{name},
	per_file_note	=> $arg{per_file_note},
	perl_version	=> $arg{perl_version},
	prune		=> $arg{prune},
	# provides	=> $provides,
	# requires	=> \%requires,
	skip_name	=> $arg{skip_name},
	_normalize_path	=> $arg{_normalize_path},
    }, ref $class || $class;
}

sub all_prereq_ok {
    my ( $self, @file ) = _unpack_args( @_ );
    @file
	or @file = grep { -d } qw{ blib/arch blib/lib blib/script t };

    my $need_skip = 1;
    my $ok = 1;

    File::Find::find(
	{
	    wanted	=> sub {
		if ( $self->{_normalize_path} ) {
		    $self->{_normalize_path}->();
		    if ( $self->{prune}{$_} ) {
			$File::Find::prune = 1;
			return;
		    }
		}
		_is_perl( $_ )
		    or return;
		$need_skip = 0;
		$self->file_prereq_ok( $_ )
		    or $ok = 0;
		return;
	    },
	    no_chdir	=> 1,
	    preprocess	=> sub { return( sort @_ ) },
	},
	@file,
    );

    if ( $need_skip ) {
	state $TEST = Test::More->builder();
	local $Test::Builder::Level = _nest_depth();
	# $TEST->skip( "$file does not use any modules" );
	$TEST->skip( 'No Perl files found' );
    }

    return $ok;
}

sub file_prereq_ok {
    my ( $self, $file, @arg ) = _unpack_args( @_ );
    @arg
	and confess(
	'Usage: $tpm->file_prereq_ok( $file ) or file_prereq_ok( $file )' );

    # Because this gets us a pre-built object I use $Test::Builder::Level
    # (localized) to get tests reported relative to the correct file and
    # line, rather than setting the 'level' attribute.
    state $TEST = Test::More->builder();

    if ( $self->{per_file_note} ne '' ) {
	# We are not interested in the actual test number, but we need
	# to know how many digits it is so that the note can be indented
	# properly.
	$TEST->note( '' );
	$TEST->note(
	    ' ' x ( 4 + length( $TEST->current_test() + 1 ) ),
	    _format(
		$self->{per_file_note},
		{
		    f	=> $file,
		}
	    ),
	);
    }

    my $need_skip = 1;
    my $ok = 1;
    my %module_found;

    state $extor = Module::Extract::Use->new();

    foreach my $usage (
	sort { $a->{module} cmp $b->{module} }
	@{ $extor->get_modules_with_details( $file ) }
    ) {
	local $Test::Builder::Level = _nest_depth();
	my $module = $usage->{module};

	# The following is needed because Module::Extract::Use tries too
	# hard to find return() statements embedded in other statements.
	$module =~ m/ \A [\w:]+ \z /smx
	    or next;

	# The following is needed because Module::Extract::Use returns
	# duplicate 'require' statements because it finds them both in
	# the scan for PPI::Statement::Include objects and in the scan
	# for PPI::Token::Word 'require' objects.
	$module_found{$module}++
	    and next;

	$need_skip = 0;
	$TEST->ok(
	    $self->{has}{$module} || 0,
	    _format(
		$self->{name},
		{
		    f	=> $file,
		    m	=> $module,
		},
	    ),
	) or $ok = 0;

    }

    if ( $need_skip ) {
	local $Test::Builder::Level = _nest_depth();
	# $TEST->skip( "$file does not use any modules" );
	$TEST->skip( _format(
		$self->{skip_name},
		{
		    f	=> $file,
		},
	    ),
	);
    }

    return $ok;
}

sub _format {
    my ( $tplt, $sub ) = @_;
    $tplt =~ s| % ( . ) | $sub->{$1} // $1 |smxge;
    return $tplt;
}

sub prereq_ok {
    my ( $perl_version, $name, $accept ) = @_;
    my $self = __PACKAGE__->new(
	accept		=> $accept,
	name		=> $name,
	perl_version	=> $perl_version // $],
    );
    return $self->all_prereq_ok();
}

sub _is_perl {
    my ( $file ) = @_;
    -T $file
	or return 0;
    $file =~ m/ [.] (?: (?i: pl ) | pm | t ) \z /smx
	and return 1;
    open my $fh, '<', $file
	or return 0;
    local $_ = <$fh>;
    close $fh;
    defined
	or return 0;
    return m/ \A [#]! .* perl /smx;
}

{
    my %ignore;
    BEGIN {
	%ignore = map { $_ => 1 } __PACKAGE__, qw{ DB File::Find };
    }

    sub _nest_depth {
	my $nest = 0;
	$nest++ while $ignore{ caller( $nest ) || '' };
	return $nest;
    }
}

# All the __normalize_path_* subroutines operate on $_. They take no
# arguments and return nothing relevant. The names are File::Spec::
# OS-specific class names, and the intent is that anything supported by
# File::Spec should appear here.

sub __normalize_path_AmigaOS {}	# Assumed based on File::Spec::AmigaOS

sub __normalize_path_Cygwin {}	# I believe.

sub __normalize_path_OS2 { s| \\ |/|smxg; }	## no critic (RequireFinalReturn)

sub __normalize_path_Unix {}

sub __normalize_path_VMS {
    croak( 'Can not normalize VMS paths' );
}

sub __normalize_path_Win32 { s| \\ |/|smxg; }	## no critic (RequireFinalReturn)

sub _unpack_args {
    my @arg = @_;
    my $self = ( ref( $arg[0] ) && ref( $arg[0] )->isa( __PACKAGE__ ) ) ?
	shift @arg :
	__PACKAGE__->new();
    return ( $self, @arg );
}

sub __validate_meta_file {
    my ( $name, $arg ) = @_;
    if ( Scalar::Util::blessed( $arg->{$name} ) &&
	$arg->{$name}->isa( 'CPAN::Meta' )
    ) {
	$arg->{"_$name"} = $arg->{$name};
	return;
    }
    __validate_ARRAY( $name, $arg );
    @{ $arg->{$name} }
	or croak( "'$name' must specify at least one file" );
    foreach my $fn ( @{ $arg->{$name} } ) {
	-r $fn
	    or next;
	$arg->{$name} = $fn;
	$arg->{"_$name"} = CPAN::Meta->load_file( $fn );
	return;
    }
    1 == @{ $arg }
	and croak( "$arg->{$name}[0] not readable" );
    local $" = ', ';
    croak( "None of @{ $arg->{$name} } readable" );
}

sub __validate_prune {
    my ( $name, $arg ) = @_;
    __validate_ARRAY( $name, $arg );
    my %rslt;
    foreach ( @{ $arg->{$name} } ) {
	$arg->{_normalize_path} ||= __PACKAGE__->can(
	    "__normalize_path_$arg->{path_type}" )
	|| croak( "Invalid path type '$arg->{path_type}'" );
	$arg->{_normalize_path}->();
	$rslt{$_} = 1;
    }
    $arg->{_normalize_path} ||= undef;
    $arg->{$name} = \%rslt;
    return;
}

sub __validate_ARRAY {
    my ( $name, $arg ) = @_;
    ref $arg->{$name}
	or $arg->{$name} = [ $arg->{$name} ];
    REF_ARRAY eq ref $arg->{$name}
	or croak( "'$name' must be a SCALAR or an ARRAY reference" );
    return;
}

1;

__END__

=head1 NAME

Test::Prereq::Meta - Test distribution prerequisites against CPAN meta data file.

=head1 SYNOPSIS

 use Test::More 0.88; # For done_testing();
 use Test::Prereq::Meta qw{ prereq_ok };
 
 prereq_ok();
 
 done_testing();

=head1 DESCRIPTION

This Perl module tests whether a Perl module or file's prerequisites are
all accounted for in the meta data for its distribution. It was inspired
by Brian D. Foy's L<Test::Prereq|Test::Prereq>, and like it uses
L<Module::Extract::Use|Module::Extract::Use> to determine what modules a
given Perl script/module needs. But unlike L<Test::Prereq|Test::Prereq>
this module loads prerequisites from the distribution's meta data file
(hence this module's name) using L<CPAN::Meta|CPAN::Meta>, and is thus
independent of the distribution's build mechanism.

Each file tested has a test generated for each distinct module used. If
a file uses no modules, a skipped test is generated.

B<Note> that this package requires Perl 5.10, a requirement it inherits
from L<Module::Extract::Use|Module::Extract::Use>. If you are writing a
test that might be run under an older Perl, you would need to do
something like the following, which works if the prerequisite test is in
its own file:

 use Test::More 0.88; # For done_testing();
 
 "$]" >= 5.010
   or plan skip_all => 'Perl 5.10 or higher required';
 require Test::Prereq::Meta;
 Test::Prereq::Meta->import( 'prereq_ok' );
 
 prereq_ok();
 
 done_testing();

There are no exports by default, but anything so documented can be
exported, and export tag C<:all> exports everything exportable.

=head1 METHODS

This class supports the following public methods:

=head2 new

 my $tpm = Test::Prereq::Meta->new();

This static method instantiates the test object and reads in the meta
data that contain the prerequisites. It accepts the following arguments
as name/value pairs:

=over

=item accept

This argument is the name of a module, or a reference to an array of
module names. These modules will be passed even if they are not listed
as prerequisites.

The default is C<[]>, that is, a reference to an empty array.

=item meta_file

This argument specifies the name of the file that contains the meta
data, or a reference to an array of file names. In the latter case the
first file that is readable will be used.

The default is

 [ qw{ MYMETA.json MYMETA.yml META.json META.yml } ]

An exception will be thrown if none of the specified files is readable.

This argument can also be a L<CPAN::Meta|CPAN::Meta> object.

B<Caveat:> I am unsure what the correct search order should be among the
meta files. For the purposes of this module F<.json> and F<.yml> should
provide equivalent information, but F<MYMETA.*> may not be equivalent to
F<META.*>. I have placed F<MYMETA.*> first because it is easier to
regenerate, but this may change if some compelling reason to change
emerges.

=item name

This argument specifies the name of the tests generated.

Selected data can be substituted into the given name for each specific
test. Substitutions are introduced by the C<'%'> character. The
following substitutions are defined:

=over

=item C<'%f'> substitutes the name of the file being tested;

=item C<'%m'> substitutes the name of the module being required;

=item C<'%%'> substitutes a literal C<'%'>.

=back

All other substitutions are undefined in the formal sense that the
author makes no commitment as to what they do, and whatever they do the
author reserves the right to change it without notice.

The default value is C<'Prereq test: %f uses %m'>.

=item per_file_note

This argument specifies the note to be inserted before the tests of each
file. A value of C<''> suppresses the note.  This argument defines the
same substitutions as L<name|/name>, except that C<%m> is undefined.

This note will be indented so as to align with the names of subsequent
tests, if any.

The default value is C<'%f'>.

=item perl_version

This argument specifies the version of Perl whose core modules are to be
accepted even if they are not listed as prerequisites. This version
should be specified as it appears in C<$]> or (more to the point) as it
is expected by L<Module::CoreList|Module::CoreList>.

The following special-case versions are also provided:

=over

=item none

This has the effect of requiring core modules to be included in the
prerequisites unless otherwise exempted (e.g. by being included in the
L<'accept'|/accept> list).

=item this

This specifies the version of Perl that is running the test, and is
equivalent to specifying the value of C<$]>.

=back

The default is C<'none'>.

=item prune

This argument should be considered B<experimental>. There are obvious
portability issues, and VMS is currently unsupported because I have no
such platform on which to develop or test. It may become necessary to
change this in incompatible ways with little (or no) notice, or retract
it completely. B<Caveat coder.>

This argument specifies the names of files to prune from the scan done
by L<all_prereq_ok()|/all_prereq_ok>. The specification is in POSIX
form, relative to the distribution directory. In the case of directories
(which is the anticipated use) all files in the directory will also be
ignored.  A single file can be specified as a scalar; otherwise the
value is a reference to an array of file names.

The specifications are matched against the file names reported by
L<File::Find|File::Find> (normalized to POSIX form).

The default is C<[]>, i.e. prune nothing.

=item skip_name

This argument specifies the name of any skipped tests. It defines the
same substitutions as L<name|/name>, except that C<%m> is undefined.

The default value is C<'Prereq test: %f does not use any modules'>.

=back

=head2 all_prereq_ok

 $tpm->all_prereq_ok()

This method takes as arguments one or more file names, which are
searched by L<File::Find|File::Find>. Any Perl files found are passed to
L<file_prereq_ok()|/file_prereq_ok>. Perl files are defined as text
files whose names end in F<.PL> (case-insensitive), F<.pm>
(case-sensitive), or F<.t> (case-sensitive), or text files having a
shebang line which contains the string C<'perl'>.

This method returns a true value if all tests either passed or skipped,
or a false value if any test failed.

If no arguments are specified, the arguments default to

 ( qw{ blib/arch blib/lib blib/script t } )

This method can also be exported and called as a subroutine, in which
case it functions as though its invocant were the default object, i.e.
one instantiated with no arguments.

=head2 file_prereq_ok

 $tpm->file_prereq_ok( $file_name );

This method takes as argument exactly one file name. This is assumed to
be a Perl file, and all modules required by it (by C<require()>,
C<use()>, C<use base>, or C<use parent>) are checked against the
requirements specified in the meta data, in ASCIIbetical order. Modules
which appear in the prerequisites generate a passing test. Modules which
do not appear in the prerequisites generate a failing test. If the file
does not require any modules, a skipped test is generated.

This method returns a true value if all tests either passed or skipped,
or a false value if any test failed.

This method can also be exported and called as a subroutine, in which
case it functions as though its invocant were the default object, i.e.
one instantiated with no arguments.

=head2 prereq_ok

 prereq_ok( $perl_version, $name, $accept );

This subroutine (B<not> method) is intended to correspond to the
same-named subroutine in F<Test::Prereq|Test::Prereq>, and takes the
same arguments. Unlike L<new()|/new>, C<$perl_version> defaults to C<$]>
for compatibility with L<Test::Prereq|Test::Prereq>.

This subroutine returns a true value if all tests either passed or
skipped, or a false value if any test failed.

=head1 BUGS/RESTRICTIONS

This module relies on Brian D. Foy's
L<Module::Extract::Use|Module::Extract::Use> to determine what modules a
given Perl source file requires. This in turn relies on the L<PPI|PPI>
parse of the file, that is, on a static analysis.

This means that things like C<require $some_file> or
C<eval "require $some_module"> will be missed.

It also means that this module relies on
L<Module::Extract::Use|Module::Extract::Use>'s idea of what statements
cause a module to be loaded. As of this writing this appears to be
anything that parses as a
L<PPI::Statement::Include|PPI::Statement::Include> (with special-case
code for C<use base> and C<use parent>), plus a good effort to find
C<require()> calls that are embedded in other statements.

This module uses the meta data C<'provides'> information to determine
what modules are provided by the distribution. If this is absent, it
uses L<Module::Metadata|Module::Metadata> to determine provided modules
directly from F<blib/lib/>.

=head1 SEE ALSO

L<Test::Dependencies|Test::Dependencies> by Erik Huelsmann, which
expects to receive dependencies in a L<CPAN::Meta|CPAN::Meta> object,
and scans a list of files provided by the user. Internally, it uses
L<Pod::Strip|Pod::Strip> to remove POD, and then regular expressions to
find C<use()>, C<with()>, C<extends()>, and C<use base> statements.

L<Test::Prereq|Test::Prereq> by Brian D. Foy, which intercepts the
L<ExtUtils::MakeMaker|ExtUtils::MakeMaker> C<WriteMakefile()>, or
L<Test::Prereq::Build|Test::Prereq::Build> (same distribution) which
intercepts L<Module::Build|Module::Build> C<new()> calls to figure out
what dependencies have been declared. It uses
L<Module::Extract::Use|Module::Extract::Use> (and ultimately L<PPI|PPI>)
to find C<use()>, C<no()>, C<require()>, C<use base> and C<use parent>
statements.

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
