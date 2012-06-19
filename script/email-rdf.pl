#!perl

package My::Email::Folder;

use strict;
use warnings FATAL => 'all';

use Carp               ();
use Getopt::Long       ();
use Term::ReadPassword ();
use Pod::Usage         ();

use base qw(Email::Folder);
use Email::MIME ();
use Email::RDF  ();

sub bless_message {
    Email::MIME->new($_[1]);
}

package main;

use Path::Class ();

Getopt::Long::Configure(qw(no_ignore_case));

my %options = (
#    attach => '/tmp/
);

Getopt::Long::GetOptions(
    'a|attachments=s' => \$options{attach},
);

my $emrdf = Email::RDF->new(%options);

for my $f (@ARGV) {
    my $folder = My::Email::Folder->new($f) or die $!;

    $emrdf->add($folder->messages);
}

binmode STDOUT, ':utf8';
print $emrdf->serialize('rdfxml');

=head1 NAME

email-rdf - Convert email to RDF

=head1 SYNOPSIS

    email-rdf [OPTIONS...] [FOLDERS]

=head1 DESCRIPTION

C<email-rdf> will turn an email message or folder into an RDF graph.

=head1 OPTIONS


=head1 AUTHOR

Dorian Taylor, C<< <dorian at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-email-rdf at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Email-RDF>.  I will
be notified, and then you'll automatically be notified of progress on
your bug as I make changes.


=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Email::RDF

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Email-RDF>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Email-RDF>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Email-RDF>

=item * Search CPAN

L<http://search.cpan.org/dist/Email-RDF/>

=back

=head1 LICENSE AND COPYRIGHT

Copyright 2012 L<Dorian Taylor|http://doriantaylor.com/>.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    L<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
